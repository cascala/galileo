//! `galileo mcp`: a Model Context Protocol server, so AI assistants (Claude Desktop,
//! Claude Code, any MCP client) can run exact math in Galileo.
//!
//! The server speaks JSON-RPC 2.0 over stdin/stdout, one message per line. It offers
//! the tools `execute` (run code in a session that keeps its definitions), `inspect`
//! (help for a command), `complete` (names that start with a prefix) and `reset`
//! (start over), and the language and command references as resources.
//!
//! The engine does not run in the server process. It runs in a worker process
//! (`galileo mcp --worker`) that the server starts, so a run that takes too long or
//! uses too much memory is stopped by stopping the worker:
//!
//! * time: each `execute` has a time limit (`--timeout`, 30 s by default); past it the
//!   worker is stopped, a new one is started and the earlier cells are run again in it,
//!   so the session keeps its definitions;
//! * memory: the worker's address space is limited (`--memory`, 2048 MB by default;
//!   enforced on Linux, so in Docker);
//! * files: the worker has no access to the disk. Its files live in memory, unless
//!   `--dir` names a folder: then that folder is the worker's "/" and the only place
//!   it can read or write.

use crate::display::{Shown, shown};
use crate::eval::{Output, Session, comment_notes};
use crate::expr::{Expr, Obj};
use crate::help;
use crate::plot::{base64, jstr};
use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const PROTOCOLS: [&str; 4] = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"];

/// The most text one `execute` hands back, in characters
const MAX_TEXT: usize = 100_000;

// ---------------------------------------------------------------- JSON

/// A JSON value. Objects keep their keys in order.
#[derive(Clone, Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Arr(Vec<Json>),
    Obj(Vec<(String, Json)>),
}

impl Json {
    pub fn parse(s: &str) -> Result<Json, String> {
        let mut p = JsonParser {
            s: s.as_bytes(),
            i: 0,
        };
        let v = p.value()?;
        p.ws();
        if p.i != p.s.len() {
            return Err(format!("unexpected text at {}", p.i));
        }
        Ok(v)
    }

    pub fn get(&self, k: &str) -> Option<&Json> {
        match self {
            Json::Obj(fields) => fields.iter().find(|(n, _)| n == k).map(|(_, v)| v),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Json::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Json::Num(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Json::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn dump(&self) -> String {
        match self {
            Json::Null => "null".into(),
            Json::Bool(b) => b.to_string(),
            Json::Num(x) if !x.is_finite() => "null".into(),
            Json::Num(x) if x.fract() == 0.0 && x.abs() < 9e15 => format!("{}", *x as i64),
            Json::Num(x) => format!("{}", x),
            Json::Str(s) => jstr(s),
            Json::Arr(v) => format!(
                "[{}]",
                v.iter().map(Json::dump).collect::<Vec<_>>().join(",")
            ),
            Json::Obj(f) => format!(
                "{{{}}}",
                f.iter()
                    .map(|(k, v)| format!("{}:{}", jstr(k), v.dump()))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        }
    }
}

fn obj(fields: Vec<(&str, Json)>) -> Json {
    Json::Obj(
        fields
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect(),
    )
}

fn s(t: &str) -> Json {
    Json::Str(t.to_string())
}

struct JsonParser<'a> {
    s: &'a [u8],
    i: usize,
}

impl JsonParser<'_> {
    fn ws(&mut self) {
        while self.i < self.s.len() && matches!(self.s[self.i], b' ' | b'\t' | b'\n' | b'\r') {
            self.i += 1;
        }
    }

    fn eat(&mut self, lit: &str) -> bool {
        if self.s[self.i..].starts_with(lit.as_bytes()) {
            self.i += lit.len();
            true
        } else {
            false
        }
    }

    fn value(&mut self) -> Result<Json, String> {
        self.ws();
        match self.s.get(self.i) {
            None => Err("unexpected end of JSON".into()),
            Some(b'{') => {
                self.i += 1;
                let mut fields = vec![];
                self.ws();
                if self.eat("}") {
                    return Ok(Json::Obj(fields));
                }
                loop {
                    self.ws();
                    let k = match self.value()? {
                        Json::Str(k) => k,
                        _ => return Err("object keys must be strings".into()),
                    };
                    self.ws();
                    if !self.eat(":") {
                        return Err(format!("expected : at {}", self.i));
                    }
                    let v = self.value()?;
                    fields.push((k, v));
                    self.ws();
                    if self.eat(",") {
                        continue;
                    }
                    if self.eat("}") {
                        return Ok(Json::Obj(fields));
                    }
                    return Err(format!("expected , or }} at {}", self.i));
                }
            }
            Some(b'[') => {
                self.i += 1;
                let mut items = vec![];
                self.ws();
                if self.eat("]") {
                    return Ok(Json::Arr(items));
                }
                loop {
                    items.push(self.value()?);
                    self.ws();
                    if self.eat(",") {
                        continue;
                    }
                    if self.eat("]") {
                        return Ok(Json::Arr(items));
                    }
                    return Err(format!("expected , or ] at {}", self.i));
                }
            }
            Some(b'"') => {
                self.i += 1;
                self.string().map(Json::Str)
            }
            Some(b't') if self.eat("true") => Ok(Json::Bool(true)),
            Some(b'f') if self.eat("false") => Ok(Json::Bool(false)),
            Some(b'n') if self.eat("null") => Ok(Json::Null),
            Some(c) if *c == b'-' || c.is_ascii_digit() => {
                let start = self.i;
                while self.i < self.s.len()
                    && matches!(
                        self.s[self.i],
                        b'-' | b'+' | b'.' | b'e' | b'E' | b'0'..=b'9'
                    )
                {
                    self.i += 1;
                }
                let t = std::str::from_utf8(&self.s[start..self.i]).unwrap_or("");
                t.parse()
                    .map(Json::Num)
                    .map_err(|_| format!("bad number {}", t))
            }
            Some(_) => Err(format!("unexpected character at {}", self.i)),
        }
    }

    fn hex4(&mut self) -> Result<u32, String> {
        let t = self.s.get(self.i..self.i + 4).ok_or("bad \\u escape")?;
        let t = std::str::from_utf8(t).map_err(|_| "bad \\u escape")?;
        self.i += 4;
        u32::from_str_radix(t, 16).map_err(|_| "bad \\u escape".into())
    }

    fn string(&mut self) -> Result<String, String> {
        let mut out: Vec<u8> = vec![];
        loop {
            let c = *self.s.get(self.i).ok_or("unterminated string")?;
            self.i += 1;
            match c {
                b'"' => return String::from_utf8(out).map_err(|_| "invalid UTF-8".into()),
                b'\\' => {
                    let e = *self.s.get(self.i).ok_or("unterminated string")?;
                    self.i += 1;
                    let ch = match e {
                        b'"' => '"',
                        b'\\' => '\\',
                        b'/' => '/',
                        b'b' => '\u{8}',
                        b'f' => '\u{c}',
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        b'u' => {
                            let hi = self.hex4()?;
                            let code = if (0xd800..0xdc00).contains(&hi) && self.eat("\\u") {
                                let lo = self.hex4()?;
                                0x10000 + ((hi - 0xd800) << 10) + (lo.wrapping_sub(0xdc00) & 0x3ff)
                            } else {
                                hi
                            };
                            char::from_u32(code).unwrap_or('\u{fffd}')
                        }
                        _ => return Err("bad escape".into()),
                    };
                    let mut buf = [0u8; 4];
                    out.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
                }
                c => out.push(c),
            }
        }
    }
}

// ---------------------------------------------------------------- options

#[derive(Clone, Debug)]
struct Options {
    timeout: f64,
    memory_mb: u64,
    dir: Option<PathBuf>,
}

const USAGE: &str = "usage: galileo mcp [--dir FOLDER] [--timeout SECONDS] [--memory MB]\n\
\n  Serves the Model Context Protocol on stdin/stdout, for AI assistants.\n\
\x20 --dir FOLDER       the folder Galileo may read and write (seen as /); without it,\n\
\x20                    files live in memory and the disk is out of reach\n\
\x20 --timeout SECONDS  time limit for one execute (default 30)\n\
\x20 --memory MB        memory limit for the engine (default 2048, at least 1024)";

fn options(args: &[String]) -> Result<Options, String> {
    let mut o = Options {
        timeout: 30.0,
        memory_mb: 2048,
        dir: None,
    };
    let mut it = args.iter();
    while let Some(a) = it.next() {
        let mut val = || {
            it.next()
                .cloned()
                .ok_or_else(|| format!("{} needs a value", a))
        };
        match a.as_str() {
            "--timeout" => {
                o.timeout = val()?
                    .parse::<f64>()
                    .ok()
                    .filter(|t| *t > 0.0)
                    .ok_or("--timeout takes a positive number of seconds")?
            }
            "--memory" => {
                o.memory_mb = val()?
                    .parse::<u64>()
                    .ok()
                    .filter(|m| *m >= 1024)
                    .ok_or("--memory takes a number of megabytes, at least 1024")?
            }
            "--dir" => {
                let d = PathBuf::from(val()?);
                let d = d
                    .canonicalize()
                    .map_err(|e| format!("--dir {}: {}", d.display(), e))?;
                if !d.is_dir() {
                    return Err(format!("--dir {}: not a folder", d.display()));
                }
                o.dir = Some(d);
            }
            other => return Err(format!("unknown option {}\n{}", other, USAGE)),
        }
    }
    Ok(o)
}

/// `galileo mcp ...`: the server, or (with --worker) the engine process it starts
pub fn main(args: &[String]) -> i32 {
    if matches!(
        args.first().map(String::as_str),
        Some("-h") | Some("--help")
    ) {
        println!("{}", USAGE);
        return 0;
    }
    let worker = args.first().map(String::as_str) == Some("--worker");
    let rest = if worker { &args[1..] } else { args };
    let o = match options(rest) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("galileo mcp: {}", e);
            return 2;
        }
    };
    if worker {
        run_worker(&o);
    } else {
        serve(o);
    }
    0
}

// ---------------------------------------------------------------- worker

#[cfg(unix)]
fn limit_memory(mb: u64) {
    #[repr(C)]
    struct RLimit {
        cur: u64,
        max: u64,
    }
    unsafe extern "C" {
        fn setrlimit(resource: i32, rlim: *const RLimit) -> i32;
    }
    #[cfg(target_os = "linux")]
    const RLIMIT_AS: i32 = 9;
    #[cfg(not(target_os = "linux"))]
    const RLIMIT_AS: i32 = 5;
    let bytes = mb.saturating_mul(1 << 20);
    let r = RLimit {
        cur: bytes,
        max: bytes,
    };
    // best effort: macOS may refuse it
    unsafe {
        setrlimit(RLIMIT_AS, &r);
    }
}

#[cfg(not(unix))]
fn limit_memory(_mb: u64) {}

/// The worker's view of a --dir folder: files mirrored into the in-memory files
/// before each run, and the engine's writes copied back after it
struct Folder {
    root: PathBuf,
    /// "/a/b.csv" -> (modified, size) as last seen
    seen: BTreeMap<String, (Option<std::time::SystemTime>, u64)>,
}

const FOLDER_FILE_MAX: u64 = 32 << 20;
const FOLDER_TOTAL_MAX: u64 = 256 << 20;

impl Folder {
    fn walk(
        &self,
        dir: &Path,
        rel: &str,
        depth: usize,
        found: &mut Vec<(String, PathBuf, std::fs::Metadata)>,
        dirs: &mut Vec<String>,
    ) {
        if depth > 8 {
            return;
        }
        let Ok(rd) = std::fs::read_dir(dir) else {
            return;
        };
        for e in rd.flatten() {
            let name = e.file_name().to_string_lossy().into_owned();
            // symbolic links could lead out of the folder: skip them
            let Ok(md) = std::fs::symlink_metadata(e.path()) else {
                continue;
            };
            let r = format!("{}/{}", rel, name);
            if md.is_dir() {
                if !name.starts_with('.') {
                    dirs.push(r.clone());
                    self.walk(&e.path(), &r, depth + 1, found, dirs);
                }
            } else if md.is_file() {
                found.push((r, e.path(), md));
            }
        }
    }

    fn sync_in(&mut self) {
        let mut found = vec![];
        let mut dirs = vec![];
        self.walk(&self.root.clone(), "", 0, &mut found, &mut dirs);
        for d in &dirs {
            crate::vfs::mkdir(d);
        }
        let mut total = 0u64;
        let mut now = BTreeMap::new();
        for (r, path, md) in found {
            let key = (md.modified().ok(), md.len());
            if md.len() > FOLDER_FILE_MAX || total + md.len() > FOLDER_TOTAL_MAX {
                crate::vfs::put_stub(&r, "too large to open here");
                now.insert(r, key);
                continue;
            }
            total += md.len();
            if self.seen.get(&r) != Some(&key) {
                if let Ok(bytes) = std::fs::read(&path) {
                    crate::vfs::put(&r, bytes);
                }
            }
            now.insert(r, key);
        }
        for gone in self.seen.keys().filter(|k| !now.contains_key(*k)) {
            crate::vfs::unput(gone);
        }
        self.seen = now;
    }

    /// The real path for an in-memory one, if no part of it is a symbolic link
    fn real(&self, p: &str) -> Option<PathBuf> {
        let mut at = self.root.clone();
        for part in p.split('/').filter(|x| !x.is_empty()) {
            if part == ".." {
                return None;
            }
            at.push(part);
            if let Ok(md) = std::fs::symlink_metadata(&at) {
                if md.file_type().is_symlink() {
                    return None;
                }
            }
        }
        Some(at)
    }

    fn sync_out(&mut self) {
        use crate::vfs::Change;
        for (c, data) in crate::vfs::take_changes() {
            match c {
                Change::Write(p) => {
                    let (Some(path), Some(bytes)) = (self.real(&p), data) else {
                        continue;
                    };
                    if let Some(parent) = path.parent() {
                        let _ = std::fs::create_dir_all(parent);
                    }
                    if std::fs::write(&path, &bytes).is_ok() {
                        if let Ok(md) = std::fs::metadata(&path) {
                            self.seen.insert(p, (md.modified().ok(), md.len()));
                        }
                    }
                }
                Change::Remove(p) => {
                    if let Some(path) = self.real(&p) {
                        let _ = std::fs::remove_file(path);
                    }
                    self.seen.remove(&p);
                }
            }
        }
    }
}

/// A plot as a PNG, for the assistant to look at
fn png_of(e: &Expr) -> Option<Vec<u8>> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Figure(_) | Obj::Figure3D(_) | Obj::Graph(_) => {
                crate::files::picture(e, "png").ok()
            }
            Obj::Report(r) => r.shown_value().and_then(|v| png_of(&v)),
            _ => None,
        },
        _ => None,
    }
}

/// Run a cell in the worker: {"outputs": [...], "error": null | {"name", "message"}}
fn worker_execute(sess: &mut Session, code: &str, plot_data: bool) -> Json {
    let mut outputs = vec![];
    let mut error = Json::Null;
    for (r, quiet, notes) in sess.run_notes(code) {
        match r {
            Ok(o) => {
                if quiet {
                    continue;
                }
                let mut f: Vec<(&str, Json)> = vec![];
                match (&o, shown(&o)) {
                    (Output::Exit, _) => f.push((
                        "text",
                        s("exit does nothing here; use the reset tool to start over"),
                    )),
                    (_, Shown::Nothing) => {}
                    (_, Shown::Text(t)) => f.push(("text", Json::Str(t))),
                    (_, Shown::Rich(b)) => {
                        let get = |k: &str| b.iter().find(|(m, _)| *m == k).map(|(_, v)| v.clone());
                        f.push(("text", Json::Str(get("text/plain").unwrap_or_default())));
                        if let Some(l) = get("text/latex") {
                            f.push(("latex", Json::Str(l)));
                        }
                        if let Output::Value(v) = &o {
                            if let Some(png) = png_of(v) {
                                f.push(("png", Json::Str(base64(&png))));
                            }
                        }
                        if plot_data {
                            if let Some(svg) = get("image/svg+xml") {
                                f.push(("svg", Json::Str(svg)));
                            }
                            if let Some(p) = get("application/vnd.plotly.v1+json") {
                                f.push(("plotly", Json::parse(&p).unwrap_or(Json::Str(p))));
                            }
                        }
                    }
                }
                if !notes.is_empty() {
                    f.push(("notes", Json::Str(comment_notes(&notes))));
                }
                if !f.is_empty() {
                    outputs.push(obj(f));
                }
            }
            Err(e) => {
                let (name, msg) = match e.strip_prefix("parse error: ") {
                    Some(m) => ("SyntaxError", m.to_string()),
                    None => ("Error", e.clone()),
                };
                error = obj(vec![("name", s(name)), ("message", Json::Str(msg))]);
            }
        }
    }
    obj(vec![("outputs", Json::Arr(outputs)), ("error", error)])
}

fn worker_complete(sess: &Session, prefix: &str) -> Json {
    let mut names: Vec<String> = help::names().iter().map(|n| n.to_string()).collect();
    names.extend(crate::geometry::TEMPLATES.iter().map(|n| n.to_string()));
    names.extend(sess.variable_names());
    names.sort();
    names.dedup();
    names.retain(|n| n.starts_with(prefix));
    obj(vec![(
        "matches",
        Json::Arr(names.into_iter().map(Json::Str).collect()),
    )])
}

/// The worker: requests "<op> <length>\n<body>" on stdin, one JSON line back for each
fn run_worker(o: &Options) {
    limit_memory(o.memory_mb);
    crate::vfs::use_memory();
    let mut folder = o.dir.clone().map(|root| Folder {
        root,
        seen: BTreeMap::new(),
    });
    let mut session = Session::new();
    let stdin = std::io::stdin();
    let mut input = stdin.lock();
    let stdout = std::io::stdout();
    loop {
        let mut header = String::new();
        if input.read_line(&mut header).unwrap_or(0) == 0 {
            return;
        }
        let parts: Vec<&str> = header.split_whitespace().collect();
        let Some(op) = parts.first().copied() else {
            continue;
        };
        let len: usize = parts.last().and_then(|n| n.parse().ok()).unwrap_or(0);
        let mut buf = vec![0u8; len];
        if input.read_exact(&mut buf).is_err() {
            return;
        }
        let body = String::from_utf8_lossy(&buf).into_owned();
        let reply = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| match op {
            "execute" | "execute+plot" => {
                if let Some(f) = folder.as_mut() {
                    f.sync_in();
                }
                let r = worker_execute(&mut session, &body, op == "execute+plot");
                if let Some(f) = folder.as_mut() {
                    f.sync_out();
                }
                r
            }
            "complete" => worker_complete(&session, &body),
            _ => obj(vec![("error", s("unknown request"))]),
        }))
        .unwrap_or_else(|_| {
            obj(vec![
                ("outputs", Json::Arr(vec![])),
                (
                    "error",
                    obj(vec![
                        ("name", s("InternalError")),
                        ("message", s("the engine crashed on this input")),
                    ]),
                ),
            ])
        });
        let mut out = stdout.lock();
        let _ = writeln!(out, "{}", reply.dump());
        let _ = out.flush();
    }
}

// ---------------------------------------------------------------- the worker, seen from the server

enum Failure {
    Timeout,
    /// the worker stopped: out of memory, or crashed (with what it said on stderr)
    Died(String),
}

struct Worker {
    child: Child,
    stdin: ChildStdin,
    lines: Receiver<String>,
    stderr: Arc<Mutex<String>>,
}

impl Worker {
    fn start(o: &Options) -> Result<Worker, String> {
        let exe = std::env::current_exe()
            .map_err(|e| format!("cannot find the galileo program: {}", e))?;
        let mut cmd = Command::new(exe);
        cmd.args(["mcp", "--worker", "--memory", &o.memory_mb.to_string()]);
        if let Some(d) = &o.dir {
            cmd.arg("--dir").arg(d);
        }
        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("cannot start the engine: {}", e))?;
        let stdin = child.stdin.take().ok_or("no stdin")?;
        let stdout = child.stdout.take().ok_or("no stdout")?;
        let mut err = child.stderr.take().ok_or("no stderr")?;
        let (tx, lines) = channel();
        std::thread::spawn(move || {
            for line in BufReader::new(stdout).lines() {
                let Ok(line) = line else { break };
                if tx.send(line).is_err() {
                    break;
                }
            }
        });
        let stderr = Arc::new(Mutex::new(String::new()));
        let sink = stderr.clone();
        std::thread::spawn(move || {
            let mut buf = [0u8; 4096];
            while let Ok(n) = err.read(&mut buf) {
                if n == 0 {
                    break;
                }
                let mut s = sink.lock().unwrap();
                s.push_str(&String::from_utf8_lossy(&buf[..n]));
                if s.len() > 8192 {
                    let cut = s.len() - 4096;
                    let cut = (cut..s.len()).find(|i| s.is_char_boundary(*i)).unwrap_or(0);
                    s.drain(..cut);
                }
            }
        });
        Ok(Worker {
            child,
            stdin,
            lines,
            stderr,
        })
    }

    fn call(&mut self, op: &str, body: &str, timeout: Duration) -> Result<Json, Failure> {
        let req = format!("{} {}\n{}", op, body.len(), body);
        if self
            .stdin
            .write_all(req.as_bytes())
            .and_then(|_| self.stdin.flush())
            .is_err()
        {
            return Err(self.died());
        }
        match self.lines.recv_timeout(timeout) {
            Ok(line) => Json::parse(&line)
                .map_err(|e| Failure::Died(format!("bad reply from the engine: {}", e))),
            Err(RecvTimeoutError::Timeout) => Err(Failure::Timeout),
            Err(RecvTimeoutError::Disconnected) => Err(self.died()),
        }
    }

    fn died(&mut self) -> Failure {
        let _ = self.child.wait();
        // give the stderr reader a moment to collect the last words
        std::thread::sleep(Duration::from_millis(50));
        Failure::Died(self.stderr.lock().unwrap().trim().to_string())
    }

    fn stop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        self.stop();
    }
}

// ---------------------------------------------------------------- the server

struct Server {
    o: Options,
    worker: Option<Worker>,
    /// cells that ran to the end, run again after a restart
    history: Vec<String>,
}

const INSTRUCTIONS: &str = "Galileo is a computer algebra system: exact symbolic math (algebra, calculus, \
linear algebra, differential equations, number theory, groups, tensors and differential geometry), \
numerics, statistics and plots. Use the execute tool for any calculation whose answer must be exact or \
checked: it runs Galileo code in a session that keeps its variables between calls. Syntax is close to \
MATLAB/Octave: x^2, [1 2; 3 4] for matrices, A' is the conjugate transpose and A.' the plain one, \
f(x) := x^2 defines a function, a statement ending in ; prints nothing, # or // start a comment. \
Input stays as typed until simplified: calc = \"simplify\" (the default) simplifies each result, \
calc = \"eval\" also gives decimals, and simplify(e) / eval(e) do it for one expression. \
Derivatives are deriv(f, x) or d(y, t); integrals integrate(f, x) or integrate(f, x, a, b); \
solve(eq, x); plot(sin(x), x, 0, 2*pi). Results print as Galileo input, so they can be pasted back. \
Explanations come separately as notes; set explain = \"off\", \"brief\" (default) or \"teach\". \
Use inspect for help on a command (inspect with no name lists every command by topic), and \
complete to find names. The resources galileo://docs/language and galileo://docs/commands \
hold the full language and command references.";

const LANGUAGE: &str = include_str!("../docs/language.md");
const COMMANDS: &str = include_str!("../docs/builtins.md");

fn tools() -> Json {
    let schema = |props: Vec<(&str, Json)>, required: Vec<&str>| {
        obj(vec![
            ("type", s("object")),
            ("properties", obj(props)),
            ("required", Json::Arr(required.into_iter().map(s).collect())),
            ("additionalProperties", Json::Bool(false)),
        ])
    };
    let prop = |ty: &str, d: &str| obj(vec![("type", s(ty)), ("description", s(d))]);
    let tool = |name: &str, title: &str, d: &str, input: Json, read_only: bool| {
        obj(vec![
            ("name", s(name)),
            ("title", s(title)),
            ("description", s(d)),
            ("inputSchema", input),
            (
                "annotations",
                obj(vec![
                    ("readOnlyHint", Json::Bool(read_only)),
                    ("destructiveHint", Json::Bool(false)),
                    ("openWorldHint", Json::Bool(false)),
                ]),
            ),
        ])
    };
    Json::Arr(vec![
        tool(
            "execute",
            "Run Galileo code",
            "Run Galileo code and return each result as Galileo text (with LaTeX where it helps), \
             plots as images, explanations as notes, and any error. Variables and functions stay \
             defined for later calls. Several statements can be given, separated by newlines or ;.",
            schema(
                vec![
                    (
                        "code",
                        prop("string", "Galileo code, e.g. integrate(x*exp(x), x)"),
                    ),
                    (
                        "timeout",
                        prop(
                            "number",
                            "seconds this run may take, at most the server's limit",
                        ),
                    ),
                    (
                        "plot_data",
                        prop("boolean", "also return each plot's SVG and Plotly JSON"),
                    ),
                ],
                vec!["code"],
            ),
            false,
        ),
        tool(
            "inspect",
            "Help for a command",
            "Usage, description and an example for a Galileo command or name. With no name, \
             lists every command by topic.",
            schema(
                vec![("name", prop("string", "a command, e.g. solve"))],
                vec![],
            ),
            true,
        ),
        tool(
            "complete",
            "Names that start with a prefix",
            "Galileo commands and session variables whose names start with the prefix.",
            schema(
                vec![("prefix", prop("string", "the start of a name, e.g. eig"))],
                vec!["prefix"],
            ),
            true,
        ),
        tool(
            "reset",
            "Start a new session",
            "Forget every variable, function and in-memory file, and start a new session.",
            schema(vec![], vec![]),
            false,
        ),
    ])
}

fn text_block(t: &str) -> Json {
    obj(vec![("type", s("text")), ("text", s(t))])
}

fn tool_result(content: Vec<Json>, structured: Option<Json>, is_error: bool) -> Json {
    let mut f = vec![("content", Json::Arr(content))];
    if let Some(st) = structured {
        f.push(("structuredContent", st));
    }
    f.push(("isError", Json::Bool(is_error)));
    obj(f)
}

/// cut text to at most `max` characters, saying so
fn cap(t: &str, max: usize) -> String {
    match t.char_indices().nth(max) {
        None => t.to_string(),
        Some((i, _)) => format!(
            "{}\n... (output cut at {} characters; end the statement with ; to hide it)",
            &t[..i],
            max
        ),
    }
}

impl Server {
    fn worker(&mut self) -> Result<&mut Worker, String> {
        if self.worker.is_none() {
            self.worker = Some(Worker::start(&self.o)?);
        }
        Ok(self.worker.as_mut().unwrap())
    }

    /// Start a new worker and run the earlier cells in it again. Returns a sentence
    /// on what was kept.
    fn restart(&mut self) -> String {
        if let Some(mut w) = self.worker.take() {
            w.stop();
        }
        let cells = std::mem::take(&mut self.history);
        if cells.is_empty() {
            return "A new session was started.".into();
        }
        let limit = Duration::from_secs_f64(self.o.timeout);
        let deadline = Instant::now() + limit * 2;
        let mut kept = vec![];
        for c in cells {
            let left = deadline.saturating_duration_since(Instant::now());
            let ok = match self.worker() {
                Ok(w) => w.call("execute", &c, left.min(limit)).is_ok(),
                Err(_) => false,
            };
            if !ok {
                if let Some(mut w) = self.worker.take() {
                    w.stop();
                }
                return "A new session was started; the earlier definitions could not be restored, so define them again.".into();
            }
            kept.push(c);
        }
        let n = kept.len();
        self.history = kept;
        format!(
            "A new session was started and the {} earlier cell(s) were run again, so earlier definitions are kept.",
            n
        )
    }

    fn execute(&mut self, args: &Json) -> Json {
        let Some(code) = args.get("code").and_then(Json::as_str) else {
            return tool_result(vec![text_block("execute needs code")], None, true);
        };
        let secs = args
            .get("timeout")
            .and_then(Json::as_f64)
            .filter(|t| *t > 0.0)
            .map_or(self.o.timeout, |t| t.min(self.o.timeout));
        let plot = args
            .get("plot_data")
            .and_then(Json::as_bool)
            .unwrap_or(false);
        let t0 = Instant::now();
        let reply = match self.worker() {
            Ok(w) => w.call(
                if plot { "execute+plot" } else { "execute" },
                code,
                Duration::from_secs_f64(secs),
            ),
            Err(e) => return tool_result(vec![text_block(&e)], None, true),
        };
        let ms = Json::Num(t0.elapsed().as_millis() as f64);
        let r = match reply {
            Ok(r) => r,
            Err(f) => {
                let (status, msg) = match f {
                    Failure::Timeout => (
                        "timeout",
                        format!(
                            "time limit exceeded: the run took more than {} s and was stopped.",
                            secs
                        ),
                    ),
                    Failure::Died(why)
                        if why.contains("memory allocation")
                            || why.contains("out of memory")
                            || why.contains("capacity overflow") =>
                    {
                        (
                            "memory",
                            format!(
                                "memory limit exceeded ({} MB); the run was stopped.",
                                self.o.memory_mb
                            ),
                        )
                    }
                    Failure::Died(why) => (
                        "crashed",
                        format!(
                            "the engine stopped{}",
                            if why.is_empty() {
                                ".".to_string()
                            } else {
                                format!(": {}", cap(&why, 500))
                            }
                        ),
                    ),
                };
                let after = self.restart();
                let text = format!("Error: {}. {}", msg.trim_end_matches('.'), after);
                let st = obj(vec![
                    ("status", s(status)),
                    ("outputs", Json::Arr(vec![])),
                    (
                        "error",
                        obj(vec![("name", s(status)), ("message", Json::Str(msg))]),
                    ),
                    ("session", Json::Str(after.clone())),
                    ("duration_ms", ms),
                ]);
                return tool_result(vec![text_block(&text)], Some(st), true);
            }
        };
        self.history.push(code.to_string());

        // the answer as the terminal shows it: results as Galileo, notes as // comments
        let mut text = String::new();
        let mut images = vec![];
        let mut outs = vec![];
        if let Some(Json::Arr(items)) = r.get("outputs") {
            for it in items {
                let mut f: Vec<(String, Json)> = match it {
                    Json::Obj(f) => f.clone(),
                    _ => continue,
                };
                let has_png = it.get("png").is_some();
                if let Some(t) = it.get("text").and_then(Json::as_str) {
                    if has_png {
                        // the picture says it better than the text drawing does
                        text += &format!("(plot: image {})\n", images.len() + 1);
                    } else if !t.is_empty() {
                        text += t;
                        text.push('\n');
                    }
                }
                if let Some(n) = it.get("notes").and_then(Json::as_str) {
                    text += n;
                    text.push('\n');
                }
                if let Some(png) = it.get("png").and_then(Json::as_str) {
                    images.push(obj(vec![
                        ("type", s("image")),
                        ("data", s(png)),
                        ("mimeType", s("image/png")),
                    ]));
                    f.retain(|(k, _)| k != "png");
                    f.push(("image".into(), Json::Num((images.len() - 1) as f64)));
                }
                outs.push(Json::Obj(f));
            }
        }
        let error = r.get("error").cloned().unwrap_or(Json::Null);
        if let Some(m) = error.get("message").and_then(Json::as_str) {
            let name = error.get("name").and_then(Json::as_str).unwrap_or("Error");
            text += &format!(
                "{}: {}\n",
                if name == "SyntaxError" {
                    "Syntax error"
                } else {
                    "Error"
                },
                m
            );
        }
        let failed = error != Json::Null;
        let text = cap(text.trim_end(), MAX_TEXT);
        let mut content = vec![text_block(if text.is_empty() {
            "(no output)"
        } else {
            &text
        })];
        content.extend(images);
        let st = obj(vec![
            ("status", s(if failed { "error" } else { "ok" })),
            ("outputs", Json::Arr(outs)),
            ("error", error),
            ("duration_ms", ms),
        ]);
        tool_result(content, Some(st), failed)
    }

    fn inspect(&self, args: &Json) -> Json {
        let name = args.get("name").and_then(Json::as_str).unwrap_or("").trim();
        if name.is_empty() {
            return tool_result(vec![text_block(&help::help(None))], None, false);
        }
        match help::entry(name) {
            Some(e) => tool_result(
                vec![text_block(&help::markdown(e))],
                Some(obj(vec![
                    ("found", Json::Bool(true)),
                    ("usage", s(e.usage)),
                    ("description", s(e.description)),
                    ("example", s(e.example)),
                    ("topic", s(e.topic)),
                ])),
                false,
            ),
            None => tool_result(
                vec![text_block(&help::help(Some(name)))],
                Some(obj(vec![("found", Json::Bool(false))])),
                false,
            ),
        }
    }

    fn complete(&mut self, args: &Json) -> Json {
        let prefix = args.get("prefix").and_then(Json::as_str).unwrap_or("");
        let limit = Duration::from_secs(5);
        let r = match self.worker() {
            Ok(w) => w.call("complete", prefix, limit).ok(),
            Err(_) => None,
        };
        let matches = r
            .and_then(|r| r.get("matches").cloned())
            .unwrap_or(Json::Arr(vec![]));
        let list: Vec<&str> = match &matches {
            Json::Arr(v) => v.iter().filter_map(Json::as_str).collect(),
            _ => vec![],
        };
        let text = if list.is_empty() {
            format!("no names start with {}", prefix)
        } else {
            list.join(", ")
        };
        tool_result(
            vec![text_block(&text)],
            Some(obj(vec![("matches", matches)])),
            false,
        )
    }

    fn reset(&mut self) -> Json {
        if let Some(mut w) = self.worker.take() {
            w.stop();
        }
        self.history.clear();
        tool_result(vec![text_block("A new session was started.")], None, false)
    }

    fn call_tool(&mut self, params: &Json) -> Result<Json, (i64, String)> {
        let name = params.get("name").and_then(Json::as_str).unwrap_or("");
        let empty = Json::Obj(vec![]);
        let args = params.get("arguments").unwrap_or(&empty);
        Ok(match name {
            "execute" => self.execute(args),
            "inspect" => self.inspect(args),
            "complete" => self.complete(args),
            "reset" => self.reset(),
            _ => return Err((-32602, format!("unknown tool {}", name))),
        })
    }

    /// The reply to one request, or None for a notification
    fn handle(&mut self, msg: &Json) -> Option<Json> {
        let id = msg.get("id").cloned();
        let method = msg.get("method").and_then(Json::as_str).unwrap_or("");
        let empty = Json::Obj(vec![]);
        let params = msg.get("params").unwrap_or(&empty);
        let result: Result<Json, (i64, String)> = match method {
            "initialize" => {
                let asked = params
                    .get("protocolVersion")
                    .and_then(Json::as_str)
                    .unwrap_or("");
                let version = if PROTOCOLS.contains(&asked) {
                    asked
                } else {
                    PROTOCOLS[1]
                };
                Ok(obj(vec![
                    ("protocolVersion", s(version)),
                    (
                        "capabilities",
                        obj(vec![("tools", obj(vec![])), ("resources", obj(vec![]))]),
                    ),
                    (
                        "serverInfo",
                        obj(vec![
                            ("name", s("galileo")),
                            ("title", s("Galileo")),
                            ("version", s(env!("CARGO_PKG_VERSION"))),
                        ]),
                    ),
                    ("instructions", s(INSTRUCTIONS)),
                ]))
            }
            "ping" => Ok(obj(vec![])),
            "tools/list" => Ok(obj(vec![("tools", tools())])),
            "tools/call" => self.call_tool(params),
            "resources/list" => Ok(obj(vec![(
                "resources",
                Json::Arr(vec![
                    obj(vec![
                        ("uri", s("galileo://docs/language")),
                        ("name", s("language")),
                        ("title", s("Galileo language reference")),
                        (
                            "description",
                            s(
                                "How Galileo is written: statements, numbers, operators, functions, matrices, strings, lists, records and blocks",
                            ),
                        ),
                        ("mimeType", s("text/markdown")),
                    ]),
                    obj(vec![
                        ("uri", s("galileo://docs/commands")),
                        ("name", s("commands")),
                        ("title", s("Galileo command reference")),
                        (
                            "description",
                            s("Every built-in command by topic, with an example"),
                        ),
                        ("mimeType", s("text/markdown")),
                    ]),
                ]),
            )])),
            "resources/templates/list" => Ok(obj(vec![("resourceTemplates", Json::Arr(vec![]))])),
            "resources/read" => {
                let uri = params.get("uri").and_then(Json::as_str).unwrap_or("");
                let text = match uri {
                    "galileo://docs/language" => Some(LANGUAGE),
                    "galileo://docs/commands" => Some(COMMANDS),
                    _ => None,
                };
                match text {
                    Some(t) => Ok(obj(vec![(
                        "contents",
                        Json::Arr(vec![obj(vec![
                            ("uri", s(uri)),
                            ("mimeType", s("text/markdown")),
                            ("text", s(t)),
                        ])]),
                    )])),
                    None => Err((-32002, format!("no resource {}", uri))),
                }
            }
            _ if id.is_none() => return None,
            _ => Err((-32601, format!("unknown method {}", method))),
        };
        let id = id?;
        Some(match result {
            Ok(r) => obj(vec![("jsonrpc", s("2.0")), ("id", id), ("result", r)]),
            Err((code, m)) => obj(vec![
                ("jsonrpc", s("2.0")),
                ("id", id),
                (
                    "error",
                    obj(vec![
                        ("code", Json::Num(code as f64)),
                        ("message", Json::Str(m)),
                    ]),
                ),
            ]),
        })
    }
}

fn serve(o: Options) {
    let mut server = Server {
        o,
        worker: None,
        history: vec![],
    };
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    for line in stdin.lock().lines() {
        let Ok(line) = line else { break };
        if line.trim().is_empty() {
            continue;
        }
        let reply = match Json::parse(&line) {
            Ok(Json::Arr(batch)) => {
                let r: Vec<Json> = batch.iter().filter_map(|m| server.handle(m)).collect();
                if r.is_empty() {
                    None
                } else {
                    Some(Json::Arr(r))
                }
            }
            Ok(msg) => server.handle(&msg),
            Err(e) => Some(obj(vec![
                ("jsonrpc", s("2.0")),
                ("id", Json::Null),
                (
                    "error",
                    obj(vec![
                        ("code", Json::Num(-32700.0)),
                        ("message", Json::Str(format!("parse error: {}", e))),
                    ]),
                ),
            ])),
        };
        if let Some(r) = reply {
            let mut out = stdout.lock();
            let _ = writeln!(out, "{}", r.dump());
            let _ = out.flush();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_round_trip() {
        let t = r#"{"a":[1,2.5,-3e2,true,null],"b":"x\"y\\né😀","c":{}}"#;
        let v = Json::parse(t).unwrap();
        assert_eq!(v.get("b").unwrap().as_str().unwrap(), "x\"y\\né😀");
        assert_eq!(Json::parse(&v.dump()).unwrap(), v);
        assert!(Json::parse("{\"a\":1,}").is_err());
    }

    #[test]
    fn worker_results() {
        crate::vfs::use_memory();
        let mut s = Session::new();
        let r = worker_execute(&mut s, "x = 3; x + 7", false);
        assert_eq!(r.get("error"), Some(&Json::Null));
        let Some(Json::Arr(outs)) = r.get("outputs") else {
            panic!()
        };
        assert_eq!(
            outs.last().unwrap().get("text").unwrap().as_str(),
            Some("10")
        );
        let r = worker_execute(&mut s, "1/(", false);
        assert_eq!(
            r.get("error").unwrap().get("name").unwrap().as_str(),
            Some("SyntaxError")
        );
        let r = worker_execute(&mut s, "plot(sin(t), t, 0, 3)", true);
        let Some(Json::Arr(outs)) = r.get("outputs") else {
            panic!()
        };
        let png = outs[0].get("png").unwrap().as_str().unwrap();
        assert!(png.starts_with("iVBOR"));
        assert!(outs[0].get("plotly").is_some());
    }
}
