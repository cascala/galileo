//! Evaluating parsed statements against a session (variables, `ans`).

use crate::build::{self, *};
use crate::calc;
use crate::expr::{Const, Expr, Func};
use crate::linalg::{self, Mat};
use crate::num::Number;
use crate::parser::{Node, Op, Stmt};
use crate::print::show;
use std::collections::BTreeMap;

pub type R<T> = Result<T, String>;

/// The value of a piece of Galileo source (for building results from text such as a polynomial)
pub fn value_of(src: &str) -> Expr {
    let mut s = Session::new();
    match s.run(src).pop() {
        Some(Ok(Output::Value(v))) => v,
        _ => Expr::Str(src.to_string()),
    }
}

/// An explanation of a result: plain text and Markdown
#[derive(Clone, Debug, PartialEq)]
pub struct Note {
    pub plain: String,
    pub markdown: String,
}

/// the Markdown of explanations, one paragraph each
pub fn markdown_notes(notes: &[Note]) -> String {
    notes
        .iter()
        .map(|n| n.markdown.as_str())
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// explanations as `//` comment lines
pub fn comment_notes(notes: &[Note]) -> String {
    notes
        .iter()
        .flat_map(|n| n.plain.lines())
        .map(|l| {
            if l.trim().is_empty() {
                "//".to_string()
            } else {
                format!("// {}", l)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// What a statement produced
#[derive(Clone, Debug, PartialEq)]
pub enum Output {
    Value(Expr),
    /// assignments of several names, e.g. [L,U,P] = lu(A)
    Assigned(Vec<(String, Expr)>),
    Text(String),
    Nothing,
    Exit,
}

impl Output {
    pub fn render(&self) -> String {
        match self {
            Output::Value(e) => crate::print::show_value(e),
            Output::Assigned(v) => v
                .iter()
                .map(|(n, e)| format!("{} = {}", n, crate::print::show_value(e)))
                .collect::<Vec<_>>()
                .join("\n"),
            Output::Text(s) => s.clone(),
            Output::Nothing | Output::Exit => String::new(),
        }
    }
}

pub struct Session {
    pub(crate) vars: BTreeMap<String, Expr>,
    /// names that evaluate to themselves for now (plot variables, coordinates)
    pub(crate) hidden: Vec<String>,
    /// user-defined functions: name -> (parameters, body)
    pub(crate) funcs: BTreeMap<String, (Vec<String>, Node)>,
    /// how deeply user functions are calling each other
    pub(crate) depth: usize,
    /// explanations (Markdown) for the statement being run; see [`Session::note`]
    pub(crate) notes: Vec<Note>,
    /// how many function calls are being evaluated inside each other; only
    /// the outermost call explains itself (table(factor(n), ...) explains the
    /// table, not each factorization)
    pub(crate) calls: usize,
    /// the files being loaded, innermost last (relative paths also resolve
    /// against the loading file's folder; guards against a file loading itself)
    pub(crate) loading: Vec<std::path::PathBuf>,
    /// functions defined with `function ... end`
    pub(crate) procs: BTreeMap<String, crate::lang::Proc>,
    /// results of statements inside blocks, and printed text, waiting to be shown
    pub(crate) emitted: Vec<(Output, Vec<Note>)>,
    /// how deeply blocks, loops and `function` calls are running
    pub(crate) in_block: usize,
    pub(crate) in_loop: usize,
    pub(crate) in_proc: usize,
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) fn builtins() -> BTreeMap<String, Expr> {
    let mut v = BTreeMap::new();
    v.insert("pi".into(), Expr::Const(Const::Pi));
    v.insert("e".into(), Expr::Const(Const::E));
    v.insert("i".into(), Expr::Const(Const::I));
    v.insert("j".into(), Expr::Const(Const::I));
    v.insert("true".into(), Expr::Bool(true));
    v.insert("false".into(), Expr::Bool(false));
    v.insert("explain".into(), explain_default());
    v
}

/// `explain` starts on (brief) unless the environment variable GALILEO_EXPLAIN is
/// 0, off, false or no; GALILEO_EXPLAIN=teach starts in teaching mode
fn explain_default() -> Expr {
    match std::env::var("GALILEO_EXPLAIN") {
        Ok(v) => match v.trim().to_ascii_lowercase().as_str() {
            "0" | "off" | "false" | "no" => Expr::Bool(false),
            "teach" => Expr::Str("teach".into()),
            _ => Expr::Bool(true),
        },
        Err(_) => Expr::Bool(true),
    }
}

/// the first sentence of a text
fn first_sentence(s: &str) -> &str {
    let b = s.as_bytes();
    for i in 0..b.len() {
        if b[i] == b'.' && (i + 1 == b.len() || b[i + 1] == b' ') {
            return &s[..=i];
        }
    }
    s
}

impl Session {
    pub fn new() -> Session {
        crate::assume::clear();
        Session {
            vars: builtins(),
            hidden: vec![],
            funcs: BTreeMap::new(),
            depth: 0,
            notes: vec![],
            calls: 0,
            loading: vec![],
            procs: BTreeMap::new(),
            emitted: vec![],
            in_block: 0,
            in_loop: 0,
            in_proc: 0,
        }
    }

    /// names the user defined
    pub fn variable_names(&self) -> Vec<String> {
        let b = builtins();
        self.vars
            .iter()
            .filter(|(k, v)| b.get(*k) != Some(v))
            .map(|(k, _)| k.clone())
            .collect()
    }

    pub fn get(&self, name: &str) -> Option<&Expr> {
        self.vars.get(name)
    }
    pub fn set(&mut self, name: &str, v: Expr) {
        self.vars.insert(name.to_string(), v);
    }

    /// Whether commands explain their results: the variable `explain`
    /// (true unless set to false or 0, or started off by GALILEO_EXPLAIN=0)
    pub fn explaining(&self) -> bool {
        match self.vars.get("explain") {
            Some(Expr::Bool(b)) => *b,
            Some(Expr::Num(n)) => !n.is_zero(),
            Some(Expr::Str(s)) => !matches!(s.trim(), "off" | "no" | "false" | "0"),
            _ => true,
        }
    }

    /// Whether commands teach: explain = "teach" walks through the method with the
    /// numbers of this run (commands without a teaching note give their brief one)
    pub fn teaching(&self) -> bool {
        matches!(self.vars.get("explain"), Some(Expr::Str(s)) if s.trim() == "teach")
    }

    /// Adds an explanation (Markdown) to the statement being run. Results stay
    /// plain Galileo values; explanations are shown apart from them.
    pub(crate) fn note(&mut self, md: impl Into<String>) {
        let md = md.into();
        self.note2(md.replace('`', ""), md);
    }

    /// An explanation with separate plain-text and Markdown forms
    pub(crate) fn note2(&mut self, plain: String, markdown: String) {
        if self.explaining() && self.calls <= 1 && !markdown.trim().is_empty() && !self.notes.iter().any(|n| n.markdown == markdown) {
            self.notes.push(Note { plain, markdown });
        }
    }

    /// Run one statement; returns its result and its explanations
    pub fn exec_notes(&mut self, st: &Stmt) -> (R<Output>, Vec<Note>) {
        self.notes.clear();
        self.calls = 0;
        let r = self.exec(st);
        if r.is_ok() && self.notes.is_empty() && self.explaining() {
            // every command explains itself at least by what it computes
            let call = match st {
                Stmt::Expr(Node::Call(f, _)) | Stmt::Assign(_, Node::Call(f, _)) => {
                    Some(f.as_str())
                }
                _ => None,
            };
            if let Some((f, e)) = call.and_then(|f| crate::help::entry(f).map(|e| (f, e))) {
                if !self.funcs.contains_key(f) && !self.funcs.contains_key(e.name) {
                    let s = if self.teaching() {
                        e.description.to_string()
                    } else {
                        first_sentence(e.description).to_string()
                    };
                    if f == e.name {
                        self.note(format!("`{}`: {}", f, s));
                    } else {
                        self.note(format!("`{}` (see `{}`): {}", f, e.name, s));
                    }
                }
            }
        }
        (r, std::mem::take(&mut self.notes))
    }

    /// Run source text; one output (or error) per statement
    pub fn run(&mut self, src: &str) -> Vec<R<Output>> {
        self.run_all(src, false)
            .into_iter()
            .map(|(r, _, _)| r)
            .collect()
    }

    /// Run source text; each result comes with whether it should be hidden
    /// (the statement ended in `;`). Stops at the first error.
    pub fn run_quiet(&mut self, src: &str) -> Vec<(R<Output>, bool)> {
        self.run_notes(src)
            .into_iter()
            .map(|(r, q, _)| (r, q))
            .collect()
    }

    /// Run source text: (result, hidden by `;`, explanations) per statement.
    /// Stops at the first error.
    pub fn run_notes(&mut self, src: &str) -> Vec<(R<Output>, bool, Vec<Note>)> {
        self.run_all(src, true)
    }

    /// Run source text, stopping at `exit` and (when `stop_on_error`) at the first error
    pub fn run_all(&mut self, src: &str, stop_on_error: bool) -> Vec<(R<Output>, bool, Vec<Note>)> {
        match crate::parser::parse_quiet(src) {
            Err(e) => vec![(Err(format!("parse error: {}", e)), false, vec![])],
            Ok(stmts) => {
                let mut out = vec![];
                for (s, quiet) in stmts {
                    if let Some(file) = script_file(&s) {
                        // a script's statements show their results as if typed here
                        let res = self.load_file(file);
                        let stop = res.last().is_some_and(|(r, _, _)| {
                            matches!(r, Ok(Output::Exit)) || (stop_on_error && r.is_err())
                        });
                        out.extend(res.into_iter().map(|(r, q, n)| (r, q || quiet, n)));
                        if stop {
                            break;
                        }
                        continue;
                    }
                    let (r, notes) = self.exec_notes(&s);
                    let stop = matches!(r, Ok(Output::Exit)) || (stop_on_error && r.is_err());
                    for (o, n) in self.take_emitted() {
                        out.push((Ok(o), false, n));
                    }
                    out.push((r, quiet, notes));
                    if stop {
                        break;
                    }
                }
                out
            }
        }
    }

    /// Run a script file, statement by statement, as if its lines were typed in:
    /// (result, hidden by `;`, explanations) per statement. Stops at the first
    /// error, which names the file and line (`file.gg:12: ...`). The name may be
    /// quoted, and `.gg` is added when the name has no extension and no such file
    /// exists. Relative names are tried against the current folder, then against
    /// the folder of the file doing the loading.
    pub fn load_file(&mut self, name: &str) -> Vec<(R<Output>, bool, Vec<Note>)> {
        let err = |e: String| vec![(Err(e), false, vec![])];
        if cfg!(target_arch = "wasm32") {
            return err(format!(
                "load({}): the browser version has no files to load; load(RSA) loads the RSA challenge numbers",
                name
            ));
        }
        let path = match self.find_script(name) {
            Some(p) => p,
            None => return err(format!("load {}: no such file", unquote(name))),
        };
        if self.loading.len() >= 32 || self.loading.contains(&path) {
            return err(format!("load {}: the file loads itself", path.display()));
        }
        let src = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => return err(format!("load {}: {}", path.display(), e)),
        };
        let shown = unquote(name).to_string();
        self.loading.push(path);
        let mut out = vec![];
        'chunks: for (line, chunk) in crate::parser::statement_chunks(&src) {
            let at = |e: String| {
                // a nested file's error already says where it happened
                if e.starts_with("load ") || e.contains(".gg:") {
                    e
                } else {
                    format!("{}:{}: {}", shown, line, e)
                }
            };
            let stmts = match crate::parser::parse_quiet(&chunk) {
                Ok(s) => s,
                Err(e) => {
                    out.push((Err(at(format!("parse error: {}", e))), false, vec![]));
                    break;
                }
            };
            for (s, quiet) in stmts {
                let res = match script_file(&s) {
                    Some(f) => self.load_file(f),
                    None => {
                        let (r, n) = self.exec_notes(&s);
                        let mut v: Vec<_> = self
                            .take_emitted()
                            .into_iter()
                            .map(|(o, n)| (Ok(o), false, n))
                            .collect();
                        v.push((r, quiet, n));
                        v
                    }
                };
                for (r, q, n) in res {
                    let stop = matches!(r, Ok(Output::Exit)) || r.is_err();
                    out.push((r.map_err(at), q || quiet, n));
                    if stop {
                        break 'chunks;
                    }
                }
            }
        }
        self.loading.pop();
        out
    }

    /// Where a script name points: the name as given, then with `.gg` added,
    /// each against the current folder and then the loading file's folder
    fn find_script(&self, name: &str) -> Option<std::path::PathBuf> {
        use std::path::{Path, PathBuf};
        let name = unquote(name);
        let mut names = vec![PathBuf::from(name)];
        if Path::new(name).extension().is_none() {
            names.push(PathBuf::from(format!("{}.gg", name)));
        }
        let mut bases: Vec<Option<PathBuf>> = vec![None];
        if let Some(dir) = self.loading.last().and_then(|f| f.parent()) {
            bases.push(Some(dir.to_path_buf()));
        }
        for b in &bases {
            for n in &names {
                let p = match b {
                    Some(d) if n.is_relative() => d.join(n),
                    Some(_) => continue,
                    None => n.clone(),
                };
                if p.is_file() {
                    return Some(p.canonicalize().unwrap_or(p));
                }
            }
        }
        None
    }

    /// Run source text and render it like the REPL does: results, then their
    /// explanations as `//` comments (so the whole output is Galileo source)
    pub fn run_to_string(&mut self, src: &str) -> String {
        self.render_run(src, true)
    }

    /// Results only, as text (no explanations)
    pub fn run_values(&mut self, src: &str) -> String {
        self.render_run(src, false)
    }

    fn render_run(&mut self, src: &str, with_notes: bool) -> String {
        let mut out = vec![];
        for (r, quiet, notes) in self.run_all(src, false) {
            match r {
                Ok(o) => {
                    let _ = quiet;
                    out.push(o.render());
                    if with_notes {
                        out.push(comment_notes(&notes));
                    }
                }
                Err(e) => out.push(format!("Error: {}", e)),
            }
        }
        out.into_iter()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn exec(&mut self, s: &Stmt) -> R<Output> {
        match s {
            Stmt::For(..)
            | Stmt::While(..)
            | Stmt::If(..)
            | Stmt::Function(..)
            | Stmt::Break
            | Stmt::Continue
            | Stmt::Return(_) => self.exec_block_stmt(s),
            Stmt::SetIndex(name, idx, zero, v) => self.set_index(name, idx, *zero, v),
            Stmt::Assign(name, _)
                if name.contains('.')
                    && !self.is_field_path(name)
                    && !self.vars.contains_key(name)
                    && self
                        .vars
                        .get(name.split('.').next().unwrap_or(""))
                        .is_some_and(|v| !matches!(v, Expr::Sym(_))) =>
            {
                Err(format!("{} is not a record", name.split('.').next().unwrap_or("")))
            }
            Stmt::Assign(name, n) if name.contains('.') && self.is_field_path(name) => {
                let v = self.eval(n)?;
                self.set_field(name, v)
            }
            Stmt::Define(name, params, body) if self.define_is_assignment(name, params) => {
                let idx: Vec<Node> = params.iter().map(|p| Node::Ident(p.clone())).collect();
                self.set_index(name, &idx, false, body)
            }
            Stmt::Expr(Node::Call(f, args)) if f == "print" || f == "disp" => {
                self.lang_call(f, args).unwrap()?;
                Ok(Output::Nothing)
            }
            Stmt::Expr(Node::Call(f, args)) if self.procs.contains_key(f) => {
                self.calls += 1;
                let r = self.call_proc(f, args);
                self.calls -= 1;
                match r? {
                    Some(v) => {
                        self.set("ans", v.clone());
                        Ok(Output::Value(v))
                    }
                    None => Ok(Output::Nothing),
                }
            }
            Stmt::Expr(n) => {
                let v = self.eval(n)?;
                self.note_report(&v);
                self.set("ans", v.clone());
                Ok(Output::Value(v))
            }
            Stmt::Assign(name, n) => {
                let v = self.eval(n)?;
                self.note_report(&v);
                self.set(name, v.clone());
                Ok(Output::Value(v))
            }
            Stmt::MultiAssign(names, n) => {
                let vals = match n {
                    Node::Call(f, args) if f == "lu" && args.len() == 1 => {
                        let m = self.matrix_arg(&args[0], "lu")?;
                        let (l, u, p) = linalg::lu(&m)?;
                        match names.len() {
                            // two outputs: A = L*U with L = P^T * L
                            2 => vec![
                                Expr::Matrix(linalg::matmul(&linalg::transpose(&p), &l)?),
                                Expr::Matrix(u),
                            ],
                            3 => vec![Expr::Matrix(l), Expr::Matrix(u), Expr::Matrix(p)],
                            _ => return Err("lu returns 2 or 3 values".into()),
                        }
                    }
                    Node::Call(f, args) if f == "eig" && args.len() == 1 && names.len() == 2 => {
                        let m = self.matrix_arg(&args[0], "eig")?;
                        let (v, vals) = crate::cas_cmd::eigvec(&m)?;
                        let n = vals.len();
                        let d: Vec<Vec<Expr>> = (0..n)
                            .map(|i| {
                                (0..n)
                                    .map(|j| {
                                        if i == j {
                                            vals[i].clone()
                                        } else {
                                            Expr::zero()
                                        }
                                    })
                                    .collect()
                            })
                            .collect();
                        vec![Expr::Matrix(v), Expr::Matrix(d)]
                    }
                    Node::Call(f, args) if crate::la_cmd::is_multi(f) => {
                        self.la_multi(f, args, names.len()).unwrap()?
                    }
                    _ => {
                        return Err("only lu, eig, qr, svd and jordan return several values".into());
                    }
                };
                let pairs: Vec<(String, Expr)> = names.iter().cloned().zip(vals).collect();
                for (n, v) in &pairs {
                    self.set(n, v.clone());
                }
                Ok(Output::Assigned(pairs))
            }
            Stmt::Command(c, arg) => self.command(c, arg.as_deref()),
            Stmt::Define(name, params, body) => {
                if Func::from_name(name).is_some() || ["exp", "sqrt"].contains(&name.as_str()) {
                    return Err(format!(
                        "{} is a built-in function; pick another name",
                        name
                    ));
                }
                // `ans` means the last result at the time of the definition, not when
                // the function is called: d(x) := ans after deriv(f(x), x) is the derivative
                let body = match self.vars.get("ans").cloned() {
                    Some(v) if !params.iter().any(|p| p == "ans") && uses_ident(body, "ans") => {
                        replace_ident(body, "ans", &expr_node(&v)?)
                    }
                    _ => body.clone(),
                };
                let text = format!(
                    "{}({}) := {}",
                    name,
                    params.join(", "),
                    crate::parser::source(&body)
                );
                self.funcs.insert(name.clone(), (params.clone(), body));
                Ok(Output::Text(text))
            }
        }
    }

    /// p.mass = ...: the part before the first dot is a record, or not a value yet
    fn is_field_path(&self, name: &str) -> bool {
        if self.vars.contains_key(name) || name.ends_with('.') || name.contains("..") {
            return false;
        }
        let base = name.split('.').next().unwrap_or("");
        match self.vars.get(base) {
            None => true,
            Some(Expr::Sym(s)) => s == base,
            Some(v) => crate::lang::as_record(v).is_some(),
        }
    }

    /// x(k) = v is an assignment, not a function definition, when x already
    /// holds a vector, matrix or list, or inside a block when every k has a whole-number value
    fn define_is_assignment(&self, name: &str, params: &[String]) -> bool {
        if self.funcs.contains_key(name) {
            return false;
        }
        match self.vars.get(name) {
            Some(Expr::Matrix(_)) => return true,
            Some(v) if crate::lang::as_list(v).is_some() => return true,
            _ => {}
        }
        self.in_block > 0
            && params.iter().all(|p| {
                self.vars
                    .get(p)
                    .and_then(|v| v.num())
                    .is_some_and(|n| n.is_integer())
            })
    }

    /// a report shows its value; its text becomes the explanation
    fn note_report(&mut self, v: &Expr) {
        if let Expr::Obj(o) = v {
            if let crate::expr::Obj::Report(r) = &**o {
                let (t, md) = (r.text.clone(), r.markdown.clone());
                self.note2(t, md);
            }
        }
    }

    fn command(&mut self, c: &str, arg: Option<&str>) -> R<Output> {
        match c {
            "who" | "whos" => {
                let b = builtins();
                let lines: Vec<String> = self
                    .vars
                    .iter()
                    .filter(|(k, v)| b.get(*k) != Some(v))
                    .map(|(k, v)| format!("{}:\t{}", k, type_name(v)))
                    .collect();
                let mut text = format!("Defined variables:\n{}", lines.join("\n"));
                if !self.funcs.is_empty() {
                    let fs: Vec<String> = self
                        .funcs
                        .iter()
                        .map(|(k, (p, b))| {
                            format!("{}({}) := {}", k, p.join(", "), crate::parser::source(b))
                        })
                        .collect();
                    text = format!("{}\nDefined functions:\n{}", text, fs.join("\n"));
                }
                Ok(Output::Text(text))
            }
            "clear" => {
                self.vars = builtins();
                self.funcs.clear();
                Ok(Output::Nothing)
            }
            "exit" | "quit" => Ok(Output::Exit),
            "pwd" => Ok(Output::Text(
                std::env::current_dir()
                    .map_err(|e| e.to_string())?
                    .display()
                    .to_string(),
            )),
            "ls" => {
                let mut names: Vec<String> = std::fs::read_dir(".")
                    .map_err(|e| e.to_string())?
                    .filter_map(|e| e.ok().map(|e| e.file_name().to_string_lossy().into_owned()))
                    .collect();
                names.sort();
                Ok(Output::Text(names.join("\n")))
            }
            "cd" => {
                let dir = arg.ok_or("cd needs a directory")?;
                std::env::set_current_dir(dir).map_err(|e| format!("cd {}: {}", dir, e))?;
                Ok(Output::Nothing)
            }
            "load" => {
                let file = arg.ok_or("load needs a file name")?;
                if unquote(file).eq_ignore_ascii_case("rsa") {
                    let (plain, md) = self.load_rsa();
                    self.note2(plain, md);
                    return Ok(Output::Nothing);
                }
                let mut last = Output::Nothing;
                for (r, _, _) in self.load_file(file) {
                    last = r?;
                }
                Ok(last)
            }
            "help" => Ok(Output::Text(crate::help::help(arg))),
            _ => Err(format!("unknown command {}", c)),
        }
    }

    /// load(RSA): defines RSA100 .. RSA2048, and RSA100p, RSA100q etc. for the factored ones
    fn load_rsa(&mut self) -> (String, String) {
        use crate::rsa_data::{FACTORED_2026, RSA_NUMBERS};
        let int = |s: &str| {
            Expr::Num(Number::from_bigint(
                num_bigint::BigInt::parse_bytes(s.as_bytes(), 10).expect("RSA digits"),
            ))
        };
        let (mut done, mut open) = (vec![], vec![]);
        for (name, n, f) in RSA_NUMBERS {
            self.vars.insert(name.to_string(), int(n));
            if let Some((p, q)) = f {
                self.vars.insert(format!("{}p", name), int(p));
                self.vars.insert(format!("{}q", name), int(q));
                done.push(*name);
            } else if FACTORED_2026.contains(name) {
                open.push(format!("{} (factored in 2026, factors not included)", name));
            } else {
                open.push(name.to_string());
            }
        }
        let head = format!(
            "Loaded {} RSA challenge numbers, named by decimal digits (RSA100 .. RSA617) or by bits (RSA576 .. RSA2048).",
            RSA_NUMBERS.len()
        );
        let plain = format!(
            "{}\nFactored, with factors as RSA...p and RSA...q: {}\nNot factored here: {}",
            head,
            done.join(", "),
            open.join(", ")
        );
        let md = format!(
            "{}\n\n* **Factored**, with factors as `RSA...p` and `RSA...q`: {}\n* **Not factored here:** {}",
            head,
            done.join(", "),
            open.join(", ")
        );
        (plain, md)
    }

    pub(crate) fn matrix_arg(&mut self, n: &Node, f: &str) -> R<Mat> {
        match self.eval(n)? {
            Expr::Matrix(m) => Ok(m),
            other => Err(format!("{} needs a matrix, got {}", f, show(&other))),
        }
    }

    pub fn eval(&mut self, n: &Node) -> R<Expr> {
        if crate::factorlab::tick() {
            return Err("out of time (factoring lab budget)".into());
        }
        let v = match n {
            Node::Num(s) => Expr::Num(Number::parse(s).ok_or_else(|| format!("bad number {}", s))?),
            Node::Str(s) => Expr::Str(s.clone()),
            Node::Ident(name)
                if name.split_once('.').is_some_and(|(b, _)| b == "const") && !self.vars.contains_key("const") =>
            {
                let key = name.split_once('.').map(|(_, k)| k).unwrap_or_default();
                crate::units::constant(key).ok_or_else(|| {
                    format!(
                        "{} is not a constant; the constants are {}",
                        name,
                        crate::units::CONSTANTS.iter().map(|c| c.0).collect::<Vec<_>>().join(", ")
                    )
                })?
            }
            Node::Ident(name) if name.contains('.') && !self.vars.contains_key(name) => {
                match self.get_field(name) {
                    Some(r) => r?,
                    None => self.lookup(name),
                }
            }
            Node::Ident(name) => self.lookup(name),
            Node::List(items) => {
                let mut v = vec![];
                for x in items {
                    v.push(self.eval(x)?);
                }
                crate::lang::list(v)
            }
            Node::Record(fields) => {
                let mut v: Vec<(String, Expr)> = vec![];
                for (k, x) in fields {
                    let x = self.eval(x)?;
                    match v.iter_mut().find(|(n, _)| n == k) {
                        Some(slot) => slot.1 = x,
                        None => v.push((k.clone(), x)),
                    }
                }
                crate::lang::record(v)
            }
            Node::Neg(a) => {
                let a = self.eval(a)?;
                arith(Op::Mul, Expr::int(-1), a)?
            }
            Node::Bin(op, a, b) => {
                let a = self.eval(a)?;
                // short-circuit boolean operators
                match op {
                    Op::And if !truthy(&a)? => return Ok(Expr::Bool(false)),
                    Op::Or if truthy(&a)? => return Ok(Expr::Bool(true)),
                    Op::And | Op::Or => return Ok(Expr::Bool(truthy(&self.eval(b)?)?)),
                    _ => {}
                }
                let b = self.eval(b)?;
                match op {
                    Op::Eq | Op::Ne | Op::Lt | Op::Gt | Op::Le | Op::Ge => compare(*op, a, b)?,
                    _ => arith(*op, a, b)?,
                }
            }
            Node::Range(a, step, b) => {
                let a = self.eval(a)?;
                let s = match step {
                    Some(s) => self.eval(s)?,
                    None => Expr::one(),
                };
                let b = self.eval(b)?;
                range(&a, &s, &b)?
            }
            Node::Matrix(rows) => {
                let mut out: Vec<Vec<Expr>> = vec![];
                for r in rows {
                    // a row may contain row vectors / matrices that are concatenated
                    let mut parts: Vec<Mat> = vec![];
                    for x in r {
                        match self.eval(x)? {
                            Expr::Matrix(m) => parts.push(m),
                            v if v.is_scalar()
                                || matches!(v, Expr::Bool(_) | Expr::Str(_))
                                || crate::ffield::as_elem(&v).is_some()
                                || crate::qring::as_elem(&v).is_some()
                                || crate::quat::as_quat(&v).is_some() =>
                            {
                                parts.push(vec![vec![v]])
                            }
                            v => return Err(format!("{} can't be a matrix element", show(&v))),
                        }
                    }
                    // empty blocks ([]) vanish, so [a, rest] works when rest is []
                    parts.retain(|p| !p.is_empty() && !p[0].is_empty());
                    let Some(h) = parts.first().map(|p| p.len()) else {
                        continue;
                    };
                    if parts.iter().any(|p| p.len() != h) {
                        return Err("matrix blocks in a row must have the same height".into());
                    }
                    for k in 0..h {
                        out.push(parts.iter().flat_map(|p| p[k].clone()).collect());
                    }
                }
                if out.iter().any(|r| r.len() != out[0].len()) {
                    return Err("not all rows have the same size".into());
                }
                // [x] is just x
                if out.len() == 1 && out[0].len() == 1 {
                    out.pop().unwrap().pop().unwrap()
                } else {
                    Expr::Matrix(out)
                }
            }
            Node::Transpose(a) => {
                if let Node::Ident(n) = &**a {
                    if !self.vars.contains_key(n) && !self.hidden.contains(n) && !matches!(n.as_str(), "pi" | "e" | "i" | "inf") {
                        return Err(format!(
                            "{}' is the transpose of {}, which has no value; for a derivative write d({}, t) (or d({}, x), ...)",
                            n, n, n, n
                        ));
                    }
                }
                match self.eval(a)? {
                    Expr::Matrix(m) => Expr::Matrix(linalg::transpose(&linalg::map(&m, conj_expr))),
                    v => conj_expr(&v),
                }
            }
            Node::Index(a, idx) => {
                let target = self.eval(a)?;
                if matches!(target, Expr::Matrix(_) | Expr::Str(_))
                    || crate::lang::as_list(&target).is_some()
                {
                    let what = format!("{}[...]", crate::parser::source(a));
                    return self.index_value(&target, idx, true, &what);
                }
                let mut ix = vec![];
                for i in idx {
                    let v = self.eval(i)?;
                    let v = if v.num().is_some_and(|n| !n.is_exact() && n.is_integer()) {
                        Expr::int(v.num().unwrap().to_f64() as i128)
                    } else {
                        v
                    };
                    ix.push(
                        v.num()
                            .and_then(|n| n.as_int())
                            .filter(|&k| k >= 0)
                            .ok_or("indices must be non-negative integers")?
                            as usize,
                    );
                }
                let m = match target {
                    Expr::Matrix(m) => m,
                    Expr::Obj(o) => return crate::commands::index_obj(&o, &ix),
                    // a scalar is a 1x1 matrix: x[0] and x[0,0] are x
                    v if v.is_scalar() && ix.iter().all(|&k| k == 0) && ix.len() <= 2 => {
                        return Ok(v);
                    }
                    v => return Err(format!("{} can't be indexed", show(&v))),
                };
                let (r, c) = linalg::dims(&m);
                let oob = || "index out of bounds".to_string();
                match ix.as_slice() {
                    [i] if r == 1 => m[0].get(*i).cloned().ok_or_else(oob)?,
                    [i] if c == 1 => m.get(*i).map(|row| row[0].clone()).ok_or_else(oob)?,
                    [i] => Expr::Matrix(vec![m.get(*i).cloned().ok_or_else(oob)?]),
                    [i, j] => m
                        .get(*i)
                        .and_then(|row| row.get(*j))
                        .cloned()
                        .ok_or_else(oob)?,
                    _ => return Err("use one or two indices".into()),
                }
            }
            Node::Call(f, args) => {
                self.calls += 1;
                let r = self.call(f, args);
                self.calls -= 1;
                r?
            }
        };
        check(&v)?;
        Ok(v)
    }

    fn call(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let arity = |n: usize| -> R<()> {
            if args.len() == n {
                Ok(())
            } else {
                Err(format!(
                    "{} takes {} argument{}",
                    f,
                    n,
                    if n == 1 { "" } else { "s" }
                ))
            }
        };
        if self.procs.contains_key(f) {
            return self
                .call_proc(f, args)?
                .ok_or_else(|| format!("{} gives no value", f));
        }
        if self.funcs.contains_key(f) {
            return self.call_user(f, args);
        }
        if let Some(v) = self
            .vars
            .get(f)
            .filter(|_| !self.hidden.iter().any(|h| h == f))
            .filter(|v| {
                matches!(v, Expr::Matrix(_) | Expr::Str(_)) || crate::lang::as_list(v).is_some()
            })
            .cloned()
        {
            if args.is_empty() || args.len() > 2 {
                return Err(format!("{}(...) takes one or two positions", f));
            }
            return self.index_value(&v, args, false, f);
        }
        // a result with explanations indexes like the value it shows
        if let Some(Expr::Obj(o)) = self
            .vars
            .get(f)
            .filter(|_| !self.hidden.iter().any(|h| h == f))
            .cloned()
        {
            if let crate::expr::Obj::Ring(r) = &*o {
                if args.len() != 1 {
                    return Err(format!("{}(x) takes one argument", f));
                }
                let x = self.eval_hidden(&args[0], &["a".to_string()])?;
                return r.from_expr(&x).map(crate::ffield::value);
            }
            if let crate::expr::Obj::QRing(r) = &*o {
                if args.len() != 1 {
                    return Err(format!("{}(x) takes one argument", f));
                }
                let x = self.eval_hidden(&args[0], std::slice::from_ref(&r.var))?;
                return r.from_expr(&x).map(crate::qring::value);
            }
            if let crate::expr::Obj::Report(r) = &*o {
                if let Some(Expr::Matrix(m)) = r.shown_value() {
                    return self.index(f, &m, args);
                }
            }
        }
        if f == "if" {
            if args.len() != 3 {
                return Err("usage: if(condition, value if true, value if false)".into());
            }
            let c = self.eval(&args[0])?;
            return match c {
                Expr::Bool(b) => self.eval(&args[if b { 1 } else { 2 }]),
                Expr::Num(n) => self.eval(&args[if n.is_zero() { 2 } else { 1 }]),
                other => Err(format!(
                    "if: can't tell whether {} is true or false",
                    show(&other)
                )),
            };
        }
        // reciprocal functions are written with the six basic ones
        let recip = match f {
            "sec" => Some(Func::Cos),
            "csc" => Some(Func::Sin),
            "cot" => Some(Func::Tan),
            "sech" => Some(Func::Cosh),
            "csch" => Some(Func::Sinh),
            "coth" => Some(Func::Tanh),
            _ => None,
        };
        if let Some(func) = recip {
            arity(1)?;
            let a = self.eval(&args[0])?;
            return elementwise(&a, &|x| {
                if let Expr::Num(Number::Float(v)) = x {
                    return Expr::float(1.0 / func.apply(*v));
                }
                calc::simplify(&build::div(Expr::one(), build::func(func, x.clone())))
            });
        }
        if let Some(func) = Func::from_name(f) {
            arity(1)?;
            let a = self.eval(&args[0])?;
            let a = if func == Func::Abs || !crate::units::has_units(&a) {
                a
            } else {
                crate::units::plain_arg(f, &a)?
            };
            return elementwise(&a, &|x| {
                // decimals directly (fast for large arrays); outside the real domain
                // (log(-1.0)) the symbolic rules give the complex value
                if let Expr::Num(Number::Float(v)) = x {
                    let y = func.apply(*v);
                    if y.is_finite() {
                        return Expr::float(y);
                    }
                }
                // |a + bi| = sqrt(a^2 + b^2) for a complex argument
                if func == Func::Abs && crate::extra_cmd::has_i(x) {
                    if let Ok((re, im)) = crate::extra_cmd::parts(x) {
                        let r = calc::simplify(&build::sqrt(build::add2(
                            build::pow(re, Expr::int(2)),
                            build::pow(im, Expr::int(2)),
                        )));
                        if !crate::extra_cmd::has_i(&r) {
                            return r;
                        }
                    }
                }
                build::func(func, x.clone())
            });
        }
        match f {
            "exp" | "sqrt" => {
                arity(1)?;
                let a = self.eval(&args[0])?;
                if crate::units::has_units(&a) && !matches!(a, Expr::Matrix(_)) {
                    if f == "sqrt" {
                        return arith(Op::Pow, a, Expr::Num(Number::rat(1, 2).unwrap()));
                    }
                    let a = crate::units::plain_arg(f, &a)?;
                    return Ok(build::exp(a));
                }
                let g = if f == "exp" { build::exp } else { build::sqrt };
                elementwise(&a, &|x| {
                    if let Expr::Num(Number::Float(v)) = x {
                        if f == "exp" || *v >= 0.0 {
                            let y = if f == "exp" { v.exp() } else { v.sqrt() };
                            if y.is_finite() {
                                return Expr::float(y);
                            }
                        }
                    }
                    g(x.clone())
                })
            }
            "d" => self.d_call(args),
            "deriv" | "diff" => {
                arity(2)?;
                let x = match &args[1] {
                    Node::Ident(n) => n.clone(),
                    _ => return Err("deriv(y, x): x must be a variable name".into()),
                };
                let y = self.eval_hidden(&args[0], std::slice::from_ref(&x))?;
                Ok(calc::deriv(&y, &x))
            }
            "simplify" | "expand" | "factor" | "eval" | "complexity" | "info" => {
                if args.is_empty() || args.len() > 2 {
                    return Err(format!("{} takes 1 argument", f));
                }
                let a = self.eval(&args[0])?;
                if f == "eval" && args.len() == 2 {
                    let d = self.eval(&args[1])?;
                    let d = d
                        .num()
                        .and_then(|n| n.as_int())
                        .filter(|&k| (1..=100_000).contains(&k))
                        .ok_or("eval(x, digits): digits is a whole number from 1 to 100000")?;
                    // a quantity: its number to that many digits, with its units
                    if crate::units::has_units(&a) && !matches!(a, Expr::Matrix(_)) {
                        if let Some((c, u)) = crate::units::split(&a).filter(|(c, _)| !crate::units::has_units(c)) {
                            let c = crate::prec::eval_digits(&c, d as u32)?;
                            return Ok(build::mul2(c, crate::units::expr_of(&u)));
                        }
                    }
                    return crate::prec::eval_digits(&a, d as u32);
                }
                if f == "factor" && args.len() == 2 {
                    let field = self.eval(&args[1])?;
                    return self.factor_over(&a, &field);
                }
                Ok(match f {
                    "simplify" => calc::simplify(&a),
                    "expand" => calc::expand(&a),
                    "factor" => match a.num().and_then(|n| n.as_bigint()) {
                        // factor(360) = [2, 3; 3, 2; 5, 1] (primes and exponents);
                        // a^n ± b^n splits algebraically first
                        Some(n) if n.bits() > 1 => {
                            let (m, t) = self
                                .factor_integer(&args[0], &n)
                                .map_err(|e| format!("factor: {}", e))?;
                            self.note(&format!("{} = {}\n\n`factor` of a whole number gives one row per prime: the prime and its exponent. Numbers like a^n ± b^n are first cut along their algebraic factors; then trial division, Pollard's rho and the elliptic curve method split the rest, and each factor is checked to be prime (Miller–Rabin). A part written C*k* (k digits) is composite but did not split in reasonable time; its row holds the whole composite number.", n, t.replace('\n', "\n\n")));
                            m
                        }
                        _ if crate::extra_cmd::is_huge(&a) => {
                            return Err(crate::extra_cmd::not_whole(&a, "factor"));
                        }
                        _ => crate::factor_cmd::factor_poly(&a)
                            .or_else(|| crate::cas::factor_poly(&a))
                            .unwrap_or_else(|| calc::factor(&a)),
                    },
                    "eval" => complex_evalf(&calc::evalf(&a)),
                    "complexity" => Expr::int(a.size() as i128),
                    _ => Expr::Str(structure(&a)),
                })
            }
            "subs" => {
                arity(3)?;
                let e = self.eval(&args[0])?;
                let x = match &args[1] {
                    Node::Ident(n) => n.clone(),
                    _ => return Err("subs(expr, x, value): x must be a variable name".into()),
                };
                let v = self.eval(&args[2])?;
                // a list of values into a scalar expression: one result per value
                if let (Expr::Matrix(m), false) = (&v, matches!(e, Expr::Matrix(_))) {
                    return Ok(Expr::Matrix(m.iter().map(|row| row.iter().map(|val| calc::simplify(&calc::subst(&e, &x, val))).collect()).collect()));
                }
                Ok(calc::subst(&e, &x, &v))
            }
            "and" | "or" | "xor" => {
                arity(2)?;
                let a = truthy(&self.eval(&args[0])?)?;
                let b = truthy(&self.eval(&args[1])?)?;
                Ok(Expr::Bool(match f {
                    "and" => a && b,
                    "or" => a || b,
                    _ => a ^ b,
                }))
            }
            "not" => {
                arity(1)?;
                Ok(Expr::Bool(!truthy(&self.eval(&args[0])?)?))
            }
            "inv" => {
                arity(1)?;
                match self.eval(&args[0])? {
                    Expr::Matrix(m) => {
                        let inv = linalg::inverse(&m)?;
                        Ok(Expr::Matrix(
                            inv.iter()
                                .map(|r| r.iter().map(crate::ratfun::tidy).collect())
                                .collect(),
                        ))
                    }
                    v if v.is_scalar() => arith(Op::Pow, v, Expr::int(-1)),
                    _ => Err("inv needs a matrix".into()),
                }
            }
            "det" => {
                arity(1)?;
                let m = self.matrix_arg(&args[0], "det")?;
                linalg::det(&m)
            }
            "transpose" => {
                arity(1)?;
                let m = self.matrix_arg(&args[0], "transpose")?;
                Ok(Expr::Matrix(linalg::transpose(&m)))
            }
            "lu" => Err("use [L,U] = lu(A) or [L,U,P] = lu(A)".into()),
            "norm" => {
                let m = match self.eval(args.first().ok_or("norm takes a matrix")?)? {
                    Expr::Matrix(m) => m,
                    v if crate::qring::as_elem(&v).is_some() && args.len() == 1 => {
                        self.note("The field norm: the determinant of multiplication by the element (the product of its conjugates).");
                        return Ok(crate::qring::qnum(crate::qring::as_elem(&v).unwrap().norm()));
                    }
                    v if crate::quat::as_quat(&v).is_some() && args.len() == 1 => {
                        self.note("|q| = sqrt(a^2 + b^2 + c^2 + d^2); it is multiplicative: |p q| = |p| |q|.");
                        return Ok(crate::quat::as_quat(&v).unwrap().norm());
                    }
                    other => return Err(format!("norm needs a matrix, got {}", show(&other))),
                };
                let p = match args.get(1) {
                    None => "2".to_string(),
                    Some(Node::Ident(s)) => s.clone(),
                    Some(Node::Str(s)) => s.clone(),
                    Some(n) => show(&self.eval(n)?),
                };
                // a vector's length, exact: norm([1 1]) = sqrt(2)
                let (r, c) = linalg::dims(&m);
                if (r == 1 || c == 1) && p == "2" {
                    let sq = m
                        .iter()
                        .flatten()
                        .map(|x| build::pow(x.clone(), Expr::int(2)))
                        .collect();
                    return Ok(calc::simplify(&build::sqrt(calc::simplify(&build::add(
                        sq,
                    )))));
                }
                linalg::norm(&m, &p)
            }
            "eye" | "ones" | "zeros" | "rand" => {
                if args.is_empty() {
                    return Ok(match f {
                        "zeros" => Expr::zero(),
                        "rand" => Expr::float(rand_f64()),
                        _ => Expr::one(),
                    });
                }
                let n = self.size_arg(&args[0])?;
                let m = match args.get(1) {
                    Some(a) => self.size_arg(a)?,
                    None => n,
                };
                Ok(Expr::Matrix(match f {
                    "eye" => linalg::identity(n, m),
                    "ones" => linalg::filled(n, m, Expr::one()),
                    "zeros" => linalg::filled(n, m, Expr::zero()),
                    _ => (0..n)
                        .map(|_| (0..m).map(|_| Expr::float(rand_f64())).collect())
                        .collect(),
                }))
            }
            "linspace" | "logspace" => {
                if args.len() < 2 || args.len() > 3 {
                    return Err(format!("{}(a, b[, n])", f));
                }
                let a = self.eval(&args[0])?;
                let b = self.eval(&args[1])?;
                let n = match args.get(2) {
                    Some(x) => self.size_arg(x)?,
                    None => {
                        if f == "linspace" {
                            100
                        } else {
                            50
                        }
                    }
                };
                let (a, b) = (calc::evalf(&a), calc::evalf(&b));
                let (Some(a), Some(b)) = (a.num(), b.num()) else {
                    return Err(format!("{} needs numbers", f));
                };
                let (a, b) = (a.to_f64(), b.to_f64());
                let row: Vec<Expr> = (0..n)
                    .map(|k| {
                        let t = if n > 1 {
                            a + (b - a) * k as f64 / (n - 1) as f64
                        } else {
                            a
                        };
                        Expr::float(if f == "linspace" { t } else { 10f64.powf(t) })
                    })
                    .collect();
                Ok(Expr::Matrix(vec![row]))
            }
            "max" | "min" => {
                let mut vals = vec![];
                for a in args {
                    match self.eval(a)? {
                        Expr::Matrix(m) => vals.extend(m.into_iter().flatten()),
                        v => vals.push(v),
                    }
                }
                let mut best: Option<(f64, Expr)> = None;
                for v in vals {
                    let x = calc::evalf(&v)
                        .num()
                        .ok_or_else(|| format!("{} needs numbers", f))?
                        .to_f64();
                    if best
                        .as_ref()
                        .is_none_or(|(b, _)| if f == "max" { x > *b } else { x < *b })
                    {
                        best = Some((x, v));
                    }
                }
                best.map(|b| b.1)
                    .ok_or_else(|| format!("{} needs arguments", f))
            }
            _ => match self.lang_call(f, args) {
                Some(r) => r,
                None => self.command_call(f, args),
            },
        }
    }

    /// f(a, b) for a user-defined f: the body evaluated with the parameters bound
    fn call_user(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let mut vals = vec![];
        for a in args {
            vals.push(self.eval(a)?);
        }
        if self.depth >= 2000 {
            return Err(format!(
                "{}: too many nested calls (does the recursion stop?)",
                f
            ));
        }
        self.depth += 1;
        let r = self.run_user(f.to_string(), vals);
        self.depth -= 1;
        r
    }

    /// A(k): entry k of a vector, or row k of a matrix; A(i, j): one entry. Counting from 1.
    fn index(&mut self, f: &str, m: &[Vec<Expr>], args: &[Node]) -> R<Expr> {
        let mut ks = vec![];
        for a in args {
            let v = self.eval(a)?;
            let k = v
                .num()
                .filter(|n| n.is_exact())
                .and_then(|n| n.as_int())
                .ok_or_else(|| {
                    format!("{}(...): positions are whole numbers, got {}", f, show(&v))
                })?;
            ks.push(k);
        }
        let (rows, cols) = (m.len() as i128, m.first().map_or(0, |r| r.len()) as i128);
        let bad = |k: i128, n: i128| -> R<usize> {
            if k < 1 || k > n {
                Err(format!("{}: position {} is outside 1..{}", f, k, n))
            } else {
                Ok(k as usize - 1)
            }
        };
        match ks[..] {
            [k] if rows == 1 => Ok(m[0][bad(k, cols)?].clone()),
            [k] if cols == 1 => Ok(m[bad(k, rows)?][0].clone()),
            [k] => Ok(Expr::Matrix(vec![m[bad(k, rows)?].clone()])),
            [i, j] => Ok(m[bad(i, rows)?][bad(j, cols)?].clone()),
            _ => Err(format!("{}(...) takes one or two positions", f)),
        }
    }

    /// Runs a user function. A call in tail position (the whole body, or a
    /// branch of an if that is the whole body) does not nest: the loop just
    /// rebinds the parameters, so f(n, k) := if(..., k, f(n, k + 1)) can run
    /// millions of steps.
    fn run_user(&mut self, mut f: String, mut vals: Vec<Expr>) -> R<Expr> {
        let mut steps: u64 = 0;
        loop {
            let (params, body) = self.funcs.get(&f).cloned().unwrap();
            if params.len() != vals.len() {
                return Err(format!(
                    "{} takes {} argument{}",
                    f,
                    params.len(),
                    if params.len() == 1 { "" } else { "s" }
                ));
            }
            let saved: Vec<Option<Expr>> =
                params.iter().map(|p| self.vars.get(p).cloned()).collect();
            let hidden = self.hidden.clone();
            self.hidden.retain(|h| !params.contains(h));
            for (p, v) in params.iter().zip(vals) {
                self.vars.insert(p.clone(), v);
            }
            let r = self.tail(&body);
            self.hidden = hidden;
            for (p, v) in params.iter().zip(saved) {
                match v {
                    Some(v) => self.vars.insert(p.clone(), v),
                    None => self.vars.remove(p),
                };
            }
            match r? {
                Tail::Value(e) => return Ok(e),
                Tail::Call(g, v) => {
                    steps += 1;
                    if steps > 10_000_000 {
                        return Err(format!(
                            "{}: still running after 10 million steps (does the recursion stop?)",
                            g
                        ));
                    }
                    f = g;
                    vals = v;
                }
            }
        }
    }

    fn tail(&mut self, node: &Node) -> R<Tail> {
        if let Node::Call(name, args) = node {
            if self.funcs.contains_key(name) {
                let mut vals = vec![];
                for a in args {
                    vals.push(self.eval(a)?);
                }
                return Ok(Tail::Call(name.clone(), vals));
            }
            if name == "if" && args.len() == 3 {
                let c = self.eval(&args[0])?;
                let pick = match c {
                    Expr::Bool(b) => b,
                    Expr::Num(n) => !n.is_zero(),
                    other => {
                        return Err(format!(
                            "if: can't tell whether {} is true or false",
                            show(&other)
                        ));
                    }
                };
                return self.tail(&args[if pick { 1 } else { 2 }]);
            }
        }
        Ok(Tail::Value(self.eval(node)?))
    }

    /// d(y, t), d(y, t, 2): the derivative of a function y at t (an unknown
    /// one stays symbolic), or d(expr, x[, k]) the k-th derivative of expr
    fn d_call(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: d(y, t) or d(y, t, k): the (k-th) derivative of the function y at t (y may be unknown); d(expr, x[, k]) differentiates expr with respect to x";
        if args.len() != 2 && args.len() != 3 {
            return Err(usage.into());
        }
        let k = match args.get(2) {
            None => 1,
            Some(n) => match self.eval(n)?.num().and_then(|v| v.as_int()) {
                Some(k) if (1..=50).contains(&k) => k as usize,
                _ => return Err("d(y, t, k): k is a whole number from 1 to 50".into()),
            },
        };
        if let Node::Ident(y) = &args[0] {
            let bound = self.vars.contains_key(y) || self.hidden.contains(y);
            if let Some((params, body)) = self.funcs.get(y).cloned() {
                if params.len() != 1 {
                    return Err(format!("d({}, ...): {} takes {} arguments; write d({}(...), x) for a partial derivative", y, y, params.len(), y));
                }
                let p = &params[0];
                let mut e = self.eval_hidden(&body, std::slice::from_ref(p))?;
                for _ in 0..k {
                    e = calc::deriv(&e, p);
                }
                let v = self.eval(&args[1])?;
                return Ok(calc::subst(&e, p, &v));
            }
            let same = matches!(&args[1], Node::Ident(t) if t == y);
            if !bound && !same && !matches!(y.as_str(), "pi" | "e" | "i" | "inf") {
                let at = self.eval(&args[1])?;
                return Ok(Expr::Apply(format!("{}{}", y, "'".repeat(k)), vec![at]));
            }
        }
        let x = match &args[1] {
            Node::Ident(n) => n.clone(),
            _ => return Err("d(expr, x): x must be a variable name".into()),
        };
        let mut e = self.eval_hidden(&args[0], std::slice::from_ref(&x))?;
        for _ in 0..k {
            e = calc::deriv(&e, &x);
        }
        Ok(e)
    }

    /// Value of a variable; stored expressions pick up values assigned later
    /// (K = gausscurvature(t); R = 2; K uses R = 2)
    pub(crate) fn lookup(&self, name: &str) -> Expr {
        if self.hidden.iter().any(|h| h == name) {
            return Expr::Sym(name.to_string());
        }
        match self.vars.get(name) {
            None => Expr::Sym(name.to_string()),
            Some(v @ (Expr::Obj(_) | Expr::Bool(_) | Expr::Str(_))) => v.clone(),
            Some(v) => self.resolve(v, &mut vec![name.to_string()]),
        }
    }

    /// Substitute the values of bound symbols (but not hidden ones, nor the
    /// names in `stack`, which guards against x = x + 1 loops)
    pub(crate) fn resolve(&self, e: &Expr, stack: &mut Vec<String>) -> Expr {
        match e {
            Expr::Sym(s) => {
                if stack.contains(s) || self.hidden.contains(s) {
                    return e.clone();
                }
                match self.vars.get(s) {
                    Some(v) if v.is_scalar() && v != e => {
                        stack.push(s.clone());
                        let r = self.resolve(v, stack);
                        stack.pop();
                        r
                    }
                    _ => e.clone(),
                }
            }
            Expr::Num(_) | Expr::Const(_) | Expr::Bool(_) | Expr::Str(_) | Expr::Obj(_) => {
                e.clone()
            }
            // a matrix of plain numbers (fast for large arrays)
            Expr::Matrix(m)
                if m.iter().flatten().all(|x| {
                    matches!(x, Expr::Num(_) | Expr::Const(_) | Expr::Bool(_) | Expr::Str(_))
                }) =>
            {
                e.clone()
            }
            _ => {
                if e.symbols().iter().all(|s| {
                    !self.vars.contains_key(s) || self.hidden.contains(s) || stack.contains(s)
                }) {
                    return e.clone();
                }
                e.map_children(&mut |c| self.resolve(c, stack))
            }
        }
    }

    /// Evaluate with some names temporarily unbound
    pub(crate) fn eval_hidden(&mut self, n: &Node, names: &[String]) -> R<Expr> {
        let k = self.hidden.len();
        self.hidden.extend(names.iter().cloned());
        let r = self.eval(n);
        self.hidden.truncate(k);
        r
    }

    fn size_arg(&mut self, n: &Node) -> R<usize> {
        let v = self.eval(n)?;
        v.num()
            .filter(|n| n.is_integer() && !n.is_negative())
            .map(|n| n.to_f64() as usize)
            .ok_or_else(|| format!("{} is not a valid size", show(&v)))
    }
}

fn type_name(v: &Expr) -> String {
    match v {
        Expr::Num(_) => "Scalar".into(),
        Expr::Bool(_) => "Boolean".into(),
        Expr::Matrix(m) => format!(
            "Matrix:{}(r),{}(c)",
            m.len(),
            m.first().map_or(0, |r| r.len())
        ),
        Expr::Sym(_) => "Variable".into(),
        Expr::Str(_) => "String".into(),
        Expr::Obj(o) => o.kind().into(),
        _ => "Expression".into(),
    }
}

thread_local!(static RAND_STATE: std::cell::Cell<u64> = const { std::cell::Cell::new(0) });

/// Seed the generator behind rand(); hosts without a clock (WebAssembly) call this
pub fn seed_random(seed: u64) {
    RAND_STATE.with(|s| s.set(seed | 1));
}

fn clock_seed() -> u64 {
    #[cfg(target_arch = "wasm32")]
    {
        0x9e37_79b9_7f4a_7c15
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(1, |d| d.as_nanos() as u64)
            | 1
    }
}

pub(crate) fn rand_f64() -> f64 {
    RAND_STATE.with(|s| {
        let mut x = s.get();
        if x == 0 {
            x = clock_seed();
        }
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        s.set(x);
        (x >> 11) as f64 / (1u64 << 53) as f64
    })
}

pub(crate) fn truthy(e: &Expr) -> R<bool> {
    match e {
        Expr::Bool(b) => Ok(*b),
        Expr::Num(n) => Ok(!n.is_zero()),
        // a matrix is true when every entry is (as in Matlab)
        Expr::Matrix(m) if !m.is_empty() => {
            for x in m.iter().flatten() {
                if !truthy(x)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        other => Err(format!("{} is not a boolean", show(other))),
    }
}

enum Tail {
    Value(Expr),
    Call(String, Vec<Expr>),
}

fn elementwise(a: &Expr, f: &dyn Fn(&Expr) -> Expr) -> R<Expr> {
    match a {
        Expr::Matrix(m) => Ok(Expr::Matrix(linalg::map(m, f))),
        v if v.is_scalar() => Ok(f(v)),
        v => Err(format!("can't apply a function to {}", show(v))),
    }
}

/// Arithmetic on scalars and matrices
pub fn arith(op: Op, a: Expr, b: Expr) -> R<Expr> {
    use Expr::Matrix as M;
    // "x = " + 3 joins text
    if op == Op::Add && (matches!(a, Expr::Str(_)) || matches!(b, Expr::Str(_))) {
        return Ok(Expr::Str(format!(
            "{}{}",
            crate::print::show(&a),
            crate::print::show(&b)
        )));
    }
    // elementwise .* ./ .^
    if matches!(op, Op::EMul | Op::EDiv | Op::EPow) {
        let scalar = match op {
            Op::EMul => Op::Mul,
            Op::EDiv => Op::Div,
            _ => Op::Pow,
        };
        // two numbers: straight to the number rules (fast for large arrays)
        let one = |p: Expr, q: Expr| -> R<Expr> {
            match (scalar, &p, &q) {
                (Op::Mul, Expr::Num(_), Expr::Num(_)) => Ok(mul2(p, q)),
                (Op::Div, Expr::Num(_), Expr::Num(y)) if !y.is_zero() => Ok(div(p, q)),
                _ => arith(scalar, p, q),
            }
        };
        return match (a, b) {
            (M(x), M(y)) => {
                let (dx, dy) = (linalg::dims(&x), linalg::dims(&y));
                if dx != dy {
                    return Err(format!(
                        "elementwise {}: sizes differ ({}x{} and {}x{})",
                        match op {
                            Op::EMul => ".*",
                            Op::EDiv => "./",
                            _ => ".^",
                        },
                        dx.0,
                        dx.1,
                        dy.0,
                        dy.1
                    ));
                }
                let mut out = vec![];
                for (rx, ry) in x.into_iter().zip(y) {
                    let mut row = vec![];
                    for (p, q) in rx.into_iter().zip(ry) {
                        row.push(one(p, q)?);
                    }
                    out.push(row);
                }
                Ok(M(out))
            }
            (M(x), s) => {
                let mut out = vec![];
                for r in x {
                    let mut row = vec![];
                    for p in r {
                        row.push(one(p, s.clone())?);
                    }
                    out.push(row);
                }
                Ok(M(out))
            }
            (s, M(x)) => {
                let mut out = vec![];
                for r in x {
                    let mut row = vec![];
                    for p in r {
                        row.push(one(s.clone(), p)?);
                    }
                    out.push(row);
                }
                Ok(M(out))
            }
            (x, y) => arith(scalar, x, y),
        };
    }
    if (crate::quat::as_quat(&a).is_some() || crate::quat::as_quat(&b).is_some())
        && !matches!(&a, M(_))
        && !matches!(&b, M(_))
    {
        return crate::quat::arith(op, &a, &b);
    }
    if (crate::qring::as_elem(&a).is_some() || crate::qring::as_elem(&b).is_some())
        && !matches!(&a, M(_))
        && !matches!(&b, M(_))
    {
        return crate::qring::arith(op, &a, &b);
    }
    if crate::ffield::as_elem(&a).is_some() || crate::ffield::as_elem(&b).is_some() {
        if !matches!(&a, M(_)) && !matches!(&b, M(_)) {
            return crate::ffield::arith(op, &a, &b);
        }
    }
    for v in [&a, &b] {
        if matches!(v, Expr::Bool(_) | Expr::Str(_)) {
            return Err(format!("can't do arithmetic with {}", show(v)));
        }
    }
    // quantities with units: dimension checks, conversions, cancelling
    if !matches!(&a, M(_)) && !matches!(&b, M(_)) && (crate::units::has_units(&a) || crate::units::has_units(&b)) {
        match op {
            Op::Add | Op::Sub => {
                if let Some(r) = crate::units::add(&a, &b, op == Op::Sub)? {
                    return Ok(r);
                }
            }
            Op::Pow => {
                if let Some(r) = crate::units::pow(&a, &b) {
                    return Ok(crate::units::tidy(r));
                }
                if crate::units::has_units(&b) {
                    return Err(format!("{}^{}: an exponent can't have units", show(&a), show(&b)));
                }
            }
            Op::Mul => return Ok(crate::units::tidy(mul2(a, b))),
            Op::Div => return Ok(crate::units::tidy(div(a, b))),
            _ => {}
        }
    }
    Ok(match (op, a, b) {
        (Op::Add, M(x), M(y)) => M(linalg::zip(&x, &y, "sum", |p, q| {
            add2(p.clone(), q.clone())
        })?),
        (Op::Sub, M(x), M(y)) => M(linalg::zip(&x, &y, "difference", |p, q| {
            sub(p.clone(), q.clone())
        })?),
        (Op::Add, M(x), s) | (Op::Add, s, M(x)) => {
            M(linalg::map(&x, |p| add2(p.clone(), s.clone())))
        }
        (Op::Sub, M(x), s) => M(linalg::map(&x, |p| sub(p.clone(), s.clone()))),
        (Op::Sub, s, M(x)) => M(linalg::map(&x, |p| sub(s.clone(), p.clone()))),
        (Op::Mul, M(x), M(y)) => {
            let r = linalg::matmul(&x, &y)?;
            // a row times a column is a number (the dot product)
            if x.len() == 1 && x[0].len() > 1 && r.len() == 1 && r[0].len() == 1 {
                r[0][0].clone()
            } else {
                M(r)
            }
        }
        (Op::Mul, M(x), s) => M(linalg::map(&x, |p| mul2(p.clone(), s.clone()))),
        (Op::Mul, s, M(x)) => M(linalg::map(&x, |p| mul2(s.clone(), p.clone()))),
        (Op::Div, M(x), M(y)) => M(linalg::matmul(&x, &linalg::inverse(&y)?)?),
        (Op::Div, M(x), s) => M(linalg::map(&x, |p| div(p.clone(), s.clone()))),
        (Op::Div, _, M(_)) => return Err("can't divide by a matrix; use inv(A)".into()),
        (Op::Solve, M(x), M(y))
            if (x.iter().chain(&y).flatten().any(crate::vcalc::has_float))
                && x.iter().chain(&y).flatten().all(|e| e.symbols().is_empty()) =>
        {
            // decimals: solve with the exact fractions they stand for (no rounding
            // noise), then give decimals back
            let ex = |m: &linalg::Mat| linalg::map(m, crate::vcalc::exactify);
            let r = arith(Op::Solve, M(ex(&x)), M(ex(&y)))?;
            match r {
                M(m) => M(linalg::map(&m, calc::evalf)),
                o => o,
            }
        }
        (Op::Solve, M(x), M(y)) if linalg::dims(&x).0 > linalg::dims(&x).1 => {
            // more equations than unknowns: least squares, A^T A x = A^T b
            let xt = linalg::transpose(&x);
            let n = linalg::matmul(&xt, &x)?;
            let r = linalg::matmul(&xt, &y)?;
            M(linalg::solve(&n, &r).map_err(|_| {
                "A\\b: the columns of A are dependent, so the least squares solution is not unique".to_string()
            })?)
        }
        (Op::Solve, M(x), M(y)) => M(linalg::solve(&x, &y)?),
        (Op::Solve, ..) => return Err("A\\b needs two matrices".into()),
        (Op::Pow, M(x), s) => {
            let n = s
                .num()
                .and_then(|n| n.as_int())
                .ok_or("matrix powers must be integers")?;
            let base = if n < 0 {
                linalg::inverse(&x)?
            } else {
                x.clone()
            };
            let (r, c) = linalg::dims(&x);
            if r != c {
                return Err("only square matrices have powers".into());
            }
            let mut acc = linalg::identity(r, r);
            for _ in 0..n.abs() {
                acc = linalg::matmul(&acc, &base)?;
            }
            M(acc)
        }
        (Op::Pow, _, M(_)) => return Err("matrix exponents are not supported".into()),
        (Op::Add, x, y) => add2(x, y),
        (Op::Sub, x, y) => sub(x, y),
        (Op::Mul, x, y) => mul2(x, y),
        (Op::Div, x, y) => div(x, y),
        (Op::Pow, x, y) => pow(x, y),
        (Op::EMul | Op::EDiv | Op::EPow, ..) => unreachable!(),
        (Op::And | Op::Or | Op::Eq | Op::Ne | Op::Lt | Op::Gt | Op::Le | Op::Ge, ..) => {
            return Err("comparisons are not arithmetic".into());
        }
    })
}

/// a == b, a < b, ...: true or false when that can be decided; a symbolic
/// a == b stays an equation (for solve, dsolve and implicitplot)
pub fn compare(op: Op, a: Expr, b: Expr) -> R<Expr> {
    // 3 m > 200 cm: compare the difference with 0
    if !matches!(a, Expr::Matrix(_)) && !matches!(b, Expr::Matrix(_))
        && (crate::units::has_units(&a) || crate::units::has_units(&b))
    {
        let d = arith(Op::Sub, a, b)?;
        let c = match crate::units::split(&d) {
            Some((c, _)) if !crate::units::has_units(&c) => c,
            _ => return Err(format!("can't compare quantities with units {}", show(&d))),
        };
        return compare(op, c, Expr::int(0));
    }
    if crate::ffield::as_elem(&a).is_some() || crate::ffield::as_elem(&b).is_some() {
        let eq = crate::ffield::equal(&a, &b)?;
        return match op {
            Op::Eq => Ok(Expr::Bool(eq)),
            Op::Ne => Ok(Expr::Bool(!eq)),
            _ => Err("elements of a finite ring can't be ordered".into()),
        };
    }
    if crate::quat::as_quat(&a).is_some() || crate::quat::as_quat(&b).is_some() {
        let eq = crate::quat::equal(&a, &b)?;
        return match op {
            Op::Eq => Ok(Expr::Bool(eq)),
            Op::Ne => Ok(Expr::Bool(!eq)),
            _ => Err("quaternions can't be ordered".into()),
        };
    }
    if crate::qring::as_elem(&a).is_some() || crate::qring::as_elem(&b).is_some() {
        let eq = crate::qring::equal(&a, &b)?;
        return match op {
            Op::Eq => Ok(Expr::Bool(eq)),
            Op::Ne => Ok(Expr::Bool(!eq)),
            _ => Err("elements of a finite ring can't be ordered".into()),
        };
    }
    if let (Expr::Bool(_), _) | (_, Expr::Bool(_)) | (Expr::Str(_), _) | (_, Expr::Str(_)) =
        (&a, &b)
    {
        return match op {
            Op::Eq => Ok(Expr::Bool(a == b)),
            Op::Ne => Ok(Expr::Bool(a != b)),
            _ => Err("only numbers can be ordered".into()),
        };
    }
    // elementwise order comparisons: [1 2 3] < 2 gives [1, 0, 0]
    if matches!(op, Op::Lt | Op::Gt | Op::Le | Op::Ge) {
        let as01 = |r: R<Expr>| -> R<Expr> {
            match r? {
                Expr::Bool(t) => Ok(Expr::int(t as i128)),
                _ => Err("comparisons in a matrix need numbers".into()),
            }
        };
        match (&a, &b) {
            (Expr::Matrix(m), Expr::Matrix(n)) => {
                if m.len() != n.len() || m.iter().zip(n).any(|(r, s)| r.len() != s.len()) {
                    return Err("compared matrices must have the same size".into());
                }
                let rows: Vec<Vec<Expr>> = m
                    .iter()
                    .zip(n)
                    .map(|(r, s)| r.iter().zip(s).map(|(x, y)| as01(compare(op, x.clone(), y.clone()))).collect::<R<_>>())
                    .collect::<R<_>>()?;
                return Ok(Expr::Matrix(rows));
            }
            (Expr::Matrix(m), y) => {
                let rows: Vec<Vec<Expr>> = m.iter().map(|r| r.iter().map(|x| as01(compare(op, x.clone(), y.clone()))).collect::<R<_>>()).collect::<R<_>>()?;
                return Ok(Expr::Matrix(rows));
            }
            (x, Expr::Matrix(n)) => {
                let rows: Vec<Vec<Expr>> = n.iter().map(|r| r.iter().map(|y| as01(compare(op, x.clone(), y.clone()))).collect::<R<_>>()).collect::<R<_>>()?;
                return Ok(Expr::Matrix(rows));
            }
            _ => {}
        }
    }
    if let (Expr::Matrix(_), _) | (_, Expr::Matrix(_)) = (&a, &b) {
        // equal matrices: same shape and entries that simplify to the same value
        let same = || -> bool {
            match (&a, &b) {
                (Expr::Matrix(x), Expr::Matrix(y)) => {
                    x.len() == y.len()
                        && x.iter().zip(y).all(|(r, s)| {
                            r.len() == s.len()
                                && r.iter().zip(s).all(|(p, q)| {
                                    p == q
                                        || matches!(
                                            compare(Op::Eq, p.clone(), q.clone()),
                                            Ok(Expr::Bool(true))
                                        )
                                })
                        })
                }
                _ => false,
            }
        };
        return match op {
            Op::Eq => Ok(Expr::Bool(same())),
            Op::Ne => Ok(Expr::Bool(!same())),
            _ => Err("matrices can't be ordered".into()),
        };
    }
    let d = calc::simplify(&sub(a.clone(), b.clone()));
    let num = |e: &Expr| calc::evalf(e).num().map(|n| n.to_f64());
    match op {
        Op::Eq | Op::Ne => {
            let same = if d.is_zero() {
                Some(true)
            } else if d.symbols().is_empty() && !d.contains_sym("i") {
                num(&d).map(|v| v.abs() < 1e-12 * (1.0 + num(&a).unwrap_or(0.0).abs()))
            } else {
                None
            };
            match (same, op) {
                (Some(s), Op::Eq) => Ok(Expr::Bool(s)),
                (Some(s), _) => Ok(Expr::Bool(!s)),
                (None, Op::Eq) => Ok(Expr::Apply("==".into(), vec![a, b])),
                (None, _) => Err(format!("can't decide whether {} ~= {}", show(&a), show(&b))),
            }
        }
        _ => {
            let (Some(x), Some(y)) = (num(&a), num(&b)) else {
                return Err(format!(
                    "can't compare {} and {}: they must be numbers",
                    show(&a),
                    show(&b)
                ));
            };
            Ok(Expr::Bool(match op {
                Op::Lt => x < y,
                Op::Gt => x > y,
                Op::Le => x <= y,
                _ => x >= y,
            }))
        }
    }
}

/// An equation a == b as the expression a - b (other expressions unchanged)
pub fn equation_lhs(e: &Expr) -> Expr {
    match e {
        Expr::Apply(f, v) if f == "==" && v.len() == 2 => sub(v[0].clone(), v[1].clone()),
        other => other.clone(),
    }
}

/// Reject division by an exact zero anywhere in the result
fn check(e: &Expr) -> R<()> {
    match e {
        Expr::Pow(b, p)
            if b.is_zero()
                && b.num().unwrap().is_exact()
                && p.num().is_some_and(|n| n.is_negative()) =>
        {
            Err("division by zero".into())
        }
        Expr::Add(v) | Expr::Mul(v) => v.iter().try_for_each(check),
        Expr::Pow(a, b) => check(a).and(check(b)),
        Expr::Func(_, a) => check(a),
        Expr::Matrix(m) => m.iter().flatten().try_for_each(check),
        _ => Ok(()),
    }
}

/// an expression as source code again, so it can go into a function body
fn expr_node(v: &Expr) -> R<Node> {
    match v {
        Expr::Matrix(m) => Ok(Node::Matrix(
            m.iter()
                .map(|r| r.iter().map(expr_node).collect::<R<_>>())
                .collect::<R<_>>()?,
        )),
        Expr::Str(s) => Ok(Node::Str(s.clone())),
        e => match crate::parser::parse(&show(e)) {
            Ok(mut st) if st.len() == 1 => match st.pop() {
                Some(crate::parser::Stmt::Expr(n)) => Ok(n),
                _ => Err(format!("cannot use {} in a function body", show(e))),
            },
            _ => Err(format!("cannot use {} in a function body", show(e))),
        },
    }
}

fn uses_ident(n: &Node, name: &str) -> bool {
    let u = |m: &Node| uses_ident(m, name);
    match n {
        Node::Ident(s) => s == name,
        Node::Neg(a) | Node::Transpose(a) => u(a),
        Node::Bin(_, a, b) => u(a) || u(b),
        Node::Range(a, s, b) => u(a) || s.as_deref().is_some_and(u) || u(b),
        Node::Call(_, args) => args.iter().any(u),
        Node::Index(a, args) => u(a) || args.iter().any(u),
        Node::Matrix(m) => m.iter().flatten().any(u),
        _ => false,
    }
}

/// body with every use of the name replaced by node
fn replace_ident(body: &Node, name: &str, node: &Node) -> Node {
    let r = |n: &Node| replace_ident(n, name, node);
    let rb = |n: &Box<Node>| Box::new(replace_ident(n, name, node));
    match body {
        Node::Ident(s) if s == name => node.clone(),
        Node::Neg(a) => Node::Neg(rb(a)),
        Node::Bin(op, a, b) => Node::Bin(*op, rb(a), rb(b)),
        Node::Range(a, s, b) => Node::Range(rb(a), s.as_ref().map(rb), rb(b)),
        Node::Call(f, args) => Node::Call(f.clone(), args.iter().map(r).collect()),
        Node::Index(a, args) => Node::Index(rb(a), args.iter().map(r).collect()),
        Node::Matrix(m) => Node::Matrix(m.iter().map(|row| row.iter().map(r).collect()).collect()),
        Node::Transpose(a) => Node::Transpose(rb(a)),
        other => other.clone(),
    }
}

/// a:step:b as a row vector; exact when the inputs are exact
fn range(a: &Expr, s: &Expr, b: &Expr) -> R<Expr> {
    let (Some(an), Some(sn), Some(bn)) = (a.num(), s.num(), b.num()) else {
        return Err("ranges need numbers".into());
    };
    if sn.is_zero() {
        return Err("range step can't be zero".into());
    }
    let count = (bn.sub(&an).to_f64() / sn.to_f64() + 1e-10).floor();
    if count < 0.0 {
        return Ok(Expr::Matrix(vec![vec![]]));
    }
    if count > 1e7 {
        return Err("range is too long".into());
    }
    // decimal steps like 0:0.1:0.5 give 0.3, not 0.30000000000000004
    let decimals = |n: &Number| -> Option<u32> {
        let s = format!("{}", n.to_f64());
        if s.contains('e') {
            None
        } else {
            Some(s.split('.').nth(1).map_or(0, |f| f.len()) as u32)
        }
    };
    let scale = match (an.is_exact() && sn.is_exact(), decimals(&an), decimals(&sn)) {
        (false, Some(x), Some(y)) if x.max(y) <= 12 => Some(10f64.powi(x.max(y) as i32)),
        _ => None,
    };
    let row = (0..=count as i128)
        .map(|k| match scale {
            Some(sc) => {
                let v = ((an.to_f64() * sc).round() + k as f64 * (sn.to_f64() * sc).round()) / sc;
                Expr::float(v)
            }
            None => Expr::Num(an.add(sn.mul(Number::int(k)))),
        })
        .collect();
    Ok(Expr::Matrix(vec![row]))
}

/// info(e): the expression tree
fn structure(e: &Expr) -> String {
    use Expr::*;
    let list = |v: &[Expr]| v.iter().map(structure).collect::<Vec<_>>().join(", ");
    match e {
        Num(n) => format!("Number({})", n),
        Sym(s) => format!("Symbol({})", s),
        Const(_) => format!("Constant({})", show(e)),
        Add(v) => format!("Add({})", list(v)),
        Mul(v) => format!("Mul({})", list(v)),
        Pow(a, b) => format!("Pow({}, {})", structure(a), structure(b)),
        Func(f, a) => format!("{}({})", f.name(), structure(a)),
        Matrix(m) => format!("Matrix({}x{})", m.len(), m.first().map_or(0, |r| r.len())),
        Bool(b) => format!("Bool({})", b),
        Str(s) => format!("String({})", s),
        Apply(f, v) => format!("Function {}({})", f, list(v)),
        Obj(o) => o.kind().to_string(),
    }
}

/// The script a statement loads: load(file), but not load(RSA)
fn script_file(s: &Stmt) -> Option<&str> {
    match s {
        Stmt::Command(c, Some(f)) if c == "load" && !unquote(f).eq_ignore_ascii_case("rsa") => {
            Some(f.as_str())
        }
        _ => None,
    }
}

/// A file name without surrounding quotes: load("my file.gg")
fn unquote(s: &str) -> &str {
    let t = s.trim();
    for q in ['"', '\''] {
        if t.len() >= 2 && t.starts_with(q) && t.ends_with(q) {
            return &t[1..t.len() - 1];
        }
    }
    t
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run(s: &str) -> String {
        Session::new().run_values(s)
    }
    #[test]
    fn basics() {
        assert_eq!(run("1+10+u+1"), "u+12");
        assert_eq!(run("8*y*u*y*u*y^5*y*u*u^6*r*y*7*y"), "56*r*u^9*y^10");
        assert_eq!(run("4*va-5*va"), "-va");
        assert_eq!(run("1/3+2/6"), "2/3");
        assert_eq!(run("a=1;b=2;c=a+b"), "1\n2\n3");
        assert_eq!(run("j^3"), "-i");
        assert_eq!(run("log(e^5*e^4)"), "9");
        assert_eq!(run("1/0"), "Error: division by zero");
        assert_eq!(run("(1&&0)||true"), "true");
        assert_eq!(
            run("A=[4 5 6;6 5 4;7 8 7];norm(inv(A)*A-eye(3),1)")
                .lines()
                .last()
                .unwrap(),
            "0"
        );
        assert_eq!(run("deriv(x^3,x)"), "3*x^2");
        assert_eq!(run("0.1:0.1:0.3").split(", ").count(), 3);
        assert_eq!(run("1:3"), "[1, 2, 3]");
    }
    #[test]
    fn explain_levels() {
        let notes = |src: &str| -> String {
            let mut s = Session::new();
            s.run_notes(src)
                .into_iter()
                .last()
                .map(|(_, _, n)| markdown_notes(&n))
                .unwrap_or_default()
        };
        // brief: the log; teach: a walk-through with this run's numbers; off: nothing
        let brief = notes("gnfs(2^128 + 1)");
        assert!(brief.contains("5. Square roots"));
        let teach = notes("explain = \"teach\"; gnfs(2^128 + 1)");
        for part in [
            "**Step 1",
            "2 · N",
            "**Step 3",
            "(a, b) = (-100, 1)",
            "**Step 6",
            "X² − Y² mod N = 0",
            "gcd(X − Y, N) = 5704689200685129054721",
        ] {
            assert!(teach.contains(part), "missing {}: {}", part, teach);
        }
        let split = notes("explain = \"teach\"; gnfs(2^78 + 1, x^4 + 4, 2^20)");
        assert!(split.contains("**It factors.**") && split.contains("1099509530626"));
        let poly = notes("explain = \"teach\"; nfspoly(2^128 + 1)");
        assert!(poly.contains("The degree is a balance") && poly.contains("0 mod 7"));
        assert_eq!(notes("explain = false; gnfs(2^128 + 1)"), "");
        // commands without a teaching note give their whole help text
        assert!(notes("explain = \"teach\"; mod(7, 3)").contains("greatest common divisor"));
    }
    #[test]
    fn tail_calls_and_big_integers() {
        let out = run("count(n, k) := if(k == n, k, count(n, k + 1)); count(100000, 0)");
        assert_eq!(out.lines().last().unwrap(), "100000");
        assert_eq!(
            run("fact(n) := if(n == 0, 1, n*fact(n-1)); fact(20)")
                .lines()
                .last()
                .unwrap(),
            "2432902008176640000"
        );
        assert_eq!(run("gcd(2^60-1, 2^40-1)"), "1048575");
        assert_eq!(run("mod([3 6 9], 7)"), "[3, 6, 2]");
        assert_eq!(
            run("tf(n) := if(n == 1, [], [2, tf(n/2)]); tf(8)")
                .lines()
                .last()
                .unwrap(),
            "[2, 2, 2]"
        );
        assert_eq!(run("[1 true]"), "[1, true]");
        assert_eq!(run("rows([1 2; 3 4; 5 6])"), "3");
        // rows [prime, exponent]; the 156-digit part that does not split
        // has a row of its own
        let f = run("factor(10^300 + 1)");
        let rows: Vec<&str> = f
            .lines()
            .map(|l| l.trim_matches(|c| " [];".contains(c)))
            .collect();
        assert_eq!(&rows[..3], &["73, 1", "137, 1", "401, 1"]);
        assert!(
            rows.iter()
                .any(|r| r.split(", ").next().unwrap().len() == 156)
        );
        assert_eq!(run("factor(expand((a*x+b)*(c*x+d)))"), "(a*x+b)*(c*x+d)");
        assert_eq!(run("factor(x^2 + 1, i)"), "(x+i)*(x-i)");
        assert_eq!(run("factor(x^4 + 1, 17)"), "(x+2)*(x+8)*(x+9)*(x+15)");
        assert_eq!(run("factor(x^2 + y^2, i)"), "(x-i*y)*(x+i*y)");
        assert!(
            Session::new()
                .run_to_string("N = (2^1062 + 1)/5; algfactor(N)")
                .contains("divides 2^1062 + 1")
        );
        assert!(
            Session::new()
                .run_to_string("N = 7^50 + 3^50; algfactor(N)")
                .contains("All 7 prime factors found")
        );
        assert!(
            Session::new()
                .run_to_string("N = 31415926^5 + 31415926 + 1; algfactor(N)")
                .contains("x^5 + x + 1 with x = 31415926")
        );
        assert!(
            Session::new()
                .run_to_string("N = 98765^4 + 4*7^4; algfactor(N)")
                .contains("x^4 + 9604 with x = 98765")
        );
        assert!(
            run("isprime(2^(2^2281 - 1) - 1)")
                .contains("too large to compute: it would have about 10^686 digits")
        );
        assert!(
            run("isprime(eval(2^(2^2281 - 1) - 1))").contains("decimal too large to represent")
        );
        assert!(run("factor(2^(10^9) + 1)").contains("about 301029996 digits"));
        assert!(run("mod(2^(10^7), 7)").contains("too large to compute"));
        let out = run("f(x) := 8 + 3*x^2 + 9*x; deriv(f(x), x); d(x) := ans; deriv(d(x), x); d(3)");
        assert_eq!(out.lines().last().unwrap(), "27");
        assert!(out.contains("d(x) := 6*x+9"));
        assert!(run("m = [1 2; 3 4]; m*2; g(k) := k*ans; g(2)").ends_with("[ 4,  8;\n 12, 16]"));
        assert_eq!(run("h(ans) := ans + 1; h(1)").lines().last().unwrap(), "2");
        assert_eq!(
            run("p = plot(x, x, 0, 1); q(x) := x + 1; q(1)")
                .lines()
                .last()
                .unwrap(),
            "2"
        );
        assert_eq!(
            run("2^200"),
            "1606938044258990275541962092341162602522202993782792835301376"
        );
        assert_eq!(run("mod(3^100, 7)"), "4");
        assert_eq!(run("isprime(2^521 - 1)"), "true");
        assert_eq!(
            run("factor(2^127 - 3)"),
            "[                            5, 2;\n                     213156431, 1;\n 31927947500766558008599290859, 1]"
        );
        assert_eq!(
            run("decode(encode(\"a message longer than fifteen characters\"))"),
            "\"a message longer than fifteen characters\""
        );
        assert_eq!(
            run("floor(10^40/7)"),
            "1428571428571428571428571428571428571428"
        );
        assert_eq!(run("1/(2^200) * 2^201"), "2");
        assert_eq!(
            run("170141183460469231731687303715884105728 - 1"),
            "170141183460469231731687303715884105727"
        );
        assert_eq!(run("rank([1 2 3; 4 5 6; 7 8 9])"), "2");
        assert_eq!(run("null([1 2 3; 4 5 6; 7 8 9])"), "[ 1;\n -2;\n  1]");
        assert_eq!(run("eigvec([2 1; 1 2])"), "[-1, 1;\n  1, 1]");
        assert_eq!(run("v = [3 5 7]; v(2)").lines().last().unwrap(), "5");
        assert_eq!(run("A = [1 2; 3 4]; A(2, 1)").lines().last().unwrap(), "3");
        assert_eq!(
            run("A = [1 2; 3 4]; A(2)").lines().last().unwrap(),
            "[3, 4]"
        );
    }
}

/// eval of something like exp(-2*i/3) or polylog(2, i): the complex number
fn complex_evalf(e: &Expr) -> Expr {
    match e {
        Expr::Matrix(m) => Expr::Matrix(m.iter().map(|r| r.iter().map(complex_evalf).collect()).collect()),
        Expr::Num(_) => e.clone(),
        _ if e.symbols().is_empty() => match crate::cnum::ceval(e, &[]) {
            Some(c) if c.re.is_finite() && c.im.is_finite() => {
                let re = Expr::float(c.re);
                if c.im == 0.0 {
                    re
                } else {
                    let im = mul2(Expr::float(c.im), Expr::Const(crate::expr::Const::I));
                    if c.re == 0.0 { im } else { add2(re, im) }
                }
            }
            _ => e.clone(),
        },
        _ => e.clone(),
    }
}

/// Complex conjugate, treating symbols as real: i becomes -i
pub(crate) fn conj_expr(e: &Expr) -> Expr {
    match e {
        Expr::Num(_) | Expr::Sym(_) | Expr::Str(_) | Expr::Bool(_) | Expr::Obj(_) => e.clone(),
        Expr::Const(crate::expr::Const::I) => crate::build::neg(e.clone()),
        _ => e.map_children(&mut |c| conj_expr(c)),
    }
}
