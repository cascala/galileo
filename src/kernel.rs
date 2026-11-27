//! `galileo --kernel`: a line protocol for driving a session from another
//! process (the Jupyter kernel in iGalileo is a thin Python wrapper over it).
//!
//! Each request is a header line followed by a body of exactly `length` bytes:
//!
//! ```text
//! execute <length>\n<code>
//! complete <cursor> <length>\n<code>
//! inspect <cursor> <length>\n<code>
//! ```
//!
//! Each reply is one line of JSON. `cursor` counts characters (Unicode code points).

use crate::display::{Bundle, Shown, shown};
use crate::eval::{Session, comment_notes, markdown_notes};
use crate::help;
use crate::plot::jstr;
use std::io::{BufRead, Read, Write};

fn bundle_json(b: &Bundle) -> String {
    let fields: Vec<String> = b
        .iter()
        .map(|(k, v)| format!("{}:{}", jstr(k), jstr(v)))
        .collect();
    format!("{{{}}}", fields.join(","))
}

/// Run a cell: {"outputs": [{"text": ...} | {"data": {mime: content}} | {"data": ..., "notes": true}], "error": null | {...}}
///
/// A result is plain Galileo; its explanation, when `explain` is on, follows it as a
/// separate output marked `"notes": true` (Markdown, and `//` comments as plain text).
pub fn execute(s: &mut Session, code: &str) -> String {
    let mut outputs = vec![];
    let mut error = "null".to_string();
    for (r, quiet, notes) in s.run_notes(code) {
        match r {
            Ok(o) => {
                if quiet {
                    continue;
                }
                match shown(&o) {
                    Shown::Rich(b) => outputs.push(format!("{{\"data\":{}}}", bundle_json(&b))),
                    Shown::Text(t) => outputs.push(format!("{{\"text\":{}}}", jstr(&t))),
                    Shown::Nothing => {}
                }
                if !notes.is_empty() {
                    // a block quote, so notebooks (and GitHub) set notes apart from results
                    let md = markdown_notes(&notes)
                        .lines()
                        .map(|l| {
                            if l.is_empty() {
                                ">".to_string()
                            } else {
                                format!("> {}", l)
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    let b: Bundle =
                        vec![("text/plain", comment_notes(&notes)), ("text/markdown", md)];
                    outputs.push(format!("{{\"data\":{},\"notes\":true}}", bundle_json(&b)));
                }
            }
            Err(e) => {
                let name = if e.starts_with("parse error") {
                    "SyntaxError"
                } else {
                    "Error"
                };
                let msg = e.strip_prefix("parse error: ").unwrap_or(&e);
                error = format!("{{\"ename\":{},\"evalue\":{}}}", jstr(name), jstr(msg));
            }
        }
    }
    format!(
        "{{\"outputs\":[{}],\"error\":{}}}",
        outputs.join(","),
        error
    )
}

fn is_name(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.'
}

/// The word around the cursor: (start, end) in characters
fn word(chars: &[char], cursor: usize) -> (usize, usize) {
    let pos = cursor.min(chars.len());
    let mut start = pos;
    while start > 0 && is_name(chars[start - 1]) {
        start -= 1;
    }
    let mut end = pos;
    while end < chars.len() && is_name(chars[end]) {
        end += 1;
    }
    (start, end)
}

/// Tab completion: built-in names and the session's variables
pub fn complete(s: &Session, code: &str, cursor: usize) -> String {
    let chars: Vec<char> = code.chars().collect();
    let end = cursor.min(chars.len());
    let (start, _) = word(&chars, end);
    let prefix: String = chars[start..end].iter().collect();
    let mut names: Vec<String> = help::names().iter().map(|s| s.to_string()).collect();
    names.extend(crate::geometry::TEMPLATES.iter().map(|s| s.to_string()));
    names.extend(s.variable_names());
    names.sort();
    names.dedup();
    let matches: Vec<String> = if prefix.is_empty() {
        vec![]
    } else {
        names
            .into_iter()
            .filter(|n| n.starts_with(&prefix))
            .map(|n| jstr(&n))
            .collect()
    };
    format!(
        "{{\"matches\":[{}],\"start\":{},\"end\":{}}}",
        matches.join(","),
        start,
        end
    )
}

/// Shift+Tab: help for the name under (or just before) the cursor
pub fn inspect(code: &str, cursor: usize) -> String {
    let chars: Vec<char> = code.chars().collect();
    let pos = cursor.min(chars.len());
    let (mut start, mut end) = word(&chars, pos);
    // cursor right after "name(" or inside the parentheses: use the function name
    if start == end {
        let mut depth = 0;
        let mut i = pos;
        while i > 0 {
            i -= 1;
            match chars[i] {
                ')' => depth += 1,
                '(' if depth == 0 => {
                    let w = word(&chars, i);
                    start = w.0;
                    end = i;
                    break;
                }
                '(' => depth -= 1,
                _ => {}
            }
        }
    }
    let w: String = chars[start..end].iter().collect();
    match help::entry(&w) {
        Some(e) => format!(
            "{{\"found\":true,\"data\":{{\"text/plain\":{},\"text/markdown\":{}}}}}",
            jstr(&help::plain(e)),
            jstr(&help::markdown(e))
        ),
        None => "{\"found\":false,\"data\":{}}".into(),
    }
}

fn handle(s: &mut Session, header: &str, body: &str) -> String {
    let parts: Vec<&str> = header.split_whitespace().collect();
    let cursor = parts.get(1).and_then(|c| c.parse().ok()).unwrap_or(0);
    match parts.first().copied() {
        Some("execute") => execute(s, body),
        Some("complete") => complete(s, body, cursor),
        Some("inspect") => inspect(body, cursor),
        Some("version") => format!("{{\"version\":{}}}", jstr(env!("CARGO_PKG_VERSION"))),
        _ => format!(
            "{{\"error\":{}}}",
            jstr(&format!("unknown request {}", header))
        ),
    }
}

/// Serve requests on stdin until it closes
pub fn serve() {
    let stdin = std::io::stdin();
    let mut input = stdin.lock();
    let stdout = std::io::stdout();
    let mut session = Session::new();
    loop {
        let mut header = String::new();
        if input.read_line(&mut header).unwrap_or(0) == 0 {
            return;
        }
        let header = header.trim();
        if header.is_empty() {
            continue;
        }
        let len: usize = header
            .split_whitespace()
            .last()
            .and_then(|n| n.parse().ok())
            .unwrap_or(0);
        let mut buf = vec![0u8; len];
        if input.read_exact(&mut buf).is_err() {
            return;
        }
        let body = String::from_utf8_lossy(&buf).into_owned();
        let reply = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| handle(&mut session, header, &body)))
            .unwrap_or_else(|_| {
                "{\"outputs\":[],\"error\":{\"ename\":\"InternalError\",\"evalue\":\"the engine crashed on this input\"}}".into()
            });
        let mut out = stdout.lock();
        let _ = writeln!(out, "{}", reply);
        let _ = out.flush();
    }
}
