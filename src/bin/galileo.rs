//! Galileo: `galileo` for an interactive prompt, `galileo file.gg` to run a file,
//! `galileo -i file.gg` to run a file and then keep the prompt open,
//! `galileo mcp` to serve the Model Context Protocol for AI assistants,
//! `galileo --kernel` to serve the notebook kernel protocol on stdin/stdout.

use galileo_rs::{Output, Session};
use std::io::{self, BufRead, IsTerminal};

/// Print one result (hidden when its statement ended in ;), then its explanations
fn show(r: Result<Output, String>, quiet: bool, notes: &[galileo_rs::eval::Note]) {
    match r {
        Ok(o) => {
            let t = o.render();
            if !quiet && !t.is_empty() {
                println!("{}", t);
            }
            // explanations as comments, so the output stays Galileo source
            let c = galileo_rs::eval::comment_notes(notes);
            if !quiet && !c.is_empty() {
                println!("{}", c);
            }
        }
        Err(e) => println!("Error: {}", e),
    }
}

/// Print results; statements ending in ; stay quiet. Returns false on exit.
fn report(s: &mut Session, src: &str) -> bool {
    for (r, quiet, notes) in s.run_notes(src) {
        if matches!(r, Ok(Output::Exit)) {
            return false;
        }
        show(r, quiet, &notes);
    }
    true
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    // deep expressions recurse deeply: give the engine a large stack
    let worker = std::thread::Builder::new()
        .stack_size(512 << 20)
        .spawn(move || run(args))
        .expect("thread");
    let code = worker.join().unwrap_or(1);
    std::process::exit(code);
}

fn run(args: Vec<String>) -> i32 {
    match args.first().map(|s| s.as_str()) {
        Some("--kernel") => {
            galileo_rs::kernel::serve();
            0
        }
        Some("mcp") => galileo_rs::mcp::main(&args[1..]),
        Some("-h") | Some("--help") => {
            println!(
                "usage: galileo [-i] [file.gg] | galileo mcp | galileo --kernel | galileo --version\n\
                 \n  galileo            interactive prompt\n\
                 \x20 galileo file.gg    run a file\n\
                 \x20 galileo -i file.gg run a file, then keep the prompt open with its definitions\n\
                 \x20 galileo mcp        serve the Model Context Protocol, for AI assistants (galileo mcp --help)"
            );
            0
        }
        Some("--version") => {
            println!("galileo {}", env!("CARGO_PKG_VERSION"));
            0
        }
        Some("-i") => {
            let mut s = Session::new();
            // the prompt opens even after an error, so it can be looked into
            for file in &args[1..] {
                if !run_file(&mut s, file) {
                    break;
                }
            }
            prompt(&mut s)
        }
        Some(file) => {
            let mut s = Session::new();
            if run_file(&mut s, file) { 0 } else { 1 }
        }
        None => prompt(&mut Session::new()),
    }
}

/// Run a script and print its results; false when it stopped on an error
fn run_file(s: &mut Session, file: &str) -> bool {
    let mut ok = true;
    for (r, quiet, notes) in s.load_file(file) {
        if r.is_err() {
            ok = false;
        }
        show(r, quiet, &notes);
    }
    ok
}

/// The prompt: line editing on a terminal, plain line reading otherwise
fn prompt(s: &mut Session) -> i32 {
    if io::stdin().is_terminal() {
        interactive(s)
    } else {
        piped(s)
    }
}

/// Input piped in (no terminal): read lines, no prompt
fn piped(s: &mut Session) -> i32 {
    let stdin = io::stdin();
    // a statement continues while brackets are open, so a matrix
    // printed over several lines can be pasted back in
    let mut buf = String::new();
    loop {
        let mut line = String::new();
        if stdin.lock().read_line(&mut line).unwrap_or(0) == 0 {
            if !buf.is_empty() {
                report(s, &buf);
            }
            return 0;
        }
        buf.push_str(&line);
        if galileo_rs::parser::needs_more(&buf) {
            continue;
        }
        let src = std::mem::take(&mut buf);
        if !report(s, &src) {
            return 0;
        }
    }
}

/// Where the prompt keeps its history: $GALILEO_HISTORY, else ~/.galileo_history
fn history_path() -> Option<std::path::PathBuf> {
    if let Some(p) = std::env::var_os("GALILEO_HISTORY") {
        return Some(p.into());
    }
    std::env::var_os("HOME").map(|h| std::path::Path::new(&h).join(".galileo_history"))
}

/// The interactive prompt, with line editing: arrow keys move through the
/// line and the history, backspace deletes, Ctrl-R searches the history,
/// Ctrl-C drops the current input, Ctrl-D quits.
fn interactive(s: &mut Session) -> i32 {
    use rustyline::error::ReadlineError;
    println!(
        "Galileo {} (type help for commands, exit to quit)",
        env!("CARGO_PKG_VERSION")
    );
    let mut rl = match rustyline::DefaultEditor::new() {
        Ok(rl) => rl,
        // no usable terminal after all: fall back to plain reading
        Err(_) => return piped(s),
    };
    let history = history_path();
    if let Some(h) = &history {
        rl.load_history(h).ok();
    }
    let mut buf = String::new();
    let code = loop {
        let prompt = if buf.is_empty() {
            "galileo> "
        } else {
            "    ...> "
        };
        match rl.readline(prompt) {
            Ok(line) => {
                buf.push_str(&line);
                buf.push('\n');
                if galileo_rs::parser::needs_more(&buf) {
                    continue;
                }
                let src = std::mem::take(&mut buf);
                let entry = src.trim_end();
                if !entry.trim().is_empty() {
                    rl.add_history_entry(entry).ok();
                }
                if !report(s, &src) {
                    break 0;
                }
            }
            // Ctrl-C: forget the unfinished statement, keep going
            Err(ReadlineError::Interrupted) => buf.clear(),
            // Ctrl-D: run what is left, then quit
            Err(ReadlineError::Eof) => {
                if !buf.is_empty() {
                    report(s, &buf);
                }
                break 0;
            }
            Err(e) => {
                eprintln!("{}", e);
                break 1;
            }
        }
    };
    if let Some(h) = &history {
        rl.save_history(h).ok();
    }
    code
}
