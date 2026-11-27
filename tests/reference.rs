//! Keeps the language reference honest. CI runs these, so a change to the language
//! fails until the reference follows it:
//!
//! * every example in docs/language.md runs and shows what the reference says;
//! * the syntax stamp in docs/language.md matches src/parser.rs, so any change to the
//!   parser needs a look at the reference (then a new stamp);
//! * docs/builtins.md is generated from the built-in help (src/help.rs);
//! * every command the engine answers to has a help entry, and every help example runs.
//!
//! After updating the reference: `GALILEO_BLESS=1 cargo test --test reference` writes
//! docs/builtins.md and the syntax stamp.

use galileo_rs::eval::Session;
use galileo_rs::expr::Expr;
use galileo_rs::help;
use galileo_rs::Output;
use std::collections::BTreeSet;
use std::path::PathBuf;

const STAMP: &str = "<!-- syntax stamp: ";

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn read(rel: &str) -> String {
    std::fs::read_to_string(root().join(rel)).unwrap_or_else(|e| panic!("{}: {}", rel, e))
}

fn blessing() -> bool {
    std::env::var("GALILEO_BLESS").is_ok_and(|v| v != "0" && !v.is_empty())
}

fn quiet_session() -> Session {
    let mut s = Session::new();
    s.set("explain", Expr::Bool(false));
    s
}

/// what the prompt shows for some input (explanations off)
fn shown(s: &mut Session, input: &str) -> Vec<String> {
    let mut out = vec![];
    for (r, quiet, _) in s.run_notes(input) {
        match r {
            Ok(Output::Exit) => break,
            Ok(o) => {
                let t = o.render();
                if !quiet && !t.is_empty() {
                    out.extend(t.lines().map(str::to_string));
                }
            }
            Err(e) => out.push(format!("Error: {}", e)),
        }
    }
    out
}

/// one input and the lines it should show
type Step = (String, Vec<String>);

/// The ```galileo transcripts of a Markdown file: (line number, steps)
fn transcripts(md: &str) -> Vec<(usize, Vec<Step>)> {
    let mut blocks = vec![];
    let mut cur: Option<(usize, Vec<Step>)> = None;
    for (n, line) in md.lines().enumerate() {
        let t = line.trim_end();
        match &mut cur {
            None if t == "```galileo" => cur = Some((n + 2, vec![])),
            None => {}
            Some(_) if t == "```" => blocks.push(cur.take().unwrap()),
            Some((_, steps)) => {
                if let Some(input) = t.strip_prefix("galileo> ") {
                    steps.push((input.to_string(), vec![]));
                } else if let Some(more) = t.strip_prefix("...> ") {
                    let last = steps.last_mut().expect("...> without galileo> before it");
                    last.0 = format!("{}\n{}", last.0, more);
                } else {
                    let last = steps.last_mut().expect("output without galileo> before it");
                    last.1.push(t.to_string());
                }
            }
        }
    }
    assert!(cur.is_none(), "unclosed ```galileo block");
    blocks
}

#[test]
fn language_examples_show_what_the_reference_says() {
    let md = read("docs/language.md");
    let blocks = transcripts(&md);
    assert!(blocks.len() > 10, "docs/language.md has too few examples");
    let mut wrong = vec![];
    for (line, steps) in blocks {
        let mut s = quiet_session();
        for (input, expected) in steps {
            let got = shown(&mut s, &input);
            if got != expected {
                wrong.push(format!(
                    "docs/language.md:{}: galileo> {}\n  the reference says:\n    {}\n  Galileo shows:\n    {}",
                    line,
                    input.replace('\n', "\n  ...> "),
                    expected.join("\n    "),
                    got.join("\n    ")
                ));
            }
        }
    }
    assert!(wrong.is_empty(), "\n{}\n", wrong.join("\n\n"));
}

/// FNV-1a of the parser source without comments and white space, so only code
/// changes count
fn parser_stamp() -> String {
    let src = read("src/parser.rs");
    let mut h: u64 = 0xcbf29ce484222325;
    for line in src.lines() {
        let t = line.trim();
        if t.starts_with("//") {
            continue;
        }
        for b in t.bytes().filter(|b| !b.is_ascii_whitespace()) {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
    }
    format!("{:016x}", h)
}

#[test]
fn reference_is_reviewed_for_the_current_parser() {
    let path = root().join("docs/language.md");
    let md = read("docs/language.md");
    let stamp = parser_stamp();
    let line = format!("{}{} -->", STAMP, stamp);
    let current = md.lines().find(|l| l.starts_with(STAMP));
    if current == Some(line.as_str()) {
        return;
    }
    if blessing() {
        let new = match current {
            Some(old) => md.replacen(old, &line, 1),
            None => format!("{}\n\n{}", line, md),
        };
        std::fs::write(path, new).unwrap();
        return;
    }
    panic!(
        "\nsrc/parser.rs changed, so the syntax may have changed. Update docs/language.md \
         for the change (and its examples), then record the review with\n\n  \
         GALILEO_BLESS=1 cargo test --test reference\n"
    );
}

#[test]
fn command_reference_is_generated_from_help() {
    let path = root().join("docs/builtins.md");
    let want = help::reference_markdown();
    let have = std::fs::read_to_string(&path).unwrap_or_default();
    if have == want {
        return;
    }
    if blessing() {
        std::fs::write(path, want).unwrap();
        return;
    }
    panic!(
        "\ndocs/builtins.md is out of date with src/help.rs. Regenerate it with\n\n  \
         GALILEO_BLESS=1 cargo test --test reference\n"
    );
}

/// names with a help entry, an alias, or a mention as name( in a usage line
fn documented() -> BTreeSet<String> {
    let mut d: BTreeSet<String> = help::ENTRIES.iter().map(|e| e.name.to_string()).collect();
    d.extend(help::ALIASES.iter().map(|a| a.0.to_string()));
    for e in help::ENTRIES {
        let u: Vec<char> = e.usage.chars().collect();
        let mut i = 0;
        while i < u.len() {
            if u[i].is_alphabetic() {
                let start = i;
                while i < u.len() && (u[i].is_alphanumeric() || u[i] == '_' || u[i] == '.') {
                    i += 1;
                }
                d.insert(u[start..i].iter().collect());
            } else {
                i += 1;
            }
        }
    }
    d
}

/// Every name the engine answers to as a function has help. Candidates are the
/// lower-case words in string literals of the engine's source; a candidate is a
/// command when calling it with no arguments gives something other than "unknown
/// function" (or the undefined function name(), for names like x0).
#[test]
fn every_command_has_help() {
    let documented = documented();
    let mut candidates = BTreeSet::new();
    for f in std::fs::read_dir(root().join("src")).unwrap() {
        let p = f.unwrap().path();
        if p.extension().is_none_or(|e| e != "rs") {
            continue;
        }
        let src: Vec<char> = std::fs::read_to_string(&p).unwrap().chars().collect();
        let mut i = 0;
        while i < src.len() {
            if src[i] != '"' {
                i += 1;
                continue;
            }
            let mut j = i + 1;
            while j < src.len() && src[j] != '"' && src[j] != '\n' {
                j += if src[j] == '\\' { 2 } else { 1 };
            }
            let lit: String = src[i + 1..j.min(src.len())].iter().collect();
            if lit.len() >= 2
                && lit.starts_with(|c: char| c.is_ascii_lowercase())
                && lit.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
            {
                candidates.insert(lit);
            }
            i = j + 1;
        }
    }
    let mut missing = vec![];
    for c in candidates.difference(&documented) {
        // cd would change the folder of the test process
        if c == "cd" {
            continue;
        }
        let mut s = quiet_session();
        let got = shown(&mut s, &format!("{}()", c)).join("\n");
        if got != format!("Error: unknown function {}", c) && got != format!("{}()", c) {
            missing.push(format!("  {}  ({})", c, got.lines().next().unwrap_or("")));
        }
    }
    assert!(
        missing.is_empty(),
        "\nThese commands have no help entry in src/help.rs (and so are missing from \
         docs/builtins.md). Add an entry, or name them in the usage line of a related \
         entry:\n{}\n",
        missing.join("\n")
    );
}

#[test]
fn every_help_example_runs() {
    // a few examples (factoring) take seconds, so the entries are split over threads
    let n = std::thread::available_parallelism().map_or(4, |n| n.get()).max(2);
    let workers: Vec<_> = (0..n)
        .map(|k| {
            std::thread::Builder::new()
                .stack_size(256 << 20)
                .spawn(move || {
                    let mut failed = vec![];
                    for e in help::ENTRIES.iter().skip(k).step_by(n) {
                        let mut s = quiet_session();
                        for line in shown(&mut s, e.example) {
                            if line.starts_with("Error: ") {
                                failed.push(format!("  help({}): {}  ->  {}", e.name, e.example, line));
                            }
                        }
                    }
                    failed
                })
                .unwrap()
        })
        .collect();
    let failed: Vec<String> = workers.into_iter().flat_map(|w| w.join().unwrap()).collect();
    assert!(failed.is_empty(), "\nhelp examples that fail:\n{}\n", failed.join("\n"));
}
