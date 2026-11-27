//! load(file), `\` line continuation, and scripts' error locations
use galileo_rs::Session;
use galileo_rs::kernel::execute;
use std::path::PathBuf;

/// A fresh folder with the given files
fn folder(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let d = std::env::temp_dir().join(format!("galileo-load-{}-{}", name, std::process::id()));
    std::fs::create_dir_all(d.join("sub")).unwrap();
    for (f, src) in files {
        std::fs::write(d.join(f), src).unwrap();
    }
    d
}

/// The results a script or the prompt prints (statements ending in ; stay quiet)
fn values(s: &mut Session, src: &str) -> String {
    s.run_notes(src)
        .into_iter()
        .filter(|(_, q, _)| !q)
        .map(|(r, _, _)| {
            r.map(|o| o.render())
                .unwrap_or_else(|e| format!("Error: {}", e))
        })
        .filter(|t| !t.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn load_shows_every_unhidden_result() {
    let d = folder("every", &[("a.gg", "a = 1;\nb = 2\nc = a + b\n")]);
    let mut s = Session::new();
    let out = values(&mut s, &format!("load({})", d.join("a.gg").display()));
    assert_eq!(out, "2\n3");
}

#[test]
fn load_adds_gg_and_takes_quotes() {
    let d = folder("names", &[("my script.gg", "x = 41 + 1\n")]);
    let mut s = Session::new();
    let p = d.join("my script");
    assert_eq!(values(&mut s, &format!("load(\"{}\")", p.display())), "42");
    assert_eq!(values(&mut s, "x"), "42");
}

#[test]
fn errors_name_file_and_line() {
    let d = folder(
        "errors",
        &[
            (
                "bad.gg",
                "a = 1\n\n// comment\nb = (2 +\n 3)\nc = zz(\nd = 4\n",
            ),
            ("run.gg", "p = 1\nq = nosuch(1)\nr = 2\n"),
        ],
    );
    let mut s = Session::new();
    let r = s.load_file(d.join("bad.gg").to_str().unwrap());
    let e = r.last().unwrap().0.clone().unwrap_err();
    assert!(e.contains("bad.gg:6: parse error"), "{}", e);
    // a runtime error stops the script on its line
    let r = s.load_file(d.join("run.gg").to_str().unwrap());
    let e = r.last().unwrap().0.clone().unwrap_err();
    assert!(e.contains("run.gg:2:"), "{}", e);
    assert!(s.get("r").is_none());
    assert!(s.get("p").is_some());
}

#[test]
fn nested_loads_resolve_next_to_the_loading_file() {
    let d = folder(
        "nested",
        &[
            ("sub/outer.gg", "x = 10\nload(inner)\ny = x + w\n"),
            ("sub/inner.gg", "w = 3;\nw*2\n"),
        ],
    );
    let mut s = Session::new();
    let out = values(
        &mut s,
        &format!("load({})", d.join("sub/outer.gg").display()),
    );
    assert_eq!(out, "10\n6\n13");
}

#[test]
fn a_file_loading_itself_stops() {
    let d = folder("self", &[]);
    let f = d.join("self.gg");
    std::fs::write(&f, format!("load({})\n", f.display())).unwrap();
    let mut s = Session::new();
    let e = s
        .load_file(f.to_str().unwrap())
        .last()
        .unwrap()
        .0
        .clone()
        .unwrap_err();
    assert!(e.contains("loads itself"), "{}", e);
}

#[test]
fn missing_file() {
    let mut s = Session::new();
    let r = execute(&mut s, "load(no/such/file)");
    assert!(r.contains("no such file"), "{}", r);
}

#[test]
fn notebook_cells_show_loaded_results() {
    let d = folder("kernel", &[("k.gg", "u = 6;\nu * 7\n")]);
    let mut s = Session::new();
    let r = execute(&mut s, &format!("load({})", d.join("k.gg").display()));
    assert!(
        r.contains("{\"text\":\"42\"}") && r.contains("\"error\":null"),
        "{}",
        r
    );
}

#[test]
fn calls_span_lines() {
    let mut s = Session::new();
    assert_eq!(
        values(&mut s, "b = max(2,\n  7)\nc = (1 +\n 2) * [1 2\n3 4]"),
        "7\n[3,  6;\n 9, 12]"
    );
}

#[test]
fn backslash_continues_a_line() {
    let mut s = Session::new();
    assert_eq!(values(&mut s, "k = 1 + \\\n  2\nk"), "3\n3");
    // with spaces or a comment after it
    assert_eq!(values(&mut s, "m = 2 * \\   // more\n  5"), "10");
    // still the solve operator elsewhere
    assert_eq!(
        values(&mut s, "[2 0; 0 4] \\ [2; 8]"),
        values(&mut s, "[1; 2]")
    );
    assert!(galileo_rs::parser::needs_more("a = 1 + \\"));
    assert!(galileo_rs::parser::needs_more("f(1,\n"));
    assert!(!galileo_rs::parser::needs_more("a = \"\\\\\"\n"));
    assert!(!galileo_rs::parser::needs_more("a = 1 // \\"));
}
