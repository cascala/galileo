//! Parity with the Scala implementation.
//!
//! `tests/parity/scala_tests.txt` holds the input/expected pairs from the
//! Scala test suite. The Rust engine prints differently (exact rationals,
//! no `2.0`), so a case passes when the Rust result is mathematically equal
//! to the Scala expectation: both are evaluated at random points and must
//! agree. For simplify and factor the Rust answer must also be no larger
//! than the Scala one.

use galileo_rs::calc::{evalf, subst};
use galileo_rs::{Expr, Output, Session};

/// Cases where the Rust engine deliberately differs, with the reason
const DIVERGENCES: &[(&str, &str)] = &[(
    "acos(cos(y))",
    "acos(cos(y)) = y only holds for 0 <= y <= pi, so Rust keeps it unsimplified",
)];

fn last_value(src: &str) -> Result<Expr, String> {
    let mut s = Session::tidy();
    let mut last = None;
    for r in s.run(src) {
        if let Output::Value(v) = r? {
            last = Some(v)
        }
    }
    last.ok_or_else(|| "no value".to_string())
}

fn numeric_at(e: &Expr, point: &[(String, f64)]) -> Option<f64> {
    let mut v = e.clone();
    for (n, x) in point {
        v = subst(&v, n, &Expr::float(*x));
    }
    evalf(&v).num().map(|n| n.to_f64())
}

fn equivalent(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Bool(x), Expr::Bool(y)) => x == y,
        (Expr::Matrix(x), Expr::Matrix(y)) => {
            x.len() == y.len()
                && x.iter().zip(y).all(|(r, s)| {
                    r.len() == s.len() && r.iter().zip(s).all(|(p, q)| equivalent(p, q))
                })
        }
        // [x] and x are the same thing
        (Expr::Matrix(x), y) | (y, Expr::Matrix(x)) if x.len() == 1 && x[0].len() == 1 => {
            equivalent(&x[0][0], y)
        }
        _ if a.is_scalar() && b.is_scalar() => {
            let mut syms = a.symbols();
            syms.extend(b.symbols());
            syms.sort();
            syms.dedup();
            let mut seed = 0x9E3779B97F4A7C15u64;
            (0..4).all(|_| {
                let point: Vec<(String, f64)> = syms
                    .iter()
                    .map(|s| {
                        seed ^= seed << 13;
                        seed ^= seed >> 7;
                        seed ^= seed << 17;
                        (
                            s.clone(),
                            0.3 + 2.0 * (seed >> 11) as f64 / (1u64 << 53) as f64,
                        )
                    })
                    .collect();
                match (numeric_at(a, &point), numeric_at(b, &point)) {
                    (Some(x), Some(y)) => (x - y).abs() <= 1e-9 * (1.0 + x.abs().max(y.abs())),
                    _ => false,
                }
            })
        }
        _ => false,
    }
}

#[test]
fn scala_test_suite_parity() {
    let text = include_str!("parity/scala_tests.txt");
    let mut mode = "plain".to_string();
    let mut failures = vec![];
    let mut passed = 0;
    let mut skipped = 0;
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line.starts_with('[')
            && line.ends_with(']')
            && line[1..line.len() - 1]
                .chars()
                .all(|c| c.is_ascii_lowercase())
        {
            mode = line.trim_matches(['[', ']']).to_string();
            continue;
        }
        let (input, expected) = line.split_once("  =>  ").expect("case line");
        if DIVERGENCES.iter().any(|(i, _)| *i == input) {
            skipped += 1;
            continue;
        }
        let src = if mode == "plain" {
            input.to_string()
        } else {
            format!("{}({})", mode, input)
        };
        let got = last_value(&src);
        let want = last_value(expected);
        let ok = match (&got, &want) {
            (Ok(g), Ok(w)) => {
                equivalent(g, w)
                    && (!(mode == "simplify" || mode == "factor") || g.size() <= w.size())
            }
            _ => false,
        };
        if ok {
            passed += 1;
        } else {
            failures.push(format!(
                "{}: {}\n   rust:  {}\n   scala: {}",
                mode,
                input,
                got.map(|g| galileo_rs::print::show(&g))
                    .unwrap_or_else(|e| format!("error {}", e)),
                expected
            ));
        }
    }
    println!(
        "parity: {} passed, {} known divergences, {} failed",
        passed,
        skipped,
        failures.len()
    );
    assert!(failures.is_empty(), "\n{}", failures.join("\n"));
}
