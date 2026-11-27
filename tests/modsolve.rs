//! solve(f, x, N): roots of polynomials in Z/N, checked against trying every residue.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show;

fn run(code: &str) -> (String, String) {
    let mut s = Session::new();
    let mut last = (String::new(), String::new());
    for (r, _, n) in s.run_notes(code) {
        let v = match r {
            Ok(Output::Value(v)) => show(&v),
            Ok(o) => format!("{:?}", o),
            Err(e) => format!("ERROR {}", e),
        };
        last = (
            v,
            n.into_iter()
                .map(|n| n.plain)
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
    last
}

fn brute(c: &[i64], n: i64) -> String {
    let r: Vec<String> = (0..n)
        .filter(|&x| {
            c.iter().rev().fold(0i128, |acc, &a| {
                (acc * x as i128 + a as i128).rem_euclid(n as i128)
            }) == 0
        })
        .map(|x| x.to_string())
        .collect();
    match r.len() {
        0 => "[]".into(),
        1 => r[0].clone(),
        _ => format!("[{}]", r.join(", ")),
    }
}

#[test]
fn against_brute_force() {
    let polys: [&[i64]; 6] = [
        &[1, 0, 0, 0, 1],
        &[-1, 0, 1],
        &[0, -1, 0, 1],
        &[5, 3, 0, 0, 1],
        &[-4, 0, 1],
        &[7, -3, 2, 5, 1],
    ];
    for c in polys {
        for n in [2i64, 8, 17, 72, 97, 105, 288, 343, 1001, 4096, 7919] {
            let f: Vec<String> = c
                .iter()
                .enumerate()
                .map(|(k, a)| format!("({})*x^{}", a, k))
                .collect();
            let code = format!("solve({}, x, {})", f.join("+"), n);
            assert_eq!(run(&code).0, brute(c, n), "{}", code);
        }
    }
}

#[test]
fn large_and_special() {
    // a 19-digit prime: gcd with x^p - x, then splitting
    assert_eq!(
        run("solve(x^2 + 1, x, 10^18 + 9)").0,
        "[333333333000000003, 666666667000000006]"
    );
    assert_eq!(run("solve(x^4 + 1, x, 2^61 - 1)").0, "[]");
    // fractions: 1/2 is 6 mod 11
    assert_eq!(run("solve(x/2 - 3, x, 11)").0, "6");
    assert!(run("solve(x/2 - 3, x, 10)").0.starts_with("ERROR"));
    assert!(run("solve(x^2 - 2, x, 1)").0.starts_with("ERROR"));
}

#[test]
fn notes() {
    let (_, b) = run("solve(x^2 - 4, x, 105)");
    assert!(
        b.contains("3·5·7") && b.contains("Chinese remainder"),
        "{}",
        b
    );
    let (_, t) = run("explain = \"teach\"; solve(x^2 - 4, x, 105)");
    for part in [
        "prime powers",
        "Fermat",
        "not a field",
        "gcd(2 − 23, 105) = 21",
        "Rabin",
        "Check",
    ] {
        assert!(t.contains(part), "missing {}: {}", part, t);
    }
    let (_, t) = run("explain = \"teach\"; solve(x^3 - x, x, 72)");
    assert!(
        t.contains("Hensel") && t.contains("either all 2 lifts"),
        "{}",
        t
    );
}
