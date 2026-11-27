//! Exact roots of cubics (Cardano) and quartics (Ferrari) from solve, checked
//! by evaluating the polynomial at each root in complex arithmetic.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::expr::{Const, Expr, Func};
use galileo_rs::print::show;

type C = (f64, f64);

fn cmul(a: C, b: C) -> C {
    (a.0 * b.0 - a.1 * b.1, a.0 * b.1 + a.1 * b.0)
}

fn cpow(a: C, p: f64) -> C {
    if a == (0.0, 0.0) {
        return (0.0, 0.0);
    }
    let (r, t) = ((a.0 * a.0 + a.1 * a.1).sqrt(), a.1.atan2(a.0));
    let (r, t) = (r.powf(p), t * p);
    (r * t.cos(), r * t.sin())
}

/// complex value of a root (numbers, pi, i, sums, products, powers, cos, acos)
fn cval(e: &Expr) -> C {
    match e {
        Expr::Num(n) => (n.to_f64(), 0.0),
        Expr::Const(Const::Pi) => (std::f64::consts::PI, 0.0),
        Expr::Const(Const::I) => (0.0, 1.0),
        Expr::Add(v) => v
            .iter()
            .map(cval)
            .fold((0.0, 0.0), |a, b| (a.0 + b.0, a.1 + b.1)),
        Expr::Mul(v) => v.iter().map(cval).fold((1.0, 0.0), cmul),
        Expr::Pow(b, p) => {
            let p = cval(p);
            assert!(p.1 == 0.0);
            cpow(cval(b), p.0)
        }
        Expr::Func(Func::Cos, a) => {
            let a = cval(a);
            assert!(a.1.abs() < 1e-12);
            (a.0.cos(), 0.0)
        }
        Expr::Func(Func::Acos, a) => {
            let a = cval(a);
            assert!(a.1.abs() < 1e-12 && a.0.abs() <= 1.0);
            (a.0.acos(), 0.0)
        }
        other => panic!("unexpected {}", show(other)),
    }
}

fn roots(code: &str) -> (Vec<Expr>, String) {
    let mut s = Session::new();
    let mut last = None;
    let mut notes = String::new();
    for (r, _, n) in s.run_notes(code) {
        if let Ok(Output::Value(v)) = r {
            last = Some(v);
        }
        notes = n
            .into_iter()
            .map(|n| n.plain)
            .collect::<Vec<_>>()
            .join("\n");
    }
    match last {
        Some(Expr::Matrix(m)) => (m.into_iter().flatten().collect(), notes),
        other => panic!("{}: {:?}", code, other),
    }
}

/// every root is exact (no decimals) and makes the polynomial vanish
fn check(c: &[f64], code: &str) -> Vec<Expr> {
    let (rs, _) = roots(code);
    assert_eq!(rs.len(), c.len() - 1, "{}", code);
    for r in &rs {
        assert!(
            !show(r).contains('.'),
            "{} gave a decimal root {}",
            code,
            show(r)
        );
        let z = cval(r);
        let mut v = (0.0, 0.0);
        for a in c.iter().rev() {
            v = cmul(v, z);
            v.0 += a;
        }
        let scale = 1.0 + (z.0 * z.0 + z.1 * z.1).sqrt();
        assert!(
            (v.0 * v.0 + v.1 * v.1).sqrt() < 1e-9 * scale.powi(c.len() as i32),
            "{}: {} is not a root",
            code,
            show(r)
        );
    }
    rs
}

#[test]
fn cubics() {
    // one real root (Cardano), three real (trigonometric form), pure cube
    check(&[2.0, -2.0, 0.0, 1.0], "solve(x^3 - 2*x + 2, x)");
    check(&[7.0, -4.0, 3.0, 2.0], "solve(2*x^3 + 3*x^2 - 4*x + 7, x)");
    let r = check(&[1.0, -3.0, 0.0, 1.0], "solve(x^3 - 3*x + 1, x)");
    assert_eq!(show(&r[0]), "2*cos(10*pi/9)");
    let r = check(&[-2.0, 0.0, 0.0, 1.0], "solve(x^3 - 2, x)");
    assert_eq!(show(&r[0]), "2^(1/3)");
    // rational roots still come out first and exactly
    check(&[-6.0, 11.0, -6.0, 1.0], "solve(x^3 - 6*x^2 + 11*x - 6, x)");
}

#[test]
fn quartics() {
    check(&[1.0, 1.0, 0.0, 0.0, 1.0], "solve(x^4 + x + 1, x)");
    check(&[5.0, 1.0, 0.0, -4.0, 1.0], "solve(x^4 - 4*x^3 + x + 5, x)");
    check(
        &[3.0, -1.0, 0.0, 4.0, 2.0],
        "solve(2*x^4 + 4*x^3 - x + 3, x)",
    );
    check(&[1.0, 0.0, -10.0, 0.0, 1.0], "solve(x^4 - 10*x^2 + 1, x)");
    let r = check(&[1.0, 0.0, 0.0, 0.0, 1.0], "solve(x^4 + 1, x)");
    assert_eq!(show(&r[0]), "-sqrt(2)/2-sqrt(2)*i/2");
    // splits over Q into quadratics; cubic pieces of a sextic
    check(&[6.0, 0.0, -5.0, 0.0, 1.0], "solve(x^4 - 5*x^2 + 6, x)");
    check(
        &[6.0, 0.0, 0.0, -5.0, 0.0, 0.0, 1.0],
        "solve(x^6 - 5*x^3 + 6, x)",
    );
}

#[test]
fn symbolic_formulas() {
    // the general formulas, checked at sample coefficients
    for (code, vals) in [
        ("solve(x^3 + p*x + q, x)", vec![("p", "-7/3"), ("q", "5/2")]),
        (
            "solve(x^4 + p*x^2 + q*x + r, x)",
            vec![("p", "3/2"), ("q", "-2"), ("r", "1/3")],
        ),
    ] {
        let (rs, notes) = roots(code);
        assert!(
            notes.contains("general formulas") || notes.contains("Ferrari"),
            "{}",
            notes
        );
        let mut s = Session::new();
        let mut sub = String::new();
        for (k, v) in &vals {
            sub = format!("{}{} = {}; ", sub, k, v);
        }
        s.run(&sub);
        for r in rs {
            let mut e = r.clone();
            for (k, v) in &vals {
                let v = match s.run(v).pop() {
                    Some(Ok(Output::Value(v))) => v,
                    o => panic!("{:?}", o),
                };
                e = galileo_rs::calc::subst(&e, k, &v);
            }
            let z = cval(&e);
            let (p, q, rr) = (vals[0].1, vals[1].1, vals.get(2).map(|x| x.1));
            let f = |t: &str| -> f64 {
                let n: Vec<f64> = t.split('/').map(|x| x.parse().unwrap()).collect();
                if n.len() == 2 { n[0] / n[1] } else { n[0] }
            };
            let (p, q) = (f(p), f(q));
            let v = match rr {
                None => {
                    let z3 = cmul(cmul(z, z), z);
                    (z3.0 + p * z.0 + q, z3.1 + p * z.1)
                }
                Some(r) => {
                    let z2 = cmul(z, z);
                    let z4 = cmul(z2, z2);
                    (z4.0 + p * z2.0 + q * z.0 + f(r), z4.1 + p * z2.1 + q * z.1)
                }
            };
            assert!(v.0.abs() + v.1.abs() < 1e-9, "{}: {}", code, show(&r));
        }
    }
}

#[test]
fn notes() {
    let (_, brief) = roots("solve(x^3 - 2*x + 2, x)");
    assert!(
        brief.contains("Cardano") && brief.contains("19/27"),
        "{}",
        brief
    );
    let (_, teach) = roots("explain = \"teach\"; solve(x^3 - 3*x + 1, x)");
    for part in ["Step 1", "casus irreducibilis", "cos(3φ)", "-1.879385"] {
        assert!(teach.contains(part), "missing {}: {}", part, teach);
    }
    let (_, teach) = roots("explain = \"teach\"; solve(x^4 + x + 1, x)");
    for part in ["Ferrari", "resolvent", "8*m^3-8*m-1", "two quadratics"] {
        assert!(teach.contains(part), "missing {}: {}", part, teach);
    }
    let (_, off) = roots("explain = \"off\"; solve(x^3 - 2*x + 2, x)");
    assert!(off.is_empty());
}

#[test]
fn low_degree_notes() {
    let (_, t) = roots("explain = \"teach\"; solve(2*x^2 - 3*x - 5, x)");
    for part in [
        "completing the square",
        "(x-3/4)^2",
        "49/16",
        "D = b^2 − 4ac = 49",
        "perfect square",
        "2*(-1)^2 − 3*(-1) − 5 = 0",
        "Vieta",
    ] {
        assert!(t.contains(part), "missing {}: {}", part, t);
    }
    let (_, t) = roots("explain = \"teach\"; solve(x^2 + 2*x + 5, x)");
    assert!(
        t.contains("±2·i") && t.contains("two complex roots"),
        "{}",
        t
    );
    let (_, t) = roots("explain = \"teach\"; solve(a*x^2 + b*x + c, x)");
    assert!(
        t.contains("a is assumed not to be 0") && t.contains("b^2-4*a*c"),
        "{}",
        t
    );
    let (_, t) = roots("explain = \"teach\"; solve(x^3 - 2*x^2 + x - 2, x)");
    for part in [
        "rational root test",
        "±1, ±2",
        "x = 2",
        "x^2+1",
        "completing the square",
    ] {
        assert!(t.contains(part), "missing {}: {}", part, t);
    }
    let (_, b) = roots("solve(x^3 - 6*x^2 + 11*x - 6, x)");
    assert!(
        b.contains("Rational roots") && b.contains("1, 2, 3"),
        "{}",
        b
    );
    let mut s = Session::new();
    let r = s.run_notes("explain = \"teach\"; solve(3*x + 7, x)");
    let n: Vec<String> = r
        .last()
        .unwrap()
        .2
        .iter()
        .map(|n| n.plain.clone())
        .collect();
    assert!(n.join(" ").contains("x = −b/a = -7/3"), "{:?}", n);
}
