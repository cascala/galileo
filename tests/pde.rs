//! Partial differential equations: heat (Crank-Nicolson), wave (leapfrog),
//! Laplace and Poisson (five-point formula), checked against exact solutions.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show;

fn one(code: &str) -> String {
    let mut s = Session::tidy();
    let mut last = String::new();
    for r in s.run(code) {
        last = match r {
            Ok(Output::Value(v)) => show(&v),
            Ok(o) => format!("{:?}", o),
            Err(e) => format!("ERROR {}", e),
        };
    }
    last.replace('\n', "")
}

fn num(code: &str) -> f64 {
    let r = one(code);
    r.parse().unwrap_or_else(|_| panic!("{} gave {}", code, r))
}

fn close(code: &str, want: f64, tol: f64) {
    let got = num(code);
    assert!((got - want).abs() < tol, "{}: got {}, want {}", code, got, want);
}

#[test]
fn heat_equation() {
    // one Fourier mode decays like exp(-pi^2 k t)
    let e = (-std::f64::consts::PI.powi(2) * 0.1).exp();
    close("s = heat(sin(pi*x), x, 0, 1, 0.1); u = s.u; max(u(end))", e, 1e-4);
    close("s = heat(sin(pi*x), x, 0, 1, 0.2, {k: 1/2}); u = s.u; max(u(end))", e, 1e-4);
    // fixed end values: the straight line is the steady state
    close("s = heat(0, x, 0, 2, 20, {left: 1, right: 3}); u = s.u; r = u(end); r(51)", 2.0, 1e-6);
    // insulated ends keep the total heat: the average stays 1/2
    close("s = heat(x, x, 0, 1, 1, {left: \"insulated\", right: \"insulated\"}); u = s.u; mean(u(end))", 0.5, 2e-3);
    // a source: u_t = u_xx + 2 tends to x (1 - x)
    close("s = heat(0, x, 0, 1, 5, {source: 2}); u = s.u; r = u(end); r(51)", 0.25, 1e-4);
    // shape of the record
    assert_eq!(one("s = heat(0, x, 0, 1, 1, {n: 10, frames: 4}); size(s.u)"), "[5, 11]");
    assert!(one("heat(0, x, 0, 1, 1, {q: 1})").starts_with("ERROR heat: unknown option q"));
    assert!(one("heat(0, x, 0, 1, -1)").starts_with("ERROR heat: the time T must be positive"));
}

#[test]
fn wave_equation() {
    // a standing wave comes back inverted after half a period
    close("s = wave(sin(pi*x), 0, x, 0, 1, 1, {frames: 2}); u = s.u; min(u(end))", -1.0, 1e-3);
    close("s = wave(sin(pi*x), 0, x, 0, 1, 2, {frames: 2}); u = s.u; max(u(end))", 1.0, 1e-3);
    // speed c: same after T / c
    close("s = wave(sin(pi*x), 0, x, 0, 1, 1/2, {c: 2, frames: 1}); u = s.u; min(u(end))", -1.0, 1e-3);
    // d'Alembert: a bump splits into two halves
    close("s = wave(exp(-400*(x - 1/2)^2), 0, x, 0, 1, 1/4, {n: 400, frames: 1}); u = s.u; max(u(end))", 0.5, 1e-2);
    // starting velocity: u = sin(pi x) sin(pi t) / pi
    close("s = wave(0, sin(pi*x), x, 0, 1, 1/2, {frames: 1}); u = s.u; max(u(end))", 1.0 / std::f64::consts::PI, 1e-3);
    // damping takes energy out
    assert!(num("s = wave(sin(pi*x), 0, x, 0, 1, 2, {damping: 1, frames: 1}); u = s.u; max(u(end))") < 0.5);
}

#[test]
fn laplace_and_poisson() {
    use std::f64::consts::PI;
    // u = sin(pi x) sinh(pi y) / sinh(pi) at the centre
    close(
        "s = laplace(0, x, 0, 1, y, 0, 1, {top: sin(pi*x)}); u = s.u; u(21, 21)",
        (PI / 2.0).sinh() / PI.sinh(),
        5e-4,
    );
    // a harmonic edge function is reproduced exactly
    close("s = laplace(x^2 - y^2, x, -1, 1, y, -1, 1); u = s.u; u(31, 21)", -0.25, 1e-8);
    // u_xx + u_yy = -2 with u = 0 on the edge of the unit square
    close("s = poisson(-2, 0, x, 0, 1, y, 0, 1); u = s.u; u(21, 21)", 0.147352, 1e-3);
    // a non-square rectangle: rows follow y
    assert_eq!(one("s = laplace(0, x, 0, 2, y, 0, 1, {n: 20}); size(s.u)"), "[11, 21]");
    // the Laplace transform still works
    assert_eq!(one("laplace(exp(-t), t, s)"), "1/(s+1)");
}

#[test]
fn pdeplot_kinds() {
    for code in [
        "pdeplot(heat(sin(pi*x), x, 0, 1, 0.1))",
        "pdeplot(heat(sin(pi*x), x, 0, 1, 0.1), \"lines\")",
        "pdeplot(wave(sin(pi*x), 0, x, 0, 1, 1), \"map\")",
        "pdeplot(laplace(0, x, 0, 1, y, 0, 1, {top: 1}))",
        "pdeplot(laplace(0, x, 0, 1, y, 0, 1, {top: 1}), \"surface\")",
    ] {
        assert!(!one(code).starts_with("ERROR"), "{} gave {}", code, one(code));
    }
    assert!(one("pdeplot(laplace(0, x, 0, 1, y, 0, 1), \"animate\")").starts_with("ERROR"));
}
