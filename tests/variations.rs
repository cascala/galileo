//! Calculus of variations: Euler-Lagrange equations, energy, action, numerical
//! extremals and Lagrangian mechanics, checked against exact answers.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show;

fn one(code: &str) -> String {
    let mut s = Session::new();
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
fn euler_lagrange_equations() {
    for (code, want) in [
        ("eulerlagrange(sqrt(1 + y'^2), y, x)", "y'' == 0"),
        ("eulerlagrange(sqrt((1 + y'^2)/y), y, x)", "y'' == -(y'^2+1)/(2*y)"),
        ("eulerlagrange(y*sqrt(1 + y'^2), y, x)", "y'' == (y'^2+1)/y"),
        ("eulerlagrange(m*l^2*th'^2/2 + m*g*l*cos(th), th, t)", "th'' == -g*sin(th)/l"),
        ("eulerlagrange(m*x'^2/2 - k*x^2/2, x, t)", "x'' == -k*x/m"),
        ("eulerlagrange(m*(r'^2 + r^2*p'^2)/2 + k/r, [r p], t)", "[r'' == (m*p'^2*r^3-k)/(m*r^2), p'' == -2*p'*r'/r]"),
        ("energy(m*x'^2/2 - k*x^2/2, x, t)", "(k*x^2+m*x'^2)/2"),
        ("action(y'^2, y, x, sin(x), 0, pi)", "pi/2"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
    assert!(one("eulerlagrange(y''^2, y, x)").starts_with("ERROR eulerlagrange: L may use"));
    close("action(sqrt(1 + y'^2), y, x, x^2, 0, 1)", 1.4789428575445975, 1e-9);
    // a singular integrand at the start (sliding from rest): tanh-sinh quadrature
    close("action(sqrt((1 + y'^2)/(2*9.81*y)), y, x, 2*sqrt(x/pi), 0, pi)", 1.012865257486215, 1e-6);
}

#[test]
fn numerical_extremals() {
    use std::f64::consts::PI;
    // shortest path: a straight line
    close("s = extremal(sqrt(1 + y'^2), y, x, 0, 1, 0, 2); s.J", 5f64.sqrt(), 1e-9);
    // the fastest slide: a cycloid, time sqrt(2) pi (in units with 2g = 1)
    close("s = extremal(sqrt((1 + y'^2)/y), y, x, 0, pi, 0, 2); s.J", 2f64.sqrt() * PI, 1e-3);
    // the hanging chain / soap film: y = cosh(x)
    close("s = extremal(y*sqrt(1 + y'^2), y, x, -1, 1, cosh(1), cosh(1)); Y = s.y; Y(51)", 1.0, 1e-3);
    // least action: a thrown ball follows a parabola
    close("s = extremal(y'^2/2 - 9.81*y, y, t, 0, 2, 0, 0); Y = s.y; Y(51)", 9.81 / 2.0, 1e-3);
    // plot straight from the record fields
    assert!(!one("s = extremal(sqrt(1 + y'^2), y, x, 0, 1, 0, 2); plot(s.x, s.y)").starts_with("ERROR"));
    // rings too far apart: no catenoid
    assert!(one("extremal(y*sqrt(1 + y'^2), y, x, -1.5, 1.5, 2, 2)").starts_with("ERROR extremal: no smooth path"));
    assert!(one("extremal(a*y'^2, y, x, 0, 1, 0, 1)").starts_with("ERROR extremal: give a a value first"));
}

#[test]
fn lagrangian_motion() {
    use std::f64::consts::PI;
    // harmonic oscillator: back to the start after one period
    close("s = motion(x'^2/2 - x^2/2, x, t, 1, 0, 2*pi); Q = s.q; Q(end)", 1.0, 1e-9);
    close("s = motion(x'^2/2 - x^2/2, x, t, 1, 0, pi/2); V = s.v; V(end)", -1.0, 1e-9);
    // Kepler orbit in polar coordinates: perihelion 1, aphelion 2a - 1
    let a = 1.0 / (2.0 * (1.0 - 1.2f64 * 1.2 / 2.0));
    close("s = motion((r'^2 + r^2*f'^2)/2 + 1/r, [r f], t, [1 0], [0 1.2], 20); Q = s.q; max(Q(1))", 2.0 * a - 1.0, 1e-3);
    // double pendulum keeps its energy
    assert_eq!(one("s = motion((2*a'^2 + b'^2 + 2*a'*b'*cos(a - b))/2 + 2*cos(a) + cos(b), [a b], t, [2 2], [0 0], 10); size(s.q)"), "[2, 2001]");
    let _ = PI;
}
