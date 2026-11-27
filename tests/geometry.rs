//! End-to-end checks for differential geometry, plots and the kernel protocol.

use galileo_rs::display::{Shown, shown};
use galileo_rs::eval::Session;
use galileo_rs::kernel;

/// Run a program and return the text of every non-quiet result
fn run(code: &str) -> String {
    let mut s = Session::new();
    let mut out = vec![];
    for (r, quiet) in s.run_quiet(code) {
        match r {
            Ok(o) if !quiet => match shown(&o) {
                Shown::Rich(b) => out.push(b[0].1.clone()),
                Shown::Text(t) => out.push(t),
                Shown::Nothing => {}
            },
            Ok(_) => {}
            Err(e) => out.push(format!("ERROR {}", e)),
        }
    }
    out.join("\n")
}

#[test]
fn schwarzschild() {
    let out = run(
        "S = metric.generate(schwarzschild);\nchristoffel(S)[1,0,0]\nriccitensor(S)\nkretschmann(S)",
    );
    assert!(out.contains("r_s*(r-r_s)/(2*r^3)"), "{}", out);
    assert!(out.contains("all components are zero"), "{}", out);
    assert!(out.contains("12*r_s^2/r^6"), "{}", out);
}

#[test]
fn sphere_and_torus() {
    assert_eq!(run("gausscurvature(metric.generate(two-sphere))"), "1/r^2");
    let out = run("T = inducedmetric([(R+a*cos(u))*cos(v) (R+a*cos(u))*sin(v) a*sin(u)], [u v])");
    assert!(out.contains("(R+a*cos(u))^2*dv^2"), "{}", out);
}

#[test]
fn undefined_functions() {
    assert_eq!(run("deriv(a(t)^2, t)"), "2*a(t)*a'(t)");
    let out = run("F = metric([t x], [-1 0; 0 a(t)^2]);\nricciscalar(F)");
    assert!(out.contains("a''(t)"), "{}", out);
    assert!(run("foo(1)").starts_with("ERROR"));
}

#[test]
fn kerr_is_vacuum() {
    let out = run(concat!(
        "S = r^2 + a^2*cos(theta)^2;\nD = r^2 - 2*M*r + a^2;\n",
        "K = metric([t r theta phi], [-(1-2*M*r/S) 0 0 -2*M*a*r*sin(theta)^2/S; 0 S/D 0 0; 0 0 S 0; ",
        "-2*M*a*r*sin(theta)^2/S 0 0 (r^2+a^2+2*M*a^2*r*sin(theta)^2/S)*sin(theta)^2]);\n",
        "riccitensor(K)"
    ));
    assert!(out.contains("all components are zero"), "{}", out);
}

#[test]
fn ode_and_plots() {
    let x = run("odesolve([y], [y], t, [1], 0, 1)")
        .parse::<f64>()
        .unwrap();
    assert!((x - std::f64::consts::E).abs() < 1e-6);
    let mut s = Session::new();
    let rs = s.run_quiet("plot(sin(x), x, 0, 2*pi)");
    let Ok(o) = &rs[0].0 else { panic!() };
    let Shown::Rich(b) = shown(o) else {
        panic!("no figure")
    };
    let kinds: Vec<&str> = b.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds,
        [
            "text/plain",
            "image/svg+xml",
            "application/vnd.plotly.v1+json"
        ]
    );
}

#[test]
fn kernel_protocol() {
    let mut s = Session::new();
    let r = kernel::execute(&mut s, "x = 3;\nx^2");
    assert_eq!(r, r#"{"outputs":[{"text":"9"}],"error":null}"#);
    let r = kernel::execute(&mut s, "1 +");
    assert!(r.contains(r#""ename":"SyntaxError""#), "{}", r);
    let r = kernel::complete(&s, "christ", 6);
    assert!(
        r.contains(r#""christoffel""#) && r.contains(r#""start":0,"end":6"#),
        "{}",
        r
    );
    assert!(kernel::inspect("riccitensor(", 12).contains(r#""found":true"#));
}
