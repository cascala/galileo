//! Polylogarithms, series k^p x^k, Gaussian integrals, entropy, reciprocal
//! trig functions, complex eval.

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
    last
}

fn close(code: &str, re: f64, im: f64) {
    let v = one(code);
    let s = v.replace("*i", "");
    let (a, b) = match s.rfind(['+', '-']).filter(|&k| k > 0 && v.contains('i') && !s[..k].ends_with('e')) {
        Some(k) => (s[..k].parse::<f64>().unwrap(), s[k..].parse::<f64>().unwrap()),
        None if v.contains('i') => (0.0, s.parse::<f64>().unwrap()),
        None => (s.parse::<f64>().unwrap(), 0.0),
    };
    assert!((a - re).abs() < 1e-12 && (b - im).abs() < 1e-12, "{} = {}", code, v);
}

#[test]
fn polylog_values() {
    assert_eq!(one("polylog(2, 1/2)"), "pi^2/12-log(2)^2/2");
    assert_eq!(one("polylog(2, 1)"), "pi^2/6");
    assert_eq!(one("polylog(1, x)"), "-log(1-x)");
    assert_eq!(one("polylog(-2, x)"), "x*(x+1)/(1-x)^3");
    assert_eq!(one("diff(polylog(3, x^2), x)"), "2*polylog(2, x^2)/x");
    // Catalan's constant, the value on the cut, and the slowly converging |z| = 1
    close("eval(polylog(2, i))", -0.20561675835602830, 0.91596559417721901);
    close("eval(polylog(2, 3))", 2.3201804233130984, -3.4513922952232027);
    close("eval(polylog(2, -1))", -0.82246703342411321, 0.0);
    close("eval(polylog(3, 0.9))", 1.0496589501864398, 0.0);
}

#[test]
fn series_sums() {
    assert_eq!(one("sum(x^k/k^2, k, 1, inf)"), "polylog(2, x)");
    assert_eq!(one("sum(x^k/k, k, 1, inf)"), "-log(1-x)");
    assert_eq!(one("sum(k^2/2^k, k, 1, inf)"), "6");
    assert_eq!(one("sum(k/3^k, k, 2, inf)"), "5/12");
    assert_eq!(one("sum(n*x^(2*n-1), n, 1, inf)"), "x/(x^2-1)^2");
    assert_eq!(one("sum(exp(-2*i*k/s)/(2*s^2*k^2), k, 1, inf)"), "polylog(2, exp(-2*i/s))/(2*s^2)");
    assert!(one("sum(k*2^k, k, 1, inf)").contains("diverges"));
}

#[test]
fn gaussian_integrals() {
    assert_eq!(
        one("integrate((a + b*q + c*q^2 + d*q^3)*exp(-(q-q0)^2/(2*v))/sqrt(2*pi*v), q, -inf, inf)"),
        "a+b*q0+c*q0^2+c*v+d*q0^3+3*d*q0*v"
    );
    assert_eq!(one("integrate(exp(-a*x^2 + b*x), x, -inf, inf)"), "exp(b^2/(4*a))*sqrt(pi/a)");
    assert_eq!(one("integrate(x^2*exp(-(x-1)^2), x, -inf, inf)"), "3*sqrt(pi)/2");
}

#[test]
fn entropy_and_reciprocals() {
    assert_eq!(one("entropy([1/2 1/2])"), "log(2)");
    assert_eq!(one("entropy([1/4 1/4 1/4 1/4], 2)"), "2");
    assert_eq!(one("entropy([1/2 1/2; 1/2 1/2])"), "0");
    assert!(one("entropy([1/2 1/4])").contains("add up to"));
    assert_eq!(one("sech(x)^2"), "1/cosh(x)^2");
    assert_eq!(one("sec(0)"), "1");
    close("eval(exp(-2*i/3))", 0.7858872607769480, -0.6183698030697370);
}
