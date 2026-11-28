//! Wave 5: Laurent series, residues, poles and contour integrals.

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

#[test]
fn laurent_and_residues() {
    assert_eq!(one("laurent(exp(z)/z^3, z, 0, 2)"), "1/z^3+1/z^2+1/(2*z)+1/6+z/24+z^2/120");
    assert_eq!(one("residue(1/(z^2+1), z, i)"), "-i/2");
    assert_eq!(one("residue(1/sin(z)^2, z, 0)"), "0");
    assert_eq!(one("residue(z^2*exp(1/z), z, 0)"), "1/6");
    assert_eq!(one("residue(exp(z)/(z-1)^3, z, 1)"), "e/2");
    assert_eq!(one("residue(cos(z)/sin(z), z, pi)"), "1");
    assert_eq!(one("laurent(sin(z)/z, z, 0, 4)"), "1-z^2/6+z^4/120");
    assert!(one("laurent(sqrt(z), z, 0, 2)").contains("branch point"));
    assert_eq!(one("poles((z+1)/(z^2*(z-2)), z)"), "[0, 2, -3/4;\n 2, 1,  3/4]");
}

#[test]
fn contour_integrals() {
    assert_eq!(one("cint(1/z, z, exp(i*t), t, 0, 2*pi)"), "6.28318530718*i");
    assert_eq!(one("integrate(exp(4*pi*i*s), s, 0, 1)"), "0");
    assert_eq!(one("abs(1 + i*sqrt(3))"), "2");
    assert!(!one("cplot(z^2, z, -1, 1, -1, 1)").starts_with("ERROR"));
    assert!(!one("cmap(exp(z), z, -1, 1, -1, 1)").starts_with("ERROR"));
}

#[test]
fn non_analytic_integrands() {
    assert_eq!(one("cint(conj(z), z, t + i*t, t, 0, 1)"), "1.0");
    assert_eq!(one("cint(conj(z), z, t + i*t^2, t, 0, 1)"), "1.0+0.333333333333*i");
    assert_eq!(one("cint(real(z), z, t + i*t, t, 0, 1)"), "0.5+0.5*i");
}

#[test]
fn real_integrals_by_residues() {
    assert_eq!(one("integrate(cos(x)/(x^2 + 1), x, -inf, inf)"), "pi/e");
    assert_eq!(one("integrate(sin(x)/x, x, 0, inf)"), "pi/2");
    assert_eq!(one("integrate(cos(2*x)/(x^2 + 1), x, 0, inf)"), "pi/(2*exp(2))");
    assert_eq!(one("integrate(cos(x)/(x^2 + 4)^2, x, -inf, inf)"), "3*pi/(16*exp(2))");
    assert_eq!(one("integrate(sin(x)/(x*(x^2+1)), x, -inf, inf)"), "pi*(e-1)/e");
    assert!(one("integrate(cos(x)/x, x, -inf, inf)").starts_with("ERROR"));
}

#[test]
fn rational_and_period_integrals() {
    assert_eq!(one("integrate(1/(1 + x^4), x, -inf, inf)"), "sqrt(2)*pi/2");
    assert_eq!(one("integrate(1/(2 + cos(t)), t, 0, 2*pi)"), "2*pi/sqrt(3)");
    assert_eq!(one("integrate(cos(t)^2/(2 + sin(t)), t, 0, 2*pi)"), "4*pi-2*sqrt(3)*pi");
    assert_eq!(one("integrate(sin(t)^2/(5 + 4*cos(t)), t, 0, 2*pi)"), "pi/4");
    assert_eq!(one("solve(z^2*(z^2+4*i*z-1), z)"), "[0, -i*(sqrt(3)+2), i*(sqrt(3)-2)]");
}
