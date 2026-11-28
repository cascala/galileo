//! Wave 1: special functions, vector calculus, polynomial systems, optimization
//! and the field plots.

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

/// the value of the last statement, as digits
fn close(code: &str, want: f64) {
    let code = match code.rsplit_once(';') {
        Some((head, tail)) => format!("{}; eval({})", head, tail.trim()),
        None => format!("eval({})", code),
    };
    let code = code.as_str();
    let v: f64 = one(code).parse().unwrap_or_else(|_| panic!("{} gave {}", code, one(code)));
    assert!((v - want).abs() < 1e-9 * (1.0 + want.abs()), "{} = {} (want {})", code, v, want);
}

#[test]
fn special_values() {
    assert_eq!(one("gamma(5)"), "24");
    assert_eq!(one("gamma(1/2)"), "sqrt(pi)");
    assert_eq!(one("gamma(7/2)"), "15*sqrt(pi)/8");
    assert_eq!(one("gamma(-3/2)"), "4*sqrt(pi)/3");
    assert!(one("gamma(0)").contains("pole"));
    assert_eq!(one("zeta(2)"), "pi^2/6");
    assert_eq!(one("zeta(4)"), "pi^4/90");
    assert_eq!(one("zeta(-1)"), "-1/12");
    assert_eq!(one("zeta(3)"), "zeta(3)");
    assert_eq!(one("erf(0)"), "0");
    assert_eq!(one("erf(-x)"), "-erf(x)");
    assert_eq!(one("besselj(0, 0)"), "1");
    assert_eq!(one("beta(2, 3)"), "1/12");
    close("eval(zeta(3))", 1.2020569031595942);
    close("erf(1.0)", 0.8427007929497149);
    close("besselj(1, 1.0)", 0.44005058574493355);
    close("gamma(2.5)", 1.329340388179137);
    close("x = 2.0; erf(x)", 0.9953222650189527);
}

#[test]
fn special_calculus() {
    assert_eq!(one("diff(besselj(0, x), x)"), "-besselj(1, x)");
    assert_eq!(one("diff(gamma(x), x)"), "digamma(x)*gamma(x)");
    assert_eq!(one("integrate(exp(-x^2), x, 0, inf)"), "sqrt(pi)/2");
    assert_eq!(one("integrate(x^2*exp(-x^2/2), x, -inf, inf)"), "sqrt(2)*sqrt(pi)");
    assert_eq!(one("integrate(exp(-x^2/2)/sqrt(2*pi), x, -1, 1)"), "erf(sqrt(2)/2)");
    assert_eq!(one("limit(erf(x), x, inf)"), "1");
    assert_eq!(one("legendre(3, x)"), "5*x^3/2-3*x/2");
    assert_eq!(one("hermite(2, x)"), "4*x^2-2");
    assert_eq!(one("chebyshev(3, x)"), "4*x^3-3*x");
}

#[test]
fn vector_calculus() {
    assert_eq!(one("grad(x^2*y + y^3, [x y])"), one("[2*x*y, x^2+3*y^2]"));
    assert_eq!(one("div([x*y, y*z, z*x], [x y z])"), "x+y+z");
    assert_eq!(one("curl([-y, x, 0], [x y z])"), one("[0, 0, 2]"));
    assert_eq!(one("curl([-y, x], [x y])"), "2");
    assert_eq!(one("curl(grad(x*y*z, [x y z]), [x y z])"), one("[0, 0, 0]"));
    assert_eq!(one("div(curl([x*y, y*z^2, x^3], [x y z]), [x y z])"), "0");
    assert_eq!(one("laplacian(1/r, [r theta phi], \"spherical\")"), "0");
    assert_eq!(one("laplacian(log(r), [r theta], \"polar\")"), "0");
    assert_eq!(one("det(jacobian([r*cos(t), r*sin(t)], [r t]))"), "r");
    assert_eq!(one("potential([2*x*y, x^2 + 3*y^2], [x y])"), "x^2*y+y^3");
    assert_eq!(one("potential([-y, x], [x y])"), one("[]"));
    assert_eq!(one("lineint([-y, x], [x y], [cos(t), sin(t)], t, 0, 2*pi)"), "2*pi");
    assert_eq!(one("lineint(1, [x y], [R*cos(t), R*sin(t)], t, 0, 2*pi)"), "2*pi*R");
    assert_eq!(
        one("flux([x, y, z], [x y z], [sin(u)*cos(v), sin(u)*sin(v), cos(u)], u, 0, pi, v, 0, 2*pi)"),
        "4*pi"
    );
    assert_eq!(
        one("surfint(1, [x y z], [R*sin(u)*cos(v), R*sin(u)*sin(v), R*cos(u)], u, 0, pi, v, 0, 2*pi)"),
        "4*pi*R^2"
    );
}

#[test]
fn systems_and_optimization() {
    assert_eq!(one("solve([x^2 + y^2 - 25, x*y - 12], [x y])"), one("[-4 -3; -3 -4; 3 4; 4 3]"));
    assert_eq!(one("solve([x^2 + y^2 - 1, x - y], [x y])"), one("[-sqrt(2)/2, -sqrt(2)/2; sqrt(2)/2, sqrt(2)/2]"));
    assert_eq!(one("critical(x^3 - 3*x*y + y^3, [x y])"), one("[0 0; 1 1]"));
    assert_eq!(one("classify(x^3 - 3*x*y + y^3, [x y], [1 1])"), "min");
    assert_eq!(one("classify(x^3 - 3*x*y + y^3, [x y], [0 0])"), "saddle");
    assert_eq!(one("classify(x^4 + y^4, [x y], [0 0])"), "inconclusive");
    assert_eq!(one("lagrange(x*y, x^2 + y^2 == 2, [x y])"), one("[-1 -1; -1 1; 1 -1; 1 1]"));
    assert_eq!(one("lagrange(x + 2*y + 3*z, x^2 + y^2 + z^2 - 14, [x y z])"), one("[-1 -2 -3; 1 2 3]"));
    assert_eq!(one("minimize(x^3 - 3*x, x, -3, 2)"), "-3");
    assert_eq!(one("maximize(x^3 - 3*x, x, -3, 3/2)"), "-1");
    assert_eq!(one("minimize((x-1)^2 + (y+2)^2 + 3, [x y])"), one("[1 -2]"));
    let r = one("minimize((1-x)^2 + 100*(y-x^2)^2, [x y], [-1 1])");
    assert!(r.starts_with("[1.0") || r.starts_with("[0.99999"), "{}", r);
    let r = one("fsolve([x^2 + y^2 - 4, exp(x) - y], [x y], [1 1])");
    assert!(r.starts_with("[0.63926"), "{}", r);
}

#[test]
fn field_plots() {
    for code in [
        "contour(x^2 - y^2, x, -2, 2, y, -2, 2)",
        "contour(x*y, x, -2, 2, y, -2, 2, [-1 1])",
        "fieldplot([-y, x], x, -2, 2, y, -2, 2)",
        "show(contour(x^2+y^2, x, -1, 1, y, -1, 1, 5), fieldplot(grad(x^2+y^2, [x y]), x, -1, 1, y, -1, 1))",
    ] {
        let mut s = Session::tidy();
        let r = s.run(code).pop().unwrap();
        assert!(matches!(r, Ok(Output::Value(galileo_rs::expr::Expr::Obj(_)))), "{}", code);
    }
}
