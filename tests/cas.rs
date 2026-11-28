//! Integration, limits, Taylor series, solving and the smaller helpers.

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
    last
}

#[test]
fn antiderivatives() {
    for (f, want) in [
        ("x^2", "x^3/3"),
        ("x*exp(x)", "exp(x)*(x-1)"),
        ("log(x)", "x*log(x)-x"),
        ("1/(1+x^2)", "atan(x)"),
        ("x*exp(x^2)", "exp(x^2)/2"),
        ("1/sqrt(1-x^2)", "asin(x)"),
        ("exp(x)*sin(x)", "exp(x)*(sin(x)-cos(x))/2"),
    ] {
        assert_eq!(
            one(&format!("integrate({}, x)", f)),
            want,
            "integrate({})",
            f
        );
    }
    assert_eq!(one("integrate(exp(-x^2), x)"), "sqrt(pi)*erf(x)/2");
}

#[test]
fn definite_integrals() {
    assert_eq!(one("integrate(sin(x), x, 0, pi)"), "2");
    assert_eq!(one("integrate(1/(1+x^2), x, -inf, inf)"), "pi");
    assert_eq!(one("integrate(log(x), x, 0, 1)"), "-1");
    assert_eq!(one("integrate(x^2, x, 0, a)"), "a^3/3");
    assert!(one("integrate(1/x^2, x, -1, 1)").contains("diverges"));
    assert_eq!(one("integrate(exp(-x^2), x, -inf, inf)"), "sqrt(pi)");
}

#[test]
fn limits() {
    assert_eq!(one("limit(sin(x)/x, x, 0)"), "1");
    assert_eq!(one("limit((1-cos(x))/x^2, x, 0)"), "1/2");
    assert_eq!(one("limit((1+1/x)^x, x, inf)"), "e");
    assert_eq!(one("limit(x*log(x), x, 0, right)"), "0");
    assert_eq!(one("limit(x^5*exp(-x), x, inf)"), "0");
    assert_eq!(one("limit(1/x, x, 0, left)"), "-inf");
    assert!(one("limit(1/x, x, 0)").contains("does not exist"));
}

#[test]
fn taylor_series() {
    assert_eq!(one("taylor(sin(x)/x, x, 0, 4)"), "x^4/120-x^2/6+1");
    assert_eq!(one("taylor(1/(1-x), x, 0, 3)"), "x^3+x^2+x+1");
}

#[test]
fn solving() {
    assert_eq!(
        one("solve(x^2 - 5*x + 6, x)"),
        "Matrix(2 3)".replace("Matrix(2 3)", &one("[2 3]"))
    );
    assert_eq!(one("solve(x^3 - 6*x^2 + 11*x - 6, x)"), one("[1 2 3]"));
    assert_eq!(one("solve(2*x + 3, x)"), "-3/2");
    assert_eq!(one("solve([x + y - 3, x - y - 1], [x y])"), one("[2 1]"));
    let r: f64 = one("fsolve(cos(x) - x, x, 1)").parse().unwrap();
    assert!((r - 0.7390851332151607).abs() < 1e-12);
}

#[test]
fn helpers() {
    assert_eq!(one("factor(x^3 - 6*x^2 + 11*x - 6)"), "(x-3)*(x-2)*(x-1)");
    assert_eq!(one("sum(k^2, k, 1, 10)"), "385");
    assert_eq!(one("sum(x^k/factorial(k), k, 0, 3)"), "x^3/6+x^2/2+x+1");
    assert_eq!(one("binomial(10, 3)"), "120");
    assert_eq!(one("mod(-7, 3)"), "2");
    assert_eq!(one("eig([2 1; 1 2])"), one("[1 3]"));
    assert_eq!(one("cross([1 0 0], [0 1 0])"), one("[0 0 1]"));
    assert_eq!(one("coeff((x+1)^3, x, 2)"), "3");
}

#[test]
fn polynomial_gcd() {
    assert_eq!(one("gcd(x^4 - 1, x^6 - 1)"), "x^2-1");
    assert_eq!(one("gcd(2*x^2 - 2, 4*x - 4)"), "x-1");
    assert_eq!(one("gcd(x^4 + 1, x^17 - x, 17)"), "x^4+1");
    assert_eq!(one("gcd(x^4 + 1, x^3 - x, 3)"), "1");
    assert_eq!(one("gcd(12, 18)"), "6");
}

#[test]
fn factorial_ratios() {
    // big factorials are quick (binary splitting), and so is their ratio
    let t = std::time::Instant::now();
    assert_eq!(
        one("factorial(10010)/factorial(10000)"),
        "10055132181657863239677710775463890028800"
    );
    assert_eq!(
        one("factorial(99990)/factorial(100000)"),
        "1/99955008699055063270306822366827310265723712000000"
    );
    assert_eq!(one("factorial(100000)").len(), 456574);
    assert!(t.elapsed().as_secs() < 20, "factorials took {:?}", t.elapsed());
    // a symbolic argument stays as factorial(n); simplify cancels the ratio
    // (and, like every simplify, multiplies the result out)
    assert_eq!(one("factorial(n)"), "factorial(n)");
    assert_eq!(one("simplify(factorial(n+2)/factorial(n))"), "n^2+3*n+2");
    assert_eq!(one("simplify(factorial(n)/factorial(n+3))"), "1/(n^3+6*n^2+11*n+6)");
    assert_eq!(one("simplify(factorial(n+1)/factorial(n-1))"), "n^2+n");
    assert_eq!(one("simplify(gamma(x+2)/gamma(x))"), "x^2+x");
    assert_eq!(one("simplify(factorial(n)/gamma(n))"), "n");
    assert_eq!(one("f = factorial(n+1)/factorial(n-1); n = 7; f"), "56");
    assert_eq!(one("diff(factorial(x), x)"), "digamma(x+1)*factorial(x)");
    assert_eq!(one("factorial(-1)"), "ERROR factorial needs a non-negative integer");
}
