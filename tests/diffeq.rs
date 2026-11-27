//! Wave 3: assumptions, Heaviside and Dirac, Laplace transforms, more dsolve
//! (separable, systems, Cauchy-Euler), series solutions and Fourier series.

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
fn assumptions() {
    assert_eq!(one("assume(a > 0); sqrt(a^2)"), "a");
    assert_eq!(one("assume(a > 0); int(exp(-a*x), x, 0, inf)"), "1/a");
    assert_eq!(one("assume(n, integer); sin(n*pi)"), "0");
    assert_eq!(one("assume(n, integer); cos(n*pi)"), "(-1)^n");
    assert_eq!(one("assume(s > 0); int(exp(-s*t)*sin(t), t, 0, inf)"), "1/(s^2+1)");
    assert_eq!(one("assume(a > 0); forget(a); sqrt(a^2)"), one("sqrt(a^2)"));
}

#[test]
fn heaviside_dirac_integrals() {
    assert_eq!(one("int(abs(x), x, -1, 2)"), "5/2");
    assert_eq!(one("int(heaviside(x-1)*x, x, 0, 3)"), "4");
    assert_eq!(one("int(sign(x)*sin(x), x, -pi, pi)"), "4");
    assert_eq!(one("int(dirac(x-2)*x^2, x, 0, 5)"), "4");
    assert_eq!(one("int(dirac(x)*x, x, 1, 3)"), "0");
    assert_eq!(one("heaviside(0)"), "1/2");
}

#[test]
fn laplace_pairs() {
    assert_eq!(one("laplace(t^2, t, s)"), "2/s^3");
    assert_eq!(one("laplace(heaviside(t-2), t, s)"), "exp(-2*s)/s");
    assert_eq!(one("ilaplace(1/(s^2 - 4), s, t)"), "sinh(2*t)/2");
    assert_eq!(one("ilaplace(1/(s^2+4), s, t)"), "sin(2*t)/2");
    assert_eq!(one("ilaplace(1/(s*(s + 1)), s, t)"), "1-exp(-t)");
    assert_eq!(one("partfrac((x + 3)/(x^2 - 1), x)"), "2/(x-1)-1/(x+1)");
}

#[test]
fn dsolve_more() {
    assert_eq!(one("dsolve(y' == y*(1 - y/10), y, x, 0, 1)"), "10*exp(x)/(exp(x)+9)");
    assert_eq!(one("dsolve(y' == x/y, y, x, 0, 2)"), "sqrt(x^2+4)");
    assert_eq!(one("dsolve(y' == y^2, y, x, 0, 1)"), "1/(1-x)");
    assert_eq!(one("dsolve(x^2*y'' + x*y' - 4*y == 0, y, x)"), "C1*x^2+C2/x^2");
    assert_eq!(one("dsolve([x' == -x + y, y' == -x - y], [x, y], t, 0, [1, 0])"), "[exp(-t)*cos(t), -exp(-t)*sin(t)]");
    assert_eq!(one("dsolve(y'' == 1/x, y, x)"), "C1+C2*x+x*log(x)-x");
    assert_eq!(one("dsolve(v' == -u/M, v, M, M0, 0)"), "u*log(M0)-u*log(M)");
    assert_eq!(
        one("dsolve([x' == u, y' == v, u' == -2*x + y, v' == x - 2*y], [x, y, u, v], t, 0, [1, 0, 0, 0])"),
        "[(cos(t)+cos(sqrt(3)*t))/2, cos(t)/2-cos(sqrt(3)*t)/2, -(sqrt(3)*sin(sqrt(3)*t)+sin(t))/2, sqrt(3)*sin(sqrt(3)*t)/2-sin(t)/2]"
    );
}

#[test]
fn series_and_fourier() {
    assert_eq!(one("seriessolve(y'' + y == 0, y, x, 6, [1, 0])"), "-x^6/720+x^4/24-x^2/2+1");
    assert_eq!(one("fourier(x, x, pi, 3)"), "2*sin(x)-sin(2*x)+2*sin(3*x)/3");
    assert_eq!(one("fourier(x^2, x, pi, 2)"), "pi^2/3-4*cos(x)+cos(2*x)");
}

#[test]
fn trig_products_and_window() {
    assert_eq!(one("int(sin(3*x)*cos(5*x), x)"), "cos(2*x)/4-cos(8*x)/16");
    assert_eq!(one("int(sin(3*x)*sin(5*x), x, -pi, pi)"), "0");
    assert!(!one("window(plot(x, x, 0, 1), 0, 1, -1, 2)").starts_with("ERROR"));
    assert!(one("window(plot(x, x, 0, 1), 1, 0, -1, 2)").starts_with("ERROR"));
    assert_eq!(one("dsolve(P' == P*(1 - P) - 3/16, P, t, 0, 1)"), "(1-9*exp(t/2))/(4-12*exp(t/2))");
}

#[test]
fn ode_steps_refine_at_close_encounters() {
    // a near-collision orbit (speed 0.2 at distance 1, perihelion about 0.02): the fixed grid alone fails here
    let kepler = "f = [vx, vy, -x/(x^2 + y^2)^(3/2), -y/(x^2 + y^2)^(3/2)]; D = odedata(f, [x y vx vy], t, [1, 0, 0, 0.2], 0, 4, 40);";
    let e = |k: usize| format!("{} eval((D({k}, 4)^2 + D({k}, 5)^2)/2 - 1/sqrt(D({k}, 2)^2 + D({k}, 3)^2), 6)", kepler);
    assert_eq!(one(&e(1)), "-0.980000");
    assert_eq!(one(&e(41)), "-0.980000");
    // a single point is a scatter plot too
    assert!(!one("scatter([0], [0])").starts_with("ERROR"));
}
