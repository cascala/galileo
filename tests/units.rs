//! Quantities with units: 3 m/s, conversions, dimension checks, constants.

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
fn arithmetic_with_units() {
    for (code, want) in [
        ("v = 3 m/s; v*(2 s)", "6 m"),
        ("(6 m)/(2 s)", "3 m/s"),
        ("2 kg * 3 m/s^2", "6 kg*m/s^2"),
        ("1 km + 300 m", "1.3 km"),
        ("(3 km)/(1500 m)", "2"),
        ("(3 m)^2", "9 m^2"),
        ("sqrt(16 m^2)", "4 m"),
        ("sqrt((100 N/m)/(1 kg))", "10 s^-1"),
        ("sin(30 deg)", "1/2"),
        ("[1 2 3] m", "[1, 2, 3] m"),
        ("[(1 m) (2 s)]", "[(1 m), (2 s)]"),
        ("t = 4; (2*t) m", "8 m"),
        ("-3 m", "-3 m"),
        ("3 m > 200 cm", "true"),
        ("1/(2 s)", "(1/2) s^-1"),
        ("unit(5, \"J/(kg*K)\")", "5 J/(K*kg)"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
}

#[test]
fn conversions_and_constants() {
    for (code, want) in [
        ("convert(2 kg*3 m/s^2, N)", "6 N"),
        ("convert(100 km/h, m/s)", "27.7777777777778 m/s"),
        ("convert(1 eV, J)", "1.602176634e-19 J"),
        ("convert(1 kWh, MJ)", "3.6 MJ"),
        ("convert(1 kW*h, J)", "3600000 J"),
        ("convert(5 N)", "5 kg*m/s^2"),
        ("const.c", "299792458 m/s"),
        ("dimension(const.G)", "length^3/(mass*time^2)"),
        ("h = 10 m; convert(sqrt(2*h/const.g))", "(200*sqrt(10/196133)) s"),
        ("h = 10 m; eval(convert(sqrt(2*h/const.g)))", "1.4280869812290344 s"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
}

#[test]
fn dimension_errors() {
    assert!(one("2 m + 3 s").starts_with("ERROR can't add 2 m and 3 s"));
    assert!(one("3 m + 2").starts_with("ERROR can't add"));
    assert!(one("exp(2 m)").starts_with("ERROR exp(2 m)"));
    assert!(one("convert(3 m, s)").starts_with("ERROR can't convert"));
    assert!(one("const.foo").starts_with("ERROR const.foo is not a constant"));
}

#[test]
fn unit_names_stay_variables() {
    // m, s, g are free names except right after a number, a matrix or ")"
    assert_eq!(one("m = 2; s = 3; m*s"), "6");
    assert_eq!(one("[0 1 s]"), "[0, 1, s]");
    assert_eq!(one("for k = [1 2] s = k; end; s"), "2");
    assert_eq!(one("x = 3; x m"), "ERROR parse error: unexpected input near 'm'");
    // printed quantities read back as the same value
    for code in ["7 m/s^2", "(3/2) kg", "(1/2) s^-1", "[1, 2] N", "(2*t+3) m"] {
        let shown = one(code);
        assert_eq!(one(&shown), shown, "{}", code);
    }
}

#[test]
fn latex_of_quantities() {
    let mut s = Session::new();
    let mut tex = vec![];
    for code in ["9.81 m/s^2", "5 uF", "3 kohm", "(2*t+3) m", "[1 2] N"] {
        if let Some(Ok(Output::Value(v))) = s.run(code).pop() {
            tex.push(galileo_rs::latex::latex(&v));
        }
    }
    assert_eq!(
        tex,
        [
            "9.81\\,\\mathrm{m/s^{2}}",
            "5\\,\\mathrm{\\mu F}",
            "3\\,\\mathrm{k\\Omega}",
            "\\left(2 \\, t + 3\\right)\\,\\mathrm{m}",
            "\\begin{pmatrix} 1\\,\\mathrm{N} & 2\\,\\mathrm{N} \\end{pmatrix}",
        ]
    );
}

#[test]
fn bare_numbers() {
    assert_eq!(one("value(3 km)"), "3000");
    assert_eq!(one("value(100 km/h, m/s)"), "27.7777777777778");
    assert_eq!(one("value([1 2] km, m)"), "[1000, 2000]");
}

#[test]
fn rounding_and_alike_units() {
    assert_eq!(one("round(194.03950962779996 m, 1)"), "194.0 m");
    assert_eq!(one("eval(0.0009031764719805297 kg/m, 3)"), "0.000903 kg/m");
    // units of one kind in a product merge, keeping the SI one
    assert_eq!(one("(2 cm^2)*(1 kg/m^3)"), "0.0002 kg/m");
    assert_eq!(one("(2 km)*(3 m)"), "6000 m^2");
}
