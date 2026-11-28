//! Wave 4: descriptive statistics, distributions, random samples, tests,
//! least squares and histograms.

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

fn close(code: &str, want: f64, tol: f64) {
    let got: f64 = one(code).parse().unwrap_or(f64::NAN);
    assert!((got - want).abs() < tol, "{code}: {got} vs {want}");
}

#[test]
fn descriptive_exact() {
    let x = "x = [2 4 4 4 5 5 7 9]; ";
    assert_eq!(one(&format!("{x}mean(x)")), "5");
    assert_eq!(one(&format!("{x}median(x)")), "9/2");
    assert_eq!(one(&format!("{x}var(x)")), "32/7");
    assert_eq!(one(&format!("{x}summary(x)")), "[2, 4, 9/2, 11/2, 9]");
    assert_eq!(one(&format!("{x}sum(x > 4)")), "4");
    assert_eq!(one("linfit([1 2 3 4], [2 4 5 8])"), "19*x/10");
}

#[test]
fn distributions() {
    assert_eq!(one("binopdf(3, 10, 1/2)"), "15/128");
    assert_eq!(one("sum(k*binopdf(k, 10, 1/2), k, 0, 10)"), "5");
    close("normcdf(1.96)", 0.975002104851780, 1e-12);
    close("norminv(0.975)", 1.959963984540054, 1e-9);
    close("tinv(0.975, 10)", 2.228138851986273, 1e-8);
    close("chi2inv(0.95, 3)", 7.814727903251178, 1e-8);
    close("tcdf(2.0, 5)", 0.9490302605850709, 1e-9);
}

#[test]
fn random_is_repeatable() {
    assert_eq!(one("seed(7); randint(1, 6, 5)"), one("seed(7); randint(1, 6, 5)"));
    close("seed(1); abs(mean(randn(1, 20000)))", 0.0, 0.03);
    close("seed(1); eval(mean(randpois(3, 20000)))", 3.0, 0.06);
}

#[test]
fn inference() {
    close("chi2test([18 22 20 25 15], [20 20 20 20 20])", 0.5746972058298042, 1e-9);
    close("ttest([5.1 4.9 5.3 5.0 5.2], 5)", 0.2301996, 1e-5);
    assert!(one("ci([1 2 3 4 5])").starts_with('['));
    assert!(!one("hist([1 2 2 3 3 3], 3)").starts_with("ERROR"));
    assert!(!one("bar([1 3 3 1])").starts_with("ERROR"));
}

#[test]
fn lists_and_plots() {
    // distributions apply to each entry of a list
    assert_eq!(one("binopdf([0 1 2 3], 3, 1/2)"), "[1/8, 3/8, 3/8, 1/8]");
    // a row times a column is a number
    assert_eq!(one("k = [0 1 2 3]; P = [1 3 3 1]/8; k*P'"), "3/2");
    // subs with a list evaluates at each value
    assert_eq!(one("subs(2*x + 1, x, [1 2 3])"), "[3, 5, 7]");
    // plot and scatter of two lists, bars with a given width
    assert!(one("plot([1 2 3], [2 4 5])").contains("Figure") || !one("plot([1 2 3], [2 4 5])").starts_with("ERROR"));
    assert!(!one("scatter([1 2 3], [2 4 5])").starts_with("ERROR"));
    assert!(!one("bar([1 2 3], [2 4 5], 0.4)").starts_with("ERROR"));
    assert!(one("plot([1 2 3], [2 4])").starts_with("ERROR"));
    assert_eq!(one("round(-0.001, 2)"), "0.0");
}
