//! fft, ifft, freqs and animate.

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
fn fourier_transforms() {
    assert_eq!(one("fft([1 0 0 0])"), "[1.0, 1.0, 1.0, 1.0]");
    assert_eq!(one("fft([1 2 3 4])"), "[10.0, -2.0+2.0*i, -2.0, -2.0-2.0*i]");
    assert_eq!(one("ifft(fft([1 2 3]))"), "[1.0, 2.0, 3.0]");
    assert_eq!(one("freqs(5, 1)"), "[0.0, 0.2, 0.4, -0.4, -0.2]");
    // a 5 Hz sine sampled 64 times in a second: all of it at 5 Hz and -5 Hz
    assert_eq!(one("t = (0:63)/64; X = fft(sin(2*pi*5*t)); abs(X(6))"), "32.0");
    assert_eq!(one("t = (0:63)/64; X = fft(sin(2*pi*5*t)); abs(X(5))"), "0.0");
}

#[test]
fn animations() {
    let mut s = Session::new();
    let r = s.run("p = animate(sin(x - t), x, 0, 10, t, 0, 2*pi, 12)").pop().unwrap().unwrap();
    let Output::Value(v) = r else { panic!() };
    let b = galileo_rs::display::bundle(&v).unwrap();
    let plotly = &b.iter().find(|x| x.0 == "application/vnd.plotly.v1+json").unwrap().1;
    assert!(plotly.contains("\"frames\"") && plotly.contains("\"sliders\""));
    assert_eq!(plotly.matches("\"traces\"").count(), 12);
    assert!(one("animate(cos(t), sin(t), t, 0, 2*pi)").contains("<figure>") || !one("animate(cos(t), sin(t), t, 0, 2*pi)").starts_with("ERROR"));
    assert!(!one("animate([0 1 2], [0 1 4])").starts_with("ERROR"));
    assert!(one("animate(sin(x), x)").starts_with("ERROR"));
    assert!(one("animate(1, 2, 3)").starts_with("ERROR usage: animate"));
}
