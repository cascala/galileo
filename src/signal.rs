//! The discrete Fourier transform: fft(x), ifft(X), and freqs(n, dt) for the
//! frequencies of its entries.

use crate::eval::{R, Session};
use crate::expr::Expr;
use crate::num::Number;
use crate::parser::Node;
use crate::print::show;
use std::f64::consts::PI;

type C = (f64, f64);

/// X_k = sum_n x_n e^(-2 pi i k n / N) (sign +1 and a factor 1/N for the inverse);
/// radix 2 for powers of two, the plain sum otherwise
pub fn dft(x: &[C], inverse: bool) -> Vec<C> {
    let n = x.len();
    let sign = if inverse { 1.0 } else { -1.0 };
    let mut out = if n.is_power_of_two() { radix2(x, sign) } else { plain(x, sign) };
    if inverse {
        for v in out.iter_mut() {
            v.0 /= n as f64;
            v.1 /= n as f64;
        }
    }
    out
}

fn plain(x: &[C], sign: f64) -> Vec<C> {
    let n = x.len();
    (0..n)
        .map(|k| {
            let mut s = (0.0, 0.0);
            for (j, v) in x.iter().enumerate() {
                let a = sign * 2.0 * PI * ((k * j) % n) as f64 / n as f64;
                let (c, si) = (a.cos(), a.sin());
                s.0 += v.0 * c - v.1 * si;
                s.1 += v.0 * si + v.1 * c;
            }
            s
        })
        .collect()
}

fn radix2(x: &[C], sign: f64) -> Vec<C> {
    let n = x.len();
    if n == 1 {
        return vec![x[0]];
    }
    let even: Vec<C> = x.iter().step_by(2).copied().collect();
    let odd: Vec<C> = x.iter().skip(1).step_by(2).copied().collect();
    let (e, o) = (radix2(&even, sign), radix2(&odd, sign));
    let mut out = vec![(0.0, 0.0); n];
    for k in 0..n / 2 {
        let a = sign * 2.0 * PI * k as f64 / n as f64;
        let (c, s) = (a.cos(), a.sin());
        let t = (o[k].0 * c - o[k].1 * s, o[k].0 * s + o[k].1 * c);
        out[k] = (e[k].0 + t.0, e[k].1 + t.1);
        out[k + n / 2] = (e[k].0 - t.0, e[k].1 - t.1);
    }
    out
}

/// a complex number as an expression, with rounding noise (1e-12 of the
/// largest entry) set to 0
fn cexpr(v: C, tiny: f64) -> Expr {
    // 12 significant digits: 2.0, not 1.9999999999999998
    let clean = |x: f64| if x.abs() <= tiny { 0.0 } else { format!("{:.11e}", x).parse().unwrap_or(x) };
    let (re, im) = (clean(v.0), clean(v.1));
    if im == 0.0 {
        Expr::float(re)
    } else {
        crate::build::from_gauss(Number::Float(re), Number::Float(im))
    }
}

impl Session {
    pub(crate) fn signal_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "fft" | "ifft" => self.fft_cmd(f, args),
            "freqs" => self.freqs_cmd(args),
            _ => return None,
        };
        Some(r)
    }

    fn fft_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("usage: {}(x) for a row or column of numbers", f));
        };
        let v = self.eval(a)?;
        let Expr::Matrix(m) = &v else {
            return Err(format!("{}: needs a list of numbers, such as [1 2 3 4]", f));
        };
        let column = m.len() > 1;
        if column && m.iter().any(|r| r.len() != 1) {
            return Err(format!("{}: needs a row or a column, not a matrix", f));
        }
        let mut x = vec![];
        for e in m.iter().flatten() {
            let c = crate::cnum::ceval(e, &[]).ok_or_else(|| format!("{}: {} is not a number", f, show(e)))?;
            x.push((c.re, c.im));
        }
        let y = dft(&x, f == "ifft");
        let big = y.iter().map(|v| v.0.abs().max(v.1.abs())).fold(0.0, f64::max);
        let tiny = big * 1e-12;
        let vals: Vec<Expr> = y.into_iter().map(|v| cexpr(v, tiny)).collect();
        Ok(Expr::Matrix(if column { vals.into_iter().map(|e| vec![e]).collect() } else { vec![vals] }))
    }

    /// freqs(n, dt): the frequency of each entry of fft for n samples dt apart,
    /// 0, 1/(n dt), 2/(n dt), ..., then the negative ones (as numpy's fftfreq)
    fn freqs_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [n, dt] = args else {
            return Err("usage: freqs(n, dt) for n samples taken dt apart".into());
        };
        let n = self.number(n, "the number of samples")?;
        let dt = self.number(dt, "the time step")?;
        if n < 1.0 || n.fract() != 0.0 || dt <= 0.0 {
            return Err("freqs(n, dt): a whole number n >= 1 and dt > 0".into());
        }
        let n = n as i64;
        let row = (0..n)
            .map(|k| {
                let k = if k < (n + 1) / 2 { k } else { k - n };
                Expr::float(k as f64 / (n as f64 * dt))
            })
            .collect();
        Ok(Expr::Matrix(vec![row]))
    }
}
