//! Numeric evaluation of expressions at complex points, for contour
//! integrals and complex plots.

use crate::expr::{Const, Expr, Func};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct C {
    pub re: f64,
    pub im: f64,
}

impl C {
    pub fn new(re: f64, im: f64) -> C {
        C { re, im }
    }
    pub fn abs(self) -> f64 {
        self.re.hypot(self.im)
    }
    pub fn arg(self) -> f64 {
        self.im.atan2(self.re)
    }
    pub fn add(self, o: C) -> C {
        C::new(self.re + o.re, self.im + o.im)
    }
    pub fn sub(self, o: C) -> C {
        C::new(self.re - o.re, self.im - o.im)
    }
    pub fn mul(self, o: C) -> C {
        C::new(self.re * o.re - self.im * o.im, self.re * o.im + self.im * o.re)
    }
    pub fn scale(self, k: f64) -> C {
        C::new(self.re * k, self.im * k)
    }
    pub fn div(self, o: C) -> C {
        let d = o.re * o.re + o.im * o.im;
        C::new((self.re * o.re + self.im * o.im) / d, (self.im * o.re - self.re * o.im) / d)
    }
    pub fn exp(self) -> C {
        let r = self.re.exp();
        C::new(r * self.im.cos(), r * self.im.sin())
    }
    /// principal logarithm
    pub fn ln(self) -> C {
        C::new(self.abs().ln(), self.arg())
    }
    pub fn powc(self, e: C) -> C {
        if self.re == 0.0 && self.im == 0.0 {
            return if e.re > 0.0 { C::new(0.0, 0.0) } else { C::new(f64::INFINITY, 0.0) };
        }
        self.ln().mul(e).exp()
    }
    pub fn powi(self, n: i64) -> C {
        let mut r = C::new(1.0, 0.0);
        let mut b = if n < 0 { C::new(1.0, 0.0).div(self) } else { self };
        let mut k = n.unsigned_abs();
        while k > 0 {
            if k & 1 == 1 {
                r = r.mul(b);
            }
            b = b.mul(b);
            k >>= 1;
        }
        r
    }
    pub fn is_finite(self) -> bool {
        self.re.is_finite() && self.im.is_finite()
    }
}

const I: C = C { re: 0.0, im: 1.0 };
const ONE: C = C { re: 1.0, im: 0.0 };

fn sin(z: C) -> C {
    C::new(z.re.sin() * z.im.cosh(), z.re.cos() * z.im.sinh())
}
fn cos(z: C) -> C {
    C::new(z.re.cos() * z.im.cosh(), -z.re.sin() * z.im.sinh())
}
fn sinh(z: C) -> C {
    C::new(z.re.sinh() * z.im.cos(), z.re.cosh() * z.im.sin())
}
fn cosh(z: C) -> C {
    C::new(z.re.cosh() * z.im.cos(), z.re.sinh() * z.im.sin())
}
fn sqrt(z: C) -> C {
    z.powc(C::new(0.5, 0.0))
}

/// The value of e with the given variables set, or None for an unsupported expression
pub fn ceval(e: &Expr, env: &[(&str, C)]) -> Option<C> {
    Some(match e {
        Expr::Num(n) => C::new(n.to_f64(), 0.0),
        Expr::Const(Const::I) => I,
        Expr::Const(Const::Pi) => C::new(std::f64::consts::PI, 0.0),
        Expr::Const(Const::E) => C::new(std::f64::consts::E, 0.0),
        Expr::Sym(s) => env.iter().find(|(n, _)| n == s)?.1,
        Expr::Add(v) => {
            let mut acc = C::new(0.0, 0.0);
            for t in v {
                acc = acc.add(ceval(t, env)?);
            }
            acc
        }
        Expr::Mul(v) => {
            let mut acc = ONE;
            for t in v {
                acc = acc.mul(ceval(t, env)?);
            }
            acc
        }
        Expr::Pow(b, x) => {
            if let Expr::Const(Const::E) = **b {
                return Some(ceval(x, env)?.exp());
            }
            let bv = ceval(b, env)?;
            if let Expr::Num(n) = &**x {
                if let Some(k) = n.as_int().filter(|k| k.abs() < 1_000_000) {
                    return Some(bv.powi(k as i64));
                }
            }
            bv.powc(ceval(x, env)?)
        }
        Expr::Func(f, a) => {
            let z = ceval(a, env)?;
            match f {
                Func::Sin => sin(z),
                Func::Cos => cos(z),
                Func::Tan => sin(z).div(cos(z)),
                Func::Sinh => sinh(z),
                Func::Cosh => cosh(z),
                Func::Tanh => sinh(z).div(cosh(z)),
                Func::Log => z.ln(),
                Func::Abs => C::new(z.abs(), 0.0),
                // asin z = -i log(iz + sqrt(1 - z^2)), and similar
                Func::Asin => I.scale(-1.0).mul(I.mul(z).add(sqrt(ONE.sub(z.mul(z)))).ln()),
                Func::Acos => I.scale(-1.0).mul(z.add(I.mul(sqrt(ONE.sub(z.mul(z))))).ln()),
                Func::Atan => I.scale(0.5).mul(ONE.sub(I.mul(z)).ln().sub(ONE.add(I.mul(z)).ln())),
                Func::Asinh => z.add(sqrt(z.mul(z).add(ONE))).ln(),
                Func::Acosh => z.add(sqrt(z.add(ONE)).mul(sqrt(z.sub(ONE)))).ln(),
                Func::Atanh => ONE.add(z).ln().sub(ONE.sub(z).ln()).scale(0.5),
            }
        }
        Expr::Apply(f, v) if f == "polylog" && v.len() == 2 => {
            let s = ceval(&v[0], env)?;
            let z = ceval(&v[1], env)?;
            if s.im != 0.0 {
                return None;
            }
            let (re, im) = crate::special::polylog_c(s.re, (z.re, z.im));
            C::new(re, im)
        }
        Expr::Apply(f, v) if crate::special::is_special(f, v.len()) => {
            let mut a = vec![];
            for x in v {
                let c = ceval(x, env)?;
                if c.im.abs() > 1e-14 * c.re.abs().max(1.0) {
                    return None;
                }
                a.push(c.re);
            }
            C::new(crate::special::value(f, &a), 0.0)
        }
        _ => return None,
    })
}
