//! eval(x, digits): the value of an expression to many digits.
//!
//! Values are computed in fixed point (a big integer counting units of
//! 2^-p) with p a little larger than the digits asked for, then once more
//! with more bits; when the two agree to the digits asked for, that is the
//! answer (otherwise the precision grows and it tries again).

use crate::expr::{Const, Expr, Func};
use crate::num::{self, Number};
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};

type R<T> = Result<T, String>;

/// a fixed-point number: v / 2^p
#[derive(Clone)]
struct Fx {
    v: BigInt,
    p: u64,
}

fn one(p: u64) -> BigInt {
    BigInt::one() << p
}

impl Fx {
    fn from_rat(r: &BigRational, p: u64) -> Fx {
        let n = (r.numer() << p).div_floor(r.denom());
        Fx { v: n, p }
    }
    fn int(k: i64, p: u64) -> Fx {
        Fx {
            v: BigInt::from(k) << p,
            p,
        }
    }
    fn to_rat(&self) -> BigRational {
        BigRational::new(self.v.clone(), one(self.p))
    }
    fn add(&self, o: &Fx) -> Fx {
        Fx {
            v: &self.v + &o.v,
            p: self.p,
        }
    }
    fn sub(&self, o: &Fx) -> Fx {
        Fx {
            v: &self.v - &o.v,
            p: self.p,
        }
    }
    fn neg(&self) -> Fx {
        Fx {
            v: -&self.v,
            p: self.p,
        }
    }
    fn mul(&self, o: &Fx) -> Fx {
        Fx {
            v: (&self.v * &o.v) >> self.p,
            p: self.p,
        }
    }
    fn div(&self, o: &Fx) -> R<Fx> {
        if o.v.is_zero() {
            return Err("division by zero".into());
        }
        Ok(Fx {
            v: (&self.v << self.p).div_floor(&o.v),
            p: self.p,
        })
    }
    fn halve(&self, k: u64) -> Fx {
        Fx {
            v: &self.v >> k,
            p: self.p,
        }
    }
    fn double(&self, k: u64) -> Fx {
        Fx {
            v: &self.v << k,
            p: self.p,
        }
    }
    fn is_neg(&self) -> bool {
        self.v.is_negative()
    }
    fn is_zero(&self) -> bool {
        self.v.is_zero()
    }
    fn to_f64(&self) -> f64 {
        self.to_rat().to_f64().unwrap_or(f64::NAN)
    }
    fn sqrt(&self) -> R<Fx> {
        if self.is_neg() {
            return Err("square root of a negative number (complex values are not computed to many digits yet)".into());
        }
        Ok(Fx {
            v: (&self.v << self.p).sqrt(),
            p: self.p,
        })
    }
}

/// sum of a series whose terms come from `next(k, previous term)`, until terms vanish
fn series(p: u64, first: Fx, mut next: impl FnMut(u64, &Fx) -> R<Fx>) -> R<Fx> {
    let mut sum = first.clone();
    let mut term = first;
    let mut k = 1;
    while !term.is_zero() {
        term = next(k, &term)?;
        sum = sum.add(&term);
        k += 1;
        if k > 10 * p + 1000 {
            break;
        }
    }
    Ok(sum)
}

/// atan(1/n) for a whole number n
fn atan_inv(n: i64, p: u64) -> Fx {
    let x = Fx::int(1, p).div(&Fx::int(n, p)).unwrap();
    let n2 = BigInt::from(n * n);
    // sum (-1)^k / ((2k+1) n^(2k+1))
    let mut sum = x.clone();
    let mut pow = x.v.clone();
    let mut k: i64 = 1;
    loop {
        pow = &pow / &n2;
        if pow.is_zero() {
            break;
        }
        let t = &pow / BigInt::from(2 * k + 1);
        if k % 2 == 1 {
            sum.v -= t;
        } else {
            sum.v += t;
        }
        k += 1;
    }
    sum
}

fn pi(p: u64) -> Fx {
    // Machin: pi = 16 atan(1/5) - 4 atan(1/239)
    atan_inv(5, p).double(4).sub(&atan_inv(239, p).double(2))
}

fn ln2(p: u64) -> Fx {
    // ln 2 = 2 atanh(1/3)
    atanh_small(&Fx::int(1, p).div(&Fx::int(3, p)).unwrap()).double(1)
}

/// atanh(x) for small |x| by its series
fn atanh_small(x: &Fx) -> Fx {
    let x2 = x.mul(x);
    let mut sum = x.clone();
    let mut pow = x.clone();
    let mut k: i64 = 1;
    loop {
        pow = pow.mul(&x2);
        if pow.is_zero() {
            break;
        }
        sum.v += &pow.v / BigInt::from(2 * k + 1);
        k += 1;
    }
    sum
}

fn exp(x: &Fx) -> R<Fx> {
    let p = x.p;
    if x.to_f64() > 1e7 {
        return Err("exp: the result is too large".into());
    }
    // exp(x) = exp(x / 2^k)^(2^k) with |x / 2^k| < 2^-8
    let mag = x.v.bits() as i64 - p as i64;
    let k = (mag + 8).max(0) as u64;
    let y = x.halve(k);
    let mut r = series(p, Fx::int(1, p), |n, t| {
        Ok(t.mul(&y).div(&Fx::int(n as i64, p))?)
    })?;
    for _ in 0..k {
        r = r.mul(&r);
    }
    Ok(r)
}

fn ln(x: &Fx) -> R<Fx> {
    if x.is_neg() || x.is_zero() {
        return Err("log of a number that is not positive (complex values are not computed to many digits yet)".into());
    }
    let p = x.p;
    // x = m * 2^k with m in [1, 2); ln x = k ln 2 + 2 atanh((m - 1)/(m + 1))
    let k = x.v.bits() as i64 - 1 - p as i64;
    let m = if k >= 0 {
        x.halve(k as u64)
    } else {
        x.double((-k) as u64)
    };
    let one = Fx::int(1, p);
    let t = m.sub(&one).div(&m.add(&one))?;
    let base = atanh_small(&t).double(1);
    let l2 = ln2(p);
    Ok(base.add(&Fx {
        v: l2.v * BigInt::from(k),
        p,
    }))
}

/// sin and cos together, after reducing x to [-pi, pi]
fn sin_cos(x: &Fx) -> R<(Fx, Fx)> {
    let p = x.p;
    if x.to_f64().abs() > 1e12 {
        return Err("sin and cos of very large numbers are not computed to many digits".into());
    }
    let pi = pi(p);
    let two_pi = pi.double(1);
    let k = (x.to_rat() / two_pi.to_rat()).round().to_integer();
    let y = x.sub(&Fx {
        v: &two_pi.v * k,
        p,
    });
    // halve 8 times, then double angles back up
    let h = 8;
    let z = y.halve(h);
    let z2 = z.mul(&z);
    let s = series(p, z.clone(), |n, t| {
        Ok(t.mul(&z2)
            .div(&Fx::int(((2 * n) * (2 * n + 1)) as i64, p))?
            .neg())
    })?;
    let c = series(p, Fx::int(1, p), |n, t| {
        Ok(t.mul(&z2)
            .div(&Fx::int(((2 * n - 1) * (2 * n)) as i64, p))?
            .neg())
    })?;
    let (mut s, mut c) = (s, c);
    for _ in 0..h {
        let s2 = s.mul(&c).double(1);
        let c2 = c.mul(&c).sub(&s.mul(&s));
        s = s2;
        c = c2;
    }
    Ok((s, c))
}

fn atan(x: &Fx) -> R<Fx> {
    let p = x.p;
    let one = Fx::int(1, p);
    if x.is_neg() {
        return Ok(atan(&x.neg())?.neg());
    }
    if x.v > one.v {
        // atan x = pi/2 - atan(1/x)
        return Ok(pi(p).halve(1).sub(&atan(&one.div(x)?)?));
    }
    // halve the angle: atan x = 2 atan(x / (1 + sqrt(1 + x^2)))
    let mut y = x.clone();
    let mut k = 0;
    for _ in 0..8 {
        y = y.div(&one.add(&one.add(&y.mul(&y)).sqrt()?))?;
        k += 1;
    }
    let y2 = y.mul(&y);
    let mut sum = y.clone();
    let mut pow = y.clone();
    let mut n: i64 = 1;
    loop {
        pow = pow.mul(&y2).neg();
        if pow.is_zero() {
            break;
        }
        sum = sum.add(&Fx {
            v: &pow.v / BigInt::from(2 * n + 1),
            p,
        });
        n += 1;
    }
    Ok(sum.double(k))
}

fn value(e: &Expr, p: u64) -> R<Fx> {
    let one = || Fx::int(1, p);
    Ok(match e {
        Expr::Num(n) => match n.rational() {
            Some(r) => Fx::from_rat(&r, p),
            None => {
                // a float: its exact binary value
                let f = n.to_f64();
                let r = BigRational::from_float(f).ok_or("not a finite number")?;
                Fx::from_rat(&r, p)
            }
        },
        Expr::Const(Const::Pi) => pi(p),
        Expr::Const(Const::E) => exp(&one())?,
        Expr::Const(Const::I) => {
            return Err("complex values are not computed to many digits yet".into());
        }
        Expr::Add(v) => {
            let mut s = Fx::int(0, p);
            for t in v {
                s = s.add(&value(t, p)?);
            }
            s
        }
        Expr::Mul(v) => {
            let mut s = one();
            for t in v {
                s = s.mul(&value(t, p)?);
            }
            s
        }
        Expr::Pow(b, x) => {
            if let Some(n) = x.num().filter(|n| n.is_exact()) {
                if let Some(k) = n.as_int() {
                    let base = value(b, p)?;
                    let mut acc = one();
                    let mut sq = base;
                    let mut m = k.unsigned_abs();
                    while m > 0 {
                        if m & 1 == 1 {
                            acc = acc.mul(&sq);
                        }
                        sq = sq.mul(&sq);
                        m >>= 1;
                    }
                    return if k < 0 { one().div(&acc) } else { Ok(acc) };
                }
                if n == Number::rat(1, 2).unwrap() {
                    return value(b, p)?.sqrt();
                }
                if n == Number::rat(-1, 2).unwrap() {
                    return one().div(&value(b, p)?.sqrt()?);
                }
            }
            if **b == Expr::Const(Const::E) {
                return exp(&value(x, p)?);
            }
            let base = value(b, p)?;
            exp(&ln(&base)?.mul(&value(x, p)?))?
        }
        Expr::Func(f, a) => {
            let x = value(a, p)?;
            match f {
                Func::Sin => sin_cos(&x)?.0,
                Func::Cos => sin_cos(&x)?.1,
                Func::Tan => {
                    let (s, c) = sin_cos(&x)?;
                    s.div(&c)?
                }
                Func::Atan => atan(&x)?,
                Func::Asin | Func::Acos => {
                    let o = one();
                    if x.v.abs() > o.v {
                        return Err(format!(
                            "{}: outside [-1, 1] (complex values are not computed to many digits yet)",
                            f.name()
                        ));
                    }
                    let c = o.sub(&x.mul(&x)).sqrt()?;
                    let asin = if c.is_zero() {
                        let h = pi(p).halve(1);
                        if x.is_neg() { h.neg() } else { h }
                    } else {
                        atan(&x.div(&c)?)?
                    };
                    if *f == Func::Asin {
                        asin
                    } else {
                        pi(p).halve(1).sub(&asin)
                    }
                }
                Func::Log => ln(&x)?,
                Func::Abs => {
                    if x.is_neg() {
                        x.neg()
                    } else {
                        x
                    }
                }
                Func::Sinh | Func::Cosh | Func::Tanh => {
                    let a = exp(&x)?;
                    let b = one().div(&a)?;
                    match f {
                        Func::Sinh => a.sub(&b).halve(1),
                        Func::Cosh => a.add(&b).halve(1),
                        _ => a.sub(&b).div(&a.add(&b))?,
                    }
                }
                Func::Asinh => ln(&x.add(&x.mul(&x).add(&one()).sqrt()?))?,
                Func::Acosh => ln(&x.add(&x.mul(&x).sub(&one()).sqrt()?))?,
                Func::Atanh => ln(&one().add(&x).div(&one().sub(&x))?)?.halve(1),
            }
        }
        Expr::Sym(s) => return Err(format!("{} has no value", s)),
        Expr::Apply(f, _) => {
            return Err(format!("{} is not computed to many digits yet", f));
        }
        other => return Err(format!("{} is not a number", crate::print::show(other))),
    })
}

/// The value of `e` to `digits` significant digits, as a many-digit decimal
pub fn eval_digits(e: &Expr, digits: u32) -> R<Expr> {
    if digits == 0 || digits > 100_000 {
        return Err("eval(x, digits): digits from 1 to 100000".into());
    }
    if let Expr::Matrix(m) = e {
        let mut out = vec![];
        for r in m {
            let mut row = vec![];
            for x in r {
                row.push(eval_digits(x, digits)?);
            }
            out.push(row);
        }
        return Ok(Expr::Matrix(out));
    }
    let mut bits = (digits as f64 * std::f64::consts::LOG2_10) as u64 + 64;
    let mut prev: Option<BigRational> = None;
    let mut zeros = 0;
    for _ in 0..6 {
        let v = value(e, bits)?.to_rat();
        // fixed point loses digits near 0: a value below 2^-(bits/2) counts as 0
        // when it stays that small as the precision grows
        if v.abs() < BigRational::new(BigInt::one(), BigInt::one() << (bits / 2)) {
            zeros += 1;
            if zeros == 2 {
                return Ok(Expr::Num(num::dec(BigRational::zero(), digits)));
            }
            bits = bits * 3 / 2 + 64;
            continue;
        }
        zeros = 0;
        let r = num::round_sig(&v, digits);
        if let Some(q) = &prev {
            if *q == r || v.is_zero() {
                return Ok(Expr::Num(num::dec(r, digits)));
            }
        }
        prev = Some(r);
        bits = bits * 3 / 2 + 64;
    }
    // the value is 0, or it does not settle (cancellation): give the last result
    let r = prev.unwrap_or_else(BigRational::zero);
    Ok(Expr::Num(num::dec(r, digits)))
}
