//! Numbers: exact rationals, with floating point as the inexact fallback.
//!
//! Integer literals stay exact (`1/3 + 2/6` is `2/3`). Exact numbers have no
//! size limit: small ones use i128, larger ones switch to big integers
//! automatically and never become floats.
//!
//! Decimal literals are exact decimals (Xd): `0.1` is exactly 1/10, so
//! `0.1 + 0.2 - 0.3` is 0. An exact decimal is an exact number that prints
//! as a decimal: everything stays exact until eval (sqrt(0.5) is sqrt(2)/2,
//! sin(0.5) stays sin(0.5)). A result without a finite decimal form is an
//! exact fraction (0.1/3 is 1/30). The setting `exactdigits` (no limit by
//! default) can turn decimals with more digits than that in their numerator
//! or denominator into 64-bit floats, and `exactdigits = 0` makes every
//! decimal a float. Floats are contagious: anything combined with a float
//! is a float.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

/// Results with more bits than this stay symbolic (2^(10^9) is not expanded).
pub const MAX_BITS: u64 = 1 << 22;

thread_local! {
    static EXACT_DIGITS: std::cell::Cell<u32> = const { std::cell::Cell::new(NO_LIMIT) };
}

/// `exactdigits = inf`, the default: decimals are never turned into floats
pub const NO_LIMIT: u32 = u32::MAX;

/// How many digits the numerator and denominator of an exact decimal may
/// have before it becomes a float (0: decimals are floats)
pub fn exact_digits() -> u32 {
    EXACT_DIGITS.with(|c| c.get())
}

/// Sets `exactdigits` (NO_LIMIT for no limit)
pub fn set_exact_digits(d: u32) {
    EXACT_DIGITS.with(|c| c.set(d));
}

/// 10^k as i128 (k <= 38)
fn ten_i128(k: u32) -> i128 {
    10i128.pow(k)
}

/// whether n/d (lowest terms) has a finite decimal form: d = 2^a 5^b
fn terminating(mut d: i128) -> bool {
    while d % 2 == 0 {
        d /= 2;
    }
    while d % 5 == 0 {
        d /= 5;
    }
    d == 1
}

/// n/d (d > 0, in lowest terms) as an exact decimal; an exact fraction when
/// it has no finite decimal form (0.1/3 is 1/30), and a float when it has
/// more digits than `exactdigits` allows
fn xd(n: i128, d: i128) -> Number {
    let k = exact_digits();
    if k == 0 {
        return Float(rat_f64(n, d));
    }
    if !terminating(d) {
        return Rat(n, d);
    }
    if k <= 38 {
        let lim = ten_i128(k);
        if n.unsigned_abs() >= lim as u128 || d >= lim {
            return Float(rat_f64(n, d));
        }
    }
    Xd(n, d)
}

/// an exact rational as an exact decimal (or a float when too large)
fn xd_big(r: BigRational) -> Number {
    if let (Some(n), Some(d)) = (r.numer().to_i128(), r.denom().to_i128()) {
        return xd(n, d);
    }
    let k = exact_digits();
    if k == 0 {
        return Float(big_f64(&r));
    }
    if !big_terminating(r.denom()) {
        return norm(r);
    }
    if k != NO_LIMIT {
        let lim = ten_pow(k);
        if r.numer().abs() >= lim || *r.denom() >= lim {
            return Float(big_f64(&r));
        }
    }
    XdBig(Rc::new(r))
}

fn big_terminating(d: &BigInt) -> bool {
    let mut d = d.clone();
    let (two, five) = (BigInt::from(2), BigInt::from(5));
    while (&d % &two).is_zero() {
        d /= &two;
    }
    while (&d % &five).is_zero() {
        d /= &five;
    }
    d.is_one()
}

/// n/d rounded once to the nearest f64
fn rat_f64(n: i128, d: i128) -> f64 {
    const M: u128 = 1 << 53;
    if n.unsigned_abs() <= M && d as u128 <= M {
        n as f64 / d as f64
    } else {
        big_f64(&BigRational::new_raw(BigInt::from(n), BigInt::from(d)))
    }
}

fn big_f64(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or_else(|| {
        // too large for f64
        let s = if r.is_negative() { -1.0 } else { 1.0 };
        if r.numer().bits() > r.denom().bits() {
            s * f64::INFINITY
        } else {
            0.0
        }
    })
}

/// a + b, a * b with i128 n/d pairs in lowest terms (None on overflow)
fn rat_add(a: i128, b: i128, c: i128, d: i128) -> Option<(i128, i128)> {
    let g = gcd(b, d);
    let l = (b / g).checked_mul(d)?;
    let n = a.checked_mul(l / b)?.checked_add(c.checked_mul(l / d)?)?;
    let g2 = gcd(n, l).max(1);
    Some((n / g2, l / g2))
}
fn rat_mul(a: i128, b: i128, c: i128, d: i128) -> Option<(i128, i128)> {
    let g1 = gcd(a, d).max(1);
    let g2 = gcd(c, b).max(1);
    let n = (a / g1).checked_mul(c / g2)?;
    let m = (b / g2).checked_mul(d / g1)?;
    Some((n, m))
}

#[derive(Clone, Debug)]
pub enum Number {
    /// numerator, denominator; denominator > 0 and gcd == 1
    Rat(i128, i128),
    /// an exact rational that does not fit in Rat (never one that does)
    Big(Rc<BigRational>),
    Float(f64),
    /// a decimal with many digits, from eval(x, digits) or a long decimal
    /// literal: the value rounded to that many significant digits. Inexact,
    /// like a float; arithmetic keeps the smaller number of digits.
    Dec(Rc<BigRational>, u32),
    /// an exact decimal: a decimal literal, or + - * / of them, holding the
    /// exact value numerator/denominator (denominator > 0, lowest terms).
    /// Inexact as far as symbolic work goes (it prints as a decimal and
    /// functions give decimal values), but its arithmetic is exact.
    Xd(i128, i128),
    /// an exact decimal too large for i128
    XdBig(Rc<BigRational>),
}

use Number::*;

fn gcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a
}

/// 10^k
fn ten_pow(k: u32) -> BigInt {
    num_traits::pow(BigInt::from(10), k as usize)
}

/// floor(log10(|r|)) for r != 0
fn log10_floor(r: &BigRational) -> i64 {
    let a = r.abs();
    // estimate from bit lengths, then correct
    let bits = a.numer().bits() as i64 - a.denom().bits() as i64;
    let mut e = ((bits as f64) * std::f64::consts::LOG10_2).floor() as i64 - 1;
    let ten = BigRational::from_integer(BigInt::from(10));
    let pw = |k: i64| -> BigRational {
        if k >= 0 {
            BigRational::from_integer(ten_pow(k as u32))
        } else {
            BigRational::new(BigInt::from(1), ten_pow((-k) as u32))
        }
    };
    let mut p = pw(e);
    while p > a {
        e -= 1;
        p = p / ten.clone();
    }
    loop {
        let next = p.clone() * ten.clone();
        if next > a {
            return e;
        }
        e += 1;
        p = next;
    }
}

/// r rounded to `digits` significant decimal digits
pub fn round_sig(r: &BigRational, digits: u32) -> BigRational {
    if r.is_zero() {
        return r.clone();
    }
    let e = log10_floor(r);
    let k = digits as i64 - 1 - e;
    let scale = if k >= 0 {
        BigRational::from_integer(ten_pow(k as u32))
    } else {
        BigRational::new(BigInt::from(1), ten_pow((-k) as u32))
    };
    (r * scale.clone()).round() / scale
}

/// a value as a many-digit decimal
pub fn dec(r: BigRational, digits: u32) -> Number {
    let digits = digits.clamp(1, 100_000);
    Dec(Rc::new(round_sig(&r, digits)), digits)
}

/// the text of a many-digit decimal: all its digits, as a literal that reads back
fn fmt_dec(r: &BigRational, digits: u32) -> String {
    if r.is_zero() {
        return format!("0.{}", "0".repeat(digits.max(1) as usize));
    }
    let mut e = log10_floor(r);
    let k = digits as i64 - 1 - e;
    let scale = |k: i64| {
        if k >= 0 {
            BigRational::from_integer(ten_pow(k as u32))
        } else {
            BigRational::new(BigInt::from(1), ten_pow((-k) as u32))
        }
    };
    let mut m = (r.abs() * scale(k)).round().to_integer().to_string();
    if m.len() > digits as usize {
        // rounding carried into a new digit (9.99 -> 10.0)
        m.truncate(digits as usize);
        e += 1;
    }
    let sign = if r.is_negative() { "-" } else { "" };
    let d = digits as i64;
    if (-6..d.max(1).min(40)).contains(&e) || (e >= 0 && e < d) {
        if e >= 0 {
            let (a, b) = m.split_at((e + 1) as usize);
            if b.is_empty() {
                format!("{}{}.0", sign, a)
            } else {
                format!("{}{}.{}", sign, a, b)
            }
        } else {
            format!("{}0.{}{}", sign, "0".repeat((-e - 1) as usize), m)
        }
    } else {
        let (a, b) = m.split_at(1);
        format!("{}{}.{}e{}", sign, a, if b.is_empty() { "0" } else { b }, e)
    }
}

/// a decimal literal with more significant digits than a float holds: its
/// exact value and digit count
fn long_decimal(t: &str) -> Option<(BigRational, u32)> {
    let (mant, exp) = match t.find(['e', 'E']) {
        Some(p) => (&t[..p], t[p + 1..].parse::<i64>().ok()?),
        None => (t, 0),
    };
    let (neg, mant) = match mant.strip_prefix('-') {
        Some(m) => (true, m),
        None => (false, mant),
    };
    let (ip, fp) = match mant.find('.') {
        Some(p) => (&mant[..p], &mant[p + 1..]),
        None => return None,
    };
    if !(ip.chars().all(|c| c.is_ascii_digit()) && fp.chars().all(|c| c.is_ascii_digit())) {
        return None;
    }
    let all = format!("{}{}", ip, fp);
    let sig = all.trim_start_matches('0').len();
    if sig <= 17 {
        return None;
    }
    let n: BigInt = all.parse().ok()?;
    let n = if neg { -n } else { n };
    let shift = exp - fp.len() as i64;
    if shift.unsigned_abs() > 1_000_000 {
        return None;
    }
    let r = if shift >= 0 {
        BigRational::from_integer(n * ten_pow(shift as u32))
    } else {
        BigRational::new(n, ten_pow((-shift) as u32))
    };
    Some((r, sig as u32))
}

/// the exact value of a decimal literal like 0.1, 2.5e-3 or 1e20
pub fn decimal_value(t: &str) -> Option<BigRational> {
    let (mant, exp) = match t.find(['e', 'E']) {
        Some(p) => (&t[..p], t[p + 1..].trim_start_matches('+').parse::<i64>().ok()?),
        None => (t, 0),
    };
    let (neg, mant) = match mant.strip_prefix('-') {
        Some(m) => (true, m),
        None => (false, mant),
    };
    let (ip, fp) = match mant.find('.') {
        Some(p) => (&mant[..p], &mant[p + 1..]),
        None => (mant, ""),
    };
    if ip.is_empty() && fp.is_empty() {
        return None;
    }
    if !(ip.chars().all(|c| c.is_ascii_digit()) && fp.chars().all(|c| c.is_ascii_digit())) {
        return None;
    }
    let all = format!("{}{}", ip, fp);
    let n: BigInt = if all.is_empty() { BigInt::zero() } else { all.parse().ok()? };
    let n = if neg { -n } else { n };
    let shift = exp - fp.len() as i64;
    if shift.unsigned_abs() > 400 {
        return None;
    }
    Some(if shift >= 0 {
        BigRational::from_integer(n * ten_pow(shift as u32))
    } else {
        BigRational::new(n, ten_pow((-shift) as u32))
    })
}

/// a normalized big rational as a Number, small when it fits
fn norm(r: BigRational) -> Number {
    match (r.numer().to_i128(), r.denom().to_i128()) {
        (Some(n), Some(d)) => Rat(n, d),
        _ => Big(Rc::new(r)),
    }
}

impl Number {
    pub fn int(n: i128) -> Number {
        Rat(n, 1)
    }
    pub fn from_bigint(n: BigInt) -> Number {
        norm(BigRational::from_integer(n))
    }
    pub fn from_big(r: BigRational) -> Number {
        norm(r)
    }
    pub fn zero() -> Number {
        Rat(0, 1)
    }
    pub fn one() -> Number {
        Rat(1, 1)
    }
    pub fn rat(n: i128, d: i128) -> Option<Number> {
        if d == 0 {
            return None;
        }
        let g = gcd(n, d);
        let (n, d) = (n / g, d / g);
        if d < 0 {
            return match (n.checked_neg(), d.checked_neg()) {
                (Some(n), Some(d)) => Some(Rat(n, d)),
                _ => Some(norm(BigRational::new(n.into(), d.into()))),
            };
        }
        Some(Rat(n, d))
    }
    /// Parse a numeric literal: integers are exact (of any length), decimals are floats.
    pub fn parse(s: &str) -> Option<Number> {
        let t = s.trim_end_matches(['f', 'F', 'd', 'D']);
        if !t.is_empty() && t.chars().all(|c| c.is_ascii_digit() || c == '-') {
            if let Ok(n) = t.parse::<i128>() {
                return Some(Rat(n, 1));
            }
            if let Ok(n) = t.parse::<BigInt>() {
                return Some(Number::from_bigint(n));
            }
        }
        if exact_digits() > 0 {
            if let Some(r) = decimal_value(t) {
                return Some(xd_big(r));
            }
        }
        if let Some((r, d)) = long_decimal(t) {
            return Some(Dec(Rc::new(r), d));
        }
        t.parse::<f64>().ok().map(Float)
    }

    /// the digits of a many-digit decimal (None for other numbers)
    pub fn digits(&self) -> Option<u32> {
        if let Dec(_, d) = self { Some(*d) } else { None }
    }

    /// the value as a big rational, for exact numbers and many-digit decimals
    pub fn rational(&self) -> Option<BigRational> {
        match self {
            Dec(r, _) => Some((**r).clone()),
            _ => self.big(),
        }
    }

    /// whether this is an exact decimal
    pub fn is_xd(&self) -> bool {
        matches!(self, Xd(..) | XdBig(_))
    }

    /// an exact decimal as the plain exact number with its value (others unchanged)
    pub fn to_exact(&self) -> Number {
        match self {
            Xd(n, d) => Rat(*n, *d),
            XdBig(r) => norm((**r).clone()),
            _ => self.clone(),
        }
    }

    /// the value of a float: numeric functions evaluate these directly
    pub fn decimal(&self) -> Option<f64> {
        match self {
            Float(f) => Some(*f),
            _ => None,
        }
    }

    /// an exact decimal with the same value as this exact number (others unchanged)
    pub fn to_xd(&self) -> Number {
        match self {
            Rat(n, d) => xd(*n, *d),
            Big(r) => xd_big((**r).clone()),
            _ => self.clone(),
        }
    }

    /// arithmetic where a many-digit decimal takes part: Some(result) when
    /// neither side is a float (then the result keeps the smaller digit count)
    fn dec_op(&self, o: &Number, f: impl Fn(BigRational, BigRational) -> Option<BigRational>) -> Option<Option<Number>> {
        let d = match (self, o) {
            (Dec(_, a), Dec(_, b)) => (*a).min(*b),
            (Dec(_, a), x) | (x, Dec(_, a)) if x.is_exact() => *a,
            _ => return None,
        };
        Some(f(self.rational()?, o.rational()?).map(|r| dec(r, d)))
    }

    /// arithmetic where an exact decimal takes part and the other side is an
    /// exact number or exact decimal: the exact result, as an exact decimal
    /// (or a float when it has too many digits)
    fn xd_op(
        &self,
        o: &Number,
        small: fn(i128, i128, i128, i128) -> Option<(i128, i128)>,
        big: fn(BigRational, BigRational) -> BigRational,
    ) -> Option<Number> {
        if !(self.is_xd() || o.is_xd()) || !self.is_exact() || !o.is_exact() {
            return None;
        }
        let pair = |n: &Number| match n {
            Rat(a, b) | Xd(a, b) => Some((*a, *b)),
            _ => None,
        };
        if let (Some((a, b)), Some((c, d))) = (pair(self), pair(o)) {
            if let Some((n, m)) = small(a, b, c, d) {
                return Some(xd(n, m));
            }
        }
        Some(xd_big(big(self.big()?, o.big()?)))
    }

    /// the exact value as a big rational (None for floats)
    pub fn big(&self) -> Option<BigRational> {
        match self {
            Rat(n, d) => Some(BigRational::new_raw(BigInt::from(*n), BigInt::from(*d))),
            Big(r) => Some((**r).clone()),
            Xd(n, d) => Some(BigRational::new_raw(BigInt::from(*n), BigInt::from(*d))),
            XdBig(r) => Some((**r).clone()),
            Float(_) | Dec(..) => None,
        }
    }
    /// the exact integer value, of any size
    pub fn as_bigint(&self) -> Option<BigInt> {
        match self {
            Rat(n, 1) | Xd(n, 1) => Some(BigInt::from(*n)),
            Big(r) | XdBig(r) if r.is_integer() => Some(r.numer().clone()),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            Rat(n, d) => *n as f64 / *d as f64,
            Big(r) => r.to_f64().unwrap_or_else(|| {
                // too large for f64
                let s = if r.is_negative() { -1.0 } else { 1.0 };
                if r.numer().bits() > r.denom().bits() {
                    s * f64::INFINITY
                } else {
                    0.0
                }
            }),
            Float(f) => *f,
            Dec(r, _) => Big(r.clone()).to_f64(),
            Xd(n, d) => rat_f64(*n, *d),
            XdBig(r) => big_f64(r),
        }
    }
    pub fn is_exact(&self) -> bool {
        !matches!(self, Float(_) | Dec(..))
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Rat(n, _) => *n == 0,
            Big(_) | XdBig(_) => false,
            Float(f) => *f == 0.0,
            Dec(r, _) => r.is_zero(),
            Xd(n, _) => *n == 0,
        }
    }
    pub fn is_one(&self) -> bool {
        match self {
            Rat(n, d) => *n == 1 && *d == 1,
            Big(_) | XdBig(_) => false,
            Float(f) => *f == 1.0,
            Dec(r, _) => r.is_one(),
            Xd(n, d) => *n == 1 && *d == 1,
        }
    }
    pub fn is_minus_one(&self) -> bool {
        match self {
            Rat(n, d) => *n == -1 && *d == 1,
            Big(_) | XdBig(_) => false,
            Float(f) => *f == -1.0,
            Dec(r, _) => (-(**r).clone()).is_one(),
            Xd(n, d) => *n == -1 && *d == 1,
        }
    }
    pub fn is_negative(&self) -> bool {
        match self {
            Rat(n, _) | Xd(n, _) => *n < 0,
            Big(r) | Dec(r, _) | XdBig(r) => r.is_negative(),
            Float(f) => *f < 0.0,
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            Rat(_, d) | Xd(_, d) => *d == 1,
            Big(r) | Dec(r, _) | XdBig(r) => r.is_integer(),
            Float(f) => f.fract() == 0.0 && f.is_finite(),
        }
    }
    /// Exact integer value, if this is an exact integer that fits in i128
    pub fn as_int(&self) -> Option<i128> {
        match self {
            Rat(n, 1) | Xd(n, 1) => Some(*n),
            _ => None,
        }
    }
    pub fn numer(&self) -> Option<i128> {
        if let Rat(n, _) | Xd(n, _) = self { Some(*n) } else { None }
    }
    pub fn denom(&self) -> Option<i128> {
        if let Rat(_, d) | Xd(_, d) = self { Some(*d) } else { None }
    }

    pub fn neg(&self) -> Number {
        match self {
            Rat(n, d) => n
                .checked_neg()
                .map(|n| Rat(n, *d))
                .unwrap_or_else(|| norm(-self.big().unwrap())),
            Big(r) => norm(-(**r).clone()),
            Float(f) => Float(-f),
            Dec(r, d) => Dec(Rc::new(-(**r).clone()), *d),
            Xd(n, d) => match n.checked_neg() {
                Some(m) => Xd(m, *d),
                None => xd_big(-BigRational::new_raw(BigInt::from(*n), BigInt::from(*d))),
            },
            XdBig(r) => xd_big(-(**r).clone()),
        }
    }
    pub fn abs(&self) -> Number {
        if self.is_negative() {
            self.neg()
        } else {
            self.clone()
        }
    }

    pub fn add(&self, o: impl Borrow<Number>) -> Number {
        let o = o.borrow();
        if let Some(Some(r)) = self.dec_op(o, |x, y| Some(x + y)) {
            return r;
        }
        if let Some(r) = self.xd_op(o, rat_add, |x, y| x + y) {
            return r;
        }
        if let (Rat(a, b), Rat(c, d)) = (self, o) {
            let (a, b, c, d) = (*a, *b, *c, *d);
            let r = (|| {
                let g = gcd(b, d);
                let l = (b / g).checked_mul(d)?;
                let n = a.checked_mul(l / b)?.checked_add(c.checked_mul(l / d)?)?;
                Number::rat(n, l)
            })();
            if let Some(r) = r {
                return r;
            }
        }
        match (self.big(), o.big()) {
            (Some(x), Some(y)) => norm(x + y),
            _ => Float(self.to_f64() + o.to_f64()),
        }
    }
    pub fn sub(&self, o: impl Borrow<Number>) -> Number {
        self.add(o.borrow().neg())
    }
    pub fn mul(&self, o: impl Borrow<Number>) -> Number {
        let o = o.borrow();
        if let Some(Some(r)) = self.dec_op(o, |x, y| Some(x * y)) {
            return r;
        }
        if let Some(r) = self.xd_op(o, rat_mul, |x, y| x * y) {
            return r;
        }
        if let (Rat(a, b), Rat(c, d)) = (self, o) {
            let (a, b, c, d) = (*a, *b, *c, *d);
            let g1 = gcd(a, d).max(1);
            let g2 = gcd(c, b).max(1);
            let r =
                (|| Number::rat((a / g1).checked_mul(c / g2)?, (b / g2).checked_mul(d / g1)?))();
            if let Some(r) = r {
                return r;
            }
        }
        match (self.big(), o.big()) {
            (Some(x), Some(y)) => norm(x * y),
            _ => Float(self.to_f64() * o.to_f64()),
        }
    }
    pub fn recip(&self) -> Option<Number> {
        match self {
            Rat(0, _) => None,
            Rat(n, d) => Number::rat(*d, *n),
            Big(r) => Some(norm(r.recip())),
            Float(f) => Some(Float(1.0 / f)),
            Dec(r, _) if r.is_zero() => None,
            Dec(r, d) => Some(dec(r.recip(), *d)),
            Xd(0, _) => None,
            Xd(n, d) => Some(if *n < 0 {
                match (d.checked_neg(), n.checked_neg()) {
                    (Some(a), Some(b)) => xd(a, b),
                    _ => xd_big(BigRational::new_raw(BigInt::from(*d), BigInt::from(*n))),
                }
            } else {
                xd(*d, *n)
            }),
            XdBig(r) => Some(xd_big(r.recip())),
        }
    }
    pub fn div(&self, o: impl Borrow<Number>) -> Option<Number> {
        Some(self.mul(o.borrow().recip()?))
    }

    /// self^e when the result is a plain number. Returns None when an exact
    /// result does not exist (e.g. 2^(1/2)) or would be gigantic, so the
    /// caller keeps it symbolic.
    pub fn pow(&self, e: impl Borrow<Number>) -> Option<Number> {
        let e = e.borrow();
        if let (Dec(r, d), true) = (self, e.is_exact() && e.is_integer()) {
            let k = e.as_int()?;
            if k.unsigned_abs() > 100_000 || (r.is_zero() && k < 0) {
                return None;
            }
            let p = num_traits::pow(r.abs(), k.unsigned_abs() as usize);
            let p = if r.is_negative() && k % 2 != 0 { -p } else { p };
            return Some(dec(if k < 0 { p.recip() } else { p }, *d));
        }
        // an exact decimal to an exact power: the exact result, as a decimal
        if self.is_xd() || e.is_xd() {
            if !self.is_exact() || !e.is_exact() {
                let r = self.to_f64().powf(e.to_f64());
                return if r.is_nan() { None } else { Some(Float(r)) };
            }
            let r = self.to_exact().pow(e.to_exact())?;
            return Some(if self.is_xd() { r.to_xd() } else { r });
        }
        if !self.is_exact() || !e.is_exact() {
            let r = self.to_f64().powf(e.to_f64());
            return if r.is_nan() { None } else { Some(Float(r)) };
        }
        let eb = e.big()?;
        if eb.is_integer() {
            // 0, 1 and -1 to any whole power
            if self.is_zero() {
                return if eb.is_negative() {
                    None
                } else if eb.is_zero() {
                    Some(Number::one())
                } else {
                    Some(Number::zero())
                };
            }
            if self.is_one() {
                return Some(Number::one());
            }
            if self.is_minus_one() {
                let odd = (eb.numer() % BigInt::from(2)).is_zero();
                return Some(if odd { Number::one() } else { Number::int(-1) });
            }
            if eb.is_negative() {
                return self.recip()?.pow(e.neg());
            }
            let p = eb.numer().to_u32()?;
            if let Rat(n, d) = self {
                if let (Some(a), Some(b)) = (n.checked_pow(p), d.checked_pow(p)) {
                    return Number::rat(a, b);
                }
            }
            let b = self.big()?;
            let bits = b.numer().bits().max(b.denom().bits());
            if bits.saturating_mul(p as u64) > MAX_BITS {
                return None;
            }
            return Some(norm(BigRational::new_raw(
                b.numer().pow(p),
                b.denom().pow(p),
            )));
        }
        // rational exponent p/q: exact if numerator and denominator are perfect q-th powers
        if self.is_negative() {
            return None;
        }
        let q = eb.denom().to_u32()?;
        let b = self.big()?;
        let rn = b.numer().nth_root(q);
        let rd = b.denom().nth_root(q);
        if rn.pow(q) != *b.numer() || rd.pow(q) != *b.denom() {
            return None;
        }
        norm(BigRational::new_raw(rn, rd)).pow(Number::from_bigint(eb.numer().clone()))
    }

    pub fn cmp_value(&self, o: impl Borrow<Number>) -> Ordering {
        let o = o.borrow();
        if let (Rat(a, b), Rat(c, d)) = (self, o) {
            if let (Some(x), Some(y)) = (a.checked_mul(*d), c.checked_mul(*b)) {
                return x.cmp(&y);
            }
        }
        match (self.rational(), o.rational()) {
            (Some(x), Some(y)) if !matches!(self, Float(_)) && !matches!(o, Float(_)) => x.cmp(&y),
            _ => self.to_f64().total_cmp(&o.to_f64()),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, o: &Number) -> bool {
        match (self, o) {
            (Rat(a, b), Rat(c, d)) => a == c && b == d,
            (Big(a), Big(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Dec(a, p), Dec(b, q)) => a == b && p == q,
            (Xd(a, b), Xd(c, d)) => a == c && b == d,
            (XdBig(a), XdBig(b)) => a == b,
            _ => false, // 1 and 1.0 are different expressions
        }
    }
}

pub fn fmt_float(f: f64) -> String {
    if f.is_nan() {
        return "NaN".into();
    }
    if f.is_infinite() {
        return if f > 0.0 { "Inf".into() } else { "-Inf".into() };
    }
    let a = f.abs();
    if a != 0.0 && !(1e-5..1e16).contains(&a) {
        let s = format!("{:e}", f);
        return s;
    }
    let s = format!("{}", f);
    if s.contains('.') || s.contains('e') {
        s
    } else {
        s + ".0"
    }
}

/// An exact decimal as text: the shortest float text when that reads back as
/// the same value (0.1, 1e-12, 2.5), all its digits when it is a terminating
/// decimal with more digits than a float shows, and the nearest float's text
/// otherwise (1/30 as 0.03333333333333333)
fn fmt_xd(r: &BigRational) -> String {
    let f = big_f64(r);
    let s = fmt_float(f);
    if f.is_finite() && decimal_value(&s).is_some_and(|v| v == *r) {
        return s;
    }
    // terminating: d = 2^a 5^b
    let (two, five) = (BigInt::from(2), BigInt::from(5));
    let mut m = r.denom().clone();
    let (mut twos, mut fives) = (0u32, 0u32);
    while (&m % &two).is_zero() {
        m /= &two;
        twos += 1;
    }
    while (&m % &five).is_zero() {
        m /= &five;
        fives += 1;
    }
    if !m.is_one() {
        return format!("{}/{}", r.numer(), r.denom());
    }
    let k = twos.max(fives);
    // n/d = n * (10^k/d) / 10^k
    let scaled = r.numer() * (ten_pow(k) / r.denom());
    let neg = scaled.is_negative();
    let digits = scaled.abs().to_string();
    let k = k as usize;
    let (ip, fp) = if digits.len() > k {
        (digits[..digits.len() - k].to_string(), digits[digits.len() - k..].to_string())
    } else {
        ("0".to_string(), format!("{}{}", "0".repeat(k - digits.len()), digits))
    };
    let fp = fp.trim_end_matches('0');
    format!("{}{}.{}", if neg { "-" } else { "" }, ip, if fp.is_empty() { "0" } else { fp })
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rat(n, 1) => write!(f, "{}", n),
            Rat(n, d) => write!(f, "{}/{}", n, d),
            Big(r) if r.is_integer() => write!(f, "{}", r.numer()),
            Big(r) => write!(f, "{}/{}", r.numer(), r.denom()),
            Float(x) => write!(f, "{}", fmt_float(*x)),
            Dec(r, d) => write!(f, "{}", fmt_dec(r, *d)),
            Xd(n, d) => write!(f, "{}", fmt_xd(&BigRational::new_raw(BigInt::from(*n), BigInt::from(*d)))),
            XdBig(r) => write!(f, "{}", fmt_xd(r)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rational_arithmetic() {
        let a = Number::rat(1, 3).unwrap();
        let b = Number::rat(2, 6).unwrap();
        assert_eq!(a.add(b), Number::rat(2, 3).unwrap());
        assert_eq!(
            Number::int(-12).div(Number::int(3)).unwrap(),
            Number::int(-4)
        );
        assert_eq!(
            Number::rat(4, 9).unwrap().pow(Number::rat(1, 2).unwrap()),
            Number::rat(2, 3)
        );
        assert_eq!(Number::int(2).pow(Number::rat(1, 2).unwrap()), None);
        assert_eq!(Number::int(2).pow(Number::int(-2)), Number::rat(1, 4));
        assert_eq!(format!("{}", Number::Float(3.0)), "3.0");
        assert_eq!(format!("{}", Number::rat(-5, 6).unwrap()), "-5/6");
    }
}
