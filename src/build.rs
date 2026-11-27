//! Smart constructors that keep expressions canonical.

use crate::expr::{Const, Expr, Func, cmp, term_cmp};
use crate::num::Number;
use Expr::*;

pub fn add(terms: Vec<Expr>) -> Expr {
    let mut constant = Number::zero();
    let mut groups: Vec<(Expr, Number)> = vec![];
    let mut stack: Vec<Expr> = terms;
    stack.reverse();
    while let Some(t) = stack.pop() {
        match t {
            Add(inner) => stack.extend(inner.into_iter().rev()),
            Num(n) => constant = constant.add(n),
            other => {
                let (c, rest) = other.coeff_and_rest();
                match groups.iter_mut().find(|(r, _)| *r == rest) {
                    Some(g) => g.1 = g.1.add(c),
                    None => groups.push((rest, c)),
                }
            }
        }
    }
    let mut out: Vec<Expr> = groups
        .into_iter()
        .filter(|(_, c)| !c.is_zero())
        .map(|(r, c)| scale(c, r))
        .collect();
    out.sort_by(term_cmp);
    if !constant.is_zero() {
        out.push(Num(constant.clone()));
    }
    match out.len() {
        0 => Num(constant.clone()), // keeps 0.0 a float
        1 => out.pop().unwrap(),
        _ => Add(out),
    }
}

/// c * e without re-simplifying e (e has no numeric coefficient)
fn scale(c: Number, e: Expr) -> Expr {
    if c.is_one() {
        return e;
    }
    match e {
        Mul(mut f) => {
            f.insert(0, Num(c));
            Mul(f)
        }
        Num(n) => Num(n.mul(c)),
        other => Mul(vec![Num(c), other]),
    }
}

/// a + b*i with numbers a, b (b may be 0)
pub fn gauss(e: &Expr) -> Option<(Number, Number)> {
    match e {
        Num(n) => Some((n.clone(), Number::zero())),
        Const(Const::I) => Some((Number::zero(), Number::one())),
        Mul(f) if f.len() == 2 && f[1] == Const(Const::I) => {
            f[0].num().map(|n| (Number::zero(), n))
        }
        Add(t) => {
            let (mut re, mut im) = (Number::zero(), Number::zero());
            for x in t {
                let (a, b) = match x {
                    Add(_) => return None,
                    other => gauss(other)?,
                };
                re = re.add(a);
                im = im.add(b);
            }
            Some((re, im))
        }
        _ => None,
    }
}

pub fn from_gauss(re: Number, im: Number) -> Expr {
    add(vec![Num(re), scale(im, Const(Const::I))])
}

fn gauss_mul(a: (Number, Number), b: (Number, Number)) -> (Number, Number) {
    (
        a.0.mul(&b.0).sub(a.1.mul(&b.1)),
        a.0.mul(&b.1).add(a.1.mul(&b.0)),
    )
}

/// z^n for a Gaussian number z and a (not too large) integer n
fn gauss_pow(z: (Number, Number), n: i128) -> Option<(Number, Number)> {
    let base = if n < 0 {
        let norm = z.0.mul(&z.0).add(z.1.mul(&z.1));
        (z.0.div(&norm)?, z.1.neg().div(&norm)?)
    } else {
        z
    };
    let mut acc = (Number::one(), Number::zero());
    for _ in 0..n.abs() {
        acc = gauss_mul(acc, base.clone());
    }
    Some(acc)
}

/// a Gaussian number that is not real and not just a multiple of i
fn complex_sum(e: &Expr) -> bool {
    matches!(e, Add(_)) && gauss(e).is_some_and(|(_, b)| !b.is_zero())
}

pub fn mul(factors: Vec<Expr>) -> Expr {
    // (2+3*i)*(1-i) = 5+i: complex numbers multiply out
    if factors.iter().any(complex_sum) {
        let (zs, rest): (Vec<Expr>, Vec<Expr>) =
            factors.into_iter().partition(|f| gauss(f).is_some());
        let z = zs.iter().fold((Number::one(), Number::zero()), |acc, f| {
            gauss_mul(acc, gauss(f).unwrap())
        });
        let mut out = rest;
        out.push(from_gauss(z.0, z.1));
        return mul_inner(out);
    }
    mul_inner(factors)
}

fn mul_inner(factors: Vec<Expr>) -> Expr {
    let mut coeff = Number::one();
    let mut groups: Vec<(Expr, Expr)> = vec![];
    let mut stack = factors;
    stack.reverse();
    while let Some(f) = stack.pop() {
        match f {
            Mul(inner) => stack.extend(inner.into_iter().rev()),
            Num(n) => coeff = coeff.mul(n),
            other => {
                let (b, e) = other.base_exp();
                match groups.iter_mut().find(|(gb, _)| *gb == b) {
                    Some(g) => g.1 = add(vec![g.1.clone(), e]),
                    None => groups.push((b, e)),
                }
            }
        }
    }
    if coeff.is_zero() && coeff.is_exact() {
        return Expr::zero();
    }
    combine_sqrts(&mut groups, &mut coeff);
    let mut out = vec![];
    let mut again = false;
    for (b, e) in groups {
        let p = pow(b, e);
        match p {
            Num(n) => coeff = coeff.mul(n),
            Mul(_) => {
                again = true;
                out.push(p)
            }
            p if p.is_one() => {}
            p => out.push(p),
        }
    }
    if again {
        out.push(Num(coeff));
        return mul_inner(out);
    }
    if coeff.is_zero() && coeff.is_exact() {
        return Expr::zero();
    }
    out.sort_by(cmp);
    if out.is_empty() {
        return Num(coeff);
    }
    if !coeff.is_one() {
        out.insert(0, Num(coeff));
    }
    if out.len() == 1 {
        out.pop().unwrap()
    } else {
        Mul(out)
    }
}

/// sqrt(2)*sqrt(6) = sqrt(12) = 2*sqrt(3), sqrt(10)/sqrt(2) = sqrt(5): square roots of
/// positive integers combine when there are two or more of them
fn combine_sqrts(groups: &mut Vec<(Expr, Expr)>, coeff: &mut Number) {
    let half = |e: &Expr| -> Option<i128> {
        let n = e.num()?;
        if n == Number::rat(1, 2)? {
            Some(1)
        } else if n == Number::rat(-1, 2)? {
            Some(-1)
        } else {
            None
        }
    };
    let is_root = |(b, e): &(Expr, Expr)| {
        b.num().is_some_and(|n| n.is_integer() && !n.is_negative() && n.as_int().is_some()) && half(e).is_some()
    };
    let roots = groups.iter().filter(|g| is_root(g)).count();
    if roots == 1 {
        // 6/sqrt(2) = 3*sqrt(2): the denominator divides the coefficient
        let i = groups.iter().position(&is_root).unwrap();
        let n = groups[i].0.num().unwrap().as_int().unwrap();
        if half(&groups[i].1) == Some(-1) && n > 1 && coeff.is_exact() {
            if let (Some(a), Some(b)) = (coeff.numer(), coeff.denom()) {
                if a % n == 0 {
                    *coeff = Number::rat(a / n, b).unwrap();
                    groups[i].1 = Expr::Num(Number::rat(1, 2).unwrap());
                }
            }
        }
        return;
    }
    if roots < 2 {
        return;
    }
    let (mut p, mut q) = (1i128, 1i128);
    for g in groups.iter().filter(|g| is_root(g)) {
        let n = g.0.num().unwrap().as_int().unwrap();
        let ok = if half(&g.1) == Some(1) { p.checked_mul(n).map(|x| p = x) } else { q.checked_mul(n).map(|x| q = x) };
        if ok.is_none() || p > 1 << 60 || q > 1 << 60 {
            return;
        }
    }
    let Some(pq) = p.checked_mul(q) else { return };
    groups.retain(|g| !is_root(g));
    if q != 1 {
        *coeff = coeff.mul(Number::rat(1, q).unwrap());
    }
    groups.push((Expr::int(pq), Expr::Num(Number::rat(1, 2).unwrap())));
}

pub fn pow(b: Expr, e: Expr) -> Expr {
    // an integral float exponent means the same as the exact integer: x^2.0 = x^2
    let e = match e {
        Num(Number::Float(f)) if f.fract() == 0.0 && f.abs() < 9.0e15 => Expr::int(f as i128),
        // an exact decimal exponent is the exact number it is: x^0.5 = x^(1/2)
        Num(n) if n.is_xd() => Num(n.to_exact()),
        other => other,
    };
    if crate::assume::any() {
        // (a^2)^(1/2) = a for a > 0
        if let Pow(bb, e1) = &b {
            if crate::assume::sign(bb) == Some(1) {
                return pow((**bb).clone(), mul2((**e1).clone(), e.clone()));
            }
        }
        // (-1)^(2n) = 1 and (-1)^(3n) = (-1)^n for an integer n
        if b == Expr::int(-1) {
            if let Mul(v) = &e {
                if let Some(c) = v[0].num().filter(|c| c.is_integer()).and_then(|c| c.as_int()) {
                    let rest = mul(v[1..].to_vec());
                    if c.abs() > 1 && crate::assume::is_integer(&rest) {
                        return if c % 2 == 0 { Expr::one() } else { pow(b, rest) };
                    }
                }
            }
        }
    }
    if let Num(en) = &e {
        let en = en.clone();
        if en.is_zero() {
            return Expr::one();
        }
        if en.is_one() {
            return b;
        }
        match &b {
            Num(bn) => {
                if let Some(r) = bn.pow(&en) {
                    return Num(r);
                }
                // an exact decimal works as the exact number it is: sqrt(0.5) = sqrt(2)/2
                if bn.is_xd() || en.is_xd() {
                    return pow(Num(bn.to_exact()), Num(en.to_exact()));
                }
                // pull perfect powers out of roots: 8^(1/2) = 2*2^(1/2)
                if let (&Number::Rat(n, d), &Number::Rat(p, q)) = (bn, &en) {
                    if n > 0 && q > 1 {
                        let (kn, mn) = split_root(n, q);
                        let (kd, md) = split_root(d, q);
                        if kn != 1 || kd != 1 {
                            let k = Num(Number::rat(kn, kd).unwrap());
                            let m = Num(Number::rat(mn, md).unwrap());
                            return mul(vec![pow(k, Expr::int(p)), pow(m, e.clone())]);
                        }
                    }
                }
                // the same for big numbers, with small primes
                if let (Number::Big(r), &Number::Rat(p, q)) = (bn, &en) {
                    if !bn.is_negative() && q > 1 && q <= 64 {
                        let (kn, mn) = split_root_big(r.numer(), q as u32);
                        let (kd, md) = split_root_big(r.denom(), q as u32);
                        if !(kn == 1.into() && kd == 1.into()) {
                            let k = Num(Number::from_big(num_rational::BigRational::new(kn, kd)));
                            let m = Num(Number::from_big(num_rational::BigRational::new(mn, md)));
                            return mul(vec![pow(k, Expr::int(p)), pow(m, e.clone())]);
                        }
                    }
                }
                // (-n)^(p/2) = i^p * n^(p/2)
                if bn.is_negative() && en.is_exact() && en.denom() == Some(2) {
                    let p = en.numer().unwrap();
                    return mul(vec![
                        pow(Const(Const::I), Expr::int(p)),
                        pow(Num(bn.neg()), e.clone()),
                    ]);
                }
            }
            b if en.is_integer()
                && en.is_exact()
                && complex_sum(b)
                && en.as_int().is_some_and(|n| n.abs() <= 64) =>
            {
                if let Some(z) = gauss_pow(gauss(b).unwrap(), en.as_int().unwrap()) {
                    return from_gauss(z.0, z.1);
                }
            }
            Const(Const::I) if en.is_integer() && en.is_exact() => {
                return match en.as_int().unwrap().rem_euclid(4) {
                    0 => Expr::one(),
                    1 => Const(Const::I),
                    2 => Expr::int(-1),
                    _ => mul(vec![Expr::int(-1), Const(Const::I)]),
                };
            }
            Pow(b2, e2) if en.is_integer() => {
                return pow((**b2).clone(), mul(vec![(**e2).clone(), e]));
            }
            Mul(fs) if en.is_integer() => {
                return mul(fs.iter().map(|f| pow(f.clone(), e.clone())).collect());
            }
            Pow(b2, e2) if positive_constant(b2) => {
                return pow((**b2).clone(), mul(vec![(**e2).clone(), e]));
            }
            // (400*pi^2)^(1/2) = 20*pi: roots of products of positive constants
            Mul(fs) if fs.iter().all(positive_constant) => {
                return mul(fs.iter().map(|f| pow(f.clone(), e.clone())).collect());
            }
            _ => {}
        }
    } else if b.is_one() {
        return b;
    }
    // e^log(x) = x
    if b == Const(Const::E) {
        if let Func(Func::Log, x) = &e {
            return (**x).clone();
        }
        // Euler: e^(i*k*pi) = cos(k*pi) + i*sin(k*pi) when both are known exactly
        if let Some(k) = i_pi_multiple(&e) {
            if let (Some(c), Some(s)) = (sin_pi(k.add(Number::rat(1, 2).unwrap())), sin_pi(k)) {
                return add(vec![c, mul(vec![s, Const(Const::I)])]);
            }
        }
    }
    Pow(Box::new(b), Box::new(e))
}

/// n = b^k with the largest k > 1, if there is one
fn perfect_power(n: i128) -> Option<(i128, i128)> {
    for k in (2..=126).rev() {
        let b = (n as f64).powf(1.0 / k as f64).round() as i128;
        for c in [b - 1, b, b + 1] {
            if c >= 2 && c.checked_pow(k as u32) == Some(n) {
                return Some((c, k));
            }
        }
    }
    None
}

/// positive numbers, pi, e and their real powers
fn positive_constant(e: &Expr) -> bool {
    match e {
        Num(n) => !n.is_negative() && !n.is_zero(),
        Const(Const::Pi | Const::E) => true,
        Pow(b, x) => matches!(**b, Const(Const::Pi | Const::E)) && x.is_number(),
        _ => false,
    }
}

/// n = k^q * m with k as large as trial division (up to 10^5) finds
fn split_root(n: i128, q: i128) -> (i128, i128) {
    let (mut k, mut m) = (1i128, n);
    let mut p = 2i128;
    while p <= 100_000 && p * p <= m {
        let pq = p.checked_pow(q as u32);
        match pq {
            Some(pq) if m % pq == 0 => {
                m /= pq;
                k *= p;
            }
            _ => p += 1,
        }
    }
    (k, m)
}

fn split_root_big(n: &num_bigint::BigInt, q: u32) -> (num_bigint::BigInt, num_bigint::BigInt) {
    use num_traits::Zero;
    let (mut k, mut m) = (num_bigint::BigInt::from(1), n.clone());
    // an exact q-th power of a big number
    let r = m.nth_root(q);
    if r.pow(q) == m {
        return (r, num_bigint::BigInt::from(1));
    }
    for p in crate::numtheory::primes_upto(10_000) {
        let pq = num_bigint::BigInt::from(p).pow(q);
        while (&m % &pq).is_zero() {
            m /= &pq;
            k *= p;
        }
    }
    (k, m)
}

pub fn neg(e: Expr) -> Expr {
    mul(vec![Expr::int(-1), e])
}
/// two plain decimals (fast path for large arrays)
fn floats(a: &Expr, b: &Expr) -> Option<(f64, f64)> {
    match (a, b) {
        (Num(Number::Float(x)), Num(Number::Float(y))) => Some((*x, *y)),
        (Num(Number::Float(x)), Num(n)) | (Num(n), Num(Number::Float(x))) if n.is_exact() => {
            // keep the order of a and b
            let (x, y) = (*x, n.to_f64());
            if matches!(a, Num(Number::Float(_))) {
                Some((x, y))
            } else {
                Some((y, x))
            }
        }
        _ => None,
    }
}
pub fn sub(a: Expr, b: Expr) -> Expr {
    if let Some((x, y)) = floats(&a, &b) {
        return Num(Number::Float(x - y));
    }
    add(vec![a, neg(b)])
}
pub fn div(a: Expr, b: Expr) -> Expr {
    if let Some((x, y)) = floats(&a, &b) {
        if y != 0.0 {
            return Num(Number::Float(x / y));
        }
    }
    mul(vec![a, pow(b, Expr::int(-1))])
}
pub fn add2(a: Expr, b: Expr) -> Expr {
    if let Some((x, y)) = floats(&a, &b) {
        return Num(Number::Float(x + y));
    }
    add(vec![a, b])
}
pub fn mul2(a: Expr, b: Expr) -> Expr {
    if let Some((x, y)) = floats(&a, &b) {
        return Num(Number::Float(x * y));
    }
    mul(vec![a, b])
}
pub fn exp(x: Expr) -> Expr {
    pow(Const(Const::E), x)
}
pub fn sqrt(x: Expr) -> Expr {
    pow(x, Num(Number::rat(1, 2).unwrap()))
}
pub fn rat(n: i128, d: i128) -> Expr {
    Num(Number::rat(n, d).unwrap())
}
pub fn pi() -> Expr {
    Const(Const::Pi)
}

/// `k*pi` with rational k -> Some(k)
fn pi_multiple(e: &Expr) -> Option<Number> {
    match e {
        Const(Const::Pi) => Some(Number::one()),
        Mul(f) if f.len() == 2 && f[1] == Const(Const::Pi) => f[0].num().filter(|n| n.is_exact()),
        _ => None,
    }
}

/// `k*i*pi` with rational k -> Some(k)
fn i_pi_multiple(e: &Expr) -> Option<Number> {
    let Mul(f) = e else { return None };
    let mut k = Number::one();
    let (mut has_i, mut has_pi) = (false, false);
    for x in f {
        match x {
            Num(n) if n.is_exact() => k = k.mul(n),
            Const(Const::I) if !has_i => has_i = true,
            Const(Const::Pi) if !has_pi => has_pi = true,
            _ => return None,
        }
    }
    if has_i && has_pi { Some(k) } else { None }
}

/// exact sin(k*pi) for k with denominator 1, 2, 3, 4 or 6
fn sin_pi(k: Number) -> Option<Expr> {
    let (n, d) = (k.numer()?, k.denom()?);
    // reduce to k in [0, 2) in units of 1/12
    let twelfths = (n * (12 / if 12 % d == 0 { d } else { return None })).rem_euclid(24);
    let s3 = || mul(vec![rat(1, 2), sqrt(Expr::int(3))]);
    let s2 = || mul(vec![rat(1, 2), sqrt(Expr::int(2))]);
    // sin(pi/12) = (sqrt(6) - sqrt(2))/4, sin(5*pi/12) = (sqrt(6) + sqrt(2))/4
    let s15 = || {
        add(vec![
            mul(vec![rat(1, 4), sqrt(Expr::int(6))]),
            mul(vec![rat(-1, 4), sqrt(Expr::int(2))]),
        ])
    };
    let s75 = || {
        add(vec![
            mul(vec![rat(1, 4), sqrt(Expr::int(6))]),
            mul(vec![rat(1, 4), sqrt(Expr::int(2))]),
        ])
    };
    let v = match twelfths {
        1 | 11 => s15(),
        5 | 7 => s75(),
        13 | 23 => neg(s15()),
        17 | 19 => neg(s75()),
        0 | 12 => Expr::zero(),
        2 | 10 => rat(1, 2),
        3 | 9 => s2(),
        4 | 8 => s3(),
        6 => Expr::one(),
        14 | 22 => rat(-1, 2),
        15 | 21 => neg(s2()),
        16 | 20 => neg(s3()),
        18 => Expr::int(-1),
        _ => return None,
    };
    Some(v)
}

/// exact tan(k*pi) for k a multiple of 1/12 (not at the poles)
fn tan_pi(k: Number) -> Option<Expr> {
    let (n, d) = (k.numer()?, k.denom()?);
    if 12 % d != 0 {
        return None;
    }
    let t = (n * (12 / d)).rem_euclid(12);
    let r3 = || sqrt(Expr::int(3));
    let v = match t {
        0 => Expr::zero(),
        1 => sub(Expr::int(2), r3()),
        2 => mul(vec![rat(1, 3), r3()]),
        3 => Expr::one(),
        4 => r3(),
        5 => add2(Expr::int(2), r3()),
        7 => neg(add2(Expr::int(2), r3())),
        8 => neg(r3()),
        9 => Expr::int(-1),
        10 => mul(vec![rat(-1, 3), r3()]),
        11 => neg(sub(Expr::int(2), r3())),
        _ => return None,
    };
    Some(v)
}

/// asin(x) for the textbook values x = 1/2, sqrt(2)/2, sqrt(3)/2 (x >= 0)
fn asin_known(x: &Expr) -> Option<Expr> {
    let half = |e: Expr| mul(vec![rat(1, 2), e]);
    let k = if *x == rat(1, 2) {
        rat(1, 6)
    } else if *x == half(sqrt(Expr::int(2))) || *x == pow(sqrt(Expr::int(2)), Expr::int(-1)) {
        rat(1, 4)
    } else if *x == half(sqrt(Expr::int(3))) {
        rat(1, 3)
    } else {
        return None;
    };
    Some(mul(vec![k, pi()]))
}

pub fn func(f: Func, x: Expr) -> Expr {
    use Func::*;
    if let Num(n) = &x {
        if !n.is_exact() {
            let v = n.to_f64();
            return Expr::float(f.apply(v));
        }
    }
    // sin(acos(x)) = sqrt(1 - x^2) and friends
    if let Func(g, inner) = &x {
        let y = (**inner).clone();
        let root = || sqrt(sub(Expr::one(), pow(y.clone(), Expr::int(2))));
        match (f, g) {
            (Sin, Asin) | (Cos, Acos) | (Tan, Atan) => return y,
            (Sin, Acos) | (Cos, Asin) => return root(),
            _ => {}
        }
    }
    let negated = x.is_negative_term();
    match f {
        Sin | Tan | Asin | Atan | Sinh | Tanh | Asinh | Atanh if negated => {
            return neg(func(f, neg(x)));
        }
        Cos | Abs | Cosh if negated => return func(f, neg(x)),
        _ => {}
    }
    if let Some(k) = crate::assume::integer_pi_multiple(&x) {
        // n an integer: sin(n pi) = 0, cos(n pi) = (-1)^n
        match f {
            Sin | Tan => return Expr::zero(),
            Cos => return crate::assume::minus_one_power(&k),
            _ => {}
        }
    }
    if f == Abs && crate::assume::any() {
        match crate::assume::sign(&x) {
            Some(s) if s >= 0 => return x,
            Some(_) => return neg(x),
            None => {}
        }
    }
    match f {
        Sin => {
            if let Some(v) = pi_multiple(&x).and_then(sin_pi) {
                return v;
            }
            if x.is_zero() {
                return Expr::zero();
            }
        }
        Cos => {
            if let Some(k) = pi_multiple(&x) {
                if let Some(v) = sin_pi(k.add(Number::rat(1, 2).unwrap())) {
                    return v;
                }
            }
            if x.is_zero() {
                return Expr::one();
            }
        }
        Sinh | Tanh | Asinh | Atanh if x.is_zero() => return Expr::zero(),
        Cosh if x.is_zero() => return Expr::one(),
        Acosh if x.is_one() => return Expr::zero(),
        Tan => {
            if x.is_zero() {
                return Expr::zero();
            }
            if let Some(k) = pi_multiple(&x) {
                if k.is_integer() {
                    return Expr::zero();
                }
                if k.denom() == Some(4) {
                    return Expr::int(if k.numer().unwrap().rem_euclid(4) == 1 {
                        1
                    } else {
                        -1
                    });
                }
                if let Some(v) = tan_pi(k) {
                    return v;
                }
            }
        }
        Asin => {
            if x.is_zero() {
                return Expr::zero();
            }
            if x.is_one() {
                return mul(vec![rat(1, 2), pi()]);
            }
            if let Some(v) = asin_known(&x) {
                return v;
            }
        }
        Acos => {
            if x.is_zero() {
                return mul(vec![rat(1, 2), pi()]);
            }
            if x.is_one() {
                return Expr::zero();
            }
            if x == Expr::int(-1) {
                return pi();
            }
            if let Some(v) = asin_known(&x) {
                return sub(mul(vec![rat(1, 2), pi()]), v);
            }
            if let Some(v) = asin_known(&neg(x.clone())) {
                return add2(mul(vec![rat(1, 2), pi()]), v);
            }
        }
        Atan => {
            if x.is_zero() {
                return Expr::zero();
            }
            if x.is_one() {
                return mul(vec![rat(1, 4), pi()]);
            }
            if x == sqrt(Expr::int(3)) {
                return mul(vec![rat(1, 3), pi()]);
            }
            if x == mul(vec![rat(1, 3), sqrt(Expr::int(3))]) {
                return mul(vec![rat(1, 6), pi()]);
            }
        }
        Log => {
            if x.is_one() {
                return Expr::zero();
            }
            if let Num(Number::Rat(n, d)) = x.clone() {
                // log(1/4) = -log(4), log(8) = 3*log(2)
                if n == 1 && d > 1 {
                    return neg(func(Log, Expr::int(d)));
                }
                if d == 1 && n > 1 {
                    if let Some((b, k)) = perfect_power(n) {
                        return mul2(Expr::int(k), func(Log, Expr::int(b)));
                    }
                }
            }
            if x == Const(Const::E) {
                return Expr::one();
            }
            if let Pow(b, e) = &x {
                if **b == Const(Const::E) {
                    return (**e).clone();
                }
            }
        }
        Abs => {
            if let Num(n) = &x {
                return Num(n.abs());
            }
            // |a + b*i| = sqrt(a^2 + b^2)
            if let Some((a, b)) = gauss(&x) {
                return sqrt(Num(a.mul(&a).add(b.mul(&b))));
            }
            if let Const(Const::Pi | Const::E) = x {
                return x;
            }
        }
        _ => {}
    }
    Expr::Func(f, Box::new(x))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print::show;
    fn x() -> Expr {
        Expr::sym("x")
    }
    fn y() -> Expr {
        Expr::sym("y")
    }
    #[test]
    fn collects_like_terms() {
        let e = add(vec![
            x(),
            mul(vec![Expr::int(2), x()]),
            Expr::int(3),
            y(),
            neg(y()),
        ]);
        assert_eq!(e, add(vec![mul(vec![Expr::int(3), x()]), Expr::int(3)]));
    }
    #[test]
    fn combines_powers() {
        assert_eq!(
            mul(vec![x(), x(), pow(x(), Expr::int(3))]),
            pow(x(), Expr::int(5))
        );
        assert_eq!(div(x(), x()), Expr::one());
    }
    #[test]
    fn imaginary_unit() {
        let i = Const(Const::I);
        assert_eq!(mul(vec![i.clone(), i.clone()]), Expr::int(-1));
        assert_eq!(sqrt(Expr::int(-1)), i);
        assert_eq!(show(&pow(i, Expr::int(3))), "-i");
    }
    #[test]
    fn trig_values() {
        assert_eq!(
            func(Func::Cos, mul(vec![Expr::int(3), pi()])),
            Expr::int(-1)
        );
        assert_eq!(func(Func::Sin, mul(vec![rat(1, 6), pi()])), rat(1, 2));
        assert_eq!(func(Func::Cos, neg(x())), func(Func::Cos, x()));
    }
}
