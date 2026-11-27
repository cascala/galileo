//! Commands for the tutorials: number theory, complex numbers, trigonometric
//! identities, tables, implicit plots (conics), permutation groups,
//! symbolic sums and linear ODEs (dsolve).

use crate::build::*;
use crate::calc::{self, deriv, expand, simplify, subst};
use crate::cas;
use crate::eval::{R, Session, equation_lhs};
use crate::expr::{Const, Expr, Func, Obj};
use crate::num::Number;
use crate::numeric;
use crate::numtheory as nt;
use crate::parser::Node;
use crate::plot::{Figure, Series};
use crate::print::show;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::rc::Rc;

fn name(n: &Node, what: &str) -> R<String> {
    match n {
        Node::Ident(s) => Ok(s.clone()),
        _ => Err(format!("{} must be a variable name", what)),
    }
}

fn row(v: Vec<Expr>) -> Expr {
    Expr::Matrix(vec![v])
}

pub(crate) fn int_of(e: &Expr, f: &str) -> R<i128> {
    let v = match e {
        Expr::Num(n) if !n.is_exact() && n.is_integer() && n.to_f64().abs() < 1e15 => {
            return Ok(n.to_f64() as i128);
        }
        e => e.num().filter(|n| n.is_exact()).and_then(|n| n.as_int()),
    };
    v.ok_or_else(|| format!("{} needs whole numbers, got {}", f, show(e)))
}

fn nonneg(e: &Expr, f: &str) -> R<u128> {
    let n = int_of(e, f)?;
    if n < 0 {
        return Err(format!(
            "{} needs a number that is not negative, got {}",
            f, n
        ));
    }
    Ok(n as u128)
}

fn uint(n: u128) -> Expr {
    bnum(BigInt::from(n))
}

/// log10 of the number of digits of the largest power of numbers in e, for
/// numbers too large to compute exactly (they stay symbolic, like 2^(10^9))
fn log10_digits(e: &Expr) -> Option<f64> {
    match e {
        Expr::Pow(b, x) => {
            let (b, x) = (b.num()?, x.num()?);
            let lb = b.abs().to_f64().log10();
            // log10 of the exponent, also when it is too large for a float
            let lx = match x.as_bigint() {
                Some(k) => {
                    let f = k.to_f64().unwrap_or(f64::INFINITY);
                    if f.is_finite() {
                        f.abs().log10()
                    } else {
                        k.bits() as f64 * std::f64::consts::LOG10_2
                    }
                }
                None => x.to_f64().abs().log10(),
            };
            (lb > 0.0 && lx.is_finite()).then(|| lx + lb.log10())
        }
        Expr::Add(v) | Expr::Mul(v) => v.iter().filter_map(log10_digits).reduce(f64::max),
        _ => None,
    }
}

/// a number too large to compute exactly, kept as a power (2^(10^9) - 1)
pub(crate) fn is_huge(e: &Expr) -> bool {
    !matches!(e, Expr::Num(_)) && e.symbols().is_empty() && log10_digits(e).is_some_and(|l| l > 5.0)
}

/// why e is not a whole number we can work with, in words
pub(crate) fn not_whole(e: &Expr, f: &str) -> String {
    if let Some(l) = log10_digits(e).filter(|_| is_huge(e)) {
        let digits = if l < 15.0 {
            format!("about {:.0} digits", 10f64.powf(l))
        } else {
            format!("about 10^{:.0} digits", l)
        };
        return format!(
            "{}: this number is too large to compute: it would have {}. Galileo computes exact whole numbers up to about a million digits",
            f, digits
        );
    }
    if let Some(n) = e.num().filter(|n| !n.is_exact() && !n.to_f64().is_finite()) {
        return format!(
            "{}: got {}, a decimal too large to represent. eval turns whole numbers into decimals; leave it out to keep numbers exact",
            f, n
        );
    }
    if let Some(n) = e.num().filter(|n| !n.is_exact() && n.is_integer()) {
        return format!(
            "{}: got the decimal {}, whose last digits are lost. eval turns whole numbers into decimals; leave it out to keep numbers exact",
            f, n
        );
    }
    let mut s = show(e);
    if s.len() > 80 {
        s = format!("{}...", &s[..77]);
    }
    format!("{} needs whole numbers, got {}", f, s)
}

/// a whole number of any size (or a float that is exactly a small whole number)
pub(crate) fn big_of(e: &Expr, f: &str) -> R<BigInt> {
    let v = match e {
        Expr::Num(n) if !n.is_exact() && n.is_integer() && n.to_f64().abs() < 1e15 => {
            return Ok(BigInt::from(n.to_f64() as i128));
        }
        e => e.num().and_then(|n| n.as_bigint()),
    };
    v.ok_or_else(|| not_whole(e, f))
}

fn big_nonneg(e: &Expr, f: &str) -> R<BigInt> {
    let n = big_of(e, f)?;
    if n.is_negative() {
        return Err(format!(
            "{} needs a number that is not negative, got {}",
            f, n
        ));
    }
    Ok(n)
}

pub(crate) fn bnum(n: BigInt) -> Expr {
    Expr::Num(Number::from_bigint(n))
}

/// apply an integer function to a number or to each entry of a matrix
fn each(v: &Expr, f: &mut dyn FnMut(&Expr) -> R<Expr>) -> R<Expr> {
    match v {
        Expr::Matrix(m) => {
            let mut out = vec![];
            for r in m {
                let mut o = vec![];
                for x in r {
                    o.push(f(x)?);
                }
                out.push(o);
            }
            Ok(Expr::Matrix(out))
        }
        x => f(x),
    }
}

/// 360 -> "2^3 * 3^2 * 5"
pub fn factored_text(n: &BigInt) -> R<String> {
    if n.is_zero() {
        return Ok("0".into());
    }
    let mut parts = vec![];
    if n.is_negative() {
        parts.push("-1".to_string());
    }
    let a = n.abs();
    if a.is_one() {
        return Ok(if n.is_negative() {
            "-1".into()
        } else {
            "1".into()
        });
    }
    for (p, e) in nt::factorize(&a)? {
        parts.push(if e == 1 {
            p.to_string()
        } else {
            format!("{}^{}", p, e)
        });
    }
    Ok(parts.join(" * "))
}

// ---------- complex numbers ----------

/// a sum of number * x^k terms in a single variable x
pub fn univariate_poly(e: &Expr) -> bool {
    let syms = e.symbols();
    if syms.len() != 1 {
        return false;
    }
    let x = Expr::sym(&syms[0]);
    let mono = |t: &Expr| -> bool {
        let power = |f: &Expr| {
            *f == x
                || matches!(f, Expr::Pow(b, k) if **b == x && k.num().is_some_and(|n| n.is_integer() && !n.is_negative()))
        };
        match t {
            Expr::Num(_) => true,
            Expr::Mul(f) => f.len() == 2 && f[0].is_number() && power(&f[1]),
            other => power(other),
        }
    };
    matches!(e, Expr::Add(v) if v.iter().all(mono))
}

pub fn has_i(e: &Expr) -> bool {
    match e {
        Expr::Const(Const::I) => true,
        Expr::Add(v) | Expr::Mul(v) | Expr::Apply(_, v) => v.iter().any(has_i),
        Expr::Pow(a, b) => has_i(a) || has_i(b),
        Expr::Func(_, a) => has_i(a),
        _ => false,
    }
}

fn cmul(a: (Expr, Expr), b: (Expr, Expr)) -> (Expr, Expr) {
    (
        sub(
            mul2(a.0.clone(), b.0.clone()),
            mul2(a.1.clone(), b.1.clone()),
        ),
        add2(mul2(a.0, b.1), mul2(a.1, b.0)),
    )
}

/// (real part, imaginary part), treating every variable as real
pub fn parts(e: &Expr) -> R<(Expr, Expr)> {
    if !has_i(e) {
        return Ok((e.clone(), Expr::zero()));
    }
    match e {
        Expr::Const(Const::I) => Ok((Expr::zero(), Expr::one())),
        Expr::Add(v) => {
            let (mut re, mut im) = (vec![], vec![]);
            for t in v {
                let (a, b) = parts(t)?;
                re.push(a);
                im.push(b);
            }
            Ok((add(re), add(im)))
        }
        Expr::Mul(v) => {
            let mut acc = (Expr::one(), Expr::zero());
            for f in v {
                acc = cmul(acc, parts(f)?);
                acc = (expand(&acc.0), expand(&acc.1));
            }
            Ok(acc)
        }
        Expr::Pow(b, p) if **b == Expr::Const(Const::E) => {
            // e^(a + b i) = e^a (cos b + i sin b)
            let (a, t) = parts(p)?;
            let r = exp(a);
            Ok((
                mul2(r.clone(), func(Func::Cos, t.clone())),
                mul2(r, func(Func::Sin, t)),
            ))
        }
        Expr::Pow(b, p) if !has_i(p) && p.num().is_some_and(|n| n.is_exact() && n.is_integer()) => {
            let n = p.num().unwrap().as_int().unwrap();
            if n.abs() > 40 {
                return Err("power too large to split into real and imaginary parts".into());
            }
            let z = parts(b)?;
            let z = if n < 0 {
                // 1/(a + b i) = (a - b i)/(a^2 + b^2)
                let d = simplify(&add2(
                    pow(z.0.clone(), Expr::int(2)),
                    pow(z.1.clone(), Expr::int(2)),
                ));
                (div(z.0, d.clone()), div(neg(z.1), d))
            } else {
                z
            };
            let mut acc = (Expr::one(), Expr::zero());
            for _ in 0..n.abs() {
                acc = cmul(acc, z.clone());
                acc = (expand(&acc.0), expand(&acc.1));
            }
            Ok(acc)
        }
        Expr::Func(Func::Sin, a) => {
            let (x, y) = parts(a)?;
            Ok((
                mul2(func(Func::Sin, x.clone()), func(Func::Cosh, y.clone())),
                mul2(func(Func::Cos, x), func(Func::Sinh, y)),
            ))
        }
        Expr::Func(Func::Cos, a) => {
            let (x, y) = parts(a)?;
            Ok((
                mul2(func(Func::Cos, x.clone()), func(Func::Cosh, y.clone())),
                neg(mul2(func(Func::Sin, x), func(Func::Sinh, y))),
            ))
        }
        Expr::Func(Func::Abs, _) => Ok((e.clone(), Expr::zero())),
        _ => Err(format!(
            "can't split {} into real and imaginary parts",
            show(e)
        )),
    }
}

fn tidy_parts(e: &Expr) -> R<(Expr, Expr)> {
    let (a, b) = parts(e)?;
    Ok((simplify(&a), simplify(&b)))
}

/// exact argument for the textbook angles, else atan2
fn angle(re: &Expr, im: &Expr) -> Expr {
    let (x, y) = (numeric::value(re), numeric::value(im));
    if x.is_finite() && y.is_finite() && (x != 0.0 || y != 0.0) {
        let t = y.atan2(x);
        let exact = re.num().is_none_or(|n| n.is_exact()) && im.num().is_none_or(|n| n.is_exact());
        if exact {
            // multiples of pi/12
            let k = t / std::f64::consts::PI * 12.0;
            if (k - k.round()).abs() < 1e-9 {
                return mul2(rat(k.round() as i128, 12), pi());
            }
        }
        if re.symbols().is_empty() && im.symbols().is_empty() {
            return Expr::float(t);
        }
    }
    Expr::Apply("atan2".into(), vec![im.clone(), re.clone()])
}

// ---------- trigonometric identities ----------

fn split_angle(a: &Expr) -> Option<(Expr, Expr)> {
    match a {
        Expr::Add(v) if v.len() >= 2 => Some((v[0].clone(), add(v[1..].to_vec()))),
        Expr::Mul(f) => {
            let n = f[0]
                .num()
                .filter(|n| n.is_exact() && n.is_integer())?
                .as_int()?;
            if n >= 2 {
                let x = mul(f[1..].to_vec());
                Some((mul2(Expr::int(n - 1), x.clone()), x))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// sin(a+b) -> sin a cos b + cos a sin b, sin(2x) -> 2 sin x cos x, ...
pub fn trig_expand(e: &Expr) -> Expr {
    let e = e.map_children(&mut |c| trig_expand(c));
    match &e {
        Expr::Func(f @ (Func::Sin | Func::Cos | Func::Tan), a) => {
            let Some((p, q)) = split_angle(a) else {
                return e;
            };
            let (sp, cp) = (
                trig_expand(&func(Func::Sin, p.clone())),
                trig_expand(&func(Func::Cos, p.clone())),
            );
            let (sq, cq) = (func(Func::Sin, q.clone()), func(Func::Cos, q.clone()));
            let r = match f {
                Func::Sin => add2(mul2(sp, cq), mul2(cp, sq)),
                Func::Cos => sub(mul2(cp, cq), mul2(sp, sq)),
                _ => {
                    let (tp, tq) = (trig_expand(&func(Func::Tan, p)), func(Func::Tan, q));
                    return div(add2(tp.clone(), tq.clone()), sub(Expr::one(), mul2(tp, tq)));
                }
            };
            expand(&r)
        }
        _ => e,
    }
}

fn trig_factor(f: &Expr) -> Option<(bool, Expr)> {
    match f {
        Expr::Func(Func::Sin, a) => Some((true, (**a).clone())),
        Expr::Func(Func::Cos, a) => Some((false, (**a).clone())),
        _ => None,
    }
}

/// one product term: rewrite products of sines and cosines as sums
fn reduce_term(t: &Expr) -> Expr {
    let factors: Vec<Expr> = match t {
        Expr::Mul(f) => f.clone(),
        other => vec![other.clone()],
    };
    // unfold sin(x)^n into n factors
    let mut flat = vec![];
    for f in factors {
        match &f {
            Expr::Pow(b, p)
                if trig_factor(b).is_some()
                    && p.num()
                        .is_some_and(|n| n.is_exact() && n.is_integer() && !n.is_negative()) =>
            {
                for _ in 0..p.num().unwrap().as_int().unwrap() {
                    flat.push((**b).clone());
                }
            }
            _ => flat.push(f),
        }
    }
    let idx: Vec<usize> = (0..flat.len())
        .filter(|&i| trig_factor(&flat[i]).is_some())
        .collect();
    if idx.len() < 2 {
        return t.clone();
    }
    let (s1, a) = trig_factor(&flat[idx[0]]).unwrap();
    let (s2, b) = trig_factor(&flat[idx[1]]).unwrap();
    let (d, s) = (sub(a.clone(), b.clone()), add2(a.clone(), b.clone()));
    let half = rat(1, 2);
    let pair = match (s1, s2) {
        (true, true) => mul2(half, sub(func(Func::Cos, d), func(Func::Cos, s))),
        (false, false) => mul2(half, add2(func(Func::Cos, d), func(Func::Cos, s))),
        (true, false) => mul2(half, add2(func(Func::Sin, s), func(Func::Sin, d))),
        (false, true) => mul2(half, sub(func(Func::Sin, s), func(Func::Sin, d))),
    };
    let rest: Vec<Expr> = flat
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != idx[0] && *i != idx[1])
        .map(|(_, f)| f.clone())
        .collect();
    let mut all = rest;
    all.push(pair);
    trig_reduce(&expand(&mul(all)))
}

/// products and powers of sin/cos -> sums of sines and cosines of multiple angles
pub fn trig_reduce(e: &Expr) -> Expr {
    let e = expand(e);
    match &e {
        Expr::Add(v) => add(v.iter().map(reduce_term).collect()),
        other => reduce_term(other),
    }
}

/// Solutions of an equation in one trigonometric function of x, such as
/// 2 sin(x) - 1 = 0 or cos(x)^2 = 1/4: the solutions with 0 <= x < 2 pi
pub fn solve_trig(f: &Expr, x: &str) -> Option<R<Vec<Expr>>> {
    fn find(e: &Expr, x: &str, out: &mut Vec<Func>) {
        match e {
            Expr::Func(fun @ (Func::Sin | Func::Cos | Func::Tan), a) if **a == Expr::sym(x) => {
                if !out.contains(fun) {
                    out.push(*fun);
                }
            }
            _ => {
                let _ = e.map_children(&mut |c| {
                    find(c, x, out);
                    c.clone()
                });
            }
        }
    }
    let mut fs = vec![];
    find(f, x, &mut fs);
    if fs.len() != 1 {
        return None;
    }
    let fun = fs[0];
    let u = "u__";
    fn replace(e: &Expr, from: &Expr, to: &Expr) -> Expr {
        if e == from {
            to.clone()
        } else {
            e.map_children(&mut |c| replace(c, from, to))
        }
    }
    let g = replace(f, &func(fun, Expr::sym(x)), &Expr::sym(u));
    if g.contains_sym(x) {
        return None;
    }
    let roots = match cas::solve(&g, u) {
        Ok(r) => r,
        Err(e) => return Some(Err(e)),
    };
    let two_pi = mul2(Expr::int(2), pi());
    let norm = |t: Expr| -> Expr {
        let v = numeric::value(&t);
        let k = (v / (2.0 * std::f64::consts::PI)).floor();
        let r = simplify(&sub(t, mul2(Expr::int(k as i128), two_pi.clone())));
        if (numeric::value(&r) - 2.0 * std::f64::consts::PI).abs() < 1e-12 {
            Expr::zero()
        } else {
            r
        }
    };
    let mut out: Vec<Expr> = vec![];
    for r in roots {
        let v = numeric::value(&r);
        if !v.is_finite() || has_i(&r) {
            continue;
        }
        let cands = match fun {
            Func::Sin if v.abs() <= 1.0 + 1e-12 => {
                let a = func(Func::Asin, r.clone());
                vec![a.clone(), sub(pi(), a)]
            }
            Func::Cos if v.abs() <= 1.0 + 1e-12 => {
                let a = func(Func::Acos, r.clone());
                vec![a.clone(), neg(a)]
            }
            Func::Tan => {
                let a = func(Func::Atan, r.clone());
                vec![a.clone(), add2(a, pi())]
            }
            _ => vec![],
        };
        let float = r.num().is_some_and(|n| !n.is_exact());
        for c in cands {
            let c = if float {
                calc::evalf(&norm(c))
            } else {
                norm(c)
            };
            if !out
                .iter()
                .any(|o| (numeric::value(o) - numeric::value(&c)).abs() < 1e-12)
            {
                out.push(c);
            }
        }
    }
    out.sort_by(|a, b| numeric::value(a).total_cmp(&numeric::value(b)));
    if out.is_empty() {
        return Some(Err("no solution".into()));
    }
    Some(Ok(out))
}

/// Equations with x inside one exponential or logarithm, such as
/// 70 e^(-10 k) = 40 or 2^x = 10: solve for the inside, then invert
pub fn solve_inverse(f: &Expr, x: &str) -> Option<R<Vec<Expr>>> {
    fn find(e: &Expr, x: &str, out: &mut Vec<Expr>) {
        let inner = match e {
            Expr::Pow(b, h) if b.is_number() || matches!(**b, Expr::Const(_)) => h.contains_sym(x),
            Expr::Func(Func::Log, h) => h.contains_sym(x),
            _ => false,
        };
        if inner {
            if !out.contains(e) {
                out.push(e.clone());
            }
            return;
        }
        let _ = e.map_children(&mut |c| {
            find(c, x, out);
            c.clone()
        });
    }
    fn replace(e: &Expr, from: &Expr, to: &Expr) -> Expr {
        if e == from {
            to.clone()
        } else {
            e.map_children(&mut |c| replace(c, from, to))
        }
    }
    let mut found = vec![];
    find(f, x, &mut found);
    if found.len() != 1 {
        return None;
    }
    let t = found.remove(0);
    let g = replace(f, &t, &Expr::sym("u__"));
    if g.contains_sym(x) {
        return None;
    }
    let roots = match cas::solve(&g, "u__") {
        Ok(r) => r,
        Err(e) => return Some(Err(e)),
    };
    let mut out: Vec<Expr> = vec![];
    for r in roots {
        if has_i(&r) {
            continue;
        }
        let (inside, target) = match &t {
            Expr::Func(Func::Log, h) => ((**h).clone(), exp(r.clone())),
            Expr::Pow(b, h) => {
                let v = numeric::value(&r);
                if v.is_finite() && v <= 0.0 {
                    continue;
                }
                (
                    (**h).clone(),
                    div(func(Func::Log, r.clone()), func(Func::Log, (**b).clone())),
                )
            }
            _ => continue,
        };
        match cas::solve(&sub(inside, target), x) {
            Ok(xs) => {
                for v in xs {
                    let v = simplify(&v);
                    if !has_i(&v) && !out.iter().any(|o| cas::is_zero(&sub(o.clone(), v.clone()))) {
                        out.push(v);
                    }
                }
            }
            Err(e) => return Some(Err(e)),
        }
    }
    if out.is_empty() {
        return Some(Err("no solution".into()));
    }
    Some(Ok(out))
}

// ---------- implicit curves (marching squares) ----------

fn contour(
    f: &dyn Fn(f64, f64) -> f64,
    x0: f64,
    x1: f64,
    y0: f64,
    y1: f64,
    n: usize,
) -> Vec<(f64, f64)> {
    let xs: Vec<f64> = (0..=n)
        .map(|i| x0 + (x1 - x0) * i as f64 / n as f64)
        .collect();
    let ys: Vec<f64> = (0..=n)
        .map(|j| y0 + (y1 - y0) * j as f64 / n as f64)
        .collect();
    let v: Vec<Vec<f64>> = xs
        .iter()
        .map(|&x| ys.iter().map(|&y| f(x, y)).collect())
        .collect();
    let mut segs: Vec<((f64, f64), (f64, f64))> = vec![];
    let cross = |pa: (f64, f64), va: f64, pb: (f64, f64), vb: f64| {
        let t = va / (va - vb);
        (pa.0 + t * (pb.0 - pa.0), pa.1 + t * (pb.1 - pa.1))
    };
    for i in 0..n {
        for j in 0..n {
            let c = [
                (xs[i], ys[j]),
                (xs[i + 1], ys[j]),
                (xs[i + 1], ys[j + 1]),
                (xs[i], ys[j + 1]),
            ];
            let w = [v[i][j], v[i + 1][j], v[i + 1][j + 1], v[i][j + 1]];
            if w.iter().any(|z| !z.is_finite()) {
                continue;
            }
            let mut pts = vec![];
            for k in 0..4 {
                let (a, b) = (k, (k + 1) % 4);
                if (w[a] < 0.0) != (w[b] < 0.0) {
                    pts.push(cross(c[a], w[a], c[b], w[b]));
                }
            }
            match pts.len() {
                2 => segs.push((pts[0], pts[1])),
                4 => {
                    // saddle: decide by the centre value
                    let mid = (w[0] + w[1] + w[2] + w[3]) / 4.0;
                    if (mid < 0.0) == (w[0] < 0.0) {
                        segs.push((pts[0], pts[3]));
                        segs.push((pts[1], pts[2]));
                    } else {
                        segs.push((pts[0], pts[1]));
                        segs.push((pts[2], pts[3]));
                    }
                }
                _ => {}
            }
        }
    }
    // chain the segments into polylines, separated by NaN breaks
    let close = |a: (f64, f64), b: (f64, f64)| {
        (a.0 - b.0).abs() < 1e-9 * (1.0 + a.0.abs()) && (a.1 - b.1).abs() < 1e-9 * (1.0 + a.1.abs())
    };
    let mut used = vec![false; segs.len()];
    let mut out = vec![];
    for s in 0..segs.len() {
        if used[s] {
            continue;
        }
        used[s] = true;
        let mut line = std::collections::VecDeque::from(vec![segs[s].0, segs[s].1]);
        loop {
            let mut grew = false;
            for k in 0..segs.len() {
                if used[k] {
                    continue;
                }
                let (a, b) = segs[k];
                let (front, back) = (*line.front().unwrap(), *line.back().unwrap());
                if close(a, back) {
                    line.push_back(b);
                } else if close(b, back) {
                    line.push_back(a);
                } else if close(a, front) {
                    line.push_front(b);
                } else if close(b, front) {
                    line.push_front(a);
                } else {
                    continue;
                }
                used[k] = true;
                grew = true;
            }
            if !grew {
                break;
            }
        }
        out.extend(line);
        out.push((f64::NAN, f64::NAN));
    }
    out
}

// ---------- permutations ----------

pub(crate) fn perm_of(e: &Expr, f: &str) -> R<Vec<usize>> {
    let v: Vec<Expr> = match e {
        Expr::Matrix(m) if m.len() == 1 => m[0].clone(),
        Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => {
            m.iter().map(|r| r[0].clone()).collect()
        }
        Expr::Num(_) => vec![e.clone()],
        _ => {
            return Err(format!(
                "{}: a permutation is a row such as [2, 3, 1] (1 goes to 2, 2 to 3, 3 to 1)",
                f
            ));
        }
    };
    let n = v.len();
    let mut p = vec![];
    let mut seen = vec![false; n + 1];
    for x in &v {
        let k = int_of(x, f)?;
        if k < 1 || k as usize > n || seen[k as usize] {
            return Err(format!(
                "{}: [{}] is not a permutation of 1..{}",
                f,
                v.iter().map(show).collect::<Vec<_>>().join(", "),
                n
            ));
        }
        seen[k as usize] = true;
        p.push(k as usize);
    }
    Ok(p)
}

fn perm_expr(p: &[usize]) -> Expr {
    row(p.iter().map(|&k| Expr::int(k as i128)).collect())
}

/// (p o q)(i) = p(q(i)): first q, then p
fn compose(p: &[usize], q: &[usize]) -> Vec<usize> {
    let n = p.len().max(q.len());
    let at = |s: &[usize], i: usize| if i <= s.len() { s[i - 1] } else { i };
    (1..=n).map(|i| at(p, at(q, i))).collect()
}

fn cycles(p: &[usize]) -> Vec<Vec<usize>> {
    let mut seen = vec![false; p.len() + 1];
    let mut out = vec![];
    for s in 1..=p.len() {
        if seen[s] {
            continue;
        }
        let mut c = vec![];
        let mut k = s;
        while !seen[k] {
            seen[k] = true;
            c.push(k);
            k = p[k - 1];
        }
        if c.len() > 1 {
            out.push(c);
        }
    }
    out
}

fn cycles_text(p: &[usize]) -> String {
    let c = cycles(p);
    if c.is_empty() {
        return "()".into();
    }
    c.iter()
        .map(|c| {
            format!(
                "({})",
                c.iter()
                    .map(|k| k.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        })
        .collect()
}

fn parse_cycles(s: &str, n: usize) -> R<Vec<usize>> {
    let mut p: Vec<usize> = (1..=n).collect();
    let mut rest = s.trim();
    let mut cyc: Vec<Vec<usize>> = vec![];
    while !rest.is_empty() {
        let open = rest
            .find('(')
            .ok_or("perm: write cycles like \"(1 2 3)(4 5)\"")?;
        let close = rest.find(')').ok_or("perm: missing )")?;
        let inner = &rest[open + 1..close];
        let c: Vec<usize> = inner
            .split(|ch: char| ch == ' ' || ch == ',')
            .filter(|t| !t.is_empty())
            .map(|t| {
                t.parse::<usize>()
                    .map_err(|_| format!("perm: {} is not a number", t))
            })
            .collect::<R<_>>()?;
        if c.iter().any(|&k| k < 1 || k > n) {
            return Err(format!("perm: cycle entries must be between 1 and {}", n));
        }
        cyc.push(c);
        rest = rest[close + 1..].trim();
    }
    // cycles are applied right to left, like composition
    for c in cyc.iter().rev() {
        let mut q: Vec<usize> = (1..=n).collect();
        for k in 0..c.len() {
            q[c[k] - 1] = c[(k + 1) % c.len()];
        }
        p = compose(&q, &p);
    }
    Ok(p)
}

fn perm_order(p: &[usize]) -> u128 {
    cycles(p).iter().fold(1u128, |acc, c| {
        acc / nt::small::gcd(acc, c.len() as u128) * c.len() as u128
    })
}

fn perm_sign(p: &[usize]) -> i128 {
    if cycles(p).iter().map(|c| c.len() - 1).sum::<usize>() % 2 == 0 {
        1
    } else {
        -1
    }
}

// ---------- sums ----------

/// closed forms for sums with a symbolic or infinite upper limit
pub fn symbolic_sum(f: &Expr, k: &str, a: &Expr, b: &Expr, b_inf: bool) -> R<Expr> {
    let a0 = int_of(a, "sum: the lower limit")?;
    // polynomial in k: interpolate the partial sums
    if !b_inf {
        if let Some(c) = cas::poly_coeffs(&expand(f), k) {
            let d = c.len(); // degree + 1
            let n = Expr::sym("n__");
            let pts: Vec<i128> = (0..=d as i128).map(|j| a0 - 1 + j).collect();
            let mut partial = vec![Expr::zero()];
            for j in 1..pts.len() {
                let v = simplify(&subst(f, k, &Expr::int(pts[j])));
                partial.push(add2(partial[j - 1].clone(), v));
            }
            let mut terms = vec![];
            for (j, xj) in pts.iter().enumerate() {
                let mut t = partial[j].clone();
                for (m, xm) in pts.iter().enumerate() {
                    if m != j {
                        t = mul(vec![t, sub(n.clone(), Expr::int(*xm)), rat(1, xj - xm)]);
                    }
                }
                terms.push(t);
            }
            let poly = simplify(&expand(&add(terms)));
            let r = simplify(&expand(&subst(&poly, "n__", b)));
            let fct = cas::factor_poly(&r).unwrap_or_else(|| calc::factor(&r));
            return Ok(if fct.size() <= r.size() { fct } else { r });
        }
    }
    // geometric: f(k+1)/f(k) does not depend on k
    let next = subst(f, k, &add2(Expr::sym(k), Expr::one()));
    let ratio = simplify(&div(next, f.clone()));
    if !ratio.contains_sym(k) && !ratio.is_one() {
        let first = simplify(&subst(f, k, &Expr::int(a0)));
        if b_inf {
            let r = numeric::value(&ratio);
            if r.is_finite() && r.abs() >= 1.0 {
                return Err(format!(
                    "sum: the series diverges (each term is {} times the previous one)",
                    show(&ratio)
                ));
            }
            return Ok(simplify(&div(first, sub(Expr::one(), ratio))));
        }
        let count = add2(sub(b.clone(), Expr::int(a0)), Expr::one());
        return Ok(simplify(&div(
            mul2(first, sub(pow(ratio.clone(), count), Expr::one())),
            sub(ratio, Expr::one()),
        )));
    }
    // k^p r^k: a polylogarithm, rational in r for whole p >= 0
    if b_inf {
        if let Some(r) = poly_geo_sum(f, k, a0)? {
            return Ok(r);
        }
    }
    // p-series 1/k^p with p <= 1 diverge
    if b_inf {
        if let Some(c) = cas::poly_coeffs(&simplify(&div(Expr::one(), f.clone())), k) {
            if c.len() == 2 && cas::is_zero(&c[0]) {
                return Err("sum: the series diverges (the harmonic series 1 + 1/2 + 1/3 + ... grows without bound)".into());
            }
        }
    }
    // zeta(2), zeta(4), zeta(6)
    if b_inf && a0 == 1 {
        for (m, v) in [
            (2, div(pow(pi(), Expr::int(2)), Expr::int(6))),
            (4, div(pow(pi(), Expr::int(4)), Expr::int(90))),
            (6, div(pow(pi(), Expr::int(6)), Expr::int(945))),
        ] {
            if cas::is_zero(&sub(f.clone(), pow(Expr::sym(k), Expr::int(-m)))) {
                return Ok(v);
            }
        }
    }
    Err("sum: no closed form found for this sum (polynomials, geometric series, k^p x^k and x^k/k^p, and 1/k^2, 1/k^4, 1/k^6 are known)".into())
}

/// sum over k >= a0 of c k^p r^k (p a whole number, r and c free of k)
fn poly_geo_sum(f: &Expr, k: &str, a0: i128) -> R<Option<Expr>> {
    use crate::cnum::{ceval, C};
    let a1 = a0.max(1);
    let at = |j: i128| simplify(&subst(f, k, &Expr::int(j)));
    let (f1, f2) = (at(a1), at(a1 + 1));
    if f1.is_zero() {
        return Ok(None);
    }
    // numeric check of f(k) = f(a1) (k/a1)^p r^(k - a1) at a few k
    let syms: Vec<String> = f.symbols().into_iter().filter(|s| s != k).collect();
    let env: Vec<(&str, C)> = syms.iter().enumerate().map(|(j, s)| (s.as_str(), C::new(0.31 + 0.17 * j as f64, 0.0))).collect();
    for p in (-8i128..=8).filter(|p| *p != 0) {
        let r = simplify(&mul2(div(f2.clone(), f1.clone()), pow(rat(a1, a1 + 1), Expr::int(p))));
        let fits = [a1 + 2, a1 + 3, a1 + 6].iter().all(|&j| {
            let lhs = ceval(&at(j), &env);
            let rhs = ceval(&mul(vec![f1.clone(), pow(rat(j, a1), Expr::int(p)), pow(r.clone(), Expr::int(j - a1))]), &env);
            match (lhs, rhs) {
                (Some(u), Some(v)) => u.sub(v).abs() <= 1e-9 * u.abs().max(1e-300),
                _ => false,
            }
        });
        if !fits {
            continue;
        }
        let rv = numeric::value(&r);
        if rv.is_finite() && (rv.abs() > 1.0 || rv.abs() == 1.0 && p >= -1) {
            return Err("sum: the series diverges (its terms do not shrink fast enough)".into());
        }
        if a0 <= 0 && p < 0 {
            return Err("sum: the term is infinite at k = 0".into());
        }
        let c = simplify(&div(f1.clone(), mul2(pow(Expr::int(a1), Expr::int(p)), pow(r.clone(), Expr::int(a1)))));
        let mut li = crate::special::apply("polylog", vec![Expr::int(-p), r.clone()]);
        for j in 1..a0 {
            li = sub(li, mul2(pow(Expr::int(j), Expr::int(p)), pow(r.clone(), Expr::int(j))));
        }
        let out = simplify(&mul2(c, li));
        let fct = calc::factor(&out);
        return Ok(Some(if fct.size() < out.size() { fct } else { out }));
    }
    Ok(None)
}

// ---------- dsolve ----------

/// replace y, y', y'', y(x), y'(x) ... by placeholder names y__0, y__1, ...
pub(crate) fn ode_node(n: &Node, y: &str, x: &str, order: &mut usize) -> R<Node> {
    let ph = |k: usize, order: &mut usize| {
        *order = (*order).max(k);
        Node::Ident(format!("{}__{}", y, k))
    };
    Ok(match n {
        Node::Ident(s) if s == y => ph(0, order),
        Node::Call(s, args)
            if s == y && args.len() == 1 && matches!(&args[0], Node::Ident(v) if v == x) =>
        {
            ph(0, order)
        }
        Node::Call(s, args)
            if s == "d"
                && (2..=3).contains(&args.len())
                && (matches!(&args[0], Node::Ident(v) if v == y)
                    || matches!(&args[0], Node::Call(v, a) if v == y && a.len() == 1 && matches!(&a[0], Node::Ident(w) if w == x)))
                && matches!(&args[1], Node::Ident(v) if v == x) =>
        {
            let k = match args.get(2) {
                Some(Node::Num(s)) => s.parse::<usize>().map_err(|_| "d(y, x, k): bad derivative order")?,
                None => 1,
                _ => return Err("d(y, x, k): write the derivative order as a number".into()),
            };
            ph(k, order)
        }
        Node::Call(s, args) if s == "diff" || s == "deriv" => {
            // diff(y, x) or diff(y(x), x, 2)
            let is_y = matches!(args.first(), Some(Node::Ident(v)) if v == y)
                || matches!(args.first(), Some(Node::Call(v, _)) if v == y);
            if is_y && args.len() >= 2 {
                let k = match args.get(2) {
                    Some(Node::Num(s)) => s
                        .parse::<usize>()
                        .map_err(|_| "dsolve: bad derivative order")?,
                    None => 1,
                    _ => return Err("dsolve: write the derivative order as a number".into()),
                };
                ph(k, order)
            } else {
                Node::Call(
                    s.clone(),
                    args.iter()
                        .map(|a| ode_node(a, y, x, order))
                        .collect::<R<_>>()?,
                )
            }
        }
        Node::Call(s, args) => Node::Call(
            s.clone(),
            args.iter()
                .map(|a| ode_node(a, y, x, order))
                .collect::<R<_>>()?,
        ),
        Node::Neg(a) => Node::Neg(Box::new(ode_node(a, y, x, order)?)),
        Node::Bin(op, a, b) => Node::Bin(
            *op,
            Box::new(ode_node(a, y, x, order)?),
            Box::new(ode_node(b, y, x, order)?),
        ),
        Node::Matrix(rows) => Node::Matrix(
            rows.iter()
                .map(|r| {
                    r.iter()
                        .map(|e| ode_node(e, y, x, order))
                        .collect::<R<Vec<_>>>()
                })
                .collect::<R<_>>()?,
        ),
        other => other.clone(),
    })
}

/// Does e mention the unknown y: y itself, y(t) or a derivative d(y, t)?
fn mentions(e: &Expr, y: &str) -> bool {
    match e {
        Expr::Sym(s) => s == y,
        Expr::Apply(f, v) => f.trim_end_matches('\'') == y || v.iter().any(|a| mentions(a, y)),
        Expr::Add(v) | Expr::Mul(v) => v.iter().any(|a| mentions(a, y)),
        Expr::Pow(a, b) => mentions(a, y) || mentions(b, y),
        Expr::Func(_, a) => mentions(a, y),
        Expr::Matrix(r) => r.iter().flatten().any(|a| mentions(a, y)),
        _ => false,
    }
}

impl Session {
    /// Replace stored variables that mention one of the unknowns by their
    /// formula, so T = d(y, t)^2/2 can be used in dsolve and eulerlagrange
    pub(crate) fn inline_unknowns(&self, n: &Node, ys: &[String]) -> R<Node> {
        let rec = |a: &Node| self.inline_unknowns(a, ys);
        Ok(match n {
            Node::Ident(s) if !ys.contains(s) => match self.vars.get(s) {
                Some(v) if v.is_scalar() && ys.iter().any(|y| mentions(v, y)) => {
                    let src = format!("({})", show(v));
                    match crate::parser::parse(&src)?.pop() {
                        Some(crate::parser::Stmt::Expr(e)) => e,
                        _ => n.clone(),
                    }
                }
                _ => n.clone(),
            },
            Node::Neg(a) => Node::Neg(Box::new(rec(a)?)),
            Node::Transpose(a) => Node::Transpose(Box::new(rec(a)?)),
            Node::Bin(op, a, b) => Node::Bin(*op, Box::new(rec(a)?), Box::new(rec(b)?)),
            Node::Call(f, args) => Node::Call(f.clone(), args.iter().map(rec).collect::<R<_>>()?),
            Node::Matrix(rows) => Node::Matrix(
                rows.iter().map(|r| r.iter().map(rec).collect::<R<Vec<_>>>()).collect::<R<_>>()?,
            ),
            Node::List(v) => Node::List(v.iter().map(rec).collect::<R<_>>()?),
            other => other.clone(),
        })
    }
}

/// x^p e^(a x) (cos|sin)(b x) * c : (c, p, a, b)
fn forcing_term(t: &Expr, x: &str) -> Option<(Expr, usize, Expr, Expr)> {
    let factors = match t {
        Expr::Mul(f) => f.clone(),
        other => vec![other.clone()],
    };
    let (mut c, mut p, mut a, mut b) = (vec![], 0usize, Expr::zero(), Expr::zero());
    let mut trig = false;
    for f in factors {
        if !f.contains_sym(x) {
            c.push(f);
            continue;
        }
        match &f {
            Expr::Sym(s) if s == x => p += 1,
            Expr::Pow(base, e) if matches!(&**base, Expr::Sym(s) if s == x) => {
                p += e
                    .num()
                    .filter(|n| n.is_exact() && n.is_integer() && !n.is_negative())?
                    .as_int()? as usize;
            }
            Expr::Pow(base, e) if **base == Expr::Const(Const::E) => {
                let k = simplify(&div((**e).clone(), Expr::sym(x)));
                if k.contains_sym(x) {
                    return None;
                }
                a = add2(a, k);
            }
            Expr::Func(fun @ (Func::Sin | Func::Cos), arg) if !trig => {
                let k = simplify(&div((**arg).clone(), Expr::sym(x)));
                if k.contains_sym(x) {
                    return None;
                }
                trig = true;
                b = k;
                if *fun == Func::Sin {
                    // remember sin by a marker coefficient handled by the caller (both get a trial anyway)
                }
            }
            _ => return None,
        }
    }
    Some((mul(c), p, a, b))
}

impl Session {
    /// letters("HELLO") = [7 4 11 11 14], word([7 4 11 11 14]) = "HELLO";
    /// encode("Hi") = the bytes as one number (base 256), decode undoes it
    fn text_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        let text = |v: &Expr| match v {
            Expr::Str(s) => Ok(s.clone()),
            o => Err(format!("{} needs a text in quotes, got {}", f, show(o))),
        };
        match f {
            "letters" => {
                let s = text(&v)?;
                let ks: Vec<Expr> = s
                    .chars()
                    .filter(|c| c.is_ascii_alphabetic())
                    .map(|c| Expr::int((c.to_ascii_uppercase() as u8 - b'A') as i128))
                    .collect();
                if ks.is_empty() {
                    return Err("letters: the text has no letters".into());
                }
                Ok(if ks.len() == 1 {
                    ks[0].clone()
                } else {
                    row(ks)
                })
            }
            "word" => {
                let items: Vec<Expr> = match v {
                    Expr::Matrix(m) => m.into_iter().flatten().collect(),
                    o => vec![o],
                };
                let mut s = String::new();
                for e in &items {
                    let k = int_of(e, f)?;
                    s.push((b'A' + k.rem_euclid(26) as u8) as char);
                }
                Ok(Expr::Str(s))
            }
            "encode" => {
                let s = text(&v)?;
                if s.len() > 100_000 {
                    return Err("encode: at most 100000 characters".into());
                }
                Ok(bnum(BigInt::from_bytes_be(
                    num_bigint::Sign::Plus,
                    s.as_bytes(),
                )))
            }
            _ => {
                let n = big_of(&v, f)?;
                if n.is_negative() {
                    return Err("decode needs a number of 0 or more".into());
                }
                let bytes = if n.is_zero() {
                    vec![]
                } else {
                    n.to_bytes_be().1
                };
                Ok(Expr::Str(String::from_utf8_lossy(&bytes).into_owned()))
            }
        }
    }

    /// Some(result) if f is one of the commands here
    pub(crate) fn extra_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        if let Some(r) = self.lab_call(f, args) {
            return Some(r);
        }
        if let Some(r) = self.algebra_call(f, args) {
            return Some(r);
        }
        let r = match f {
            "isprime" | "nextprime" | "prevprime" | "totient" | "eulerphi" | "sigma" | "isqrt"
            | "primepi" | "prime" | "pollard" => self.int_unary(f, args),
            "ifactor" | "factors" | "divisors" | "primroot" => self.int_info(f, args),
            "primes" => self.primes(args),
            "powmod" => self.powmod(args),
            "modinv" | "multorder" => self.int_binary(f, args),
            "crt" => self.crt(args),
            "real" | "imag" | "conj" | "arg" | "rect" | "polar" => self.complex(f, args),
            "trigexpand" | "trigreduce" => self.trig(f, args),
            "table" => self.table(args),
            "iterate" => self.iterate(args),
            "implicitplot" => self.implicitplot(args),
            "perm" | "compose" | "pinverse" | "porder" | "psign" | "cycles" | "generate"
            | "cayley" => self.perms(f, args),
            "dsolve" => self.dsolve(args),
            "seriessolve" => self.seriessolve(args),
            "letters" | "word" | "encode" | "decode" => self.text_cmd(f, args),
            "orbitdata" => self.orbitdata(args),
            "mandelbrot" | "julia" => self.escape_set(f, args),
            "rows" | "cols" | "length" => (|| {
                let [a] = args else {
                    return Err(format!("{} takes 1 argument", f));
                };
                let (r, c) = match self.eval(a)? {
                    Expr::Matrix(m) => (m.len(), m.first().map_or(0, |r| r.len())),
                    _ => (1, 1),
                };
                Ok(Expr::int(match f {
                    "rows" => r,
                    "cols" => c,
                    _ => r.max(c),
                } as i128))
            })(),
            "polydiv" => self.polydiv(args),
            _ => return None,
        };
        Some(r)
    }

    fn arg_vals(&mut self, args: &[Node]) -> R<Vec<Expr>> {
        args.iter().map(|a| self.eval(a)).collect()
    }

    fn int_unary(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        each(&v, &mut |x| {
            let bn = big_nonneg(x, f)?;
            match f {
                "isprime" => return Ok(Expr::Bool(nt::is_prime(&bn))),
                "nextprime" => return Ok(bnum(nt::next_prime(&bn))),
                "prevprime" => {
                    return Ok(bnum(
                        nt::prev_prime(&bn).ok_or("prevprime: there is no prime below 2")?,
                    ));
                }
                "totient" | "eulerphi" => {
                    if bn.is_zero() {
                        return Err(format!("{}(0) is not defined", f));
                    }
                    return Ok(bnum(nt::totient(&bn).map_err(|e| format!("{}: {}", f, e))?));
                }
                "sigma" => {
                    if bn.is_zero() {
                        return Err("sigma(0) is not defined".into());
                    }
                    return Ok(bnum(
                        nt::divisors(&bn)
                            .map_err(|e| format!("sigma: {}", e))?
                            .iter()
                            .sum(),
                    ));
                }
                "isqrt" => return Ok(bnum(nt::isqrt(&bn))),
                "pollard" => {
                    if bn < BigInt::from(4) || nt::is_prime(&bn) {
                        return Err(format!("pollard: {} has no proper factor", bn));
                    }
                    return Ok(bnum(
                        nt::pollard_rho(&bn)
                            .ok_or("pollard: no factor found in reasonable time")?,
                    ));
                }
                _ => {}
            }
            let n = bn
                .to_u128()
                .ok_or_else(|| format!("{}: {} is too large", f, bn))?;
            Ok(match f {
                "primepi" => {
                    if n > 50_000_000 {
                        return Err("primepi: at most 50000000".into());
                    }
                    uint(nt::primes_upto(n as usize).len() as u128)
                }
                "prime" => {
                    if n == 0 || n > 3_000_000 {
                        return Err("prime(k): k from 1 to 3000000".into());
                    }
                    let mut limit = 20usize.max(
                        (n as f64 * ((n as f64).ln() + (n as f64).ln().ln().max(1.0))) as usize
                            + 10,
                    );
                    loop {
                        let ps = nt::primes_upto(limit);
                        if ps.len() >= n as usize {
                            break uint(ps[n as usize - 1] as u128);
                        }
                        limit *= 2;
                    }
                }
                _ => unreachable!(),
            })
        })
    }

    fn int_info(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        let n = big_of(&v, f)?;
        let two = BigInt::from(2);
        let err = |e: String| format!("{}: {}", f, e);
        match f {
            "ifactor" => {
                if n.abs() <= BigInt::one() {
                    return Ok(if n.is_zero() {
                        Expr::Matrix(vec![vec![Expr::int(0), Expr::int(1)]])
                    } else if n.is_negative() {
                        Expr::Matrix(vec![vec![Expr::int(-1), Expr::int(1)]])
                    } else {
                        Expr::Matrix(vec![])
                    });
                }
                let (m, t) = self.factor_integer(a, &n).map_err(err)?;
                self.note(format!(
                    "{} = {}\n\nOne row per prime: the prime and its exponent.",
                    n, t
                ));
                Ok(m)
            }
            "factors" => {
                if n < two {
                    return Err("factors: give a whole number of 2 or more".into());
                }
                let fs = nt::factorize(&n).map_err(err)?;
                Ok(Expr::Matrix(
                    fs.into_iter()
                        .map(|(p, e)| vec![bnum(p), Expr::int(e as i128)])
                        .collect(),
                ))
            }
            "divisors" => {
                if !n.is_positive() {
                    return Err("divisors: give a positive whole number".into());
                }
                let ds = nt::divisors(&n).map_err(err)?;
                if ds.len() > 100_000 {
                    return Err(format!(
                        "divisors: {} has {} divisors, too many to list",
                        n,
                        ds.len()
                    ));
                }
                Ok(row(ds.into_iter().map(bnum).collect()))
            }
            _ => {
                if n < two {
                    return Err("primroot: give a whole number of 2 or more".into());
                }
                nt::primitive_root(&n)
                    .map_err(err)?
                    .map(bnum)
                    .ok_or_else(|| format!("{} has no primitive root", n))
            }
        }
    }

    fn primes(&mut self, args: &[Node]) -> R<Expr> {
        let v = self.arg_vals(args)?;
        let (lo, hi) = match v.as_slice() {
            [b] => (0, nonneg(b, "primes")?),
            [a, b] => (nonneg(a, "primes")?, nonneg(b, "primes")?),
            _ => {
                return Err(
                    "usage: primes(n) lists the primes up to n; primes(a, b) those from a to b"
                        .into(),
                );
            }
        };
        if hi > 10_000_000 {
            return Err("primes: at most 10000000".into());
        }
        let ps: Vec<Expr> = nt::primes_upto(hi as usize)
            .into_iter()
            .filter(|&p| p as u128 >= lo)
            .map(|p| Expr::int(p as i128))
            .collect();
        if ps.len() > 100_000 {
            return Err(
                "primes: that's more than 100000 primes; use primepi(n) to count them".into(),
            );
        }
        Ok(row(ps))
    }

    fn powmod(&mut self, args: &[Node]) -> R<Expr> {
        let v = self.arg_vals(args)?;
        let [a, b, m] = v.as_slice() else {
            return Err("usage: powmod(a, b, n) is a^b mod n".into());
        };
        let (a, b, m) = (
            big_of(a, "powmod")?,
            big_of(b, "powmod")?,
            big_of(m, "powmod")?,
        );
        if !m.is_positive() {
            return Err("powmod: the modulus must be positive".into());
        }
        let base = if b.is_negative() {
            nt::modinv(&a, &m).ok_or_else(|| format!("powmod: {} has no inverse mod {}", a, m))?
        } else {
            a
        };
        Ok(bnum(nt::powmod(&base, &b.abs(), &m)))
    }

    fn int_binary(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let v = self.arg_vals(args)?;
        let [a, m] = v.as_slice() else {
            return Err(format!("usage: {}(a, n)", f));
        };
        let (a, m) = (big_of(a, f)?, big_of(m, f)?);
        if !m.is_positive() {
            return Err(format!("{}: the modulus must be positive", f));
        }
        match f {
            "modinv" => nt::modinv(&a, &m).map(bnum).ok_or_else(|| {
                format!(
                    "modinv: {} has no inverse mod {} (they share a factor)",
                    a, m
                )
            }),
            _ => Ok(bnum(
                nt::mult_order(&num_integer::Integer::mod_floor(&a, &m), &m)
                    .map_err(|e| format!("multorder: {}", e))?,
            )),
        }
    }

    fn crt(&mut self, args: &[Node]) -> R<Expr> {
        let v = self.arg_vals(args)?;
        let [r, m] = v.as_slice() else {
            return Err("usage: crt([r1, r2, ...], [m1, m2, ...]) solves x = r_i mod m_i".into());
        };
        let flat = |e: &Expr| -> R<Vec<BigInt>> {
            match e {
                Expr::Matrix(m) => m.iter().flatten().map(|x| big_of(x, "crt")).collect(),
                x => Ok(vec![big_of(x, "crt")?]),
            }
        };
        let (rs, ms) = (flat(r)?, flat(m)?);
        if rs.len() != ms.len() {
            return Err("crt: give as many remainders as moduli".into());
        }
        if ms.iter().any(|m| !m.is_positive()) {
            return Err("crt: the moduli must be positive".into());
        }
        let (x, _) = nt::crt(&rs, &ms).ok_or("crt: the moduli must have no common factors")?;
        Ok(bnum(x))
    }

    fn complex(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        each(&v, &mut |z| {
            let (re, im) = tidy_parts(z)?;
            Ok(match f {
                "real" => re,
                "imag" => im,
                "conj" => add2(re, mul2(neg(im), Expr::Const(Const::I))),
                "rect" => add2(re, mul2(im, Expr::Const(Const::I))),
                "arg" => angle(&re, &im),
                _ => {
                    let r = simplify(&sqrt(add2(
                        pow(re.clone(), Expr::int(2)),
                        pow(im.clone(), Expr::int(2)),
                    )));
                    row(vec![r, angle(&re, &im)])
                }
            })
        })
    }

    fn trig(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        each(&v, &mut |e| {
            Ok(if f == "trigexpand" {
                trig_expand(e)
            } else {
                trig_reduce(e)
            })
        })
    }

    /// evaluate a body for k = a, a+step, ..., b (the body sees k as a number)
    pub(crate) fn with_values(&mut self, body: &Node, k: &str, vals: &[Expr]) -> R<Vec<Expr>> {
        let saved = self.vars.remove(k);
        let hidden = self.hidden.clone();
        self.hidden.retain(|h| h != k);
        let mut out = vec![];
        let mut err = None;
        for v in vals {
            self.vars.insert(k.to_string(), v.clone());
            match self.eval(body) {
                Ok(r) => out.push(r),
                Err(e) => {
                    err = Some(e);
                    break;
                }
            }
        }
        self.hidden = hidden;
        match saved {
            Some(v) => self.vars.insert(k.to_string(), v),
            None => self.vars.remove(k),
        };
        match err {
            Some(e) => Err(e),
            None => Ok(out),
        }
    }

    fn table(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: table(expr, k, a, b) lists expr for k = a, a+1, ..., b; table(expr, k, a, b, step); table(expr, k, [v1, v2, ...])";
        let (body, k) = match args {
            [b, k, ..] if args.len() >= 3 => (b, name(k, "table: k")?),
            _ => return Err(usage.into()),
        };
        let vals: Vec<Expr> = match &args[2..] {
            [list] => match self.eval(list)? {
                Expr::Matrix(m) => m.into_iter().flatten().collect(),
                // [5] is just 5: a list with one value
                v @ Expr::Num(_) => vec![v],
                _ => return Err(usage.into()),
            },
            [a, b] | [a, b, _] => {
                let a = self.eval(a)?;
                let b = self.eval(b)?;
                let s = match args.get(4) {
                    Some(s) => self.eval(s)?,
                    None => Expr::one(),
                };
                let (an, bn, sn) = (
                    a.num().ok_or(usage)?,
                    b.num().ok_or(usage)?,
                    s.num().ok_or(usage)?,
                );
                if sn.is_zero() {
                    return Err("table: the step can't be 0".into());
                }
                let count = ((bn.sub(&an).to_f64() / sn.to_f64()) + 1e-9).floor();
                if count < 0.0 {
                    return Ok(Expr::Matrix(vec![vec![]]));
                }
                if count > 100_000.0 {
                    return Err("table: at most 100000 entries".into());
                }
                (0..=count as i128)
                    .map(|j| Expr::Num(an.add(sn.mul(Number::int(j)))))
                    .collect()
            }
            _ => return Err(usage.into()),
        };
        let out = self.with_values(body, &k, &vals)?;
        // rows stack into a matrix
        if !out.is_empty()
            && out
                .iter()
                .all(|r| matches!(r, Expr::Matrix(m) if m.len() == 1))
        {
            let rows: Vec<Vec<Expr>> = out
                .into_iter()
                .map(|r| {
                    if let Expr::Matrix(mut m) = r {
                        m.remove(0)
                    } else {
                        unreachable!()
                    }
                })
                .collect();
            if rows.iter().all(|r| r.len() == rows[0].len()) {
                return Ok(Expr::Matrix(rows));
            }
            return Err("table: the rows have different lengths".into());
        }
        if out
            .iter()
            .any(|v| {
                !v.is_scalar()
                    && !matches!(v, Expr::Bool(_) | Expr::Str(_))
                    && crate::ffield::as_elem(v).is_none()
                    && crate::qring::as_elem(v).is_none()
                    && crate::quat::as_quat(v).is_none()
            })
        {
            return Err("table: each entry must be a number, an expression or a row".into());
        }
        Ok(row(out))
    }

    fn num_args(&mut self, f: &str, args: &[Node]) -> R<Vec<f64>> {
        let mut out = vec![];
        for a in args {
            let v = numeric::value(&self.eval(a)?);
            if !v.is_finite() {
                return Err(format!("{}: {} is not a number", f, show(&self.eval(a)?)));
            }
            out.push(v);
        }
        Ok(out)
    }

    /// orbitdata(rule, x, x0, r, r0, r1, nr, skip, keep): for nr+1 values of r
    /// from r0 to r1, iterate x -> rule(x, r) from x0, skip the first steps and
    /// keep the next ones, as rows [r x]. Computed with decimals, fast.
    fn orbitdata(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: orbitdata(rule, x, x0, r, r0, r1, nr, skip, keep): rows [r x] of the orbits of x -> rule for nr+1 values of r";
        let [rule, x, x0, r, rest @ ..] = args else {
            return Err(usage.into());
        };
        if rest.len() != 5 {
            return Err(usage.into());
        }
        let (xn, rn) = (name(x, "orbitdata: x")?, name(r, "orbitdata: r")?);
        let body = self.eval_hidden(rule, &[xn.clone(), rn.clone()])?;
        let code = numeric::compile(&body, &[xn.as_str(), rn.as_str()]);
        let mut nums = self.num_args("orbitdata", std::slice::from_ref(x0))?;
        nums.extend(self.num_args("orbitdata", rest)?);
        let (start, r0, r1) = (nums[0], nums[1], nums[2]);
        let (nr, skip, keep) = (nums[3] as usize, nums[4] as usize, nums[5] as usize);
        if nr == 0 || keep == 0 || (nr + 1) * keep > 200_000 || skip > 1_000_000 {
            return Err("orbitdata: at most 200000 points (nr+1 times keep)".into());
        }
        let mut rows = vec![];
        for j in 0..=nr {
            let rv = r0 + (r1 - r0) * j as f64 / nr as f64;
            let mut xv = start;
            for _ in 0..skip {
                xv = numeric::run(&code, &[xv, rv]);
            }
            for _ in 0..keep {
                xv = numeric::run(&code, &[xv, rv]);
                if xv.is_finite() && xv.abs() < 1e12 {
                    rows.push(vec![Expr::float(rv), Expr::float(xv)]);
                }
            }
        }
        Ok(Expr::Matrix(rows))
    }

    /// mandelbrot(re0, re1, im0, im1, n) and julia(c, re0, re1, im0, im1, n):
    /// the grid points (n across) whose orbit under z -> z^2 + c stays within
    /// |z| <= 2 for 200 steps, as rows [re im].
    fn escape_set(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let usage = if f == "mandelbrot" {
            "usage: mandelbrot(re0, re1, im0, im1, n): the points c of an n-wide grid for which z -> z^2 + c, from z = 0, stays bounded"
        } else {
            "usage: julia(c, re0, re1, im0, im1, n): the points z0 of an n-wide grid whose orbit under z -> z^2 + c stays bounded"
        };
        let (c, rest) = if f == "julia" {
            let Some((c, rest)) = args.split_first() else {
                return Err(usage.into());
            };
            let cv = self.eval(c)?;
            let (re, im) = parts(&cv)?;
            let (re, im) = (
                numeric::value(&calc::evalf(&re)),
                numeric::value(&calc::evalf(&im)),
            );
            if !(re.is_finite() && im.is_finite()) {
                return Err(format!("julia: c must be a number, got {}", show(&cv)));
            }
            (Some((re, im)), rest)
        } else {
            (None, args)
        };
        if rest.len() != 5 {
            return Err(usage.into());
        }
        let v = self.num_args(f, rest)?;
        let (re0, re1, im0, im1) = (v[0], v[1], v[2], v[3]);
        let nx = v[4] as usize;
        let ny = ((nx as f64) * (im1 - im0) / (re1 - re0)).round().max(1.0) as usize;
        if nx < 2 || nx * ny > 400_000 {
            return Err(format!("{}: n from 2 to about 600", f));
        }
        let mut rows = vec![];
        for j in 0..=ny {
            let y = im0 + (im1 - im0) * j as f64 / ny as f64;
            for i in 0..=nx {
                let xv = re0 + (re1 - re0) * i as f64 / nx as f64;
                let ((mut zr, mut zi), (cr, ci)) = match c {
                    None => ((0.0, 0.0), (xv, y)),
                    Some(c) => ((xv, y), c),
                };
                let mut inside = true;
                for _ in 0..200 {
                    let t = zr * zr - zi * zi + cr;
                    zi = 2.0 * zr * zi + ci;
                    zr = t;
                    if zr * zr + zi * zi > 4.0 {
                        inside = false;
                        break;
                    }
                }
                if inside {
                    rows.push(vec![Expr::float(xv), Expr::float(y)]);
                }
            }
        }
        if rows.is_empty() {
            return Err(format!("{}: no point of the grid stays bounded", f));
        }
        Ok(Expr::Matrix(rows))
    }

    fn iterate(&mut self, args: &[Node]) -> R<Expr> {
        let [body, x, x0, n] = args else {
            return Err("usage: iterate(f, x, x0, n) lists x0, f(x0), f(f(x0)), ... (n steps), where f is an expression in x".into());
        };
        let x = name(x, "iterate: x")?;
        let mut cur = self.eval(x0)?;
        let n = int_of(&self.eval(n)?, "iterate")?;
        if !(0..=100_000).contains(&n) {
            return Err("iterate: from 0 to 100000 steps".into());
        }
        let mut out = vec![cur.clone()];
        for _ in 0..n {
            let mut next = self
                .with_values(body, &x, std::slice::from_ref(&cur))?
                .remove(0);
            if next.size() > 400 {
                next = calc::evalf(&next);
            }
            out.push(next.clone());
            cur = next;
        }
        Ok(row(out))
    }

    fn implicitplot(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: implicitplot(F, x, x0, x1, y, y0, y1) draws the curve F = 0 (or an equation such as x^2 + y^2 == 1) for x0 <= x <= x1, y0 <= y <= y1; a list of equations draws several curves";
        let [fnode, x, x0, x1, y, y0, y1] = args else {
            return Err(usage.into());
        };
        let (xn, yn) = (name(x, "implicitplot: x")?, name(y, "implicitplot: y")?);
        let fv = self.eval_hidden(fnode, &[xn.clone(), yn.clone()])?;
        let curves: Vec<Expr> = match fv {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            e => vec![e],
        };
        let mut lims = vec![];
        for n in [x0, x1, y0, y1] {
            let v = self.eval(n)?;
            let f = numeric::value(&v);
            if !f.is_finite() {
                return Err(usage.into());
            }
            lims.push(f);
        }
        let labels: Vec<String> = match fnode {
            Node::Matrix(rows) => rows.iter().flatten().map(crate::parser::source).collect(),
            n => vec![crate::parser::source(n)],
        };
        let mut series = vec![];
        for (i, c) in curves.iter().enumerate() {
            let g = equation_lhs(c);
            let free = numeric::free(&g, &[xn.as_str(), yn.as_str()]);
            if !free.is_empty() {
                return Err(format!(
                    "implicitplot: give values to {} first",
                    free.join(", ")
                ));
            }
            let code = numeric::compile(&g, &[xn.as_str(), yn.as_str()]);
            let pts = contour(
                &|a, b| numeric::run(&code, &[a, b]),
                lims[0],
                lims[1],
                lims[2],
                lims[3],
                200,
            );
            let label = labels.get(i).cloned().unwrap_or_else(|| show(c));
            series.push(Series {
                label,
                points: pts,
                ..Default::default()
            });
        }
        let mut fig = Figure::new(series, &xn, &yn, true);
        // keep the window the user asked for: add invisible corner points
        fig.series.push(Series {
            label: String::new(),
            points: vec![],
            ..Default::default()
        });
        fig.series.pop();
        Ok(Expr::Obj(Rc::new(Obj::Figure(fig))))
    }

    fn perms(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let v = self.arg_vals(args)?;
        match (f, v.as_slice()) {
            ("perm", [Expr::Str(s), n]) => {
                let n = int_of(n, "perm")?;
                if !(1..=1000).contains(&n) {
                    return Err("perm: n from 1 to 1000".into());
                }
                Ok(perm_expr(&parse_cycles(s, n as usize)?))
            }
            ("perm", _) => Err("usage: perm(\"(1 2 3)(4 5)\", n): the permutation of 1..n with these cycles".into()),
            ("compose", vs) if vs.len() >= 2 => {
                let mut acc = perm_of(&vs[vs.len() - 1], f)?;
                for p in vs[..vs.len() - 1].iter().rev() {
                    acc = compose(&perm_of(p, f)?, &acc);
                }
                Ok(perm_expr(&acc))
            }
            ("compose", _) => Err("usage: compose(p, q) is p after q: first q, then p".into()),
            ("pinverse", [p]) => {
                let p = perm_of(p, f)?;
                let mut q = vec![0; p.len()];
                for (i, &k) in p.iter().enumerate() {
                    q[k - 1] = i + 1;
                }
                Ok(perm_expr(&q))
            }
            ("porder", [p]) => Ok(uint(perm_order(&perm_of(p, f)?))),
            ("psign", [p]) => Ok(Expr::int(perm_sign(&perm_of(p, f)?))),
            ("cycles", [p]) => Ok(Expr::Str(cycles_text(&perm_of(p, f)?))),
            ("generate", gens) if !gens.is_empty() => {
                let gs: Vec<Vec<usize>> = gens.iter().map(|g| perm_of(g, f)).collect::<R<_>>()?;
                let n = gs.iter().map(|g| g.len()).max().unwrap();
                let pad = |g: &Vec<usize>| -> Vec<usize> { (1..=n).map(|i| if i <= g.len() { g[i - 1] } else { i }).collect() };
                let gs: Vec<Vec<usize>> = gs.iter().map(pad).collect();
                let id: Vec<usize> = (1..=n).collect();
                let mut elems = vec![id.clone()];
                let mut frontier = vec![id];
                while let Some(e) = frontier.pop() {
                    for g in &gs {
                        let h = compose(g, &e);
                        if !elems.contains(&h) {
                            if elems.len() >= 5040 {
                                return Err("generate: the group has more than 5040 elements".into());
                            }
                            elems.push(h.clone());
                            frontier.push(h);
                        }
                    }
                }
                elems.sort();
                Ok(Expr::Matrix(elems.iter().map(|p| p.iter().map(|&k| Expr::int(k as i128)).collect()).collect()))
            }
            ("generate", _) => Err("usage: generate(g1, g2, ...) lists every permutation the generators make (one per row)".into()),
            ("cayley", [Expr::Matrix(rows)]) => {
                let el: Vec<Vec<usize>> = rows.iter().map(|r| perm_of(&row(r.clone()), f)).collect::<R<_>>()?;
                let mut table = vec![];
                for a in &el {
                    let mut r = vec![];
                    for b in &el {
                        let c = compose(a, b);
                        let k = el.iter().position(|e| *e == c).ok_or("cayley: the rows are not closed under composition (not a group)")?;
                        r.push(Expr::int(k as i128 + 1));
                    }
                    table.push(r);
                }
                Ok(Expr::Matrix(table))
            }
            ("cayley", _) => Err("usage: cayley(G) where G lists a group's permutations, one per row (see generate)".into()),
            _ => Err(format!("{} takes a permutation such as [2, 3, 1]", f)),
        }
    }

    /// [quotient, remainder] of polynomial long division
    fn polydiv(&mut self, args: &[Node]) -> R<Expr> {
        let [p, q, x] = args else {
            return Err("usage: polydiv(p, q, x) gives [quotient, remainder] with p = quotient*q + remainder".into());
        };
        let x = name(x, "polydiv: x")?;
        let pv = self.eval_hidden(p, std::slice::from_ref(&x))?;
        let qv = self.eval_hidden(q, std::slice::from_ref(&x))?;
        let mut a =
            cas::poly_coeffs(&expand(&pv), &x).ok_or("polydiv: p must be a polynomial in x")?;
        let b = cas::poly_coeffs(&expand(&qv), &x).ok_or("polydiv: q must be a polynomial in x")?;
        let b: Vec<Expr> = b.into_iter().map(|c| simplify(&c)).collect();
        let mut bl = b.clone();
        while bl.last().is_some_and(cas::is_zero) {
            bl.pop();
        }
        if bl.is_empty() {
            return Err("polydiv: division by zero".into());
        }
        let m = bl.len() - 1;
        let xs = Expr::sym(&x);
        let mut quot = vec![Expr::zero(); a.len().saturating_sub(m).max(1)];
        while a.len() > m {
            let lead = simplify(&div(a[a.len() - 1].clone(), bl[m].clone()));
            let k = a.len() - 1 - m;
            for j in 0..=m {
                a[k + j] = simplify(&sub(a[k + j].clone(), mul2(lead.clone(), bl[j].clone())));
            }
            quot[k] = lead;
            a.pop();
        }
        let poly = |c: &[Expr]| {
            add(c
                .iter()
                .enumerate()
                .map(|(k, v)| mul2(v.clone(), pow(xs.clone(), Expr::int(k as i128))))
                .collect())
        };
        Ok(row(vec![poly(&quot), poly(&a)]))
    }

    fn dsolve(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: dsolve(ode, y, x) or dsolve(ode, y, x, x0, [y(x0), d(y, x0), ...]); write the equation with y, d(y, x), d(y, x, 2) (e.g. d(y, x, 2) + y == 0)";
        if matches!(args.first(), Some(Node::Matrix(_))) {
            return self.dsolve_system(args);
        }
        if args.len() != 3 && args.len() != 5 {
            return Err(usage.into());
        }
        let y = name(&args[1], "dsolve: y")?;
        let x = name(&args[2], "dsolve: x")?;
        let mut order = 0;
        let node = self.inline_unknowns(&args[0], std::slice::from_ref(&y))?;
        let node = ode_node(&node, &y, &x, &mut order)?;
        if order == 0 {
            return Err(format!("dsolve: the equation has no derivative of {}", y));
        }
        let mut hid: Vec<String> = (0..=order).map(|k| format!("{}__{}", y, k)).collect();
        hid.push(x.clone());
        let e = equation_lhs(&self.eval_hidden(&node, &hid)?);
        let ys: Vec<Expr> = (0..=order)
            .map(|k| Expr::sym(&format!("{}__{}", y, k)))
            .collect();
        // linear: E = sum a_k y^(k) - g(x)
        let mut a = vec![];
        for k in 0..=order {
            let c = simplify(&deriv(&e, &format!("{}__{}", y, k)));
            if ys.iter().any(|s| c.contains_sym(&show(s))) {
                if order == 1 {
                    return self.dsolve_separable(&e, &y, &x, args);
                }
                return Err("dsolve: only linear equations (no y^2, y*y', sin(y), ...) of order 2 and higher can be solved exactly; use odeplot for a numeric solution".into());
            }
            a.push(c);
        }
        let mut g = e.clone();
        for k in 0..=order {
            g = subst(&g, &format!("{}__{}", y, k), &Expr::zero());
        }
        let g = simplify(&neg(g));
        let xs = Expr::sym(&x);
        let constant = a.iter().all(|c| !c.contains_sym(&x));
        // y^(n) = f(x) alone: integrate n times (v' = -u/m for a rocket)
        let direct = a[..order].iter().all(|c| c.is_zero()) && !a[order].is_zero();
        let integrated = if direct {
            let mut f = simplify(&div(g.clone(), a[order].clone()));
            let mut cs = vec![];
            let mut ok = true;
            for k in 0..order {
                match cas::antiderivative(&f, &x) {
                    Some(i) => {
                        let c = Expr::sym(&format!("C{}", order - k));
                        f = simplify(&add2(i, c.clone()));
                        cs.push(c);
                    }
                    None => {
                        ok = false;
                        break;
                    }
                }
            }
            cs.reverse();
            if ok { Some((f, cs)) } else { None }
        } else {
            None
        };
        let (sol, consts) = if let Some(r) = integrated {
            r
        } else if constant {
            self.dsolve_constant(&a, &g, &x)?
        } else if order == 1 {
            // y' + p y = q: integrating factor
            let p = simplify(&div(a[0].clone(), a[1].clone()));
            let q = simplify(&div(g.clone(), a[1].clone()));
            let ip = cas::antiderivative(&p, &x)
                .ok_or("dsolve: can't integrate the coefficient of y")?;
            let mu = simplify(&exp(ip));
            let iq = cas::antiderivative(&simplify(&mul2(mu.clone(), q)), &x)
                .ok_or("dsolve: can't do the integral for this right-hand side")?;
            let c1 = Expr::sym("C1");
            (simplify(&div(add2(iq, c1.clone()), mu)), vec![c1])
        } else if let Some(b) = euler_coeffs(&a, &x) {
            // Cauchy-Euler: x = e^u turns it into constant coefficients in u
            let alpha = euler_to_constant(&b);
            let u = "u__";
            let eu = exp(Expr::sym(u));
            let an = a[order].clone();
            let gu = simplify(&subst(
                &div(mul2(g.clone(), pow(xs.clone(), Expr::int(order as i128))), an),
                &x,
                &eu,
            ));
            let (su, cs) = self.dsolve_constant(&alpha, &gu, u)?;
            let back = crate::ode2::prefer_expanded(&subst(&su, u, &func(Func::Log, xs.clone())));
            self.note(format!(
                "Cauchy–Euler equation (the coefficient of y^(k) is a multiple of x^k): try y = x^r, or substitute x = e^u to get constant coefficients. Characteristic polynomial in r: {}.",
                show(&simplify(&add(alpha.iter().enumerate().map(|(k, c)| mul2(c.clone(), pow(Expr::sym("r"), Expr::int(k as i128)))).collect())))
            ));
            (back, cs)
        } else {
            return Err(
                "dsolve: equations of order 2 and higher need constant coefficients, or Cauchy–Euler form (x^2 y'' + b x y' + c y)".into(),
            );
        };
        let _ = xs;
        if args.len() == 3 {
            return Ok(sol);
        }
        // initial conditions
        let x0 = self.eval(&args[3])?;
        let ics: Vec<Expr> = match self.eval(&args[4])? {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            v => vec![v],
        };
        if ics.len() != order {
            return Err(format!(
                "dsolve: a order-{} equation needs {} start values: y(x0){}",
                order,
                order,
                if order > 1 { ", d(y, x0), ..." } else { "" }
            ));
        }
        let mut eqs = vec![];
        let mut d = sol.clone();
        for v in &ics {
            eqs.push(sub(subst(&d, &x, &x0), v.clone()));
            d = deriv(&d, &x);
        }
        let names: Vec<String> = consts.iter().map(show).collect();
        let vals = cas::solve_linear(&eqs, &names)?;
        let mut r = sol;
        for (n, v) in names.iter().zip(vals) {
            r = subst(&r, n, &v);
        }
        Ok(simplify(&r))
    }

    /// power series solution about x = 0 of a linear equation
    fn seriessolve(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: seriessolve(ode, y, x, n) or seriessolve(ode, y, x, n, [y(0), d(y, 0), ...]): the power series of y up to x^n";
        if args.len() != 4 && args.len() != 5 {
            return Err(usage.into());
        }
        let y = name(&args[1], "seriessolve: y")?;
        let x = name(&args[2], "seriessolve: x")?;
        let n = match self.eval(&args[3])?.num().and_then(|k| k.as_int()) {
            Some(k) if (1..=40).contains(&k) => k as usize,
            _ => return Err("seriessolve: n must be a whole number from 1 to 40".into()),
        };
        let mut order = 0;
        let node = self.inline_unknowns(&args[0], std::slice::from_ref(&y))?;
        let node = ode_node(&node, &y, &x, &mut order)?;
        if order == 0 {
            return Err(format!("seriessolve: the equation has no derivative of {}", y));
        }
        if n < order {
            return Err(format!("seriessolve: n must be at least the order {}", order));
        }
        let mut hid: Vec<String> = (0..=order).map(|k| format!("{}__{}", y, k)).collect();
        hid.push(x.clone());
        let e = equation_lhs(&self.eval_hidden(&node, &hid)?);
        let mut a = vec![];
        for k in 0..=order {
            let c = simplify(&deriv(&e, &format!("{}__{}", y, k)));
            if hid[..=order].iter().any(|h| c.contains_sym(h)) {
                return Err("seriessolve: only linear equations can be solved with a power series".into());
            }
            a.push(c);
        }
        let mut g = e.clone();
        for k in 0..=order {
            g = subst(&g, &format!("{}__{}", y, k), &Expr::zero());
        }
        let g = simplify(&neg(g));
        if cas::is_zero(&subst(&a[order], &x, &Expr::zero())) {
            return Err(format!(
                "seriessolve: x = 0 is a singular point (the coefficient of the highest derivative, {}, vanishes there); a plain power series need not exist (Frobenius method)",
                show(&a[order])
            ));
        }
        // the coefficients as polynomials up to degree n
        let trunc = |f: &Expr| -> R<Expr> {
            if cas::poly_coeffs(f, &x).is_some() || !f.contains_sym(&x) {
                Ok(f.clone())
            } else {
                cas::taylor(f, &x, &Expr::zero(), n)
            }
        };
        let xs = Expr::sym(&x);
        let cs: Vec<Expr> = (0..=n).map(|k| Expr::sym(&format!("a{}", k))).collect();
        let yser = add((0..=n).map(|k| mul2(cs[k].clone(), pow(xs.clone(), Expr::int(k as i128)))).collect());
        let mut acc = vec![];
        let mut d = yser.clone();
        for ak in a.iter() {
            acc.push(mul2(trunc(ak)?, d.clone()));
            d = deriv(&d, &x);
        }
        acc.push(neg(trunc(&g)?));
        let resid = cas::poly_coeffs(&add(acc), &x).ok_or("seriessolve: the coefficients must have power series at 0")?;
        let eqs: Vec<Expr> = (0..=n - order).map(|k| resid.get(k).cloned().unwrap_or(Expr::zero())).collect();
        let unknowns: Vec<String> = (order..=n).map(|k| format!("a{}", k)).collect();
        let vals = cas::solve_linear(&eqs, &unknowns)?;
        let mut coef: Vec<Expr> = cs[..order].to_vec();
        coef.extend(vals.iter().map(simplify));
        // start values y^(k)(0) = k! a_k
        if args.len() == 5 {
            let ics: Vec<Expr> = match self.eval(&args[4])? {
                Expr::Matrix(m) => m.into_iter().flatten().collect(),
                v => vec![v],
            };
            if ics.len() != order {
                return Err(format!("seriessolve: an order-{} equation needs {} start values", order, order));
            }
            let mut f = 1i128;
            for (k, v) in ics.iter().enumerate() {
                if k > 0 {
                    f *= k as i128;
                }
                let ak = simplify(&div(v.clone(), Expr::int(f)));
                coef = coef.iter().map(|c| simplify(&subst(c, &format!("a{}", k), &ak))).collect();
            }
        }
        // the answer grouped by the free coefficients a0, a1, ...
        let free: Vec<String> = (0..order).map(|k| format!("a{}", k)).filter(|_| args.len() == 4).collect();
        let mut parts = vec![];
        let term = |c: &Expr, k: usize| mul2(c.clone(), pow(xs.clone(), Expr::int(k as i128)));
        for f in &free {
            let mut ser = vec![];
            for (k, c) in coef.iter().enumerate() {
                let ck = simplify(&deriv(c, f));
                if !cas::is_zero(&ck) {
                    ser.push(term(&ck, k));
                }
            }
            parts.push(mul2(Expr::sym(f), add(ser)));
        }
        let mut rest = vec![];
        for (k, c) in coef.iter().enumerate() {
            let mut r = c.clone();
            for f in &free {
                r = subst(&r, f, &Expr::zero());
            }
            let r = simplify(&r);
            if !cas::is_zero(&r) {
                rest.push(term(&r, k));
            }
        }
        if !rest.is_empty() {
            parts.push(add(rest));
        }
        let recur = (order..=n.min(order + 3))
            .map(|k| format!("a{} = {}", k, show(&coef[k])))
            .collect::<Vec<_>>()
            .join(", ");
        self.note(format!(
            "Power series y = a0 + a1 x + a2 x^2 + ... about x = 0: substitute it, collect powers of x and set each coefficient to zero. That gives {}, and so on. {}The answer is exact up to x^{}; the next terms are left out.",
            recur,
            if args.len() == 4 { "a0 = y(0) and a1 = y'(0) stay free. " } else { "" },
            n
        ));
        Ok(add(parts))
    }

    /// dsolve for y' = f(x) g(y); e is the equation with y__0, y__1
    fn dsolve_separable(&mut self, e: &Expr, y: &str, x: &str, args: &[Node]) -> R<Expr> {
        let (y0n, y1n) = (format!("{}__0", y), format!("{}__1", y));
        let c = simplify(&deriv(e, &y1n));
        if c.contains_sym(&y1n) {
            return Err("dsolve: write the first-order equation so it can be solved for y' (y' = f(x, y))".into());
        }
        let r = simplify(&div(neg(subst(e, &y1n, &Expr::zero())), c));
        let r = subst(&r, &y0n, &Expr::sym(y));
        // decimals: solve with exact fractions, report decimals
        let floats = crate::vcalc::has_float(&r);
        let r = if floats { crate::vcalc::exactify(&r) } else { r };
        let ic = if args.len() == 5 {
            let x0 = self.eval(&args[3])?;
            let v = match self.eval(&args[4])? {
                Expr::Matrix(m) => m.into_iter().flatten().collect::<Vec<_>>(),
                v => vec![v],
            };
            if v.len() != 1 {
                return Err("dsolve: a first-order equation needs one start value y(x0)".into());
            }
            Some((x0, v[0].clone()))
        } else {
            None
        };
        let ic = ic.map(|(a, b)| if floats { (crate::vcalc::exactify(&a), crate::vcalc::exactify(&b)) } else { (a, b) });
        let (sol, notes) = crate::ode2::separable(&r, y, x, ic.as_ref().map(|(a, b)| (a, b)))?;
        self.note(notes.join(" "));
        let back = |e: Expr| if floats { crate::calc::evalf(&e) } else { e };
        Ok(match sol {
            crate::ode2::Separable::Explicit(s) => back(s),
            crate::ode2::Separable::Implicit(l, r) => Expr::Apply("==".into(), vec![back(l), back(r)]),
        })
    }

    /// dsolve([x' == a x + b y, y' == c x + d y], [x, y], t[, t0, [x0, y0]])
    fn dsolve_system(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: dsolve([d(x, t) == ..., d(y, t) == ...], [x, y], t) or dsolve([...], [x, y], t, t0, [x(t0), y(t0)])";
        if args.len() != 3 && args.len() != 5 {
            return Err(usage.into());
        }
        let Node::Matrix(rows) = &args[0] else { return Err(usage.into()) };
        let eqs: Vec<Node> = rows.iter().flatten().cloned().collect();
        let Node::Matrix(vr) = &args[1] else { return Err(usage.into()) };
        let vars: Vec<String> = vr.iter().flatten().map(|v| name(v, "dsolve: unknown")).collect::<R<_>>()?;
        let t = name(&args[2], "dsolve: t")?;
        if eqs.len() != vars.len() {
            return Err(format!("dsolve: {} equations for {} unknowns", eqs.len(), vars.len()));
        }
        let mut hid = vec![t.clone()];
        for v in &vars {
            hid.push(format!("{}__0", v));
            hid.push(format!("{}__1", v));
        }
        let n = vars.len();
        let mut m: Vec<Vec<Expr>> = vec![];
        let mut a: Vec<Vec<Expr>> = vec![];
        let mut b = vec![];
        for q in &eqs {
            let mut node = self.inline_unknowns(q, &vars)?;
            for v in &vars {
                let mut order = 0;
                node = ode_node(&node, v, &t, &mut order)?;
                if order > 1 {
                    return Err("dsolve: a system must be first order (d(x, t), d(y, t)); write d(y, t, 2) = ... as d(y, t) = v, d(v, t) = ...".into());
                }
            }
            let e = equation_lhs(&self.eval_hidden(&node, &hid)?);
            let mut mr = vec![];
            let mut ar = vec![];
            for v in &vars {
                let d1 = simplify(&deriv(&e, &format!("{}__1", v)));
                let d0 = simplify(&deriv(&e, &format!("{}__0", v)));
                for c in [&d1, &d0] {
                    if hid.iter().any(|h| c.contains_sym(h)) {
                        return Err("dsolve: only linear systems with constant coefficients can be solved exactly; use odeplot for a numeric solution".into());
                    }
                }
                mr.push(d1);
                ar.push(neg(d0));
            }
            let mut g = e.clone();
            for h in &hid[1..] {
                g = subst(&g, h, &Expr::zero());
            }
            let g = simplify(&neg(g));
            if g.contains_sym(&t) {
                return Err("dsolve: forcing that depends on t is not supported in systems; use laplace on each equation".into());
            }
            m.push(mr);
            a.push(ar);
            b.push(g);
        }
        // M X' = A X + b  ->  X' = M^-1 A X + M^-1 b
        let bm: Vec<Vec<Expr>> = b.iter().map(|v| vec![v.clone()]).collect();
        let a2 = crate::linalg::solve(&m, &a).map_err(|_| "dsolve: the equations don't determine every derivative")?;
        let b2: Vec<Expr> = crate::linalg::solve(&m, &bm).map_err(|_| "dsolve: the equations don't determine every derivative")?.into_iter().map(|r| r[0].clone()).collect();
        let a2: Vec<Vec<Expr>> = a2.iter().map(|r| r.iter().map(simplify).collect()).collect();
        let ic = if args.len() == 5 {
            let t0 = self.eval(&args[3])?;
            let v = match self.eval(&args[4])? {
                Expr::Matrix(m) => m.into_iter().flatten().collect::<Vec<_>>(),
                v => vec![v],
            };
            Some((t0, v))
        } else {
            None
        };
        let (mut sol, notes) = crate::ode2::linear_system(&a2, &b2, &t, ic.as_ref().map(|(a, b)| (a, b.as_slice())))?;
        // a real system with real starting values has a real solution: write
        // exp(i w t) + exp(-i w t) as 2 cos(w t)
        let real_data = a2.iter().flatten().chain(&b2).all(|e| !has_i(e))
            && ic.as_ref().is_some_and(|(t0, v)| !has_i(t0) && v.iter().all(|e| !has_i(e)));
        if real_data && sol.iter().any(has_i) {
            if let Ok(re) = sol.iter().map(|e| tidy_parts(e).map(|(r, _)| r)).collect::<R<Vec<_>>>() {
                if re.iter().all(|e| !has_i(e)) {
                    sol = re;
                }
            }
        }
        let am = Expr::Matrix(a2.clone());
        let mut msg = format!(
            "Linear system X' = A X + b with X = [{}] and A = {}. ",
            vars.join(", "),
            show(&am)
        );
        msg.push_str(&notes.join(" "));
        msg.push_str(&format!(" The answer lists {} in that order.", vars.join(", ")));
        self.note(msg);
        let _ = n;
        Ok(Expr::Matrix(vec![sol]))
    }

    /// constant coefficients: characteristic roots + undetermined coefficients
    fn dsolve_constant(&mut self, a: &[Expr], g: &Expr, x: &str) -> R<(Expr, Vec<Expr>)> {
        let n = a.len() - 1;
        let r = Expr::sym("r__");
        let charpoly = add((0..=n)
            .map(|k| mul2(a[k].clone(), pow(r.clone(), Expr::int(k as i128))))
            .collect());
        let roots: Vec<Expr> = cas::solve(&charpoly, "r__")?
            .iter()
            .map(sqrt_assume)
            .collect();
        let xs = Expr::sym(x);
        // multiplicity: how many derivatives vanish at the root
        let mult = |root: &Expr| -> usize {
            let mut p = charpoly.clone();
            let mut m = 0;
            while m < n && cas::is_zero(&subst(&p, "r__", root)) {
                m += 1;
                p = deriv(&p, "r__");
            }
            m.max(1)
        };
        let mut basis: Vec<Expr> = vec![];
        let mut done: Vec<Expr> = vec![];
        for root in &roots {
            if done
                .iter()
                .any(|d| cas::is_zero(&sub(d.clone(), root.clone())))
            {
                continue;
            }
            let (re, im) = tidy_parts(root)?;
            let m = mult(root);
            if cas::is_zero(&im) {
                for j in 0..m {
                    basis.push(mul2(
                        pow(xs.clone(), Expr::int(j as i128)),
                        exp(mul2(re.clone(), xs.clone())),
                    ));
                }
                done.push(root.clone());
            } else {
                let b = if value_pos(&im) < 0.0 {
                    neg(im.clone())
                } else {
                    im.clone()
                };
                for j in 0..m {
                    let xe = mul2(
                        pow(xs.clone(), Expr::int(j as i128)),
                        exp(mul2(re.clone(), xs.clone())),
                    );
                    basis.push(mul2(
                        xe.clone(),
                        func(Func::Cos, mul2(b.clone(), xs.clone())),
                    ));
                    basis.push(mul2(xe, func(Func::Sin, mul2(b.clone(), xs.clone()))));
                }
                done.push(root.clone());
                done.push(add2(
                    re.clone(),
                    mul2(neg(im.clone()), Expr::Const(Const::I)),
                ));
            }
        }
        if basis.len() != n {
            return Err("dsolve: could not find all roots of the characteristic polynomial".into());
        }
        let consts: Vec<Expr> = (1..=n).map(|k| Expr::sym(&format!("C{}", k))).collect();
        let homogeneous = add(consts
            .iter()
            .zip(&basis)
            .map(|(c, b)| mul2(c.clone(), b.clone()))
            .collect());
        if cas::is_zero(g) {
            return Ok((homogeneous, consts));
        }
        // particular solution, term by term
        let terms = match expand(g) {
            Expr::Add(v) => v,
            t => vec![t],
        };
        // forcing terms grouped by their type e^(a x) cos/sin(b x): (a, b, max degree, terms)
        let mut groups: Vec<(Expr, Expr, usize, Vec<Expr>)> = vec![];
        for t in &terms {
            let (_, p, ea, eb) = forcing_term(t, x).ok_or_else(|| {
                format!("dsolve: can't find a particular solution for the term {} (polynomials, exponentials, sines and cosines work)", show(t))
            })?;
            match groups.iter_mut().find(|(ga, gb, _, _)| {
                cas::is_zero(&sub(ga.clone(), ea.clone()))
                    && cas::is_zero(&sub(gb.clone(), eb.clone()))
            }) {
                Some(gr) => {
                    gr.2 = gr.2.max(p);
                    gr.3.push(t.clone());
                }
                None => groups.push((ea, eb, p, vec![t.clone()])),
            }
        }
        let lop = |y: &Expr| -> Expr {
            let mut d = y.clone();
            let mut acc = vec![];
            for k in 0..=n {
                acc.push(mul2(a[k].clone(), d.clone()));
                d = deriv(&d, x);
            }
            add(acc)
        };
        let mut parts_out = vec![];
        for (gi, (ea, eb, p, gterms)) in groups.iter().enumerate() {
            let lambda = add2(ea.clone(), mul2(eb.clone(), Expr::Const(Const::I)));
            let s = if roots
                .iter()
                .any(|r| cas::is_zero(&sub(r.clone(), lambda.clone())))
            {
                mult(&lambda)
            } else {
                0
            };
            let e_ax = exp(mul2(ea.clone(), xs.clone()));
            let trig = !cas::is_zero(eb);
            let (cosx, sinx) = (
                func(Func::Cos, mul2(eb.clone(), xs.clone())),
                func(Func::Sin, mul2(eb.clone(), xs.clone())),
            );
            let mut unknowns: Vec<String> = vec![];
            let mut trial = vec![];
            for j in 0..=*p {
                let xp = pow(xs.clone(), Expr::int((j + s) as i128));
                let c = format!("A__{}_{}", gi, j);
                if trig {
                    let d = format!("B__{}_{}", gi, j);
                    trial.push(mul(vec![
                        Expr::sym(&c),
                        xp.clone(),
                        e_ax.clone(),
                        cosx.clone(),
                    ]));
                    trial.push(mul(vec![Expr::sym(&d), xp, e_ax.clone(), sinx.clone()]));
                    unknowns.push(c);
                    unknowns.push(d);
                } else {
                    trial.push(mul(vec![Expr::sym(&c), xp, e_ax.clone()]));
                    unknowns.push(c);
                }
            }
            let yp = add(trial);
            let resid = sub(lop(&yp), add(gterms.clone()));
            let vals = match match_coefficients(&resid, ea, &cosx, &sinx, x, &unknowns) {
                Some(v) => v,
                None => {
                    // numeric fallback: the residual must vanish at sample points
                    let mut eqs = vec![];
                    for k in 0..(unknowns.len() * 2 + 2) {
                        let xv = rat(2 * k as i128 + 1, 7);
                        eqs.push(calc::evalf(&subst(&resid, x, &xv)));
                    }
                    least_squares(&eqs, &unknowns)?
                }
            };
            let mut ypart = yp;
            for (u, v) in unknowns.iter().zip(vals) {
                ypart = subst(&ypart, u, &v);
            }
            parts_out.push(ypart);
        }
        let ypart = add(parts_out);
        let ypart = simplify(&ypart);
        Ok((add2(homogeneous, ypart), consts))
    }
}

/// Undetermined coefficients, exactly: divide the residual by e^(a x), write
/// cos(b x), sin(b x) as symbols, and require every coefficient to vanish
fn match_coefficients(
    resid: &Expr,
    a: &Expr,
    cosx: &Expr,
    sinx: &Expr,
    x: &str,
    unknowns: &[String],
) -> Option<Vec<Expr>> {
    fn replace(e: &Expr, from: &Expr, to: &Expr) -> Expr {
        if e == from {
            to.clone()
        } else {
            e.map_children(&mut |c| replace(c, from, to))
        }
    }
    let r = expand(&mul2(
        resid.clone(),
        exp(neg(mul2(a.clone(), Expr::sym(x)))),
    ));
    let r = replace(
        &replace(&r, cosx, &Expr::sym("C__")),
        sinx,
        &Expr::sym("S__"),
    );
    let r = expand(&r);
    let coeffs = cas::poly_coeffs(&r, x)?;
    let mut eqs = vec![];
    for c in coeffs {
        let c = expand(&c);
        let cc = simplify(&deriv(&c, "C__"));
        let sc = simplify(&deriv(&c, "S__"));
        let rest = simplify(&subst(
            &subst(&c, "C__", &Expr::zero()),
            "S__",
            &Expr::zero(),
        ));
        for q in [cc, sc, rest] {
            if q.contains_sym("C__") || q.contains_sym("S__") || q.contains_sym(x) {
                return None;
            }
            if !cas::is_zero(&q) {
                eqs.push(q);
            }
        }
    }
    let names: Vec<String> = unknowns.to_vec();
    cas::solve_linear(&eqs, &names)
        .ok()
        .map(|v| v.into_iter().map(|e| simplify(&e)).collect())
}

/// numeric value with every variable set to a (different) positive number
fn value_pos(e: &Expr) -> f64 {
    let syms = e.symbols();
    let names: Vec<&str> = syms.iter().map(|s| s.as_str()).collect();
    let vals: Vec<f64> = (0..names.len()).map(|k| 1.37 + 0.71 * k as f64).collect();
    numeric::run(&numeric::compile(e, &names), &vals)
}

/// square roots, assuming that every variable is positive:
/// sqrt(-4*w^2) = 2*w*i
/// a_k = c a_k' x^k for every k: the normalised Euler coefficients b_k
fn euler_coeffs(a: &[Expr], x: &str) -> Option<Vec<Expr>> {
    let n = a.len() - 1;
    let xs = Expr::sym(x);
    let an = a[n].clone();
    let mut b = vec![];
    for (k, c) in a.iter().enumerate() {
        let bk = simplify(&div(mul2(c.clone(), pow(xs.clone(), Expr::int((n - k) as i128))), an.clone()));
        if bk.contains_sym(x) {
            return None;
        }
        b.push(bk);
    }
    Some(b)
}

/// sum b_k x^k D^k (D = d/dx) = sum b_k T(T-1)...(T-k+1) with T = d/du:
/// the constant coefficients in u
fn euler_to_constant(b: &[Expr]) -> Vec<Expr> {
    let n = b.len() - 1;
    let mut out = vec![Expr::zero(); n + 1];
    for (k, bk) in b.iter().enumerate() {
        // falling factorial T(T-1)...(T-k+1) as coefficients
        let mut p: Vec<i128> = vec![1];
        for j in 0..k {
            let mut q = vec![0; p.len() + 1];
            for (i, c) in p.iter().enumerate() {
                q[i + 1] += c;
                q[i] -= c * j as i128;
            }
            p = q;
        }
        for (i, c) in p.iter().enumerate() {
            out[i] = add2(out[i].clone(), mul2(Expr::int(*c), bk.clone()));
        }
    }
    out.iter().map(simplify).collect()
}

fn sqrt_assume(e: &Expr) -> Expr {
    let e = e.map_children(&mut |c| sqrt_assume(c));
    let Expr::Pow(b, h) = &e else { return e };
    if **h != rat(1, 2) || b.symbols().is_empty() {
        return e;
    }
    let v = value_pos(b);
    let (inside, unit) = if v < 0.0 {
        (simplify(&neg((**b).clone())), true)
    } else {
        ((**b).clone(), false)
    };
    let factors = match &inside {
        Expr::Mul(f) => f.clone(),
        other => vec![other.clone()],
    };
    let (mut out, mut rest) = (vec![], vec![]);
    for f in factors {
        match &f {
            Expr::Num(_) => out.push(sqrt(f.clone())),
            Expr::Pow(s, p)
                if p.num().is_some_and(|n| {
                    n.is_exact() && n.is_integer() && n.as_int().unwrap() % 2 == 0
                }) =>
            {
                out.push(pow(
                    (**s).clone(),
                    Expr::int(p.num().unwrap().as_int().unwrap() / 2),
                ))
            }
            _ => rest.push(f),
        }
    }
    if !rest.is_empty() {
        out.push(sqrt(mul(rest)));
    }
    if unit {
        out.push(Expr::Const(Const::I));
    }
    mul(out)
}

/// solve an overdetermined but consistent linear system numerically, then
/// snap the answers to simple fractions when they are close to one
fn least_squares(eqs: &[Expr], vars: &[String]) -> R<Vec<Expr>> {
    let m = vars.len();
    let mut a = vec![vec![0.0f64; m]; eqs.len()];
    let mut b = vec![0.0f64; eqs.len()];
    for (i, e) in eqs.iter().enumerate() {
        let zero: Vec<(String, Expr)> = vars.iter().map(|v| (v.clone(), Expr::zero())).collect();
        let mut c0 = e.clone();
        for (v, z) in &zero {
            c0 = subst(&c0, v, z);
        }
        b[i] = -numeric::value(&c0);
        for (j, v) in vars.iter().enumerate() {
            a[i][j] = numeric::value(&simplify(&deriv(e, v)));
        }
    }
    // normal equations
    let mut n = vec![vec![0.0; m + 1]; m];
    for r in 0..m {
        for c in 0..m {
            n[r][c] = (0..eqs.len()).map(|i| a[i][r] * a[i][c]).sum();
        }
        n[r][m] = (0..eqs.len()).map(|i| a[i][r] * b[i]).sum();
    }
    for c in 0..m {
        let p = (c..m)
            .max_by(|&i, &j| n[i][c].abs().total_cmp(&n[j][c].abs()))
            .unwrap();
        n.swap(c, p);
        if n[c][c].abs() < 1e-12 {
            return Err("dsolve: could not determine the particular solution".into());
        }
        for r in 0..m {
            if r != c {
                let f = n[r][c] / n[c][c];
                for k in c..=m {
                    n[r][k] -= f * n[c][k];
                }
            }
        }
    }
    Ok((0..m).map(|i| snap(n[i][m] / n[i][i])).collect())
}

fn snap(v: f64) -> Expr {
    for d in 1..=1000i128 {
        let num = (v * d as f64).round();
        if (num / d as f64 - v).abs() < 1e-9 * (1.0 + v.abs()) && num.abs() < 1e15 {
            return rat(num as i128, d);
        }
    }
    Expr::float(v)
}

#[cfg(test)]
mod tests {
    use crate::Session;
    fn run(s: &str) -> String {
        Session::new().run_values(s)
    }
    #[test]
    fn number_theory() {
        assert_eq!(run("ifactor(360)"), "[2, 3;\n 3, 2;\n 5, 1]");
        assert_eq!(run("isprime(97)"), "true");
        assert_eq!(run("powmod(2, 10, 1000)"), "24");
        assert_eq!(run("modinv(3, 11)"), "4");
        assert_eq!(run("primes(20)"), "[2, 3, 5, 7, 11, 13, 17, 19]");
    }
    #[test]
    fn complex_parts() {
        assert_eq!(run("real(3+4*i)"), "3");
        assert_eq!(run("imag((1+i)^3)"), "2");
        assert_eq!(run("arg(1+i)"), "pi/4");
        assert_eq!(run("conj(2-5*i)"), "2+5*i");
    }
    #[test]
    fn trig() {
        assert_eq!(run("trigexpand(sin(2*x))"), "2*sin(x)*cos(x)");
        assert_eq!(
            run("simplify(trigreduce(sin(x)^2) - (1 - cos(2*x))/2)"),
            "0"
        );
    }
    #[test]
    fn sums_and_tables() {
        assert_eq!(run("sum(k, k, 1, n)"), "n*(n+1)/2");
        assert_eq!(run("sum(1/2^k, k, 1, inf)"), "1");
        assert_eq!(run("table(k^2, k, 1, 4)"), "[1, 4, 9, 16]");
    }
    #[test]
    fn groups() {
        assert_eq!(run("compose([2,3,1],[2,3,1])"), "[3, 1, 2]");
        assert_eq!(run("cycles(perm(\"(1 2 3)(4 5)\", 5))"), "\"(1 2 3)(4 5)\"");
        assert_eq!(run("size(generate([2,3,1],[2,1,3]))"), "[6, 3]");
    }
    #[test]
    fn trig_equations() {
        assert_eq!(run("solve(sin(x) == 1/2, x)"), "[pi/6, 5*pi/6]");
        assert_eq!(
            run("solve(2*sin(x)^2 - sin(x) - 1, x)"),
            "[pi/2, 7*pi/6, 11*pi/6]"
        );
    }
    #[test]
    fn odes() {
        assert_eq!(run("dsolve(d(y, x, 2) + y == 0, y, x, 0, [1, 0])"), "cos(x)");
        assert_eq!(run("dsolve(d(y, x) == 2*y, y, x, 0, [3])"), "3*exp(2*x)");
    }
}
