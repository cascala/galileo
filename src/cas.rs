//! Symbolic calculus and algebra beyond the basics: polynomials and their
//! roots, solving equations, limits, Taylor series and integration.
//!
//! Every symbolic antiderivative is checked numerically (its derivative must
//! match the integrand at sample points) before it is returned, so a gap in
//! the rules gives "can't integrate" rather than a wrong answer.

use crate::build::*;
use crate::calc::{self, deriv, simplify, subst};
use crate::expr::{Const, Expr, Func};
use crate::num::Number;
use crate::numeric;
use crate::radicals;
use Expr::*;

pub type R<T> = Result<T, String>;

fn is_x(e: &Expr, x: &str) -> bool {
    matches!(e, Sym(s) if s == x)
}

fn int(n: i128) -> Expr {
    Expr::int(n)
}

fn tidy(e: &Expr) -> Expr {
    let t = crate::ratfun::tidy(e);
    let s = simplify(e);
    if s.size() < t.size() { s } else { t }
}

/// numerically zero: exactly 0 after simplification
pub fn is_zero(e: &Expr) -> bool {
    if e.is_zero() {
        return true;
    }
    let s = simplify(e);
    if s.is_zero() {
        return true;
    }
    if s.symbols().is_empty() {
        let v = numeric::value(&s);
        return v.is_finite() && v.abs() < 1e-12;
    }
    false
}

// ---------- polynomials ----------

/// (degree, coefficient) of a term that is c * x^k with c free of x
fn monomial(t: &Expr, x: &str) -> Option<(usize, Expr)> {
    if !t.contains_sym(x) {
        return Some((0, t.clone()));
    }
    let factors = match t {
        Mul(f) => f.clone(),
        o => vec![o.clone()],
    };
    let mut k = 0usize;
    let mut rest = vec![];
    for f in factors {
        match &f {
            _ if is_x(&f, x) => k += 1,
            Pow(b, p) if is_x(b, x) => {
                let n = p.num()?;
                if !n.is_exact() || !n.is_integer() || n.is_negative() {
                    return None;
                }
                k += n.as_int()? as usize;
            }
            _ if !f.contains_sym(x) => rest.push(f),
            _ => return None,
        }
    }
    Some((k, mul(rest)))
}

/// Coefficients c0, c1, ..., cn of e as a polynomial in x, or None
pub fn poly_coeffs(e: &Expr, x: &str) -> Option<Vec<Expr>> {
    let ex = calc::expand(e);
    let terms = match &ex {
        Add(t) => t.clone(),
        o => vec![o.clone()],
    };
    let mut c: Vec<Vec<Expr>> = vec![];
    for t in terms {
        let (k, rest) = monomial(&t, x)?;
        if k > 200 {
            return None;
        }
        if c.len() <= k {
            c.resize(k + 1, vec![]);
        }
        c[k].push(rest);
    }
    let mut out: Vec<Expr> = c.into_iter().map(add).collect();
    while out.len() > 1 && is_zero(out.last().unwrap()) {
        out.pop();
    }
    if out.is_empty() {
        out.push(Expr::zero());
    }
    Some(out)
}

fn from_coeffs(c: &[Expr], x: &Expr) -> Expr {
    add(c
        .iter()
        .enumerate()
        .map(|(k, a)| mul2(a.clone(), pow(x.clone(), int(k as i128))))
        .collect())
}

type Poly = Vec<Number>;

fn exact_poly(c: &[Expr]) -> Option<Poly> {
    c.iter().map(|e| e.num().filter(|n| n.is_exact())).collect()
}

fn peval(p: &[Number], x: Number) -> Number {
    p.iter()
        .rev()
        .fold(Number::zero(), |acc, c| acc.mul(&x).add(c))
}

fn ptrim(mut p: Poly) -> Poly {
    while p.len() > 1 && p.last().unwrap().is_zero() {
        p.pop();
    }
    p
}

/// p / d with remainder (d nonzero, exact)
fn pdivmod(p: &[Number], d: &[Number]) -> (Poly, Poly) {
    let d = ptrim(d.to_vec());
    let mut r = ptrim(p.to_vec());
    if r.len() < d.len() {
        return (vec![Number::zero()], r);
    }
    let mut q = vec![Number::zero(); r.len() - d.len() + 1];
    let lead = d.last().unwrap().clone();
    while r.len() >= d.len() && !(r.len() == 1 && r[0].is_zero()) {
        let shift = r.len() - d.len();
        let c = r.last().unwrap().div(&lead).unwrap_or(Number::zero());
        q[shift] = c.clone();
        for (i, di) in d.iter().enumerate() {
            r[i + shift] = r[i + shift].sub(c.mul(di));
        }
        r.pop();
        r = ptrim(r);
        if r.len() < d.len() {
            break;
        }
    }
    (q, r)
}

fn pmul(a: &[Number], b: &[Number]) -> Poly {
    let mut out = vec![Number::zero(); a.len() + b.len() - 1];
    for (i, x) in a.iter().enumerate() {
        for (j, y) in b.iter().enumerate() {
            out[i + j] = out[i + j].add(x.mul(y));
        }
    }
    out
}

fn divisors(n: i128) -> Vec<i128> {
    let n = n.abs();
    if n == 0 || n > 10_000_000_000 {
        return vec![1];
    }
    let mut v = vec![];
    let mut k = 1i128;
    while k * k <= n {
        if n % k == 0 {
            v.push(k);
            if k != n / k {
                v.push(n / k);
            }
        }
        k += 1;
        if k > 200_000 {
            break;
        }
    }
    v
}

fn gcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a
}

/// Rational roots with multiplicities, and the cofactor left over
fn rational_roots(p: &[Number]) -> (Vec<(Number, usize)>, Poly) {
    let mut p = ptrim(p.to_vec());
    let mut roots: Vec<(Number, usize)> = vec![];
    // x = 0
    let mut m = 0;
    while p.len() > 1 && p[0].is_zero() {
        p.remove(0);
        m += 1;
    }
    if m > 0 {
        roots.push((Number::zero(), m));
    }
    if p.len() <= 1 {
        return (roots, p);
    }
    // scale to integers
    let mut l = 1i128;
    for c in &p {
        let d = c.denom().unwrap_or(1);
        l = l / gcd(l, d) * d;
        if l > 1_000_000_000_000 {
            return (roots, p);
        }
    }
    let ints: Vec<i128> = p
        .iter()
        .map(|c| c.mul(Number::int(l)).as_int().unwrap_or(0))
        .collect();
    let (a0, an) = (ints[0], *ints.last().unwrap());
    let mut cands: Vec<Number> = vec![];
    for q in divisors(an) {
        for pp in divisors(a0) {
            for s in [1, -1] {
                if let Some(r) = Number::rat(s * pp, q) {
                    if !cands.iter().any(|c| c.cmp_value(&r).is_eq()) {
                        cands.push(r);
                    }
                }
            }
        }
    }
    cands.sort_by(|a, b| a.abs().cmp_value(b.abs()).then(b.cmp_value(a)));
    for r in cands {
        let mut m = 0;
        while p.len() > 1 && peval(&p, r.clone()).is_zero() {
            p = pdivmod(&p, &[r.neg(), Number::one()]).0;
            m += 1;
        }
        if m > 0 {
            roots.push((r, m));
        }
        if p.len() <= 1 {
            break;
        }
    }
    (roots, p)
}

/// Durand-Kerner: all complex roots of a polynomial with float coefficients
pub(crate) fn numeric_roots(c: &[f64]) -> Vec<(f64, f64)> {
    let n = c.len() - 1;
    let lead = c[n];
    let a: Vec<f64> = c.iter().map(|v| v / lead).collect();
    let mut z: Vec<(f64, f64)> = (0..n)
        .map(|k| {
            let t = 0.4 + 0.9 * k as f64;
            let r = 1.0 + a.iter().map(|v| v.abs()).fold(0.0, f64::max);
            (r * t.cos() * 0.5, r * t.sin() * 0.5)
        })
        .collect();
    let cm = |a: (f64, f64), b: (f64, f64)| (a.0 * b.0 - a.1 * b.1, a.0 * b.1 + a.1 * b.0);
    let cd = |a: (f64, f64), b: (f64, f64)| {
        let d = b.0 * b.0 + b.1 * b.1;
        ((a.0 * b.0 + a.1 * b.1) / d, (a.1 * b.0 - a.0 * b.1) / d)
    };
    for _ in 0..500 {
        let mut delta: f64 = 0.0;
        for i in 0..n {
            let mut num = (0.0, 0.0);
            for k in (0..=n).rev() {
                num = cm(num, z[i]);
                num.0 += a[k];
            }
            let mut den = (1.0, 0.0);
            for j in 0..n {
                if j != i {
                    den = cm(den, (z[i].0 - z[j].0, z[i].1 - z[j].1));
                }
            }
            let w = cd(num, den);
            z[i] = (z[i].0 - w.0, z[i].1 - w.1);
            delta = delta.max(w.0.abs() + w.1.abs());
        }
        if delta < 1e-15 {
            break;
        }
    }
    for r in z.iter_mut() {
        if r.1.abs() < 1e-10 * (1.0 + r.0.abs()) {
            r.1 = 0.0;
        }
    }
    z.sort_by(|a, b| {
        (a.1 != 0.0)
            .cmp(&(b.1 != 0.0))
            .then(a.0.partial_cmp(&b.0).unwrap())
            .then(a.1.partial_cmp(&b.1).unwrap())
    });
    z
}

/// a numeric root that is really a Gaussian rational such as 1 + i: exact,
/// when the polynomial vanishes there exactly
fn exact_gaussian_root(p: &[Number], r: (f64, f64)) -> Option<Expr> {
    let snap = |v: f64| -> Option<Number> {
        (1..=60i128).find_map(|d| {
            let n = (v * d as f64).round();
            ((n / d as f64 - v).abs() < 1e-9 && n.abs() < 1e12)
                .then(|| Number::rat(n as i128, d).unwrap())
        })
    };
    let (a, b) = (snap(r.0)?, snap(r.1)?);
    let z = add2(Num(a), mul2(Num(b), Const(Const::I)));
    let x = Expr::sym("z__");
    let poly = add(p
        .iter()
        .enumerate()
        .map(|(k, c)| mul2(Num(c.clone()), pow(x.clone(), int(k as i128))))
        .collect());
    let v = crate::calc::subst(&poly, "z__", &z);
    if v.is_zero() { Some(z) } else { None }
}

fn complex_expr(r: (f64, f64)) -> Expr {
    if r.1 == 0.0 {
        Expr::float(r.0)
    } else {
        add2(Expr::float(r.0), mul2(Expr::float(r.1), Const(Const::I)))
    }
}

pub(crate) fn quadratic(a: &Expr, b: &Expr, c: &Expr) -> Vec<Expr> {
    let disc = simplify(&sub(
        pow(b.clone(), int(2)),
        mul(vec![int(4), a.clone(), c.clone()]),
    ));
    let two_a = mul2(int(2), a.clone());
    if is_zero(&disc) {
        return vec![tidy(&div(neg(b.clone()), two_a))];
    }
    let s = sqrt(disc);
    vec![
        tidy(&div(sub(neg(b.clone()), s.clone()), two_a.clone())),
        tidy(&div(add2(neg(b.clone()), s), two_a)),
    ]
}

/// Roots of the polynomial with coefficients c (low to high)
pub fn poly_roots(c: &[Expr]) -> R<Vec<Expr>> {
    let c: Vec<Expr> = c.iter().map(simplify).collect();
    let n = c.len() - 1;
    if n == 0 {
        return Err(if is_zero(&c[0]) {
            "every value is a solution".into()
        } else {
            "no solution".into()
        });
    }
    // x = 0 as a root: divide out the power of x first
    if n > 1 && is_zero(&c[0]) {
        let k = c.iter().take_while(|e| is_zero(e)).count();
        let mut out = vec![Expr::zero()];
        if k <= n {
            if let Ok(rest) = poly_roots(&c[k..]) {
                out.extend(rest.into_iter().filter(|r| !is_zero(r)));
            } else if c.len() - k > 1 {
                return poly_roots(&c[k..]).map(|_| out);
            }
        }
        return Ok(out);
    }
    // degree 1 and 2 are explained as such (the quadratic by completing the square,
    // even when its roots are rational)
    match n {
        1 => radicals::record(radicals::Trace::Linear { poly: c.clone() }),
        2 => radicals::record(radicals::Trace::Quadratic { poly: c.clone() }),
        _ => {}
    }
    if let Some(p) = exact_poly(&c) {
        let (roots, rest) = rational_roots(&p);
        if n > 2 && !roots.is_empty() {
            radicals::record(radicals::Trace::RationalRoots {
                poly: c.clone(),
                roots: roots.iter().map(|(r, m)| (Num(r.clone()), *m)).collect(),
                rest: rest.iter().cloned().map(Num).collect(),
            });
        }
        let mut out: Vec<Expr> = roots.into_iter().map(|(r, _)| Num(r)).collect();
        let pieces = if rest.len() > 4 {
            rational_factors(&rest)
        } else {
            vec![rest.clone()]
        };
        if pieces.len() > 1 && rest.len() == 5 {
            radicals::record(radicals::Trace::Factored {
                poly: rest.iter().cloned().map(Num).collect(),
                factors: pieces
                    .iter()
                    .map(|f| f.iter().cloned().map(Num).collect())
                    .collect(),
            });
        }
        for piece in pieces {
            let e: Vec<Expr> = piece.iter().cloned().map(Num).collect();
            match piece.len() {
                0 | 1 => {}
                2 => out.push(Num(piece[0].neg().div(&piece[1]).unwrap())),
                3 => {
                    if n > 2 {
                        radicals::record(radicals::Trace::Quadratic { poly: e.clone() });
                    }
                    out.extend(quadratic(&e[2], &e[1], &e[0]))
                }
                4 | 5 => out.extend(by_formula(&e)?),
                _ => {
                    radicals::record(radicals::Trace::Numeric { poly: e });
                    out.extend(
                        numeric_roots(&piece.iter().map(|v| v.to_f64()).collect::<Vec<_>>())
                            .into_iter()
                            .map(|r| {
                                exact_gaussian_root(&piece, r).unwrap_or_else(|| complex_expr(r))
                            }),
                    )
                }
            }
        }
        return Ok(out);
    }
    match n {
        1 => Ok(vec![tidy(&div(neg(c[0].clone()), c[1].clone()))]),
        2 => Ok(quadratic(&c[2], &c[1], &c[0])),
        3 | 4 => by_formula(&c),
        _ => {
            let f: Vec<f64> = c.iter().map(numeric::value).collect();
            if f.iter().all(|v| v.is_finite()) {
                Ok(numeric_roots(&f).into_iter().map(complex_expr).collect())
            } else {
                Err(format!(
                    "solve: can't solve a degree {} polynomial with symbolic coefficients",
                    n
                ))
            }
        }
    }
}

/// Roots of a cubic (Cardano) or quartic (Ferrari) in radicals, recording the steps
fn by_formula(c: &[Expr]) -> R<Vec<Expr>> {
    let found = if c.len() == 4 {
        radicals::cubic(c).map(|t| {
            let r = t.roots.clone();
            radicals::record(radicals::Trace::Cubic(t));
            r
        })
    } else {
        radicals::quartic(c).map(|t| {
            let r = t.roots.clone();
            radicals::record(radicals::Trace::Quartic(t));
            r
        })
    };
    found.ok_or_else(|| format!("solve: can't solve this degree {} polynomial", c.len() - 1))
}

/// The irreducible factors over Q of a polynomial without rational roots
/// (monic, each once per multiplicity); the polynomial itself when it does not split
fn rational_factors(p: &[Number]) -> Vec<Poly> {
    use num_bigint::BigInt;
    use num_traits::ToPrimitive;
    let whole = vec![p.to_vec()];
    let mut l = 1i128;
    for c in p {
        let d = c.denom().unwrap_or(1);
        l = l / gcd(l, d) * d;
        if l > 1_000_000_000_000 {
            return whole;
        }
    }
    let Some(ints) = p
        .iter()
        .map(|c| c.mul(Number::int(l)).as_int().map(BigInt::from))
        .collect::<Option<Vec<_>>>()
    else {
        return whole;
    };
    let Some((_, fs)) = crate::algfactor::factor_zpoly(&ints) else {
        return whole;
    };
    let mut out = vec![];
    for (f, m) in fs {
        let Some(f) = f
            .iter()
            .map(|v| v.to_i128().map(Number::int))
            .collect::<Option<Poly>>()
        else {
            return whole;
        };
        let lead = f.last().unwrap().clone();
        let f: Poly = f.iter().map(|v| v.div(&lead).unwrap()).collect();
        for _ in 0..m {
            out.push(f.clone());
        }
    }
    if out.len() <= 1 { whole } else { out }
}

/// Factor a polynomial in one variable with rational coefficients into
/// linear factors over the rationals (and what is left)
pub fn factor_poly(e: &Expr) -> Option<Expr> {
    let syms = e.symbols();
    if syms.len() != 1 {
        return None;
    }
    let x = &syms[0];
    let c = poly_coeffs(e, x)?;
    if c.len() < 3 {
        return None;
    }
    let p = exact_poly(&c)?;
    let (roots, rest) = rational_roots(&p);
    if roots.is_empty() || (roots.len() == 1 && roots[0].1 == 1 && rest.len() <= 1) {
        return None;
    }
    let xv = Expr::sym(x);
    let mut lead = rest.last().unwrap().clone();
    let mut factors = vec![];
    for (r, m) in &roots {
        // (q x - p) for r = p/q
        let (pn, qd) = (r.numer().unwrap(), r.denom().unwrap());
        lead = lead
            .div(Number::int(qd).pow(Number::int(*m as i128)).unwrap())
            .unwrap();
        let lin = sub(mul2(int(qd), xv.clone()), int(pn));
        factors.push(pow(lin, int(*m as i128)));
    }
    if rest.len() > 1 {
        let l = rest.last().unwrap().clone();
        let monic: Vec<Expr> = rest.iter().map(|v| Num(v.div(&l).unwrap())).collect();
        factors.push(from_coeffs(&monic, &xv));
    }
    factors.insert(0, Num(lead));
    let r = mul(factors);
    Some(r)
}

// ---------- solving ----------

/// Solve f = 0 for x
pub fn solve(f: &Expr, x: &str) -> R<Vec<Expr>> {
    let t = calc::together(f);
    let (n, d) = calc::num_den(&t);
    let c = poly_coeffs(&n, x).ok_or_else(|| {
        format!(
            "solve: {} is not a polynomial in {}; try fsolve(f, {}, guess) for a numeric root",
            crate::print::show(&n),
            x,
            x
        )
    })?;
    let roots = poly_roots(&c)?;
    let mut out = vec![];
    for r in roots {
        if d.contains_sym(x) && is_zero(&subst(&d, x, &r)) {
            continue;
        }
        // large formulas (Cardano, Ferrari) are compared as written: simplifying
        // their difference is slow
        let same = |o: &Expr| {
            *o == r || (o.size() + r.size() <= 120 && is_zero(&sub(o.clone(), r.clone())))
        };
        if !out.iter().any(same) {
            out.push(r);
        }
    }
    if out.is_empty() {
        return Err("no solution".into());
    }
    Ok(out)
}

/// Solve a linear system: equations (each = 0) in the unknowns
pub fn solve_linear(eqs: &[Expr], vars: &[String]) -> R<Vec<Expr>> {
    let mut a = vec![];
    let mut b = vec![];
    for e in eqs {
        let mut row = vec![];
        for v in vars {
            let c = simplify(&deriv(e, v));
            if vars.iter().any(|w| c.contains_sym(w)) {
                return Err("solve: the equations must be linear in the unknowns".into());
            }
            row.push(c);
        }
        let mut rest = e.clone();
        for v in vars {
            rest = subst(&rest, v, &Expr::zero());
        }
        a.push(row);
        b.push(vec![neg(rest)]);
    }
    if a.len() == vars.len() {
        if let Ok(sol) = crate::linalg::solve(&a, &b) {
            return Ok(sol.into_iter().map(|r| tidy(&r[0])).collect());
        }
    }
    // singular or not square: row reduce [A | b]; free unknowns stay as symbols
    let n = vars.len();
    let aug: Vec<Vec<Expr>> = a.iter().zip(&b).map(|(r, c)| r.iter().chain(c).cloned().collect()).collect();
    let (r, pivots) = crate::linalg::rref(&aug);
    if pivots.contains(&n) {
        return Err("solve: no solution; elimination leads to 0 = 1, so the equations contradict each other".into());
    }
    let mut out: Vec<Expr> = vars.iter().map(|v| Expr::sym(v)).collect();
    for (i, &p) in pivots.iter().enumerate() {
        let mut terms = vec![r[i][n].clone()];
        for (j, v) in vars.iter().enumerate() {
            if j != p && !pivots.contains(&j) && !r[i][j].is_zero() {
                terms.push(neg(mul2(r[i][j].clone(), Expr::sym(v))));
            }
        }
        out[p] = tidy(&add(terms));
    }
    Ok(out)
}

/// Newton's method for f(x) = 0 from x0
pub fn fsolve(f: &Expr, x: &str, x0: f64) -> R<f64> {
    let fc = numeric::compile(f, &[x]);
    let dc = numeric::compile(&deriv(f, x), &[x]);
    let mut xv = x0;
    for _ in 0..200 {
        let fv = numeric::run(&fc, &[xv]);
        let mut dv = numeric::run(&dc, &[xv]);
        if !fv.is_finite() {
            return Err(format!("fsolve: f is undefined at {} = {}", x, xv));
        }
        if fv == 0.0 {
            return Ok(xv);
        }
        if !dv.is_finite() || dv == 0.0 {
            let h = 1e-7 * xv.abs().max(1.0);
            dv = (numeric::run(&fc, &[xv + h]) - fv) / h;
            if dv == 0.0 || !dv.is_finite() {
                return Err("fsolve: the derivative is zero; try another starting guess".into());
            }
        }
        let step = fv / dv;
        xv -= step;
        if step.abs() <= 1e-14 * xv.abs().max(1.0) {
            return Ok(xv);
        }
    }
    let fv = numeric::run(&fc, &[xv]);
    if fv.abs() < 1e-9 {
        Ok(xv)
    } else {
        Err("fsolve: no convergence; try another starting guess".into())
    }
}

// ---------- limits ----------

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Side {
    Both,
    Left,
    Right,
}

#[derive(Clone, Debug)]
pub enum Point {
    At(Expr, Side),
    PosInf,
    NegInf,
}

#[derive(Clone, Debug)]
enum L {
    F(Expr),
    Inf,
    NInf,
}

pub fn infinity(sign: i32) -> Expr {
    if sign > 0 {
        Expr::sym("inf")
    } else {
        neg(Expr::sym("inf"))
    }
}

/// Values of e just next to the point (for signs), with other symbols absent
fn near(e: &Expr, x: &str, p: &Point) -> Vec<f64> {
    let c = numeric::compile(e, &[x]);
    match p {
        Point::PosInf => vec![numeric::run(&c, &[1e9])],
        Point::NegInf => vec![numeric::run(&c, &[-1e9])],
        Point::At(a, side) => {
            let a = numeric::value(a);
            let h = 1e-7 * a.abs().max(1.0);
            match side {
                Side::Right => vec![numeric::run(&c, &[a + h])],
                Side::Left => vec![numeric::run(&c, &[a - h])],
                Side::Both => vec![numeric::run(&c, &[a - h]), numeric::run(&c, &[a + h])],
            }
        }
    }
}

/// +1 / -1 if e has that sign on every side of the point, else None
fn sign_near(e: &Expr, x: &str, p: &Point) -> Option<i32> {
    let v = near(e, x, p);
    if v.iter().all(|t| *t > 0.0) {
        Some(1)
    } else if v.iter().all(|t| *t < 0.0) {
        Some(-1)
    } else {
        None
    }
}

fn lzero(l: &L) -> bool {
    matches!(l, L::F(v) if is_zero(v))
}

fn inf_sign(l: &L) -> i32 {
    match l {
        L::Inf => 1,
        L::NInf => -1,
        _ => 0,
    }
}

fn signed_inf(s: i32) -> L {
    if s > 0 { L::Inf } else { L::NInf }
}

fn quotient(n: &Expr, d: &Expr, x: &str, p: &Point, depth: usize) -> Option<L> {
    let ln = lim(n, x, p, depth + 1)?;
    let ld = lim(d, x, p, depth + 1)?;
    let hopital = || {
        let q = simplify(&div(deriv(n, x), deriv(d, x)));
        lim(&q, x, p, depth + 2)
    };
    match (&ln, &ld) {
        (L::F(a), L::F(b)) => {
            if !lzero(&ld) {
                Some(L::F(div(a.clone(), b.clone())))
            } else if lzero(&ln) {
                hopital()
            } else {
                let s = sign_near(&div(n.clone(), d.clone()), x, p)?;
                Some(signed_inf(s))
            }
        }
        (L::F(_), _) => Some(L::F(Expr::zero())),
        (_, L::F(b)) => {
            if lzero(&ld) {
                let s = sign_near(d, x, p)?;
                Some(signed_inf(inf_sign(&ln) * s))
            } else {
                let s = sign_near(b, x, p).or_else(|| {
                    let v = numeric::value(b);
                    if v > 0.0 {
                        Some(1)
                    } else if v < 0.0 {
                        Some(-1)
                    } else {
                        None
                    }
                })?;
                Some(signed_inf(inf_sign(&ln) * s))
            }
        }
        _ => hopital(),
    }
}

/// numerator and denominator, with e^(-x) counted as 1/e^x
fn nd(e: &Expr) -> (Expr, Expr) {
    let (n, d) = calc::num_den(e);
    let fs = match &n {
        Mul(f) => f.clone(),
        o => vec![o.clone()],
    };
    let (mut num, mut den) = (vec![], vec![d]);
    for f in fs {
        match &f {
            Pow(b, p) if p.num().is_none() && p.is_negative_term() => {
                den.push(pow((**b).clone(), neg((**p).clone())))
            }
            _ => num.push(f),
        }
    }
    (mul(num), mul(den))
}

fn contains(e: &Expr, pred: &dyn Fn(&Expr) -> bool) -> bool {
    if pred(e) {
        return true;
    }
    match e {
        Add(v) | Mul(v) | Apply(_, v) => v.iter().any(|t| contains(t, pred)),
        Pow(a, b) => contains(a, pred) || contains(b, pred),
        Func(_, a) => contains(a, pred),
        _ => false,
    }
}

thread_local!(static BUDGET: std::cell::Cell<usize> = const { std::cell::Cell::new(0) });

fn lim(e: &Expr, x: &str, p: &Point, depth: usize) -> Option<L> {
    if depth > 40 || e.size() > 400 {
        return None;
    }
    let left = BUDGET.with(|b| {
        let v = b.get();
        b.set(v.saturating_sub(1));
        v
    });
    if left == 0 {
        return None;
    }
    if !e.contains_sym(x) {
        return Some(L::F(e.clone()));
    }
    match e {
        Sym(_) => Some(match p {
            Point::At(a, _) => L::F(a.clone()),
            Point::PosInf => L::Inf,
            Point::NegInf => L::NInf,
        }),
        Add(ts) => {
            let mut fin = vec![];
            let (mut pos, mut negv) = (false, false);
            for t in ts {
                match lim(t, x, p, depth + 1)? {
                    L::F(v) => fin.push(v),
                    L::Inf => pos = true,
                    L::NInf => negv = true,
                }
            }
            match (pos, negv) {
                (true, true) => {
                    // inf - inf: try a common denominator, or the leading term of a polynomial
                    if let (Some(c), Point::PosInf | Point::NegInf) = (poly_coeffs(e, x), p) {
                        let k = c.len() - 1;
                        let lead = mul2(c[k].clone(), pow(Expr::sym(x), int(k as i128)));
                        return lim(&lead, x, p, depth + 1);
                    }
                    let t = calc::together(e);
                    if t != *e {
                        let (n, d) = calc::num_den(&t);
                        if !d.is_one() {
                            return quotient(&n, &d, x, p, depth);
                        }
                    }
                    None
                }
                (true, false) => Some(L::Inf),
                (false, true) => Some(L::NInf),
                _ => Some(L::F(add(fin))),
            }
        }
        Mul(_) => {
            let (n, d) = nd(e);
            if !d.contains_sym(x) {
                let fs = match &n {
                    Mul(f) => f.clone(),
                    o => vec![o.clone()],
                };
                if fs.len() == 1 {
                    return Some(match lim(&fs[0], x, p, depth + 1)? {
                        L::F(v) => L::F(div(v, d)),
                        other => {
                            let s = if numeric::value(&d) < 0.0 { -1 } else { 1 };
                            signed_inf(inf_sign(&other) * s)
                        }
                    });
                }
                let mut zeros = vec![];
                let mut infs = vec![];
                let mut fin = vec![];
                let mut sign = 1;
                for f in &fs {
                    let l = lim(f, x, p, depth + 1)?;
                    match l {
                        L::F(v) if is_zero(&v) => zeros.push(f.clone()),
                        L::F(v) => fin.push(v),
                        L::Inf => infs.push(f.clone()),
                        L::NInf => {
                            infs.push(f.clone());
                            sign = -sign;
                        }
                    }
                }
                let rest = div(mul(fin), d);
                if !zeros.is_empty() && !infs.is_empty() {
                    // 0 * inf: write as 0 / (1/inf)
                    // inf / (1/0) usually works (x log x = log x / x^-1); else 0 / (1/inf)
                    let (z, i) = (mul(zeros), mul(infs));
                    let has = |e: &Expr, pred: &dyn Fn(&Expr) -> bool| contains(e, pred);
                    let inf_on_top = has(&i, &|t| matches!(t, Func(Func::Log, _)))
                        || has(
                            &z,
                            &|t| matches!(t, Pow(b, q) if !q.is_number() && **b == Const(Const::E)),
                        );
                    let (top, bottom) = if inf_on_top {
                        (i.clone(), z.clone())
                    } else {
                        (z.clone(), i.clone())
                    };
                    let q = quotient(&top, &pow(bottom.clone(), int(-1)), x, p, depth + 1)
                        .or_else(|| quotient(&bottom, &pow(top, int(-1)), x, p, depth + 1))?;
                    return Some(match q {
                        L::F(v) => L::F(mul2(v, rest)),
                        other => {
                            let s = sign_near(&rest, x, p).unwrap_or(1);
                            signed_inf(inf_sign(&other) * s)
                        }
                    });
                }
                if !zeros.is_empty() {
                    return Some(L::F(Expr::zero()));
                }
                if !infs.is_empty() {
                    let v = numeric::value(&rest);
                    let s = if v.is_finite() {
                        if v > 0.0 { 1 } else { -1 }
                    } else {
                        sign_near(&rest, x, p)?
                    };
                    return Some(signed_inf(sign * s));
                }
                return Some(L::F(rest));
            }
            quotient(&n, &d, x, p, depth)
        }
        Pow(b, q) if !q.contains_sym(x) => {
            let qn = q.num();
            if qn.as_ref().is_some_and(|v| v.is_negative()) {
                return quotient(
                    &Expr::one(),
                    &pow((**b).clone(), neg((**q).clone())),
                    x,
                    p,
                    depth,
                );
            }
            match lim(b, x, p, depth + 1)? {
                L::F(v) => Some(L::F(pow(v, (**q).clone()))),
                L::Inf => Some(L::Inf),
                L::NInf => {
                    let n = qn?;
                    if !n.is_integer() {
                        return None;
                    }
                    Some(if n.as_int().unwrap_or(0) % 2 == 0 {
                        L::Inf
                    } else {
                        L::NInf
                    })
                }
            }
        }
        Pow(b, q) => {
            // b^q = e^(q log b)
            let ex = if **b == Const(Const::E) {
                (**q).clone()
            } else {
                mul2((**q).clone(), func(Func::Log, (**b).clone()))
            };
            Some(match lim(&ex, x, p, depth + 1)? {
                L::F(v) => L::F(exp(v)),
                L::Inf => L::Inf,
                L::NInf => L::F(Expr::zero()),
            })
        }
        Func(f, u) => {
            let l = lim(u, x, p, depth + 1)?;
            match l {
                L::F(v) => {
                    if *f == Func::Log && is_zero(&v) {
                        return Some(L::NInf);
                    }
                    if *f == Func::Tan && is_zero(&func(Func::Cos, v.clone())) {
                        return quotient(
                            &func(Func::Sin, (**u).clone()),
                            &func(Func::Cos, (**u).clone()),
                            x,
                            p,
                            depth,
                        );
                    }
                    Some(L::F(func(*f, v)))
                }
                L::Inf => match f {
                    Func::Log | Func::Sinh | Func::Cosh | Func::Asinh | Func::Acosh | Func::Abs => {
                        Some(L::Inf)
                    }
                    Func::Atan => Some(L::F(div(Const(Const::Pi), int(2)))),
                    Func::Tanh => Some(L::F(Expr::one())),
                    _ => None,
                },
                L::NInf => match f {
                    Func::Sinh | Func::Asinh => Some(L::NInf),
                    Func::Cosh | Func::Abs => Some(L::Inf),
                    Func::Atan => Some(L::F(div(Const(Const::Pi), int(-2)))),
                    Func::Tanh => Some(L::F(int(-1))),
                    _ => None,
                },
            }
        }
        Apply(f, args) if crate::special::is_special(f, args.len()) => {
            // the variable sits in the last argument (orders are constants)
            let k = args.len() - 1;
            if args[..k].iter().any(|a| a.contains_sym(x)) {
                return None;
            }
            match lim(&args[k], x, p, depth + 1)? {
                L::F(v) => {
                    let mut a = args.clone();
                    a[k] = v;
                    let r = crate::special::apply(f, a);
                    if numeric::value(&r).is_finite() || !r.symbols().is_empty() {
                        Some(L::F(r))
                    } else {
                        None
                    }
                }
                l => match crate::special::at_infinity(f, inf_sign(&l))? {
                    Some(v) => Some(L::F(v)),
                    None => Some(L::Inf),
                },
            }
        }
        _ => None,
    }
}

/// limit of e as x approaches the point
fn lim_top(e: &Expr, x: &str, p: &Point) -> Option<L> {
    BUDGET.with(|b| b.set(3000));
    lim(e, x, p, 0)
}

pub fn limit(e: &Expr, x: &str, p: &Point) -> R<Expr> {
    match lim_top(e, x, p) {
        Some(L::F(v)) => {
            let v = tidy(&v);
            let n = numeric::value(&v);
            if v.symbols().is_empty() && !n.is_finite() && !matches!(v, Num(_)) {
                return Err("limit: can't find the limit".into());
            }
            Ok(v)
        }
        Some(L::Inf) => Ok(infinity(1)),
        Some(L::NInf) => Ok(infinity(-1)),
        None => {
            if let Point::At(_, Side::Both) = p {
                let pa = match p {
                    Point::At(a, _) => a.clone(),
                    _ => unreachable!(),
                };
                let l = lim_top(e, x, &Point::At(pa.clone(), Side::Left));
                let r = lim_top(e, x, &Point::At(pa, Side::Right));
                if let (Some(l), Some(r)) = (l, r) {
                    let show = |l: L| match l {
                        L::F(v) => crate::print::show(&tidy(&v)),
                        L::Inf => "inf".into(),
                        L::NInf => "-inf".into(),
                    };
                    return Err(format!(
                        "limit: the limit does not exist (from the left {}, from the right {}); use limit(f, x, a, left) or right",
                        show(l),
                        show(r)
                    ));
                }
            }
            if let Some(v) = assumed_limit(e, x, p) {
                return Ok(v);
            }
            Err("limit: can't find the limit".into())
        }
    }
}

fn is_inf_expr(e: &Expr) -> i32 {
    if *e == infinity(1) {
        1
    } else if *e == infinity(-1) {
        -1
    } else {
        0
    }
}

/// Limits at infinity that depend only on the signs of the parameters:
/// exp(-s t) -> 0 for s > 0. Tried with sample values that obey the
/// assumptions; used only when every sample gives 0, or every one the same infinity.
/// polynomial in x, or bounded (sin, cos of anything), or free of x
fn tame(f: &Expr, x: &str) -> bool {
    if !f.contains_sym(x) || is_poly(f, x) {
        return true;
    }
    match f {
        Func(Func::Sin | Func::Cos, _) => true,
        Add(v) | Mul(v) => v.iter().all(|g| tame(g, x)),
        Pow(b, k) => tame(b, x) && k.num().is_some_and(|n| n.is_integer() && !n.is_negative()),
        _ => false,
    }
}

fn assumed_limit(e: &Expr, x: &str, p: &Point) -> Option<Expr> {
    if !matches!(p, Point::PosInf | Point::NegInf) {
        return None;
    }
    // a decaying exponential times polynomial or bounded factors -> 0
    if let Mul(v) = e {
        let (ex, rest): (Vec<Expr>, Vec<Expr>) =
            v.iter().cloned().partition(|f| matches!(f, Pow(b, _) if **b == Expr::Const(Const::E)));
        if !ex.is_empty() && rest.iter().all(|f| tame(f, x)) {
            let u = add(ex.iter().map(|f| match f {
                Pow(_, u) => (**u).clone(),
                _ => unreachable!(),
            }).collect());
            if let Ok(l) = limit(&u, x, p) {
                if is_inf_expr(&l) < 0 {
                    return Some(Expr::zero());
                }
            }
        }
    }
    // constant factors and sums first: the limit of c*g is c times that of g
    match e {
        Mul(v) => {
            let (c, rest): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|f| !f.contains_sym(x));
            if !c.is_empty() && !rest.is_empty() {
                let c = mul(c);
                let l = limit(&mul(rest), x, p).ok()?;
                return match is_inf_expr(&l) {
                    0 => Some(simplify(&mul2(c, l))),
                    k => crate::assume::sign(&c).filter(|s| *s != 0).map(|s| infinity(s * k)),
                };
            }
        }
        Add(v) => {
            let mut out = vec![];
            for t in v {
                let l = limit(t, x, p).ok()?;
                if is_inf_expr(&l) != 0 {
                    return None;
                }
                out.push(l);
            }
            return Some(simplify(&add(out)));
        }
        _ => {}
    }
    let mut seen: Option<i32> = None;
    let mut finite: Option<Expr> = None;
    for sample in crate::assume::samples(e, x)? {
        let mut f = e.clone();
        for (n, v) in &sample {
            f = subst(&f, n, v);
        }
        let k = match lim_top(&simplify(&f), x, p)? {
            L::F(v) if is_zero(&v) => 0,
            L::Inf => 1,
            L::NInf => -1,
            // the same number for every sample: erf(sqrt(a) x) -> 1
            L::F(v) if v.symbols().is_empty() => {
                let v = tidy(&v);
                if finite.as_ref().is_some_and(|f| !is_zero(&sub(f.clone(), v.clone()))) {
                    return None;
                }
                finite = Some(v);
                2
            }
            _ => return None,
        };
        if seen.is_some_and(|s| s != k) {
            return None;
        }
        seen = Some(k);
    }
    Some(match seen? {
        0 => Expr::zero(),
        2 => finite?,
        k => infinity(k),
    })
}

// ---------- Taylor series ----------

pub fn taylor(e: &Expr, x: &str, a: &Expr, n: usize) -> R<Expr> {
    let xv = Expr::sym(x);
    let dx = if a.is_zero() { xv } else { sub(xv, a.clone()) };
    let mut d = e.clone();
    let mut terms = vec![];
    let mut fact: i128 = 1;
    for k in 0..=n {
        if k > 0 {
            d = simplify(&deriv(&d, x));
            fact = fact
                .checked_mul(k as i128)
                .ok_or("taylor: order too high")?;
        }
        let c = match subst_ok(&d, x, a) {
            Some(v) => v,
            None => limit(&d, x, &Point::At(a.clone(), Side::Both)).map_err(|_| {
                format!(
                    "taylor: the function has no Taylor series at {} = {}",
                    x,
                    crate::print::show(a)
                )
            })?,
        };
        if c.symbols().iter().any(|s| s == "inf") {
            return Err(format!(
                "taylor: the function has no Taylor series at {} = {}",
                x,
                crate::print::show(a)
            ));
        }
        terms.push(mul(vec![
            tidy(&c),
            pow(int(fact), int(-1)),
            pow(dx.clone(), int(k as i128)),
        ]));
    }
    Ok(add(terms))
}

/// substitute, unless that divides by zero or takes log(0)
fn subst_ok(e: &Expr, x: &str, a: &Expr) -> Option<Expr> {
    if singular_at(e, x, a) {
        return None;
    }
    let v = subst(e, x, a);
    if bad(&v) {
        return None;
    }
    let s = simplify(&v);
    if bad(&s) {
        return None;
    }
    if s.symbols().is_empty() && !numeric::value(&s).is_finite() && !s.is_number() {
        // i or a complex value is fine; NaN from 0/0 is not
        if !crate::print::show(&s).contains('i') {
            return None;
        }
    }
    Some(s)
}

/// a division by zero or log(0) somewhere in e at x = a
fn singular_at(e: &Expr, x: &str, a: &Expr) -> bool {
    if !e.contains_sym(x) {
        return false;
    }
    match e {
        Pow(b, p) if p.num().is_some_and(|n| n.is_negative()) && is_zero(&subst(b, x, a)) => true,
        Func(Func::Log, u) if is_zero(&subst(u, x, a)) => true,
        Pow(b, p) => singular_at(b, x, a) || singular_at(p, x, a),
        Func(_, u) => singular_at(u, x, a),
        Add(v) | Mul(v) => v.iter().any(|t| singular_at(t, x, a)),
        _ => false,
    }
}

fn bad(e: &Expr) -> bool {
    match e {
        Pow(b, p) => (is_zero(b) && p.num().is_none_or(|n| n.is_negative())) || bad(b) || bad(p),
        Func(Func::Log, a) => is_zero(a) || bad(a),
        Func(_, a) => bad(a),
        Add(v) | Mul(v) => v.iter().any(bad),
        _ => false,
    }
}

// ---------- integration ----------

/// (slope, intercept) when u = a*x + b with a free of x and nonzero
fn linear(u: &Expr, x: &str) -> Option<(Expr, Expr)> {
    let c = poly_coeffs(u, x)?;
    if c.len() != 2 || is_zero(&c[1]) {
        return None;
    }
    Some((c[1].clone(), c[0].clone()))
}

/// antiderivative of f(u) with respect to u
fn table_func(f: Func, u: Expr) -> Option<Expr> {
    let u2p1 = add2(pow(u.clone(), int(2)), Expr::one());
    let one_m_u2 = sub(Expr::one(), pow(u.clone(), int(2)));
    Some(match f {
        Func::Sin => neg(func(Func::Cos, u)),
        Func::Cos => func(Func::Sin, u),
        Func::Tan => neg(func(Func::Log, func(Func::Cos, u))),
        Func::Sinh => func(Func::Cosh, u),
        Func::Cosh => func(Func::Sinh, u),
        Func::Tanh => func(Func::Log, func(Func::Cosh, u)),
        Func::Log => sub(mul2(u.clone(), func(Func::Log, u.clone())), u),
        Func::Atan => sub(
            mul2(u.clone(), func(Func::Atan, u.clone())),
            div(func(Func::Log, u2p1), int(2)),
        ),
        Func::Asin => add2(mul2(u.clone(), func(Func::Asin, u.clone())), sqrt(one_m_u2)),
        Func::Acos => sub(mul2(u.clone(), func(Func::Acos, u.clone())), sqrt(one_m_u2)),
        Func::Asinh => sub(mul2(u.clone(), func(Func::Asinh, u.clone())), sqrt(u2p1)),
        Func::Abs => div(mul2(u.clone(), func(Func::Abs, u)), int(2)),
        _ => return None,
    })
}

fn table(e: &Expr, x: &str) -> Option<Expr> {
    let xv = Expr::sym(x);
    match e {
        Sym(_) => Some(div(pow(xv, int(2)), int(2))),
        Func(f, u) => {
            let (a, _) = linear(u, x)?;
            Some(div(table_func(*f, (**u).clone())?, a))
        }
        Pow(b, p) if !p.contains_sym(x) => {
            if let Some((a, _)) = linear(b, x) {
                if p.num().is_some_and(|n| n.is_minus_one()) {
                    return Some(div(func(Func::Log, (**b).clone()), a));
                }
                let p1 = add2((**p).clone(), Expr::one());
                return Some(div(pow((**b).clone(), p1.clone()), mul2(p1, a)));
            }
            // squares and reciprocal squares of trig functions of a linear argument
            if let (Func(f, u), Some(n)) = (&**b, p.num().and_then(|n| n.as_int())) {
                let (a, _) = linear(u, x)?;
                let u = (**u).clone();
                let r = match (f, n) {
                    (Func::Sin, 2) => sub(
                        div(u.clone(), int(2)),
                        div(func(Func::Sin, mul2(int(2), u)), int(4)),
                    ),
                    (Func::Cos, 2) => add2(
                        div(u.clone(), int(2)),
                        div(func(Func::Sin, mul2(int(2), u)), int(4)),
                    ),
                    (Func::Cos, -2) => func(Func::Tan, u),
                    (Func::Sin, -2) => neg(div(func(Func::Cos, u.clone()), func(Func::Sin, u))),
                    (Func::Cosh, -2) => func(Func::Tanh, u),
                    (Func::Sinh, 2) => sub(
                        div(func(Func::Sinh, mul2(int(2), u.clone())), int(4)),
                        div(u, int(2)),
                    ),
                    (Func::Cosh, 2) => add2(
                        div(func(Func::Sinh, mul2(int(2), u.clone())), int(4)),
                        div(u, int(2)),
                    ),
                    (Func::Tan, 2) => sub(func(Func::Tan, u.clone()), u),
                    _ => return None,
                };
                return Some(div(r, a));
            }
            // (c + s x^2)^(-1/2)
            if p.num()
                .is_some_and(|n| n.cmp_value(Number::rat(-1, 2).unwrap()).is_eq())
            {
                let c = poly_coeffs(b, x)?;
                if c.len() == 3 && is_zero(&c[1]) {
                    let (c0, s) = (numeric::value(&c[0]), numeric::value(&c[2]));
                    if c0 > 0.0 && s < 0.0 {
                        let k = sqrt(div(neg(c[2].clone()), c[0].clone()));
                        return Some(div(func(Func::Asin, mul2(k, xv)), sqrt(neg(c[2].clone()))));
                    }
                    if c0 > 0.0 && s > 0.0 {
                        let k = sqrt(div(c[2].clone(), c[0].clone()));
                        return Some(div(func(Func::Asinh, mul2(k, xv)), sqrt(c[2].clone())));
                    }
                }
            }
            None
        }
        Pow(b, p) if !b.contains_sym(x) => {
            let (a, _) = linear(p, x)?;
            if **b == Const(Const::E) {
                Some(div(e.clone(), a))
            } else {
                Some(div(e.clone(), mul2(a, func(Func::Log, (**b).clone()))))
            }
        }
        _ => None,
    }
}

/// Rational functions with exact coefficients: polynomial division and
/// partial fractions over linear factors and at most one quadratic
fn rational(e: &Expr, x: &str) -> Option<Expr> {
    let (n, d) = calc::num_den(&calc::together(e));
    if !d.contains_sym(x) {
        return None;
    }
    let pn = exact_poly(&poly_coeffs(&n, x)?)?;
    let pd = exact_poly(&poly_coeffs(&d, x)?)?;
    let xv = Expr::sym(x);
    let (q, r) = pdivmod(&pn, &pd);
    let mut out = vec![];
    for (k, c) in q.iter().enumerate() {
        if !c.is_zero() {
            out.push(mul2(
                Num(c.div(Number::int(k as i128 + 1)).unwrap()),
                pow(xv.clone(), int(k as i128 + 1)),
            ));
        }
    }
    if r.iter().all(|c| c.is_zero()) {
        return Some(add(out));
    }
    let (roots, rest) = rational_roots(&pd);
    if rest.len() > 3 {
        return None;
    }
    // basis numerators: D / (x - r)^j, and x*D/Q, D/Q
    let deg = pd.len() - 1;
    let mut basis: Vec<Poly> = vec![];
    let mut kinds: Vec<(Option<(Number, usize)>, usize)> = vec![]; // (root, power) or quadratic with x^k
    for (rt, m) in &roots {
        let mut den = vec![Number::one()];
        for j in 1..=*m {
            den = pmul(&den, &[rt.neg(), Number::one()]);
            basis.push(pdivmod(&pd, &den).0);
            kinds.push((Some((rt.clone(), j)), 0));
        }
    }
    if rest.len() == 3 {
        let bq = pdivmod(&pd, &rest).0;
        basis.push(pmul(&bq, &[Number::zero(), Number::one()]));
        kinds.push((None, 1));
        basis.push(bq);
        kinds.push((None, 0));
    }
    if basis.len() != deg {
        return None;
    }
    let mat: Vec<Vec<Expr>> = (0..deg)
        .map(|i| {
            basis
                .iter()
                .map(|b| Num(b.get(i).cloned().unwrap_or(Number::zero())))
                .collect()
        })
        .collect();
    let rhs: Vec<Vec<Expr>> = (0..deg)
        .map(|i| vec![Num(r.get(i).cloned().unwrap_or(Number::zero()))])
        .collect();
    let sol = crate::linalg::solve(&mat, &rhs).ok()?;
    let coef: Vec<Expr> = sol.into_iter().map(|r| r[0].clone()).collect();
    let (mut bx, mut cx) = (Expr::zero(), Expr::zero());
    for (k, c) in kinds.iter().zip(coef) {
        match k {
            (Some((rt, j)), _) => {
                let lin = sub(xv.clone(), Num(rt.clone()));
                if *j == 1 {
                    out.push(mul2(c, func(Func::Log, lin)));
                } else {
                    let p = 1 - *j as i128;
                    out.push(mul(vec![c, pow(lin, int(p)), pow(int(p), int(-1))]));
                }
            }
            (None, 1) => bx = c,
            _ => cx = c,
        }
    }
    if rest.len() == 3 {
        // (B x + C) / (a x^2 + b x + c)
        let (a, b, c) = (
            Num(rest[2].clone()),
            Num(rest[1].clone()),
            Num(rest[0].clone()),
        );
        let qx = from_coeffs(&[c.clone(), b.clone(), a.clone()], &xv);
        out.push(mul2(
            div(bx.clone(), mul2(int(2), a.clone())),
            func(Func::Log, qx),
        ));
        let k = sub(cx, div(mul2(bx, b.clone()), mul2(int(2), a.clone())));
        let disc = rest[0]
            .mul(&rest[2])
            .mul(Number::int(4))
            .sub(rest[1].mul(&rest[1]));
        let lin = add2(mul2(mul2(int(2), a), xv.clone()), b);
        if disc.is_negative() {
            let s = sqrt(Num(disc.neg()));
            out.push(mul(vec![
                k,
                pow(s.clone(), int(-1)),
                func(Func::Log, div(sub(lin.clone(), s.clone()), add2(lin, s))),
            ]));
        } else {
            let s = sqrt(Num(disc));
            out.push(mul(vec![
                k,
                int(2),
                pow(s.clone(), int(-1)),
                func(Func::Atan, div(lin, s)),
            ]));
        }
    }
    Some(add(out))
}

/// replace every occurrence of `pat` in e by t
fn replace(e: &Expr, pat: &Expr, t: &Expr) -> Expr {
    if e == pat {
        return t.clone();
    }
    if let (Pow(b, p), Pow(pb, pp)) = (e, pat) {
        // x^4 with pattern x^2 is (x^2)^2
        if b == pb {
            if let (Some(a), Some(c)) = (p.num(), pp.num()) {
                if let Some(k) = a.div(c) {
                    if k.is_integer() && k.is_exact() {
                        return pow(t.clone(), Num(k));
                    }
                }
            }
        }
    }
    e.map_children(&mut |c| replace(c, pat, t))
}

/// candidate inner functions u for the substitution rule
fn inner_candidates(e: &Expr, x: &str, out: &mut Vec<Expr>) {
    match e {
        Func(_, u) => {
            out.push(e.clone());
            out.push((**u).clone());
            inner_candidates(u, x, out);
        }
        Pow(b, p) if !p.contains_sym(x) => {
            out.push((**b).clone());
            if !is_x(b, x) {
                out.push(e.clone());
            }
            inner_candidates(b, x, out);
        }
        Pow(_, p) => {
            out.push((**p).clone());
            inner_candidates(p, x, out);
        }
        Mul(f) | Add(f) => {
            for g in f {
                inner_candidates(g, x, out);
            }
        }
        _ => {}
    }
}

const T: &str = "\u{1d461}"; // a variable name the user can't type

fn substitution(e: &Expr, x: &str, depth: usize) -> Option<Expr> {
    let mut cands = vec![];
    inner_candidates(e, x, &mut cands);
    let mut seen: Vec<Expr> = vec![];
    for u in cands {
        if is_x(&u, x) || !u.contains_sym(x) || seen.contains(&u) || linear(&u, x).is_some() {
            continue;
        }
        seen.push(u.clone());
        let du = deriv(&u, x);
        if du.is_zero() {
            continue;
        }
        let ratio = simplify(&div(e.clone(), du));
        let tv = Expr::sym(T);
        let g = replace(&ratio, &u, &tv);
        let g = if g.contains_sym(x) {
            replace(&simplify(&calc::expand(&ratio)), &u, &tv)
        } else {
            g
        };
        if g.contains_sym(x) {
            continue;
        }
        if let Some(r) = integ(&g, T, depth + 1) {
            return Some(subst(&r, T, &u));
        }
    }
    None
}

fn is_poly(e: &Expr, x: &str) -> bool {
    poly_coeffs(e, x).is_some()
}

/// integration by parts for polynomial * (exp, sin, cos, ...) and polynomial * (log, atan, ...)
fn parts(e: &Expr, x: &str, depth: usize) -> Option<Expr> {
    let fs = match e {
        Mul(f) => f.clone(),
        _ => return None,
    };
    if fs.len() == 2 {
        if let Some(r) = exp_trig(&fs[0], &fs[1], x).or_else(|| exp_trig(&fs[1], &fs[0], x)) {
            return Some(r);
        }
    }
    let (polys, others): (Vec<Expr>, Vec<Expr>) = fs.into_iter().partition(|f| is_poly(f, x));
    if polys.is_empty() || others.len() != 1 {
        return None;
    }
    let p = mul(polys);
    let g = &others[0];
    let log_like = matches!(
        g,
        Func(
            Func::Log | Func::Atan | Func::Asin | Func::Acos | Func::Asinh,
            _
        )
    );
    if log_like {
        let pi = integ(&p, x, depth + 1)?;
        let rest = simplify(&mul2(pi.clone(), deriv(g, x)));
        let ri = integ(&calc::expand(&rest), x, depth + 1)?;
        Some(sub(mul2(pi, g.clone()), ri))
    } else {
        let gi = integ(g, x, depth + 1)?;
        let rest = calc::expand(&mul2(deriv(&p, x), gi.clone()));
        let ri = if rest.is_zero() {
            Expr::zero()
        } else {
            integ(&rest, x, depth + 1)?
        };
        Some(sub(mul2(p, gi), ri))
    }
}

/// e^(a x + c) sin(b x + d) and cos: the integral that comes back to itself under parts
fn exp_trig(ex: &Expr, tr: &Expr, x: &str) -> Option<Expr> {
    let Pow(base, p) = ex else { return None };
    if **base != Const(Const::E) {
        return None;
    }
    let (a, _) = linear(p, x)?;
    let Func(f @ (Func::Sin | Func::Cos), u) = tr else {
        return None;
    };
    let (b, _) = linear(u, x)?;
    let s = func(Func::Sin, (**u).clone());
    let c = func(Func::Cos, (**u).clone());
    let k = add2(pow(a.clone(), int(2)), pow(b.clone(), int(2)));
    let inner = if *f == Func::Sin {
        sub(mul2(a, s), mul2(b, c))
    } else {
        add2(mul2(a, c), mul2(b, s))
    };
    Some(div(mul2(ex.clone(), inner), k))
}

fn integ(e: &Expr, x: &str, depth: usize) -> Option<Expr> {
    if depth > 12 {
        return None;
    }
    if !e.contains_sym(x) {
        return Some(mul2(e.clone(), Expr::sym(x)));
    }
    match e {
        Add(ts) => {
            let mut out = vec![];
            let mut ok = true;
            for t in ts {
                match integ(t, x, depth + 1) {
                    Some(r) => out.push(r),
                    None => {
                        ok = false;
                        break;
                    }
                }
            }
            if ok {
                return Some(add(out));
            }
        }
        Mul(fs) => {
            let (c, v): (Vec<Expr>, Vec<Expr>) =
                fs.iter().cloned().partition(|f| !f.contains_sym(x));
            if !c.is_empty() {
                return integ(&mul(v), x, depth + 1).map(|r| mul2(mul(c), r));
            }
        }
        _ => {}
    }
    if let Some(r) = table(e, x) {
        return Some(r);
    }
    if let Some(r) = gaussian(e, x) {
        return Some(r);
    }
    if let Some(r) = rational(e, x) {
        return Some(r);
    }
    if let Some(r) = substitution(e, x, depth) {
        return Some(r);
    }
    if let Some(r) = parts(e, x, depth) {
        return Some(r);
    }
    let ex = calc::expand(e);
    if ex != *e && matches!(ex, Add(_)) {
        return integ(&ex, x, depth + 1);
    }
    None
}

/// P(x) e^(a x^2 + b x + c) with a polynomial P: write the antiderivative as
/// R(x) e^q + k * (integral of e^q), where R' + R q' = P - k, and the last
/// integral is an error function (erf for a < 0, erfi for a > 0)
fn gaussian(e: &Expr, x: &str) -> Option<Expr> {
    let fs = match e {
        Mul(f) => f.clone(),
        o => vec![o.clone()],
    };
    let mut q = None;
    let mut poly = vec![];
    for f in fs {
        match &f {
            Pow(b, p) if **b == Const(Const::E) && p.contains_sym(x) => {
                if q.is_some() {
                    return None;
                }
                q = Some((**p).clone());
            }
            _ if is_poly(&f, x) => poly.push(f),
            _ => return None,
        }
    }
    let q = q?;
    let qc = poly_coeffs(&calc::expand(&q), x)?;
    if qc.len() != 3 || is_zero(&qc[2]) {
        return None;
    }
    let (c0, b, a) = (qc[0].clone(), qc[1].clone(), qc[2].clone());
    let av = numeric::value(&a);
    let negative = if av.is_finite() { av < 0.0 } else { a.is_negative_term() };
    let p = poly_coeffs(&calc::expand(&mul(poly)), x)?;
    let n = p.len() - 1;
    // r_{m-1} = (p_m - b r_m - (m+1) r_{m+1}) / (2a), from the top down
    let mut r = vec![Expr::zero(); n + 2];
    for m in (1..=n).rev() {
        let t = sub(
            sub(p[m].clone(), mul2(b.clone(), r[m].clone())),
            mul2(int(m as i128 + 1), r[m + 1].clone()),
        );
        r[m - 1] = simplify(&div(t, mul2(int(2), a.clone())));
    }
    let k = simplify(&sub(p[0].clone(), add2(mul2(b.clone(), r[0].clone()), r[1].clone())));
    let xs = Expr::sym(x);
    let big_r = add((0..n).map(|j| mul2(r[j].clone(), pow(xs.clone(), int(j as i128)))).collect());
    let eq = exp(q.clone());
    let mut out = mul2(big_r, eq);
    if !is_zero(&k) {
        // e^q = e^(c - b^2/(4a)) e^(a (x + b/(2a))^2)
        let shift = add2(xs.clone(), div(b.clone(), mul2(int(2), a.clone())));
        let konst = exp(sub(c0, div(pow(b, int(2)), mul2(int(4), a.clone()))));
        let (name, s2) = if negative { ("erf", neg(a)) } else { ("erfi", a) };
        let base = mul(vec![
            div(mul2(sqrt(Const(Const::Pi)), rsqrt(&div(Expr::one(), s2.clone()))), int(2)),
            konst,
            crate::special::apply(name, vec![mul2(rsqrt(&s2), shift)]),
        ]);
        out = add2(out, mul2(k, base));
    }
    Some(out)
}

/// sqrt with rationals written as sqrt(p q)/q, so sqrt(1/2) = sqrt(2)/2
pub fn rsqrt(e: &Expr) -> Expr {
    if let Some(n) = e.num().filter(|n| n.is_exact() && !n.is_integer() && !n.is_negative()) {
        if let (Some(p), Some(q)) = (n.numer(), n.denom()) {
            return div(sqrt(int(p * q)), int(q));
        }
    }
    sqrt(e.clone())
}

/// d/dx F equals f at sample points (other symbols get sample values too)
fn verify(f: &Expr, big_f: &Expr, x: &str) -> bool {
    let d = deriv(big_f, x);
    let mut names: Vec<String> = vec![x.to_string()];
    for s in f.symbols().into_iter().chain(big_f.symbols()) {
        if !names.contains(&s) {
            names.push(s);
        }
    }
    let nm: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
    let (cf, cd) = (numeric::compile(f, &nm), numeric::compile(&d, &nm));
    let mut good = 0;
    let xs = [0.37, 0.71, 1.13, 1.61, 2.3, 3.7, -0.43, -1.27, 0.09, 5.3];
    for (k, &xv) in xs.iter().enumerate() {
        let mut v = vec![xv];
        for j in 1..names.len() {
            v.push(0.6 + 0.37 * ((j * 7 + k * 3) % 5) as f64);
        }
        let (a, b) = (numeric::run(&cf, &v), numeric::run(&cd, &v));
        if !a.is_finite() || !b.is_finite() {
            continue;
        }
        if (a - b).abs() > 1e-6 * (1.0 + a.abs()) {
            return false;
        }
        good += 1;
    }
    good >= 2
}

/// Symbolic antiderivative, checked; None if the rules don't find one
pub fn antiderivative(f: &Expr, x: &str) -> Option<Expr> {
    let r = match integ(f, x, 0) {
        Some(r) => r,
        None => {
            // products and powers of sines and cosines: reduce to sums first
            let g = crate::extra_cmd::trig_reduce(f);
            if g == *f {
                return None;
            }
            integ(&calc::expand(&g), x, 0)?
        }
    };
    if !verify(f, &r, x) {
        return None;
    }
    let t = tidy(&r);
    Some(if verify(f, &t, x) { t } else { r })
}

/// Adaptive Gauss-Kronrod (7, 15) quadrature
#[allow(clippy::excessive_precision)]
pub fn quad(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> R<f64> {
    const XK: [f64; 8] = [
        0.991455371120812639,
        0.949107912342758525,
        0.864864423359769073,
        0.741531185599394440,
        0.586087235467691130,
        0.405845151377397167,
        0.207784955007898468,
        0.0,
    ];
    const WK: [f64; 8] = [
        0.022935322010529225,
        0.063092092629978553,
        0.104790010322250184,
        0.140653259715525919,
        0.169004726639267903,
        0.190350578064785410,
        0.204432940075298892,
        0.209482141084727828,
    ];
    const WG: [f64; 4] = [
        0.129484966168869693,
        0.279705391489276668,
        0.381830050505118945,
        0.417959183673469388,
    ];
    fn gk(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> (f64, f64) {
        let (c, h) = ((a + b) / 2.0, (b - a) / 2.0);
        let fc = f(c);
        let (mut k, mut g) = (fc * WK[7], fc * WG[3]);
        for i in 0..7 {
            let dx = h * XK[i];
            let s = f(c - dx) + f(c + dx);
            k += WK[i] * s;
            if i % 2 == 1 {
                g += WG[i / 2] * s;
            }
        }
        (k * h, ((k - g) * h).abs())
    }
    fn rec(
        f: &dyn Fn(f64) -> f64,
        a: f64,
        b: f64,
        tol: f64,
        depth: usize,
        evals: &mut usize,
    ) -> R<f64> {
        let (v, err) = gk(f, a, b);
        *evals += 1;
        if !v.is_finite() {
            return Err("the integrand is not finite on the interval".into());
        }
        if err <= tol.max(1e-15 * v.abs()) || depth > 40 || *evals > 20000 {
            if depth > 40 || *evals > 20000 {
                if err > 1e-6 * v.abs().max(1.0) {
                    return Err("the integral does not seem to converge".into());
                }
            }
            return Ok(v);
        }
        let m = (a + b) / 2.0;
        Ok(rec(f, a, m, tol / 2.0, depth + 1, evals)? + rec(f, m, b, tol / 2.0, depth + 1, evals)?)
    }
    let mut evals = 0;
    rec(f, a, b, 1e-11, 0, &mut evals)
}

/// numeric integral, with infinite ends allowed
pub fn nintegrate(f: &Expr, x: &str, a: f64, b: f64) -> R<f64> {
    let c = numeric::compile(f, &[x]);
    let g = |t: f64| numeric::run(&c, &[t]);
    if a > b {
        return nintegrate(f, x, b, a).map(|v| -v);
    }
    match (a.is_finite(), b.is_finite()) {
        (true, true) => quad(&g, a, b),
        (true, false) => quad(
            &|t: f64| {
                let s = 1.0 - t;
                g(a + t / s) / (s * s)
            },
            0.0,
            1.0,
        ),
        (false, true) => quad(
            &|t: f64| {
                let s = 1.0 - t;
                g(b - t / s) / (s * s)
            },
            0.0,
            1.0,
        ),
        (false, false) => quad(
            &|t: f64| {
                let s = 1.0 - t * t;
                g(t / s) * (1.0 + t * t) / (s * s)
            },
            -1.0,
            1.0,
        ),
    }
}

// ---------- sums and integers ----------

pub fn factorial(n: i128) -> R<Expr> {
    if n < 0 {
        return Err("factorial needs a non-negative integer".into());
    }
    if n > 100_000 {
        return Err("factorial: at most factorial(100000), which already has 456574 digits".into());
    }
    Ok(Num(Number::from_bigint(range_product(0, n))))
}

/// (lo+1)(lo+2)...hi, and 1 when hi <= lo. Binary splitting: the halves are
/// multiplied recursively, so the big multiplications are between numbers of
/// similar size and 10000! takes milliseconds instead of minutes.
pub fn range_product(lo: i128, hi: i128) -> num_bigint::BigInt {
    use num_bigint::BigInt;
    if hi <= lo {
        return BigInt::from(1);
    }
    if hi - lo <= 16 {
        let mut acc = BigInt::from(lo + 1);
        for k in lo + 2..=hi {
            acc *= k;
        }
        return acc;
    }
    let mid = lo + (hi - lo) / 2;
    range_product(lo, mid) * range_product(mid, hi)
}

pub fn binomial(n: i128, k: i128) -> Expr {
    if k < 0 || k > n {
        return Expr::zero();
    }
    let k = k.min(n - k);
    let mut acc = Number::one();
    for j in 0..k {
        acc = acc.mul(Number::int(n - j)).div(Number::int(j + 1)).unwrap();
    }
    Num(acc)
}

pub fn gcd_int(a: i128, b: i128) -> i128 {
    gcd(a, b)
}
