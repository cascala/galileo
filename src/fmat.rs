//! Matrices over a finite ring: zmod(n), gf(p) and gf(p^k).
//!
//! A matrix whose entries are elements of one ring (with plain integers or
//! fractions allowed beside them) is a matrix over that ring. Arithmetic,
//! det, inv, rank, rref, null, A \ b, A^k, charpoly and eig are then exact in
//! the ring instead of over the rationals. Such a matrix prints as
//! `gf(17, [1, 2; 3, 4])`, which reads back as the same matrix.
//!
//! Over a field everything is Gaussian elimination. In zmod(n) with n
//! composite, det and charpoly are computed over the integers and reduced;
//! inv and A \ b go prime power by prime power (pivots that are units) and
//! are glued by the Chinese remainder theorem; rank, rref and null need a
//! field.

use crate::expr::Expr;
use crate::ffield::{self as ff, Elem, Ring};
use crate::parser::Op;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::rc::Rc;

pub type R<T> = Result<T, String>;
pub type FM = Vec<Vec<Elem>>;

/// The ring of a matrix: Some when an entry is a ring element. Every element
/// must be in the same ring; the other entries must be exact numbers.
pub fn ring_of(m: &[Vec<Expr>]) -> R<Option<Rc<Ring>>> {
    let mut ring: Option<Rc<Ring>> = None;
    for e in m.iter().flatten() {
        if let Some(x) = ff::as_elem(e) {
            match &ring {
                Some(r) if *r != x.ring => {
                    return Err(format!(
                        "a matrix can't mix elements of {} and {}",
                        r.name(),
                        x.ring.name()
                    ));
                }
                Some(_) => {}
                None => ring = Some(x.ring.clone()),
            }
        }
    }
    if let Some(r) = &ring {
        for e in m.iter().flatten() {
            if ff::as_elem(e).is_none() && !e.num().is_some_and(|n| n.is_exact()) {
                return Err(format!(
                    "a matrix over {} can only hold its elements and whole numbers, not {}",
                    r.name(),
                    crate::print::show(e)
                ));
            }
        }
    }
    Ok(ring)
}

/// The ring of a matrix or a ring element
pub fn ring_of_expr(e: &Expr) -> R<Option<Rc<Ring>>> {
    match e {
        Expr::Matrix(m) => ring_of(m),
        _ => Ok(ff::as_elem(e).map(|x| x.ring.clone())),
    }
}

/// Whether every entry is an element of one ring (for printing)
pub fn all_elems(m: &[Vec<Expr>]) -> Option<Rc<Ring>> {
    let first = ff::as_elem(m.first()?.first()?)?.ring.clone();
    m.iter()
        .flatten()
        .all(|e| ff::as_elem(e).is_some_and(|x| x.ring == first))
        .then_some(first)
}

pub fn lift(m: &[Vec<Expr>], r: &Rc<Ring>) -> R<FM> {
    m.iter()
        .map(|row| row.iter().map(|e| r.from_expr(e)).collect())
        .collect()
}

pub fn expr(m: &FM) -> Expr {
    Expr::Matrix(
        m.iter()
            .map(|r| r.iter().map(|x| ff::value(x.clone())).collect())
            .collect(),
    )
}

fn dims(m: &FM) -> (usize, usize) {
    (m.len(), m.first().map_or(0, |r| r.len()))
}

fn zero(r: &Rc<Ring>) -> Elem {
    r.int(BigInt::zero())
}

fn one(r: &Rc<Ring>) -> Elem {
    r.int(BigInt::one())
}

pub fn identity(r: &Rc<Ring>, n: usize) -> FM {
    (0..n)
        .map(|i| {
            (0..n)
                .map(|j| if i == j { one(r) } else { zero(r) })
                .collect()
        })
        .collect()
}

pub fn mul(a: &FM, b: &FM, r: &Rc<Ring>) -> R<FM> {
    let (n, k) = dims(a);
    let (k2, m) = dims(b);
    if k != k2 {
        return Err(format!(
            "incompatible matrix dimensions for product: {}x{} and {}x{}",
            n, k, k2, m
        ));
    }
    Ok((0..n)
        .map(|i| {
            (0..m)
                .map(|j| {
                    (0..k).fold(zero(r), |acc, t| acc.add(&a[i][t].mul(&b[t][j])))
                })
                .collect()
        })
        .collect())
}

fn zip(a: &FM, b: &FM, what: &str, f: impl Fn(&Elem, &Elem) -> Elem) -> R<FM> {
    if dims(a) != dims(b) {
        return Err(format!(
            "incompatible matrix dimensions for {}: {}x{} and {}x{}",
            what,
            dims(a).0,
            dims(a).1,
            dims(b).0,
            dims(b).1
        ));
    }
    Ok(a.iter()
        .zip(b)
        .map(|(x, y)| x.iter().zip(y).map(|(p, q)| f(p, q)).collect())
        .collect())
}

fn map(a: &FM, f: impl Fn(&Elem) -> Elem) -> FM {
    a.iter().map(|r| r.iter().map(&f).collect()).collect()
}

fn need_field(r: &Ring, what: &str) -> R<()> {
    if r.is_field() {
        Ok(())
    } else {
        Err(format!(
            "{}: {} is not a field ({} is not prime), so row reduction can get stuck on a zero divisor; use a prime modulus or gf(q)",
            what,
            r.name(),
            r.n
        ))
    }
}

/// Reduced row echelon form over a field and the pivot columns
pub fn rref(a: &FM, r: &Rc<Ring>) -> R<(FM, Vec<usize>)> {
    need_field(r, "rank, rref and null")?;
    let mut m = a.clone();
    let (rows, cols) = dims(&m);
    let mut piv = vec![];
    let mut i = 0;
    for j in 0..cols {
        if i == rows {
            break;
        }
        let Some(p) = (i..rows).find(|&t| !m[t][j].is_zero()) else {
            continue;
        };
        m.swap(i, p);
        let inv = m[i][j].inv()?;
        m[i] = m[i].iter().map(|x| x.mul(&inv)).collect();
        for t in 0..rows {
            if t != i && !m[t][j].is_zero() {
                let c = m[t][j].clone();
                let pr = m[i].clone();
                for (x, y) in m[t].iter_mut().zip(&pr) {
                    *x = x.add(&y.mul(&c).neg());
                }
            }
        }
        piv.push(j);
        i += 1;
    }
    Ok((m, piv))
}

/// A basis of the solutions of a*x = 0, one vector per column (empty when
/// x = 0 is the only one)
pub fn nullspace(a: &FM, r: &Rc<Ring>) -> R<FM> {
    let (m, piv) = rref(a, r)?;
    let cols = dims(a).1;
    let free: Vec<usize> = (0..cols).filter(|j| !piv.contains(j)).collect();
    if free.is_empty() {
        return Ok(vec![]);
    }
    let mut out = vec![vec![zero(r); free.len()]; cols];
    for (k, &f) in free.iter().enumerate() {
        out[f][k] = one(r);
        for (i, &p) in piv.iter().enumerate() {
            out[p][k] = m[i][f].neg();
        }
    }
    Ok(out)
}

fn square(a: &FM, what: &str) -> R<usize> {
    let (n, m) = dims(a);
    if n != m || n == 0 {
        return Err(format!("{} needs a square matrix", what));
    }
    Ok(n)
}

/// integer lifts (zmod(n) and gf(p))
fn ints(a: &FM) -> Vec<Vec<Expr>> {
    a.iter()
        .map(|r| r.iter().map(|x| x.lift()).collect())
        .collect()
}

pub fn det(a: &FM, r: &Rc<Ring>) -> R<Elem> {
    let n = square(a, "det")?;
    if !r.is_field() {
        // the determinant is a polynomial in the entries: compute it over the
        // integers and reduce
        let d = crate::linalg::det(&ints(a))?;
        return r.from_expr(&crate::calc::simplify(&d));
    }
    let mut m = a.clone();
    let mut d = one(r);
    for j in 0..n {
        let Some(p) = (j..n).find(|&t| !m[t][j].is_zero()) else {
            return Ok(zero(r));
        };
        if p != j {
            m.swap(p, j);
            d = d.neg();
        }
        d = d.mul(&m[j][j]);
        let inv = m[j][j].inv()?;
        for t in j + 1..n {
            if !m[t][j].is_zero() {
                let c = m[t][j].mul(&inv);
                let pr = m[j].clone();
                for (x, y) in m[t].iter_mut().zip(&pr) {
                    *x = x.add(&y.mul(&c).neg());
                }
            }
        }
    }
    Ok(d)
}

/// Gauss–Jordan on [a | b] with unit pivots; None when a column has no unit
/// (over a field: a is singular)
fn gauss_jordan(a: &FM, b: &FM) -> Option<FM> {
    let n = a.len();
    let mut m: FM = a
        .iter()
        .zip(b)
        .map(|(x, y)| x.iter().chain(y).cloned().collect())
        .collect();
    for j in 0..n {
        let p = (j..n).find(|&t| m[t][j].is_unit())?;
        m.swap(p, j);
        let inv = m[j][j].inv().ok()?;
        m[j] = m[j].iter().map(|x| x.mul(&inv)).collect();
        for t in 0..n {
            if t != j && !m[t][j].is_zero() {
                let c = m[t][j].clone();
                let pr = m[j].clone();
                for (x, y) in m[t].iter_mut().zip(&pr) {
                    *x = x.add(&y.mul(&c).neg());
                }
            }
        }
    }
    Some(m.into_iter().map(|row| row[n..].to_vec()).collect())
}

/// The solution of a*x = b for square, invertible a
fn solve_square(a: &FM, b: &FM, r: &Rc<Ring>) -> R<FM> {
    let singular = || {
        format!(
            "the matrix is singular over {} (its determinant is 0{}), so it has no inverse",
            r.name(),
            if r.is_field() { "" } else { " or a zero divisor" }
        )
    };
    if r.is_field() {
        return gauss_jordan(a, b).ok_or_else(singular);
    }
    // zmod(n): solve mod each prime power with unit pivots, then glue by CRT
    let f = crate::numtheory::factorize(&r.n)?;
    let (rows, cols) = dims(b);
    let mut acc: Vec<Vec<BigInt>> = vec![vec![BigInt::zero(); cols]; rows];
    let mut modulus = BigInt::one();
    for (p, e) in f {
        let q = num_traits::pow(p, e as usize);
        let rq = ff::zmod(&q)?;
        let to = |m: &FM| -> FM {
            m.iter()
                .map(|row| row.iter().map(|x| rq.int(lift_int(x))).collect())
                .collect()
        };
        let x = gauss_jordan(&to(a), &to(b)).ok_or_else(singular)?;
        let inv = crate::numtheory::modinv(&modulus, &q).unwrap();
        for i in 0..rows {
            for j in 0..cols {
                let t = ((lift_int(&x[i][j]) - &acc[i][j]) * &inv).mod_floor(&q);
                acc[i][j] += &modulus * t;
            }
        }
        modulus *= q;
    }
    Ok(acc
        .into_iter()
        .map(|row| row.into_iter().map(|v| r.int(v)).collect())
        .collect())
}

fn lift_int(x: &Elem) -> BigInt {
    x.v.first().cloned().unwrap_or_default()
}

pub fn inverse(a: &FM, r: &Rc<Ring>) -> R<FM> {
    let n = square(a, "inv")?;
    solve_square(a, &identity(r, n), r)
}

/// a \ b: the unique solution of a*x = b
pub fn solve(a: &FM, b: &FM, r: &Rc<Ring>) -> R<FM> {
    let (n, m) = dims(a);
    if dims(b).0 != n {
        return Err(format!(
            "A\\b: A has {} rows but b has {}",
            n,
            dims(b).0
        ));
    }
    if n == m {
        if let Ok(x) = solve_square(a, b, r) {
            return Ok(x);
        }
    }
    need_field(r, "A\\b")?;
    // not square or singular: row reduce [a | b]
    let aug: FM = a
        .iter()
        .zip(b)
        .map(|(x, y)| x.iter().chain(y).cloned().collect())
        .collect();
    let (red, piv) = rref(&aug, r)?;
    if piv.iter().any(|&p| p >= m) {
        return Err(format!(
            "A\\b: no solution over {}; elimination leads to 0 = 1",
            r.name()
        ));
    }
    if piv.len() < m {
        return Err(format!(
            "A\\b: the solution over {} is not unique (rank {} for {} unknowns); null(A) gives the free directions, solve([..], [..], F) the general solution",
            r.name(),
            piv.len(),
            m
        ));
    }
    let k = dims(b).1;
    Ok((0..m)
        .map(|j| {
            let i = piv.iter().position(|&p| p == j).unwrap();
            red[i][m..m + k].to_vec()
        })
        .collect())
}

pub fn pow(a: &FM, e: &BigInt, r: &Rc<Ring>) -> R<FM> {
    let n = square(a, "a matrix power")?;
    let (mut base, mut e) = if e.is_negative() {
        (inverse(a, r)?, -e)
    } else {
        (a.clone(), e.clone())
    };
    let mut acc = identity(r, n);
    while !e.is_zero() {
        if e.is_odd() {
            acc = mul(&acc, &base, r)?;
        }
        e >>= 1;
        if !e.is_zero() {
            base = mul(&base, &base, r)?;
        }
    }
    Ok(acc)
}

/// Coefficients of det(A - x I), constant term first
pub fn charpoly(a: &FM, r: &Rc<Ring>) -> R<Vec<Elem>> {
    let n = square(a, "charpoly")?;
    if !r.is_field() {
        let p = crate::cas_cmd::charpoly(&ints(a), "x")?;
        let c = crate::cas::poly_coeffs(&p, "x").ok_or("charpoly: no polynomial")?;
        return c
            .iter()
            .map(|e| r.from_expr(&crate::calc::simplify(e)))
            .collect();
    }
    // Hessenberg form by similarity, then the recurrence for det(x I - H)
    let mut h = a.clone();
    for m in 1..n {
        let Some(i) = (m..n).find(|&i| !h[i][m - 1].is_zero()) else {
            continue;
        };
        if i != m {
            h.swap(i, m);
            for row in h.iter_mut() {
                row.swap(i, m);
            }
        }
        let tinv = h[m][m - 1].inv()?;
        for i in m + 1..n {
            let u = h[i][m - 1].mul(&tinv);
            if u.is_zero() {
                continue;
            }
            for j in 0..n {
                let t = h[m][j].mul(&u);
                h[i][j] = h[i][j].add(&t.neg());
            }
            for row in h.iter_mut() {
                let t = row[i].mul(&u);
                row[m] = row[m].add(&t);
            }
        }
    }
    // p[m] = det(x I - H[..m, ..m]), coefficients low to high
    let padd = |a: &[Elem], b: &[Elem]| -> Vec<Elem> {
        (0..a.len().max(b.len()))
            .map(|i| match (a.get(i), b.get(i)) {
                (Some(x), Some(y)) => x.add(y),
                (Some(x), None) | (None, Some(x)) => x.clone(),
                _ => unreachable!(),
            })
            .collect()
    };
    let mut p: Vec<Vec<Elem>> = vec![vec![one(r)]];
    for m in 1..=n {
        // (x - h[m-1][m-1]) p[m-1]
        let prev = &p[m - 1];
        let mut shifted = vec![zero(r)];
        shifted.extend(prev.iter().cloned());
        let scaled: Vec<Elem> = prev.iter().map(|c| c.mul(&h[m - 1][m - 1]).neg()).collect();
        let mut cur = padd(&shifted, &scaled);
        let mut prod = one(r);
        for i in (1..m).rev() {
            prod = prod.mul(&h[i][i - 1]);
            let c = h[i - 1][m - 1].mul(&prod);
            let t: Vec<Elem> = p[i - 1].iter().map(|x| x.mul(&c).neg()).collect();
            cur = padd(&cur, &t);
        }
        p.push(cur);
    }
    let mut c = p.pop().unwrap();
    if n % 2 == 1 {
        c = c.iter().map(|x| x.neg()).collect();
    }
    Ok(c)
}

/// The roots in the ring of a polynomial with coefficients in it (low to high)
pub fn roots(c: &[Elem], r: &Rc<Ring>) -> R<Vec<Elem>> {
    if r.k == 1 && r.is_field() {
        let ints: Vec<BigInt> = c.iter().map(lift_int).collect();
        let (mut v, all, _, _) = crate::modsolve::roots_mod_p(&ints, &r.n);
        if all && v.is_empty() {
            return Err("every element is a root".into());
        }
        v.sort();
        return Ok(v.into_iter().map(|x| r.int(x)).collect());
    }
    if r.size() > BigInt::from(1_000_000) {
        return Err(format!(
            "roots in {} are found by trying every element, which stops at a million elements",
            r.name()
        ));
    }
    let q = r.size().to_u64().unwrap();
    let mut out = vec![];
    for t in 0..q {
        let x = r.elem(digits(t, &r.n, r.k));
        let v = c.iter().rev().fold(zero(r), |acc, k| acc.mul(&x).add(k));
        if v.is_zero() {
            out.push(x);
        }
    }
    Ok(out)
}

pub(crate) fn digits(mut t: u64, p: &BigInt, k: u32) -> Vec<BigInt> {
    let p = p.to_u64().unwrap();
    (0..k)
        .map(|_| {
            let d = t % p;
            t /= p;
            BigInt::from(d)
        })
        .collect()
}

/// Arithmetic when a side is a matrix over a finite ring (the other side a
/// matrix, a ring element or a number)
pub fn arith(op: Op, a: &Expr, b: &Expr, r: &Rc<Ring>) -> R<Expr> {
    let side = |e: &Expr| -> R<Result<FM, Elem>> {
        match e {
            Expr::Matrix(m) => lift(m, r).map(Ok),
            _ => r.from_expr(e).map(Err),
        }
    };
    let (x, y) = (side(a)?, side(b)?);
    let out = match (op, x, y) {
        (Op::Add, Ok(p), Ok(q)) => zip(&p, &q, "sum", |s, t| s.add(t))?,
        (Op::Sub, Ok(p), Ok(q)) => zip(&p, &q, "difference", |s, t| s.add(&t.neg()))?,
        (Op::Add, Ok(p), Err(s)) | (Op::Add, Err(s), Ok(p)) => map(&p, |t| t.add(&s)),
        (Op::Sub, Ok(p), Err(s)) => map(&p, |t| t.add(&s.neg())),
        (Op::Sub, Err(s), Ok(p)) => map(&p, |t| s.add(&t.neg())),
        (Op::Mul, Ok(p), Ok(q)) => {
            let m = mul(&p, &q, r)?;
            if p.len() == 1 && p[0].len() > 1 && m.len() == 1 && m[0].len() == 1 {
                return Ok(ff::value(m[0][0].clone()));
            }
            m
        }
        (Op::Mul, Ok(p), Err(s)) | (Op::Mul, Err(s), Ok(p)) => map(&p, |t| t.mul(&s)),
        (Op::Div, Ok(p), Ok(q)) => mul(&p, &inverse(&q, r)?, r)?,
        (Op::Div, Ok(p), Err(s)) => {
            let i = s.inv()?;
            map(&p, |t| t.mul(&i))
        }
        (Op::Div, Err(_), Ok(_)) => return Err("can't divide by a matrix; use inv(A)".into()),
        (Op::Solve, Ok(p), Ok(q)) => solve(&p, &q, r)?,
        (Op::Pow, Ok(p), Err(_)) => {
            let e = b
                .num()
                .filter(|n| n.is_integer())
                .and_then(|n| n.as_bigint())
                .ok_or("matrix powers must be whole numbers")?;
            pow(&p, &e, r)?
        }
        (Op::Pow, _, Ok(_)) => return Err("matrix exponents are not supported".into()),
        (_, Err(s), Err(t)) => {
            return ff::arith(op, &ff::value(s), &ff::value(t));
        }
        _ => return Err("not an operation on matrices".into()),
    };
    Ok(expr(&out))
}

/// Linear system over a ring: the coefficient rows and right-hand sides
pub fn system(a: &FM, b: &[Elem], vars: &[String], r: &Rc<Ring>) -> R<Vec<Expr>> {
    let n = vars.len();
    let aug: FM = a
        .iter()
        .zip(b)
        .map(|(row, c)| row.iter().cloned().chain(std::iter::once(c.clone())).collect())
        .collect();
    if a.len() == n && !r.is_field() {
        let bm: FM = b.iter().map(|c| vec![c.clone()]).collect();
        let x = solve_square(a, &bm, r)?;
        return Ok(x.into_iter().map(|row| ff::value(row[0].clone())).collect());
    }
    let (red, piv) = rref(&aug, r)?;
    if piv.contains(&n) {
        return Err(format!(
            "solve: no solution in {}; elimination leads to 0 = 1",
            r.name()
        ));
    }
    if piv.len() == n {
        return Ok((0..n)
            .map(|j| {
                let i = piv.iter().position(|&p| p == j).unwrap();
                ff::value(red[i][n].clone())
            })
            .collect());
    }
    // free unknowns stay as symbols; coefficients are the lifts
    use crate::build::*;
    let mut out: Vec<Expr> = vars.iter().map(|v| Expr::sym(v)).collect();
    for (i, &p) in piv.iter().enumerate() {
        let mut terms = vec![red[i][n].lift()];
        for (j, v) in vars.iter().enumerate() {
            if j != p && !piv.contains(&j) && !red[i][j].is_zero() {
                terms.push(mul2(red[i][j].neg().lift(), Expr::sym(v)));
            }
        }
        out[p] = crate::calc::simplify(&add(terms));
    }
    Ok(out)
}
