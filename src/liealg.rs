//! Lie algebras for physics and representation theory: Gell-Mann matrices,
//! structure constants, Cartan matrices and roots of the simple Lie algebras,
//! the Weyl dimension formula and Casimirs, and the irreducible
//! representations of su(n) (Gelfand–Tsetlin bases) with tensor product
//! decompositions.
//!
//! Types are named as in Bourbaki: "A2" is su(3)/sl(3), "B3" so(7), "C2"
//! sp(4), "D4" so(8), "G2", "F4", "E6", "E7", "E8". The inner product on
//! roots gives long roots length² 2.

use crate::build::*;
use crate::calc::simplify;
use crate::expr::{Const, Expr};
use crate::linalg::{self, Mat};
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;

pub type R<T> = Result<T, String>;
type Q = BigRational;

fn q(n: i64) -> Q {
    Q::from_integer(BigInt::from(n))
}

fn qexpr(r: &Q) -> Expr {
    Expr::Num(crate::num::Number::from_big(r.clone()))
}

/// sqrt of a nonnegative rational as (s/q) sqrt(t) with t squarefree
pub fn sqrt_q(r: &Q) -> Expr {
    if r.is_zero() {
        return Expr::zero();
    }
    let (p, d) = (r.numer().abs(), r.denom().clone());
    let m = &p * &d;
    let mut s = BigInt::one();
    let mut t = BigInt::one();
    if let Ok(f) = crate::numtheory::factorize(&m) {
        for (pr, e) in f {
            s *= num_traits::pow(pr.clone(), (e / 2) as usize);
            if e % 2 == 1 {
                t *= pr;
            }
        }
    } else {
        t = m;
    }
    let c = qexpr(&Q::new(s, d));
    if t.is_one() {
        c
    } else {
        simplify(&mul2(c, sqrt(Expr::Num(crate::num::Number::from_bigint(t)))))
    }
}

// ---------------------------------------------------------------- Gell-Mann

/// The generalized Gell-Mann matrices of su(n): Hermitian, traceless,
/// tr(λa λb) = 2 δab. For n = 2 the Pauli matrices, for n = 3 λ1 … λ8.
pub fn gellmann(n: usize) -> Vec<Mat> {
    let i = Expr::Const(Const::I);
    let z = || linalg::filled(n, n, Expr::zero());
    let mut out = vec![];
    for k in 1..n {
        for j in 0..k {
            let mut s = z();
            s[j][k] = Expr::one();
            s[k][j] = Expr::one();
            out.push(s);
            let mut a = z();
            a[j][k] = neg(i.clone());
            a[k][j] = i.clone();
            out.push(a);
        }
        // diagonal: sqrt(2/(l(l+1))) diag(1, …, 1, -l, 0, …) with l = k
        let l = k as i128;
        let c = sqrt_q(&Q::new(BigInt::from(2), BigInt::from(l * (l + 1))));
        let mut d = z();
        for t in 0..k {
            d[t][t] = c.clone();
        }
        d[k][k] = simplify(&mul2(Expr::int(-l), c.clone()));
        out.push(d);
    }
    out
}

// ---------------------------------------------------- structure constants

fn trace(m: &Mat) -> Expr {
    simplify(&add((0..m.len()).map(|i| m[i][i].clone()).collect()))
}

fn mm(a: &Mat, b: &Mat) -> R<Mat> {
    Ok(linalg::matmul(a, b)?
        .iter()
        .map(|r| r.iter().map(simplify).collect())
        .collect())
}

/// complex conjugate of an exact entry (real radicals and i)
fn conj(e: &Expr) -> Expr {
    let i = Expr::Const(Const::I);
    simplify(&crate::calc::subst_expr(e, &i, &neg(i.clone())))
}

fn is_hermitian(m: &Mat) -> bool {
    let n = m.len();
    (0..n).all(|i| {
        (0..n).all(|j| {
            simplify(&sub(m[i][j].clone(), conj(&m[j][i]))).is_zero()
        })
    })
}

/// The structure constants, as rows [a, b, c, value]. For a Hermitian basis
/// with tr(La Lb) = κ δab, Ta = La / sqrt(2κ) (so tr(Ta Tb) = δab / 2) and
///   kind "f": [Ta, Tb] = i f_abc Tc, f_abc = -2i tr([Ta, Tb] Tc), a < b < c
///   kind "d": {Ta, Tb} = δab/n + d_abc Tc, d_abc = 2 tr({Ta, Tb} Tc), a ≤ b ≤ c
/// Otherwise [La, Lb] = sum_c c_abc Lc, rows for a < b. Returns the rows and
/// a sentence for the notes.
pub fn structure(basis: &[Mat], kind: &str) -> R<(Mat, String)> {
    let k = basis.len();
    let n = basis.first().map_or(0, |m| m.len());
    let herm = basis.iter().all(is_hermitian);
    let mut kappa: Option<Expr> = None;
    let mut orth = herm;
    if herm {
        'g: for a in 0..k {
            for b in 0..k {
                let t = trace(&mm(&basis[a], &basis[b])?);
                if a == b {
                    match &kappa {
                        None => kappa = Some(t),
                        Some(kv) if simplify(&sub(kv.clone(), t.clone())).is_zero() => {}
                        _ => {
                            orth = false;
                            break 'g;
                        }
                    }
                } else if !t.is_zero() {
                    orth = false;
                    break 'g;
                }
            }
        }
    }
    let i = Expr::Const(Const::I);
    if orth {
        let kv = kappa.unwrap();
        let s = simplify(&sqrt(mul2(Expr::int(2), kv.clone())));
        let t: Vec<Mat> = basis
            .iter()
            .map(|m| linalg::map(m, |x| simplify(&div(x.clone(), s.clone()))))
            .collect();
        let mut rows = vec![];
        for a in 0..k {
            for b in a..k {
                if kind == "f" && b == a {
                    continue;
                }
                let ab = mm(&t[a], &t[b])?;
                let ba = mm(&t[b], &t[a])?;
                let comb: Mat = ab
                    .iter()
                    .zip(&ba)
                    .map(|(x, y)| {
                        x.iter()
                            .zip(y)
                            .map(|(p, r)| {
                                if kind == "f" {
                                    sub(p.clone(), r.clone())
                                } else {
                                    add2(p.clone(), r.clone())
                                }
                            })
                            .collect()
                    })
                    .collect();
                for c in b..k {
                    if kind == "f" && c == b {
                        continue;
                    }
                    let tr = trace(&mm(&comb, &t[c])?);
                    let v = if kind == "f" {
                        simplify(&mul(vec![Expr::int(-2), i.clone(), tr]))
                    } else {
                        simplify(&mul2(Expr::int(2), tr))
                    };
                    if !v.is_zero() {
                        rows.push(vec![
                            Expr::int(a as i128 + 1),
                            Expr::int(b as i128 + 1),
                            Expr::int(c as i128 + 1),
                            v,
                        ]);
                    }
                }
            }
        }
        let what = if kind == "f" {
            format!(
                "f_abc with [Ta, Tb] = i f_abc Tc for Ta = La/{} (so tr(Ta Tb) = δab/2), one row [a, b, c, f_abc] for each nonzero value with a < b < c; f is totally antisymmetric, so the other orders follow by sign",
                crate::print::show(&s)
            )
        } else {
            format!(
                "d_abc with {{Ta, Tb}} = δab/{} I + d_abc Tc for Ta = La/{}, one row [a, b, c, d_abc] for each nonzero value with a ≤ b ≤ c; d is totally symmetric",
                n,
                crate::print::show(&s)
            )
        };
        return Ok((rows, what));
    }
    if kind == "d" {
        return Err("structure(L, \"d\") needs a Hermitian basis with tr(La Lb) = κ δab, such as gellmann()".into());
    }
    let mut rows = vec![];
    for a in 0..k {
        for b in a + 1..k {
            let br = crate::lie::bracket(&basis[a], &basis[b])?;
            let co = crate::lie::coords(&br, basis)?;
            for (c, v) in co.into_iter().enumerate() {
                let v = simplify(&v);
                if !v.is_zero() {
                    rows.push(vec![
                        Expr::int(a as i128 + 1),
                        Expr::int(b as i128 + 1),
                        Expr::int(c as i128 + 1),
                        v,
                    ]);
                }
            }
        }
    }
    Ok((
        rows,
        "c_abc with [La, Lb] = sum_c c_abc Lc in this basis, one row [a, b, c, c_abc] for each nonzero value with a < b".into(),
    ))
}

// ------------------------------------------------- root systems by type

#[derive(Clone, Debug)]
pub struct RootSystem {
    pub name: String,
    /// (αi, αj), long roots length² 2
    pub b: Vec<Vec<Q>>,
}

impl RootSystem {
    pub fn rank(&self) -> usize {
        self.b.len()
    }
    /// a_ij = 2 (αi, αj) / (αj, αj)
    pub fn cartan(&self) -> Vec<Vec<Q>> {
        let r = self.rank();
        (0..r)
            .map(|i| (0..r).map(|j| q(2) * &self.b[i][j] / &self.b[j][j]).collect())
            .collect()
    }
    fn ip(&self, x: &[Q], y: &[Q]) -> Q {
        let r = self.rank();
        let mut s = Q::zero();
        for i in 0..r {
            for j in 0..r {
                s += &x[i] * &y[j] * &self.b[i][j];
            }
        }
        s
    }
    /// positive roots in simple-root coordinates, by height
    pub fn positive_roots(&self) -> Vec<Vec<i64>> {
        let r = self.rank();
        let mut roots: Vec<Vec<i64>> = (0..r)
            .map(|i| (0..r).map(|j| (i == j) as i64).collect())
            .collect();
        let mut known: std::collections::HashSet<Vec<i64>> = roots.iter().cloned().collect();
        let mut k = 0;
        while k < roots.len() {
            let beta = roots[k].clone();
            let bq: Vec<Q> = beta.iter().map(|&c| q(c)).collect();
            for i in 0..r {
                let ai: Vec<Q> = (0..r).map(|j| q((i == j) as i64)).collect();
                // <β, αi^∨> and p = how far down the αi-string goes
                let pair = q(2) * self.ip(&bq, &ai) / &self.b[i][i];
                let mut p = 0i64;
                loop {
                    let mut down = beta.clone();
                    down[i] -= p + 1;
                    if known.contains(&down) {
                        p += 1;
                    } else {
                        break;
                    }
                }
                if q(p) - &pair > Q::zero() {
                    let mut up = beta.clone();
                    up[i] += 1;
                    if known.insert(up.clone()) {
                        roots.push(up);
                    }
                }
            }
            k += 1;
        }
        roots.sort_by_key(|v| (v.iter().sum::<i64>(), v.iter().map(|x| -x).collect::<Vec<_>>()));
        roots
    }
    /// (αj, αj) / 2: the pairing of the fundamental weight ωj with αj
    fn half(&self, j: usize) -> Q {
        &self.b[j][j] / q(2)
    }
    /// The Weyl dimension formula: product over positive roots of
    /// (λ + ρ, β) / (ρ, β)
    pub fn dim(&self, w: &[i64]) -> BigInt {
        let mut num = Q::one();
        for beta in self.positive_roots() {
            let mut a = Q::zero();
            let mut b = Q::zero();
            for (j, &c) in beta.iter().enumerate() {
                a += q(c) * q(w[j] + 1) * self.half(j);
                b += q(c) * self.half(j);
            }
            num = num * a / b;
        }
        num.to_integer()
    }
    /// (ωi, ωj) = (A⁻¹)ij (αj, αj)/2
    fn weight_form(&self) -> R<Vec<Vec<Q>>> {
        let r = self.rank();
        let a: Mat = self
            .cartan()
            .iter()
            .map(|row| row.iter().map(qexpr).collect())
            .collect();
        let inv = linalg::inverse(&a)?;
        let mut out = vec![vec![Q::zero(); r]; r];
        for i in 0..r {
            for j in 0..r {
                let v = simplify(&inv[i][j]);
                let v = v.num().and_then(|n| n.big()).ok_or("weight form")?;
                out[i][j] = v * self.half(j);
            }
        }
        Ok(out)
    }
    /// The quadratic Casimir (λ, λ + 2ρ) / 2, which for su(n) is sum Ta Ta with
    /// tr(Ta Tb) = δab/2 in the defining representation
    pub fn casimir(&self, w: &[i64]) -> R<Q> {
        let g = self.weight_form()?;
        let r = self.rank();
        let mut s = Q::zero();
        for i in 0..r {
            for j in 0..r {
                s += q(w[i]) * q(w[j] + 2) * &g[i][j];
            }
        }
        Ok(s / q(2))
    }
}

/// "A2", "B3", "G2", ... (Bourbaki numbering)
pub fn root_system(name: &str) -> R<RootSystem> {
    let t = name.trim().to_uppercase();
    let (letter, rank) = t.split_at(1);
    let n: usize = rank
        .parse()
        .map_err(|_| format!("a Lie type is a letter and a rank, such as \"A2\" or \"G2\", not \"{}\"", name))?;
    let ok = match letter {
        "A" => n >= 1,
        "B" => n >= 2,
        "C" => n >= 3 || n == 2,
        "D" => n >= 4,
        "E" => (6..=8).contains(&n),
        "F" => n == 4,
        "G" => n == 2,
        _ => false,
    };
    if !ok || n > 12 {
        return Err(format!(
            "no simple Lie algebra of type {}: the types are A1.., B2.., C2.., D4.., E6, E7, E8, F4, G2 (rank up to 12)",
            t
        ));
    }
    let mut b = vec![vec![Q::zero(); n]; n];
    let half = || Q::new(BigInt::from(1), BigInt::from(2));
    for i in 0..n {
        b[i][i] = q(2);
    }
    let link = |b: &mut Vec<Vec<Q>>, i: usize, j: usize, v: Q| {
        b[i][j] = v.clone();
        b[j][i] = v;
    };
    match letter {
        "A" | "B" => {
            for i in 0..n - 1 {
                link(&mut b, i, i + 1, q(-1));
            }
            if letter == "B" {
                b[n - 1][n - 1] = q(1);
            }
        }
        "C" => {
            for i in 0..n {
                b[i][i] = q(1);
            }
            b[n - 1][n - 1] = q(2);
            for i in 0..n - 1 {
                link(&mut b, i, i + 1, -half());
            }
            link(&mut b, n - 2, n - 1, q(-1));
        }
        "D" => {
            for i in 0..n - 2 {
                link(&mut b, i, i + 1, q(-1));
            }
            link(&mut b, n - 3, n - 1, q(-1));
        }
        "E" => {
            // α1 - α3 - α4 - α5 - …, α2 attached to α4
            link(&mut b, 0, 2, q(-1));
            link(&mut b, 1, 3, q(-1));
            for i in 2..n - 1 {
                link(&mut b, i, i + 1, q(-1));
            }
        }
        "F" => {
            b[2][2] = q(1);
            b[3][3] = q(1);
            link(&mut b, 0, 1, q(-1));
            link(&mut b, 1, 2, q(-1));
            link(&mut b, 2, 3, -half());
        }
        _ => {
            // G2: α1 short
            b[0][0] = Q::new(BigInt::from(2), BigInt::from(3));
            link(&mut b, 0, 1, q(-1));
        }
    }
    Ok(RootSystem {
        name: format!("{}{}", letter, n),
        b,
    })
}

/// The type of a list of matrices: su(n), sl(n), gellmann(n) are A(n-1),
/// so(n) is B or D
pub fn type_of_basis(basis: &[Mat]) -> R<String> {
    let n = basis.first().map_or(0, |m| m.len());
    let k = basis.len();
    if n >= 2 && k == n * n - 1 {
        return Ok(format!("A{}", n - 1));
    }
    if n >= 5 && k == n * (n - 1) / 2 {
        return Ok(if n % 2 == 1 {
            format!("B{}", (n - 1) / 2)
        } else {
            format!("D{}", n / 2)
        });
    }
    if n == 3 && k == 3 {
        return Ok("A1".into());
    }
    Err("can't tell the type of this algebra; give it as a string such as \"A2\" or \"G2\"".into())
}

// ------------------------------------------------ su(n) representations

/// A Gelfand–Tsetlin pattern, rows from the top (length n) down to length 1
type Pattern = Vec<Vec<i64>>;

fn patterns(top: &[i64]) -> Vec<Pattern> {
    fn below(row: &[i64], acc: &mut Vec<Vec<i64>>, out: &mut Vec<Pattern>, start: &Pattern) {
        if row.len() == 1 {
            let mut p = start.clone();
            p.extend(acc.iter().cloned());
            out.push(p);
            return;
        }
        // rows interlace: row[i] >= next[i] >= row[i + 1], larger first
        let k = row.len() - 1;
        let mut next = vec![0; k];
        fn fill(
            i: usize,
            row: &[i64],
            next: &mut Vec<i64>,
            acc: &mut Vec<Vec<i64>>,
            out: &mut Vec<Pattern>,
            start: &Pattern,
        ) {
            if i == next.len() {
                acc.push(next.clone());
                below(&next.clone(), acc, out, start);
                acc.pop();
                return;
            }
            let mut v = row[i];
            while v >= row[i + 1] {
                next[i] = v;
                fill(i + 1, row, next, acc, out, start);
                v -= 1;
            }
        }
        fill(0, row, &mut next, acc, out, start);
    }
    let mut out = vec![];
    below(top, &mut vec![], &mut out, &vec![top.to_vec()]);
    out
}

/// row k (1-based length k) of a pattern
fn row(p: &Pattern, k: usize) -> &Vec<i64> {
    &p[p.len() - k]
}

/// The weight (eigenvalues of E11, …, Enn)
fn weight(p: &Pattern) -> Vec<i64> {
    let n = p.len();
    let s = |k: usize| -> i64 { if k == 0 { 0 } else { row(p, k).iter().sum() } };
    (1..=n).map(|k| s(k) - s(k - 1)).collect()
}

pub const MAX_REP_DIM: usize = 200;

/// Dynkin labels to the top row (λ1 ≥ … ≥ λn = 0)
fn top_row(w: &[i64]) -> Vec<i64> {
    let n = w.len() + 1;
    (0..n).map(|i| w[i..].iter().sum()).collect()
}

/// The su(n) representation with highest weight w (Dynkin labels): the images
/// of E_ij (i, j from 0) in an orthonormal Gelfand–Tsetlin basis, ordered from
/// the highest weight down
pub fn gt_rep(w: &[i64]) -> R<(Vec<Pattern>, HashMap<(usize, usize), Mat>)> {
    let n = w.len() + 1;
    if w.iter().any(|&x| x < 0) {
        return Err("a highest weight has Dynkin labels ≥ 0".into());
    }
    let top = top_row(w);
    let pats = patterns(&top);
    let d = pats.len();
    if d > MAX_REP_DIM {
        return Err(format!(
            "this representation has dimension {}; matrices are built up to {}",
            d, MAX_REP_DIM
        ));
    }
    let idx: HashMap<Pattern, usize> = pats.iter().cloned().enumerate().map(|(i, p)| (p, i)).collect();
    let l = |p: &Pattern, k: usize, i: usize| -> Q { q(row(p, k)[i - 1] - i as i64 + 1) };
    // unnormalized raising/lowering coefficients (Molev, Gelfand–Tsetlin bases, (2.3)–(2.5))
    let mut up: Vec<Vec<Vec<Q>>> = vec![vec![vec![Q::zero(); d]; d]; n]; // up[k][dst][src]
    let mut dn: Vec<Vec<Vec<Q>>> = vec![vec![vec![Q::zero(); d]; d]; n];
    for (s, p) in pats.iter().enumerate() {
        for k in 1..n {
            for i in 1..=k {
                let mut den = Q::one();
                for j in 1..=k {
                    if j != i {
                        den *= l(p, k, i) - l(p, k, j);
                    }
                }
                // E_{k,k+1}: m_ki + 1
                let mut pp = p.clone();
                let rk = pp.len() - k;
                pp[rk][i - 1] += 1;
                if let Some(&t) = idx.get(&pp) {
                    let mut num = Q::one();
                    for j in 1..=k + 1 {
                        num *= l(p, k, i) - l(p, k + 1, j);
                    }
                    up[k][t][s] = -(num / &den);
                }
                // E_{k+1,k}: m_ki - 1
                let mut pm = p.clone();
                pm[rk][i - 1] -= 1;
                if let Some(&t) = idx.get(&pm) {
                    let mut num = Q::one();
                    for j in 1..k {
                        num *= l(p, k, i) - l(p, k - 1, j);
                    }
                    dn[k][t][s] = num / &den;
                }
            }
        }
    }
    // squared norms: (N_up / N)^2 = b / a along each raising edge
    let mut norm2: Vec<Option<Q>> = vec![None; d];
    norm2[0] = Some(Q::one());
    let mut stack = vec![0usize];
    while let Some(s) = stack.pop() {
        let ns = norm2[s].clone().unwrap();
        for k in 1..n {
            for t in 0..d {
                // s -> t lowering with coefficient b = dn[k][t][s]; back up a = up[k][s][t]
                let b = &dn[k][t][s];
                if !b.is_zero() && norm2[t].is_none() {
                    let a = &up[k][s][t];
                    if a.is_zero() {
                        return Err("Gelfand–Tsetlin: a lowering step has no raising partner".into());
                    }
                    // (N_s / N_t)^2 = dn[t←s] / up[s←t]
                    norm2[t] = Some(&ns * a / b);
                    stack.push(t);
                }
            }
        }
    }
    if norm2.iter().any(|v| v.as_ref().is_none_or(|x| !x.is_positive())) {
        return Err("Gelfand–Tsetlin: the basis did not normalize".into());
    }
    let norm2: Vec<Q> = norm2.into_iter().map(|v| v.unwrap()).collect();
    // rational matrices of E_ij in the unnormalized basis
    let mut e: HashMap<(usize, usize), Vec<Vec<Q>>> = HashMap::new();
    for k in 1..=n {
        let mut m = vec![vec![Q::zero(); d]; d];
        for (s, p) in pats.iter().enumerate() {
            m[s][s] = q(weight(p)[k - 1]);
        }
        e.insert((k - 1, k - 1), m);
    }
    for k in 1..n {
        e.insert((k - 1, k), up[k].clone());
        e.insert((k, k - 1), dn[k].clone());
    }
    let qmul = |a: &Vec<Vec<Q>>, b: &Vec<Vec<Q>>| -> Vec<Vec<Q>> {
        let mut out = vec![vec![Q::zero(); d]; d];
        for i in 0..d {
            for t in 0..d {
                if a[i][t].is_zero() {
                    continue;
                }
                for j in 0..d {
                    if !b[t][j].is_zero() {
                        out[i][j] += &a[i][t] * &b[t][j];
                    }
                }
            }
        }
        out
    };
    for gap in 2..n {
        for i in 0..n - gap {
            let j = i + gap;
            // E_ij = [E_{i,i+1}, E_{i+1,j}],  E_ji = [E_{j,i+1}, E_{i+1,i}]
            let (a, b) = (&e[&(i, i + 1)], &e[&(i + 1, j)]);
            let (ab, ba) = (qmul(a, b), qmul(b, a));
            let m: Vec<Vec<Q>> = ab.iter().zip(&ba).map(|(x, y)| x.iter().zip(y).map(|(p, r)| p - r).collect()).collect();
            e.insert((i, j), m);
            let (a, b) = (&e[&(j, i + 1)], &e[&(i + 1, i)]);
            let (ab, ba) = (qmul(a, b), qmul(b, a));
            let m: Vec<Vec<Q>> = ab.iter().zip(&ba).map(|(x, y)| x.iter().zip(y).map(|(p, r)| p - r).collect()).collect();
            e.insert((j, i), m);
        }
    }
    // normalize: entry [t][s] times sqrt(N_t^2 / N_s^2)
    let mut out = HashMap::new();
    for ((i, j), m) in e {
        let em: Mat = (0..d)
            .map(|t| {
                (0..d)
                    .map(|s| {
                        if m[t][s].is_zero() {
                            Expr::zero()
                        } else {
                            let ratio = &norm2[t] / &norm2[s];
                            simplify(&mul2(qexpr(&m[t][s]), sqrt_q(&ratio)))
                        }
                    })
                    .collect()
            })
            .collect();
        out.insert((i, j), em);
    }
    Ok((pats, out))
}

/// ρ(X) = sum X_ij ρ(E_ij) for each matrix X of the basis
pub fn rep(basis: &[Mat], w: &[i64]) -> R<Vec<Mat>> {
    let n = basis.first().map_or(0, |m| m.len());
    if w.len() + 1 != n {
        return Err(format!(
            "rep: {}x{} matrices need a highest weight with {} Dynkin labels",
            n,
            n,
            n - 1
        ));
    }
    let (pats, e) = gt_rep(w)?;
    let d = pats.len();
    basis
        .iter()
        .map(|x| {
            let mut terms: Vec<Vec<Vec<Expr>>> = vec![vec![vec![]; d]; d];
            for i in 0..n {
                for j in 0..n {
                    let c = simplify(&x[i][j]);
                    if c.is_zero() {
                        continue;
                    }
                    let m = &e[&(i, j)];
                    for t in 0..d {
                        for s in 0..d {
                            if !m[t][s].is_zero() {
                                terms[t][s].push(mul2(c.clone(), m[t][s].clone()));
                            }
                        }
                    }
                }
            }
            Ok(terms
                .into_iter()
                .map(|r| r.into_iter().map(|v| if v.is_empty() { Expr::zero() } else { simplify(&add(v)) }).collect())
                .collect())
        })
        .collect()
}

/// The weights of the su(n) irrep with highest weight w, in Dynkin labels,
/// one per basis vector (repeated by multiplicity)
pub fn weights(w: &[i64]) -> Vec<Vec<i64>> {
    patterns(&top_row(w))
        .iter()
        .map(|p| {
            let e = weight(p);
            (0..e.len() - 1).map(|i| e[i] - e[i + 1]).collect()
        })
        .collect()
}

/// Decompose a tensor product of su(n) irreps into irreps (highest weights in
/// Dynkin labels, repeated by multiplicity, largest first)
pub fn decompose(ws: &[Vec<i64>]) -> R<Vec<Vec<i64>>> {
    let n = ws[0].len() + 1;
    let mut total: u64 = 1;
    for w in ws {
        total = total.saturating_mul(patterns(&top_row(w)).len() as u64);
    }
    if total > 200_000 {
        return Err(format!("decompose: the product has dimension {}, more than 200000", total));
    }
    // the character as a multiset of gl(n) weights
    let mut ch: HashMap<Vec<i64>, i64> = HashMap::new();
    ch.insert(vec![0; n], 1);
    for w in ws {
        let mut next = HashMap::new();
        let wts: Vec<Vec<i64>> = patterns(&top_row(w)).iter().map(weight).collect();
        for (mu, m) in &ch {
            for v in &wts {
                let s: Vec<i64> = mu.iter().zip(v).map(|(a, b)| a + b).collect();
                *next.entry(s).or_insert(0) += m;
            }
        }
        ch = next;
    }
    let mut out = vec![];
    loop {
        ch.retain(|_, m| *m != 0);
        let Some(top) = ch.keys().max().cloned() else { break };
        let m = ch[&top];
        if m < 0 || top.windows(2).any(|p| p[0] < p[1]) {
            return Err("decompose: the character did not split (internal error)".into());
        }
        for p in patterns(&top) {
            *ch.entry(weight(&p)).or_insert(0) -= m;
        }
        let dynkin: Vec<i64> = (0..n - 1).map(|i| top[i] - top[i + 1]).collect();
        for _ in 0..m {
            out.push(dynkin.clone());
        }
    }
    Ok(out)
}

pub fn dim_a(w: &[i64]) -> u64 {
    patterns(&top_row(w)).len() as u64
}

pub fn small(x: &BigInt) -> Option<i64> {
    x.to_i64()
}
