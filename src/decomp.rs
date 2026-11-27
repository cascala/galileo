//! Matrix decompositions: Gram-Schmidt and QR, Cholesky, the singular value
//! decomposition, the Jordan form, the matrix exponential and the Kronecker
//! product. Exact when the entries are exact (square roots kept as radicals),
//! numeric otherwise.

use crate::build::*;
use crate::calc::{evalf, simplify, subst};
use crate::expr::Expr;
use crate::linalg::{self, dims, matmul, transpose, Mat};
use crate::num::Number;
use std::collections::BTreeMap;

pub type R<T> = Result<T, String>;
/// P, J and the Jordan blocks (eigenvalue, size)
pub type Jordan = (Mat, Mat, Vec<(Expr, usize)>);

// ---------- numbers of the form q1 + q2*sqrt(m2) + ... (m squarefree) ----------

fn squarefree(n: i128) -> (i128, i128) {
    // n = s^2 * m with m squarefree; returns (s, m)
    let (mut s, mut m, mut p) = (1i128, n, 2i128);
    while p * p <= m {
        while m % (p * p) == 0 {
            m /= p * p;
            s *= p;
        }
        p += 1;
    }
    (s, m)
}

type Rad = BTreeMap<i128, Number>;

fn rad_parse(e: &Expr) -> Option<Rad> {
    let one = |m: i128, c: Number| {
        let mut r = Rad::new();
        if !c.is_zero() {
            r.insert(m, c);
        }
        r
    };
    match e {
        Expr::Num(n) if n.is_exact() && n.big().is_some() => {
            // exclude Gaussian numbers
            Some(one(1, n.clone()))
        }
        Expr::Pow(b, x) => {
            let (Expr::Num(bn), Expr::Num(xn)) = (&**b, &**x) else {
                return None;
            };
            let (p, q) = (bn.numer()?, bn.denom()?);
            if p <= 0 || !bn.is_exact() {
                return None;
            }
            let half = Number::rat(1, 2)?;
            let mhalf = Number::rat(-1, 2)?;
            // sqrt(p/q) = sqrt(p*q)/q
            let (s, m) = squarefree(p.checked_mul(q)?);
            if *xn == half {
                Some(one(m, Number::rat(s, q)?))
            } else if *xn == mhalf {
                // 1/sqrt(p/q) = sqrt(q/p) = sqrt(p*q)/p
                Some(one(m, Number::rat(s, p)?))
            } else {
                None
            }
        }
        Expr::Mul(v) => {
            let mut acc = one(1, Number::one());
            for f in v {
                let r = rad_parse(f)?;
                let mut out = Rad::new();
                for (m1, c1) in &acc {
                    for (m2, c2) in &r {
                        let g = num_gcd(*m1, *m2);
                        let m = (m1 / g).checked_mul(m2 / g)?;
                        let c = c1.mul(c2).mul(Number::int(g));
                        let e = out.entry(m).or_insert_with(Number::zero);
                        *e = e.add(c);
                    }
                }
                out.retain(|_, c| !c.is_zero());
                acc = out;
            }
            Some(acc)
        }
        Expr::Add(v) => {
            let mut acc = Rad::new();
            for t in v {
                for (m, c) in rad_parse(t)? {
                    let e = acc.entry(m).or_insert_with(Number::zero);
                    *e = e.add(c);
                }
            }
            acc.retain(|_, c| !c.is_zero());
            Some(acc)
        }
        _ => None,
    }
}

fn num_gcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a.max(1)
}

/// Normal form of sums of rational multiples of square roots:
/// 2/sqrt(2) = sqrt(2), sqrt(2)*sqrt(6) = 2*sqrt(3). Other expressions are simplified.
pub fn rad(e: &Expr) -> Expr {
    let s = simplify(e);
    match rad_parse(&s) {
        Some(r) => {
            let terms: Vec<Expr> = r
                .into_iter()
                .map(|(m, c)| {
                    if m == 1 {
                        Expr::Num(c)
                    } else {
                        mul2(Expr::Num(c), sqrt(Expr::int(m)))
                    }
                })
                .collect();
            simplify(&add(terms))
        }
        None => s,
    }
}

fn radm(m: &Mat) -> Mat {
    linalg::map(m, rad)
}

pub fn has_float(m: &Mat) -> bool {
    m.iter().flatten().any(crate::vcalc::has_float)
}

pub fn is_numeric(m: &Mat) -> bool {
    m.iter().flatten().all(|e| e.symbols().is_empty())
}

fn is_zero(e: &Expr) -> bool {
    let s = rad(e);
    if s.is_zero() {
        return true;
    }
    if let Some(n) = s.num() {
        if !n.is_exact() {
            return n.to_f64().abs() < 1e-12;
        }
    }
    false
}

fn dot(u: &[Expr], v: &[Expr]) -> Expr {
    rad(&add(u.iter().zip(v).map(|(a, b)| mul2(a.clone(), b.clone())).collect()))
}

fn col(m: &Mat, j: usize) -> Vec<Expr> {
    m.iter().map(|r| r[j].clone()).collect()
}

fn from_cols(c: &[Vec<Expr>], rows: usize) -> Mat {
    (0..rows).map(|i| c.iter().map(|v| v[i].clone()).collect()).collect()
}

fn axpy(v: &[Expr], s: &Expr, w: &[Expr]) -> Vec<Expr> {
    // v - s*w
    v.iter()
        .zip(w)
        .map(|(a, b)| rad(&sub(a.clone(), mul2(s.clone(), b.clone()))))
        .collect()
}

/// Orthogonal (not normalized) directions spanning the given vectors, in order;
/// for each input, Some(index into the output) or None if it depended on earlier ones
fn orthogonalize(vs: &[Vec<Expr>]) -> (Vec<Vec<Expr>>, Vec<Option<usize>>) {
    let mut ws: Vec<Vec<Expr>> = vec![];
    let mut which = vec![];
    for a in vs {
        let mut w = a.clone();
        for u in &ws {
            let c = rad(&div(dot(u, a), dot(u, u)));
            if !c.is_zero() {
                w = axpy(&w, &c, u);
            }
        }
        if w.iter().all(is_zero) {
            which.push(None);
        } else {
            // clear denominators of exact rational directions: nicer, same direction
            which.push(Some(ws.len()));
            ws.push(w);
        }
    }
    (ws, which)
}

fn norm(v: &[Expr]) -> Expr {
    rad(&sqrt(dot(v, v)))
}

fn normalize(v: &[Expr]) -> Vec<Expr> {
    let n = norm(v);
    v.iter().map(|x| rad(&div(x.clone(), n.clone()))).collect()
}

/// Orthonormal basis of the column space (Gram-Schmidt), as columns
pub fn gramschmidt(a: &Mat) -> Mat {
    let (r, c) = dims(a);
    let cols: Vec<Vec<Expr>> = (0..c).map(|j| col(a, j)).collect();
    let (ws, _) = orthogonalize(&cols);
    let qs: Vec<Vec<Expr>> = ws.iter().map(|w| normalize(w)).collect();
    from_cols(&qs, r)
}

/// A = Q*R with orthonormal columns in Q and R upper triangular (reduced QR)
pub fn qr(a: &Mat) -> R<(Mat, Mat)> {
    let (r, c) = dims(a);
    let cols: Vec<Vec<Expr>> = (0..c).map(|j| col(a, j)).collect();
    let (ws, which) = orthogonalize(&cols);
    if which.iter().any(|w| w.is_none()) {
        let k = which.iter().position(|w| w.is_none()).unwrap();
        return Err(format!(
            "qr: column {} is a combination of the columns before it; qr needs independent columns (gramschmidt(A) gives an orthonormal basis anyway)",
            k + 1
        ));
    }
    let qs: Vec<Vec<Expr>> = ws.iter().map(|w| normalize(w)).collect();
    let rm: Mat = (0..c)
        .map(|i| {
            (0..c)
                .map(|j| if j < i { Expr::zero() } else { dot(&qs[i], &cols[j]) })
                .collect()
        })
        .collect();
    Ok((from_cols(&qs, r), rm))
}

/// Cholesky: A = L*L^T with L lower triangular and a positive diagonal.
/// Also returns the pivots d (A = L0*D*L0^T) for the explanation.
pub fn cholesky(a: &Mat) -> R<(Mat, Vec<Expr>)> {
    let (n, m) = dims(a);
    if n != m {
        return Err("chol needs a square matrix".into());
    }
    for i in 0..n {
        for j in 0..i {
            if !is_zero(&sub(a[i][j].clone(), a[j][i].clone())) {
                return Err("chol needs a symmetric matrix (A = transpose(A))".into());
            }
        }
    }
    let mut l0 = linalg::identity(n, n);
    let mut d: Vec<Expr> = vec![];
    for j in 0..n {
        let dj = rad(&sub(
            a[j][j].clone(),
            add((0..j)
                .map(|k| mul(vec![l0[j][k].clone(), l0[j][k].clone(), d[k].clone()]))
                .collect()),
        ));
        if let Some(v) = evalf(&dj).num().map(|x| x.to_f64()) {
            if v <= 1e-14 {
                return Err(format!(
                    "chol: the matrix is not positive definite (pivot {} is {}); every pivot of a positive definite matrix is positive",
                    j + 1,
                    crate::print::show(&dj)
                ));
            }
        }
        for i in j + 1..n {
            let s = sub(
                a[i][j].clone(),
                add((0..j)
                    .map(|k| mul(vec![l0[i][k].clone(), l0[j][k].clone(), d[k].clone()]))
                    .collect()),
            );
            l0[i][j] = rad(&div(s, dj.clone()));
        }
        d.push(dj);
    }
    let l: Mat = (0..n)
        .map(|i| {
            (0..n)
                .map(|j| if j > i { Expr::zero() } else { rad(&mul2(l0[i][j].clone(), sqrt(d[j].clone()))) })
                .collect()
        })
        .collect();
    Ok((l, d))
}

// ---------- eigenvalues with multiplicity ----------

fn charpoly_coeffs(a: &Mat) -> R<Vec<Expr>> {
    let n = a.len();
    let x = Expr::sym("λ");
    let s: Mat = (0..n)
        .map(|i| {
            (0..n)
                .map(|j| if i == j { sub(a[i][j].clone(), x.clone()) } else { a[i][j].clone() })
                .collect()
        })
        .collect();
    let d = crate::calc::expand(&linalg::det(&s)?);
    crate::cas::poly_coeffs(&d, "λ").ok_or_else(|| "no characteristic polynomial".to_string())
}

/// Distinct eigenvalues, exact (error when only numeric roots are found)
pub fn exact_eigenvalues(a: &Mat, what: &str) -> R<Vec<Expr>> {
    let c = charpoly_coeffs(a)?;
    let roots = crate::cas::poly_roots(&c)?;
    let mut out: Vec<Expr> = vec![];
    for l in roots {
        if crate::vcalc::has_float(&l) {
            return Err(format!(
                "{}: the eigenvalues are only known numerically (the characteristic polynomial does not factor)",
                what
            ));
        }
        if !out.iter().any(|d| is_zero(&sub(d.clone(), l.clone()))) {
            out.push(l);
        }
    }
    Ok(out)
}

fn shifted(a: &Mat, l: &Expr) -> Mat {
    (0..a.len())
        .map(|i| {
            (0..a.len())
                .map(|j| if i == j { simplify(&sub(a[i][j].clone(), l.clone())) } else { a[i][j].clone() })
                .collect()
        })
        .collect()
}

fn rank(vs: &[Vec<Expr>], n: usize) -> usize {
    if vs.is_empty() {
        return 0;
    }
    linalg::rref(&from_cols(vs, n)).1.len()
}

fn apply(m: &Mat, v: &[Expr]) -> Vec<Expr> {
    m.iter().map(|r| dot(r, v)).collect()
}

/// Jordan form: A = P*J*P^-1. Returns P, J and the blocks (eigenvalue, size).
pub fn jordan(a: &Mat) -> R<Jordan> {
    let (n, m) = dims(a);
    if n != m {
        return Err("jordan needs a square matrix".into());
    }
    let mut chains: Vec<(Expr, Vec<Vec<Expr>>)> = vec![];
    for l in exact_eigenvalues(a, "jordan")? {
        let nm = shifted(a, &l);
        // kernels of N, N^2, ... until they stop growing
        let mut kernels: Vec<Vec<Vec<Expr>>> = vec![vec![]];
        let mut pw = nm.clone();
        loop {
            let k = linalg::nullspace(&pw);
            let ks: Vec<Vec<Expr>> = (0..dims(&k).1).map(|j| col(&k, j)).collect();
            if ks.len() == kernels.last().unwrap().len() || kernels.len() > n {
                break;
            }
            kernels.push(ks);
            pw = linalg::map(&matmul(&pw, &nm)?, simplify);
        }
        let p = kernels.len() - 1;
        if p == 0 {
            return Err("jordan: could not find the eigenvectors exactly".into());
        }
        let mut tops: Vec<(Vec<Expr>, usize)> = vec![];
        for k in (1..=p).rev() {
            let mut s: Vec<Vec<Expr>> = kernels[k - 1].clone();
            for (v, lvl) in &tops {
                let mut w = v.clone();
                for _ in 0..(lvl - k) {
                    w = apply(&nm, &w);
                }
                s.push(w);
            }
            for c in &kernels[k] {
                let r0 = rank(&s, n);
                s.push(c.clone());
                if rank(&s, n) > r0 {
                    tops.push((c.clone(), k));
                } else {
                    s.pop();
                }
            }
        }
        // longest chains first
        tops.sort_by_key(|t| std::cmp::Reverse(t.1));
        for (v, lvl) in tops {
            let mut chain = vec![v];
            for _ in 1..lvl {
                let next = apply(&nm, chain.last().unwrap());
                chain.push(next);
            }
            chain.reverse();
            chains.push((l.clone(), chain));
        }
    }
    let total: usize = chains.iter().map(|c| c.1.len()).sum();
    if total != n {
        return Err("jordan: could not find enough generalized eigenvectors exactly".into());
    }
    let mut pc: Vec<Vec<Expr>> = vec![];
    let mut j = linalg::filled(n, n, Expr::zero());
    let mut blocks = vec![];
    let mut at = 0;
    for (l, chain) in &chains {
        for (k, v) in chain.iter().enumerate() {
            j[at + k][at + k] = l.clone();
            if k > 0 {
                j[at + k - 1][at + k] = Expr::one();
            }
            pc.push(v.clone());
        }
        blocks.push((l.clone(), chain.len()));
        at += chain.len();
    }
    Ok((from_cols(&pc, n), j, blocks))
}

// ---------- matrix exponential ----------

fn re_im(e: &Expr) -> (Expr, Expr) {
    let s = simplify(e);
    match crate::build::gauss(&s) {
        Some((re, im)) => (Expr::Num(re), Expr::Num(im)),
        None => (s, Expr::zero()),
    }
}

/// e^(A t) exactly, for a matrix of exact numbers; t is a symbol name or None (t = 1)
pub fn expm_exact(a: &Mat, t: Option<&str>) -> R<Mat> {
    let n = a.len();
    let tv = match t {
        Some(s) => Expr::sym(s),
        None => Expr::one(),
    };
    let (p, _j, blocks) = jordan(a)?;
    // real form: complex eigenvalues a ± bi (non-defective) give rotation blocks
    let mut cols: Vec<Vec<Expr>> = vec![];
    let mut e = linalg::filled(n, n, Expr::zero());
    let mut at = 0; // position in P
    let mut out = 0; // position in the real basis
    let mut seen_conj: Vec<Expr> = vec![];
    for (l, size) in &blocks {
        let (re, im) = re_im(l);
        if im.is_zero() {
            for k in 0..*size {
                cols.push(col(&p, at + k));
            }
            for i in 0..*size {
                for jx in i..*size {
                    let k = jx - i;
                    let f = Expr::int((1..=k as i128).product::<i128>().max(1));
                    e[out + i][out + jx] = simplify(&div(
                        mul2(pow(tv.clone(), Expr::int(k as i128)), exp(mul2(l.clone(), tv.clone()))),
                        f,
                    ));
                }
            }
            out += size;
        } else {
            if *size != 1 {
                return Err("expm: repeated complex eigenvalues are not supported exactly".into());
            }
            if im.num().is_some_and(|x| x.is_negative()) {
                // its conjugate supplies the block
                if !seen_conj.iter().any(|c| is_zero(&sub(c.clone(), l.clone()))) {
                    seen_conj.push(l.clone());
                }
                at += size;
                continue;
            }
            let v = col(&p, at);
            let x: Vec<Expr> = v.iter().map(|z| re_im(z).0).collect();
            let y: Vec<Expr> = v.iter().map(|z| re_im(z).1).collect();
            cols.push(x);
            cols.push(y);
            let ea = exp(mul2(re.clone(), tv.clone()));
            let bt = mul2(im.clone(), tv.clone());
            let c = mul2(ea.clone(), cos(bt.clone()));
            let s = mul2(ea, sin(bt));
            e[out][out] = simplify(&c);
            e[out][out + 1] = simplify(&s);
            e[out + 1][out] = simplify(&neg(s));
            e[out + 1][out + 1] = simplify(&c);
            out += 2;
        }
        at += size;
    }
    if out != n {
        return Err("expm: could not build a real basis".into());
    }
    let pr = from_cols(&cols, n);
    let pinv = linalg::inverse(&pr)?;
    let r = matmul(&matmul(&pr, &e)?, &pinv)?;
    Ok(linalg::map(&r, |x| {
        // the shortest of the expanded, simplified and factored forms
        let e = crate::calc::expand(x);
        let s = simplify(&e);
        let t = crate::ratfun::tidy(&s);
        [t, s, e]
            .into_iter()
            .min_by_key(|c| crate::print::show(c).len())
            .unwrap()
    }))
}

fn cos(x: Expr) -> Expr {
    func(crate::expr::Func::Cos, x)
}
fn sin(x: Expr) -> Expr {
    func(crate::expr::Func::Sin, x)
}

fn to_f64(m: &Mat) -> R<Vec<Vec<f64>>> {
    m.iter()
        .map(|r| {
            r.iter()
                .map(|e| {
                    evalf(e)
                        .num()
                        .filter(|_| crate::build::gauss(&evalf(e)).is_none_or(|g| g.1.is_zero()))
                        .map(|n| n.to_f64())
                        .ok_or_else(|| "needs a matrix of real numbers".to_string())
                })
                .collect()
        })
        .collect()
}

fn fmat(m: &[Vec<f64>]) -> Mat {
    let big = m.iter().flatten().fold(0.0f64, |a, &b| a.max(b.abs()));
    m.iter()
        .map(|r| {
            r.iter()
                .map(|&x| {
                    if x.abs() < 1e-13 * big.max(1.0) {
                        Expr::int(0)
                    } else {
                        Expr::float(sig12(x))
                    }
                })
                .collect()
        })
        .collect()
}

/// x rounded to 12 significant digits (hides the last bits of rounding noise)
fn sig12(x: f64) -> f64 {
    if x == 0.0 || !x.is_finite() {
        return x;
    }
    let k = 11 - x.abs().log10().floor() as i32;
    if !(-290..=290).contains(&k) {
        return x;
    }
    let m = 10f64.powi(k);
    (x * m).round() / m
}

fn fmul(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let (n, k, m) = (a.len(), b.len(), b.first().map_or(0, |r| r.len()));
    (0..n)
        .map(|i| (0..m).map(|j| (0..k).map(|t| a[i][t] * b[t][j]).sum()).collect())
        .collect()
}

/// e^A numerically: scaling and squaring with a Taylor series
pub fn expm_numeric(a: &Mat) -> R<Mat> {
    let a = to_f64(a).map_err(|e| format!("expm {}", e))?;
    let n = a.len();
    let nrm = a.iter().map(|r| r.iter().map(|x| x.abs()).sum::<f64>()).fold(0.0, f64::max);
    let mut s = 0;
    while nrm / 2f64.powi(s) > 0.5 {
        s += 1;
    }
    let sc = 2f64.powi(s);
    let b: Vec<Vec<f64>> = a.iter().map(|r| r.iter().map(|x| x / sc).collect()).collect();
    let mut term: Vec<Vec<f64>> = (0..n).map(|i| (0..n).map(|j| (i == j) as i32 as f64).collect()).collect();
    let mut sum = term.clone();
    for k in 1..30 {
        term = fmul(&term, &b).into_iter().map(|r| r.into_iter().map(|x| x / k as f64).collect()).collect();
        for i in 0..n {
            for j in 0..n {
                sum[i][j] += term[i][j];
            }
        }
    }
    for _ in 0..s {
        sum = fmul(&sum, &sum);
    }
    Ok(fmat(&sum))
}

// ---------- singular value decomposition ----------

pub struct Svd {
    pub u: Mat,
    pub s: Mat,
    pub v: Mat,
    pub sigma: Vec<Expr>,
    pub exact: bool,
}

pub fn svd(a: &Mat) -> R<Svd> {
    let (m, n) = dims(a);
    if m == 0 || n == 0 {
        return Err("svd needs a matrix".into());
    }
    if !is_numeric(a) {
        return Err("svd needs a matrix of numbers".into());
    }
    if !has_float(a) {
        if let Some(r) = svd_exact(a) {
            return Ok(r);
        }
    }
    svd_numeric(a)
}

fn svd_exact(a: &Mat) -> Option<Svd> {
    let (m, n) = dims(a);
    let at = transpose(a);
    let b = linalg::map(&matmul(&at, a).ok()?, simplify);
    let c = charpoly_coeffs(&b).ok()?;
    let roots = crate::cas::poly_roots(&c).ok()?;
    let mut mus: Vec<Number> = vec![];
    for r in roots {
        let x = simplify(&r).num()?.clone();
        if !x.is_exact() || x.big().is_none() {
            return None;
        }
        if !mus.contains(&x) {
            mus.push(x);
        }
    }
    mus.sort_by(|x, y| y.cmp_value(x));
    let mut vdirs: Vec<Vec<Expr>> = vec![];
    let mut sig: Vec<Expr> = vec![];
    for mu in &mus {
        let k = linalg::nullspace(&shifted(&b, &Expr::Num(mu.clone())));
        let basis: Vec<Vec<Expr>> = (0..dims(&k).1).map(|j| col(&k, j)).collect();
        let (ws, _) = orthogonalize(&basis);
        for w in ws {
            vdirs.push(w);
            sig.push(rad(&sqrt(Expr::Num(mu.clone()))));
        }
    }
    if vdirs.len() != n {
        return None;
    }
    let vcols: Vec<Vec<Expr>> = vdirs.iter().map(|w| normalize(w)).collect();
    // U: A v_i / sigma_i for sigma_i > 0, completed with standard basis vectors
    let mut udirs: Vec<Vec<Expr>> = vec![];
    for (i, w) in vdirs.iter().enumerate() {
        if !sig[i].is_zero() {
            udirs.push(apply(a, w));
        }
    }
    let r = udirs.len();
    for k in 0..m {
        udirs.push((0..m).map(|i| Expr::int((i == k) as i128)).collect());
    }
    let (uws, _) = orthogonalize(&udirs);
    if uws.len() != m {
        return None;
    }
    let ucols: Vec<Vec<Expr>> = uws.iter().map(|w| normalize(w)).collect();
    let _ = r;
    let mut s = linalg::filled(m, n, Expr::zero());
    for i in 0..m.min(n) {
        s[i][i] = sig[i].clone();
    }
    Some(Svd {
        u: from_cols(&ucols, m),
        s,
        v: from_cols(&vcols, n),
        sigma: sig[..m.min(n)].to_vec(),
        exact: true,
    })
}

fn svd_numeric(a: &Mat) -> R<Svd> {
    let (m, n) = dims(a);
    let af = to_f64(a).map_err(|e| format!("svd {}", e))?;
    if m < n {
        let t: Mat = transpose(a);
        let r = svd_numeric(&t)?;
        return Ok(Svd { u: r.v, s: transpose(&r.s), v: r.u, sigma: r.sigma, exact: false });
    }
    // one-sided Jacobi on the columns
    let mut u = af.clone();
    let mut v: Vec<Vec<f64>> = (0..n).map(|i| (0..n).map(|j| (i == j) as i32 as f64).collect()).collect();
    for _ in 0..60 {
        let mut off = 0.0f64;
        for p in 0..n {
            for q in p + 1..n {
                let (mut al, mut be, mut ga) = (0.0, 0.0, 0.0);
                for row in &u {
                    al += row[p] * row[p];
                    be += row[q] * row[q];
                    ga += row[p] * row[q];
                }
                if ga.abs() <= 1e-15 * (al * be).sqrt() || ga == 0.0 {
                    continue;
                }
                off = off.max(ga.abs() / (al * be).sqrt());
                let zeta = (be - al) / (2.0 * ga);
                let t = zeta.signum() / (zeta.abs() + (1.0 + zeta * zeta).sqrt());
                let t = if zeta == 0.0 { 1.0 } else { t };
                let c = 1.0 / (1.0 + t * t).sqrt();
                let s = c * t;
                for row in u.iter_mut() {
                    let (x, y) = (row[p], row[q]);
                    row[p] = c * x - s * y;
                    row[q] = s * x + c * y;
                }
                for row in v.iter_mut() {
                    let (x, y) = (row[p], row[q]);
                    row[p] = c * x - s * y;
                    row[q] = s * x + c * y;
                }
            }
        }
        if off < 1e-15 {
            break;
        }
    }
    let mut sig: Vec<(f64, usize)> = (0..n)
        .map(|j| ((0..m).map(|i| u[i][j] * u[i][j]).sum::<f64>().sqrt(), j))
        .collect();
    sig.sort_by(|x, y| y.0.partial_cmp(&x.0).unwrap());
    let big = sig.first().map_or(0.0, |s| s.0);
    let mut ucols: Vec<Vec<f64>> = vec![];
    let mut vcols: Vec<Vec<f64>> = vec![];
    for &(s, j) in &sig {
        vcols.push((0..n).map(|i| v[i][j]).collect());
        if s > 1e-13 * big.max(1e-300) {
            ucols.push((0..m).map(|i| u[i][j] / s).collect());
        }
    }
    // complete U with standard basis vectors
    for k in 0..m {
        if ucols.len() == m {
            break;
        }
        let mut w: Vec<f64> = (0..m).map(|i| (i == k) as i32 as f64).collect();
        for _ in 0..2 {
            for q in &ucols {
                let d: f64 = q.iter().zip(&w).map(|(a, b)| a * b).sum();
                for i in 0..m {
                    w[i] -= d * q[i];
                }
            }
        }
        let nw = w.iter().map(|x| x * x).sum::<f64>().sqrt();
        if nw > 1e-8 {
            ucols.push(w.iter().map(|x| x / nw).collect());
        }
    }
    // sign convention: largest entry of each v column positive
    for (j, vc) in vcols.iter_mut().enumerate() {
        let big = vc.iter().cloned().fold(0.0f64, |a, b| if b.abs() > a.abs() { b } else { a });
        if big < 0.0 {
            for x in vc.iter_mut() {
                *x = -*x;
            }
            if j < ucols.len() && sig[j].0 > 1e-13 * big.abs().max(1e-300) {
                for x in ucols[j].iter_mut() {
                    *x = -*x;
                }
            }
        }
    }
    let uf: Vec<Vec<f64>> = (0..m).map(|i| ucols.iter().map(|c| c[i]).collect()).collect();
    let vf: Vec<Vec<f64>> = (0..n).map(|i| vcols.iter().map(|c| c[i]).collect()).collect();
    let mut s = vec![vec![0.0; n]; m];
    for i in 0..n.min(m) {
        s[i][i] = sig[i].0;
    }
    Ok(Svd {
        u: fmat(&uf),
        s: fmat(&s),
        v: fmat(&vf),
        sigma: sig[..n.min(m)].iter().map(|x| fmat(&[vec![x.0]])[0][0].clone()).collect(),
        exact: false,
    })
}

/// Kronecker product
pub fn kron(a: &Mat, b: &Mat) -> Mat {
    let (p, q) = dims(a);
    let (r, s) = dims(b);
    (0..p * r)
        .map(|i| {
            (0..q * s)
                .map(|j| simplify(&mul2(a[i / r][j / s].clone(), b[i % r][j % s].clone())))
                .collect()
        })
        .collect()
}

/// substitute t = value in every entry
pub fn at(m: &Mat, t: &str, v: &Expr) -> Mat {
    linalg::map(m, |e| simplify(&subst(e, t, v)))
}

pub fn radmat(m: &Mat) -> Mat {
    radm(m)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print::show;
    fn m(rows: &[&[i128]]) -> Mat {
        rows.iter().map(|r| r.iter().map(|&x| Expr::int(x)).collect()).collect()
    }
    fn s(m: &Mat) -> String {
        show(&Expr::Matrix(m.clone()))
    }
    #[test]
    fn radicals() {
        let e = div(Expr::int(2), sqrt(Expr::int(2)));
        assert_eq!(show(&rad(&e)), "sqrt(2)");
        let e = mul2(sqrt(Expr::int(2)), sqrt(Expr::int(6)));
        assert_eq!(show(&rad(&e)), "2*sqrt(3)");
    }
    #[test]
    fn qr_reconstructs() {
        let a = m(&[&[1, 1], &[1, 0], &[0, 1]]);
        let (q, r) = qr(&a).unwrap();
        let back = radm(&matmul(&q, &r).unwrap());
        assert_eq!(back, a);
        let qtq = radm(&matmul(&transpose(&q), &q).unwrap());
        assert_eq!(qtq, linalg::identity(2, 2));
    }
    #[test]
    fn cholesky_reconstructs() {
        let a = m(&[&[4, 2, 2], &[2, 5, 3], &[2, 3, 6]]);
        let (l, _) = cholesky(&a).unwrap();
        assert_eq!(radm(&matmul(&l, &transpose(&l)).unwrap()), a);
        assert!(cholesky(&m(&[&[1, 2], &[2, 1]])).is_err());
    }
    #[test]
    fn jordan_blocks() {
        let a = m(&[&[5, 4, 2, 1], &[0, 1, -1, -1], &[-1, -1, 3, 0], &[1, 1, -1, 2]]);
        let (p, j, _) = jordan(&a).unwrap();
        let back = linalg::map(&matmul(&matmul(&p, &j).unwrap(), &linalg::inverse(&p).unwrap()).unwrap(), simplify);
        assert_eq!(back, a);
        assert!(s(&j).matches('1').count() >= 2);
    }
    #[test]
    fn expm_rotation() {
        let a = m(&[&[0, -1], &[1, 0]]);
        let e = expm_exact(&a, Some("t")).unwrap();
        assert_eq!(s(&e), "[cos(t), -sin(t);\n sin(t),  cos(t)]");
    }
    #[test]
    fn svd_exact_values() {
        let a = m(&[&[3, 0], &[4, 5]]);
        let r = svd(&a).unwrap();
        assert!(r.exact);
        assert_eq!(show(&Expr::Matrix(vec![r.sigma.clone()])), "[3*sqrt(5), sqrt(5)]");
        let back = radm(&matmul(&matmul(&r.u, &r.s).unwrap(), &transpose(&r.v)).unwrap());
        assert_eq!(back, a);
    }
}
