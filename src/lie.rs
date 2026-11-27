//! Matrix Lie algebras: bases of su(n), so(n), sl(n); the bracket, coordinates
//! in a basis, the adjoint representation and the Killing form.

use crate::build::*;
use crate::calc::simplify;
use crate::expr::{Const, Expr};
use crate::linalg::{self, Mat};

pub type R<T> = Result<T, String>;

pub const MAX_N: usize = 6;

fn unit(n: usize, j: usize, k: usize) -> Mat {
    let mut m = linalg::filled(n, n, Expr::zero());
    m[j][k] = Expr::one();
    m
}

fn comb(a: &Mat, ca: Expr, b: &Mat, cb: Expr) -> Mat {
    a.iter()
        .zip(b)
        .map(|(ra, rb)| {
            ra.iter()
                .zip(rb)
                .map(|(x, y)| {
                    simplify(&add2(
                        mul2(ca.clone(), x.clone()),
                        mul2(cb.clone(), y.clone()),
                    ))
                })
                .collect()
        })
        .collect()
}

/// so(n): antisymmetric matrices. For n = 3 the basis is Lx, Ly, Lz with [Lx, Ly] = Lz.
pub fn so(n: usize) -> Vec<Mat> {
    if n == 3 {
        // rotation generators about x, y, z
        return [(2, 1), (0, 2), (1, 0)]
            .iter()
            .map(|&(j, k)| comb(&unit(3, j, k), Expr::one(), &unit(3, k, j), Expr::int(-1)))
            .collect();
    }
    let mut out = vec![];
    for j in 0..n {
        for k in j + 1..n {
            out.push(comb(
                &unit(n, k, j),
                Expr::one(),
                &unit(n, j, k),
                Expr::int(-1),
            ));
        }
    }
    out
}

/// su(n): traceless anti-Hermitian matrices. For n = 2 the basis is -i sigma_k / 2,
/// with [e1, e2] = e3 like so(3).
pub fn su(n: usize) -> Vec<Mat> {
    let i = Expr::Const(Const::I);
    if n == 2 {
        let h = |a: Expr, b: Expr, c: Expr, d: Expr| -> Mat {
            vec![
                vec![simplify(&a), simplify(&b)],
                vec![simplify(&c), simplify(&d)],
            ]
        };
        let mi2 = div(neg(i.clone()), Expr::int(2));
        let half = div(Expr::one(), Expr::int(2));
        return vec![
            h(Expr::zero(), mi2.clone(), mi2.clone(), Expr::zero()),
            h(Expr::zero(), neg(half.clone()), half, Expr::zero()),
            h(mi2.clone(), Expr::zero(), Expr::zero(), neg(mi2)),
        ];
    }
    let mut out = vec![];
    for j in 0..n {
        for k in j + 1..n {
            out.push(comb(
                &unit(n, j, k),
                Expr::one(),
                &unit(n, k, j),
                Expr::int(-1),
            ));
            out.push(comb(&unit(n, j, k), i.clone(), &unit(n, k, j), i.clone()));
        }
    }
    for j in 0..n - 1 {
        out.push(comb(
            &unit(n, j, j),
            i.clone(),
            &unit(n, j + 1, j + 1),
            neg(i.clone()),
        ));
    }
    out
}

/// sl(n): traceless matrices. For n = 2 the basis is h, e, f with
/// [h, e] = 2e, [h, f] = -2f, [e, f] = h.
pub fn sl(n: usize) -> Vec<Mat> {
    let mut out = vec![];
    for j in 0..n - 1 {
        out.push(comb(
            &unit(n, j, j),
            Expr::one(),
            &unit(n, j + 1, j + 1),
            Expr::int(-1),
        ));
    }
    for j in 0..n {
        for k in 0..n {
            if j < k {
                out.push(unit(n, j, k));
            }
        }
    }
    for j in 0..n {
        for k in 0..n {
            if j > k {
                out.push(unit(n, j, k));
            }
        }
    }
    out
}

pub fn bracket(a: &Mat, b: &Mat) -> R<Mat> {
    let ab = linalg::matmul(a, b)?;
    let ba = linalg::matmul(b, a)?;
    Ok(comb(&ab, Expr::one(), &ba, Expr::int(-1)))
}

/// coefficients c with x = sum c_k basis_k
pub fn coords(x: &Mat, basis: &[Mat]) -> R<Vec<Expr>> {
    let flat = |m: &Mat| -> Vec<Expr> { m.iter().flatten().cloned().collect() };
    let fx = flat(x);
    let cols: Vec<Vec<Expr>> = basis.iter().map(flat).collect();
    if cols.iter().any(|c| c.len() != fx.len()) {
        return Err("coords: the matrices must all have the same size".into());
    }
    // augmented matrix: one row per entry
    let aug: Mat = (0..fx.len())
        .map(|r| {
            let mut row: Vec<Expr> = cols.iter().map(|c| c[r].clone()).collect();
            row.push(fx[r].clone());
            row
        })
        .collect();
    let k = basis.len();
    let (red, piv) = linalg::rref(&aug);
    if piv.contains(&k) {
        return Err("coords: the matrix is not a combination of the basis".into());
    }
    if piv.len() < k {
        return Err("coords: the basis matrices are linearly dependent".into());
    }
    Ok((0..k).map(|i| red[i][k].clone()).collect())
}

/// matrix of ad(x) = [x, .] in the basis (column j = coordinates of [x, e_j])
pub fn adjoint(x: &Mat, basis: &[Mat]) -> R<Mat> {
    let cols: Vec<Vec<Expr>> = basis
        .iter()
        .map(|e| coords(&bracket(x, e)?, basis))
        .collect::<R<_>>()?;
    Ok(linalg::transpose(&cols))
}

/// Gram matrix of the Killing form B(x, y) = trace(ad x ad y)
pub fn killing(basis: &[Mat]) -> R<Mat> {
    let ads: Vec<Mat> = basis.iter().map(|e| adjoint(e, basis)).collect::<R<_>>()?;
    let k = basis.len();
    let mut out = vec![vec![Expr::zero(); k]; k];
    for i in 0..k {
        for j in 0..k {
            let p = linalg::matmul(&ads[i], &ads[j])?;
            out[i][j] = simplify(&add((0..k).map(|t| p[t][t].clone()).collect()));
        }
    }
    Ok(out)
}
