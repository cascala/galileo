//! Dense matrices of expressions.

use crate::build::*;
use crate::calc::{evalf, simplify};
use crate::expr::Expr;

pub type Mat = Vec<Vec<Expr>>;
pub type R<T> = Result<T, String>;

pub fn dims(m: &Mat) -> (usize, usize) {
    (m.len(), m.first().map_or(0, |r| r.len()))
}

pub fn identity(n: usize, m: usize) -> Mat {
    (0..n)
        .map(|i| (0..m).map(|j| Expr::int((i == j) as i128)).collect())
        .collect()
}

pub fn filled(n: usize, m: usize, v: Expr) -> Mat {
    vec![vec![v; m]; n]
}

pub fn transpose(m: &Mat) -> Mat {
    let (r, c) = dims(m);
    (0..c)
        .map(|j| (0..r).map(|i| m[i][j].clone()).collect())
        .collect()
}

pub fn map(m: &Mat, f: impl Fn(&Expr) -> Expr) -> Mat {
    m.iter().map(|r| r.iter().map(&f).collect()).collect()
}

pub fn zip(a: &Mat, b: &Mat, op: &str, f: impl Fn(&Expr, &Expr) -> Expr) -> R<Mat> {
    if dims(a) != dims(b) {
        return Err(format!(
            "incompatible matrix dimensions for {}: {}x{} and {}x{}",
            op,
            dims(a).0,
            dims(a).1,
            dims(b).0,
            dims(b).1
        ));
    }
    Ok(a.iter()
        .zip(b)
        .map(|(ra, rb)| ra.iter().zip(rb).map(|(x, y)| f(x, y)).collect())
        .collect())
}

pub fn matmul(a: &Mat, b: &Mat) -> R<Mat> {
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
                    add((0..k)
                        .map(|t| mul2(a[i][t].clone(), b[t][j].clone()))
                        .collect())
                })
                .collect()
        })
        .collect())
}

fn is_zero(e: &Expr) -> bool {
    e.is_zero() || simplify(e).is_zero()
}

/// Row-reduce [a | b]; returns the solution of a*x = b
pub fn solve(a: &Mat, b: &Mat) -> R<Mat> {
    let (n, m) = dims(a);
    if n != m {
        return Err("solve needs a square matrix".into());
    }
    if dims(b).0 != n {
        return Err("right-hand side has the wrong number of rows".into());
    }
    let mut aug: Mat = a
        .iter()
        .zip(b)
        .map(|(ra, rb)| ra.iter().chain(rb).cloned().collect())
        .collect();
    let w = aug[0].len();
    for col in 0..n {
        let piv = pivot_row(&aug, col, col).ok_or("matrix is singular")?;
        aug.swap(col, piv);
        let p = aug[col][col].clone();
        aug[col] = aug[col]
            .iter()
            .map(|x| simplify(&div(x.clone(), p.clone())))
            .collect();
        for r in 0..n {
            if r != col && !aug[r][col].is_zero() {
                let f = aug[r][col].clone();
                for c in 0..w {
                    aug[r][c] = simplify(&sub(
                        aug[r][c].clone(),
                        mul2(f.clone(), aug[col][c].clone()),
                    ));
                }
            }
        }
    }
    Ok(aug.into_iter().map(|r| r[n..].to_vec()).collect())
}

/// Row with the largest numeric pivot (exact zero skipped); symbolic
/// entries are accepted when they are not identically zero.
fn pivot_row(m: &Mat, col: usize, from: usize) -> Option<usize> {
    let mut best: Option<(usize, f64)> = None;
    for r in from..m.len() {
        let e = &m[r][col];
        if is_zero(e) {
            continue;
        }
        let mag = evalf(e).num().map_or(f64::INFINITY, |n| n.to_f64().abs());
        if best.is_none_or(|(_, b)| mag > b && b.is_finite()) {
            best = Some((r, mag));
        }
    }
    best.map(|b| b.0)
}

/// Reduced row echelon form, and the pivot columns.
pub fn rref(a: &Mat) -> (Mat, Vec<usize>) {
    let mut m = a.clone();
    let (n, w) = dims(&m);
    let mut pivots = vec![];
    let mut row = 0;
    for col in 0..w {
        if row == n {
            break;
        }
        let Some(piv) = pivot_row(&m, col, row) else {
            continue;
        };
        m.swap(row, piv);
        let p = m[row][col].clone();
        m[row] = m[row]
            .iter()
            .map(|x| simplify(&div(x.clone(), p.clone())))
            .collect();
        for r in 0..n {
            if r != row && !is_zero(&m[r][col]) {
                let f = m[r][col].clone();
                for c in 0..w {
                    m[r][c] = simplify(&sub(m[r][c].clone(), mul2(f.clone(), m[row][c].clone())));
                }
            }
        }
        pivots.push(col);
        row += 1;
    }
    (m, pivots)
}

/// A basis of the solutions of a*x = 0, one vector per column.
pub fn nullspace(a: &Mat) -> Mat {
    let (r, pivots) = rref(a);
    let w = dims(a).1;
    let free: Vec<usize> = (0..w).filter(|c| !pivots.contains(c)).collect();
    let mut cols: Vec<Vec<Expr>> = vec![];
    for &f in &free {
        let mut v = vec![Expr::zero(); w];
        v[f] = Expr::one();
        for (i, &p) in pivots.iter().enumerate() {
            v[p] = simplify(&neg(r[i][f].clone()));
        }
        cols.push(v);
    }
    transpose(&cols)
}

pub fn inverse(a: &Mat) -> R<Mat> {
    let (n, m) = dims(a);
    if n != m {
        return Err("only square matrices have an inverse".into());
    }
    solve(a, &identity(n, n))
}

pub fn det(a: &Mat) -> R<Expr> {
    let (n, m) = dims(a);
    if n != m {
        return Err("determinant needs a square matrix".into());
    }
    let mut a = a.clone();
    let mut d = Expr::one();
    for col in 0..n {
        let Some(piv) = pivot_row(&a, col, col) else {
            return Ok(Expr::zero());
        };
        if piv != col {
            a.swap(col, piv);
            d = neg(d);
        }
        let p = a[col][col].clone();
        d = mul2(d, p.clone());
        for r in col + 1..n {
            let f = simplify(&div(a[r][col].clone(), p.clone()));
            for c in col..n {
                a[r][c] = simplify(&sub(a[r][c].clone(), mul2(f.clone(), a[col][c].clone())));
            }
        }
    }
    Ok(simplify(&d))
}

/// LU decomposition with partial pivoting: P*A = L*U
pub fn lu(a: &Mat) -> R<(Mat, Mat, Mat)> {
    let (n, m) = dims(a);
    if n != m {
        return Err("lu needs a square matrix".into());
    }
    let mut u = a.clone();
    let mut l = identity(n, n);
    let mut perm: Vec<usize> = (0..n).collect();
    for col in 0..n {
        if let Some(piv) = pivot_row(&u, col, col) {
            if piv != col {
                u.swap(col, piv);
                perm.swap(col, piv);
                for c in 0..col {
                    let t = l[col][c].clone();
                    l[col][c] = l[piv][c].clone();
                    l[piv][c] = t;
                }
            }
        } else {
            continue;
        }
        let p = u[col][col].clone();
        for r in col + 1..n {
            let f = simplify(&div(u[r][col].clone(), p.clone()));
            l[r][col] = f.clone();
            for c in col..n {
                u[r][c] = simplify(&sub(u[r][c].clone(), mul2(f.clone(), u[col][c].clone())));
            }
        }
    }
    let p = (0..n)
        .map(|i| (0..n).map(|j| Expr::int((perm[i] == j) as i128)).collect())
        .collect();
    Ok((l, u, p))
}

fn numeric(m: &Mat) -> R<Vec<Vec<f64>>> {
    m.iter()
        .map(|r| {
            r.iter()
                .map(|e| {
                    evalf(e)
                        .num()
                        .map(|n| n.to_f64())
                        .ok_or_else(|| "norm needs numeric entries".to_string())
                })
                .collect()
        })
        .collect()
}

/// Matrix norms: 1 (max column sum), "inf" (max row sum), "fro", 2 (spectral)
pub fn norm(m: &Mat, p: &str) -> R<Expr> {
    // exact 1-norm and inf-norm when entries are exact numbers
    let absm = map(m, |e| crate::build::func(crate::expr::Func::Abs, e.clone()));
    match p {
        "1" | "inf" => {
            let lines = if p == "1" { transpose(&absm) } else { absm };
            let sums: Vec<Expr> = lines.iter().map(|l| add(l.clone())).collect();
            if sums.iter().all(|s| s.is_number()) {
                let best = sums
                    .into_iter()
                    .max_by(|a, b| a.num().unwrap().cmp_value(b.num().unwrap()))
                    .unwrap_or(Expr::zero());
                return Ok(best);
            }
            let f = numeric(&lines)?;
            Ok(Expr::float(
                f.iter().map(|l| l.iter().sum::<f64>()).fold(0.0, f64::max),
            ))
        }
        "fro" => {
            let f = numeric(m)?;
            Ok(Expr::float(
                f.iter().flatten().map(|x| x * x).sum::<f64>().sqrt(),
            ))
        }
        "2" => {
            let f = numeric(m)?;
            Ok(Expr::float(spectral_norm(&f)))
        }
        _ => Err(format!("unknown norm type {}", p)),
    }
}

/// Largest singular value, by power iteration on A^T A
fn spectral_norm(a: &[Vec<f64>]) -> f64 {
    let n = a.first().map_or(0, |r| r.len());
    if n == 0 {
        return 0.0;
    }
    let mut v = vec![1.0; n];
    let mut lambda = 0.0;
    for _ in 0..500 {
        let av: Vec<f64> = a
            .iter()
            .map(|r| r.iter().zip(&v).map(|(x, y)| x * y).sum())
            .collect();
        let w: Vec<f64> = (0..n)
            .map(|j| a.iter().zip(&av).map(|(r, y)| r[j] * y).sum())
            .collect();
        let norm = w.iter().map(|x| x * x).sum::<f64>().sqrt();
        if norm == 0.0 {
            return 0.0;
        }
        v = w.iter().map(|x| x / norm).collect();
        if (norm - lambda).abs() < 1e-14 * norm {
            lambda = norm;
            break;
        }
        lambda = norm;
    }
    lambda.sqrt()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print::show;
    fn m(rows: &[&[i128]]) -> Mat {
        rows.iter()
            .map(|r| r.iter().map(|&x| Expr::int(x)).collect())
            .collect()
    }
    #[test]
    fn inverse_times_matrix_is_identity() {
        let a = m(&[&[4, 5, 6], &[6, 5, 4], &[7, 8, 7]]);
        let inv = inverse(&a).unwrap();
        assert_eq!(matmul(&inv, &a).unwrap(), identity(3, 3));
    }
    #[test]
    fn lu_reconstructs() {
        let a = m(&[&[1, 2, 3], &[4, 5, 6], &[7, 8, 0]]);
        let (l, u, p) = lu(&a).unwrap();
        assert_eq!(matmul(&l, &u).unwrap(), matmul(&p, &a).unwrap());
    }
    #[test]
    fn norms() {
        let a = m(&[&[1, 3], &[5, -6]]);
        assert_eq!(show(&norm(&a, "1").unwrap()), "9");
        assert_eq!(det(&a).unwrap(), Expr::int(-21));
    }
}
