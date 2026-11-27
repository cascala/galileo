//! Linear algebra commands: gramschmidt, qr, chol, svd, jordan, expm, kron, diag,
//! and the several-output forms [Q,R] = qr(A), [U,S,V] = svd(A), [P,J] = jordan(A).

use crate::commands::ident;
use crate::decomp;
use crate::eval::{R, Session};
use crate::expr::Expr;
use crate::linalg::{dims, Mat};
use crate::parser::Node;
use crate::print::show;

fn m(x: Mat) -> Expr {
    Expr::Matrix(x)
}

fn list(v: &[Expr]) -> String {
    v.iter().map(show).collect::<Vec<_>>().join(", ")
}

pub(crate) fn is_multi(f: &str) -> bool {
    matches!(f, "qr" | "svd" | "jordan")
}

impl Session {
    pub(crate) fn la_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "gramschmidt" | "orth" => self.one_matrix(f, args).map(|a| {
                let q = decomp::gramschmidt(&a);
                self.gs_note(&a, &q);
                m(q)
            }),
            "qr" => Err("qr gives two matrices: [Q, R] = qr(A)".into()),
            "chol" | "cholesky" => self.one_matrix(f, args).and_then(|a| {
                let (l, d) = decomp::cholesky(&a)?;
                self.chol_note(&l, &d);
                Ok(m(l))
            }),
            "svd" => self.one_matrix(f, args).and_then(|a| {
                let r = decomp::svd(&a)?;
                self.note(format!(
                    "The singular values σ₁ ≥ σ₂ ≥ … ≥ 0 are the square roots of the eigenvalues of AᵀA{}. They measure how much A stretches: σ₁ is the largest stretch (the 2-norm of A), and the number of nonzero σ is the rank. [U, S, V] = svd(A) gives the full decomposition A = U*S*Vᵀ.",
                    if r.exact { "" } else { " (computed numerically)" }
                ));
                Ok(Expr::Matrix(vec![r.sigma]))
            }),
            "jordan" => self.one_matrix(f, args).and_then(|a| {
                let (_, j, blocks) = decomp::jordan(&a)?;
                self.jordan_note(&blocks);
                Ok(m(j))
            }),
            "expm" => self.expm(args),
            "cond" => self.one_matrix(f, args).and_then(|a| {
                let r = decomp::svd(&a)?;
                let big = r.sigma.first().cloned().unwrap_or(Expr::zero());
                let small = r.sigma.last().cloned().unwrap_or(Expr::zero());
                let (m, n) = dims(&a);
                let full = m == n;
                if small.is_zero() || !full {
                    self.note("The condition number σ_max/σ_min is infinite: the matrix is singular (or not square), so some direction is squashed to 0 and A x = b can't be solved stably.");
                    return Ok(Expr::sym("inf"));
                }
                let c = decomp::rad(&crate::build::div(big.clone(), small.clone()));
                self.note(format!(
                    "Condition number = largest/smallest singular value = {}/{}. It bounds how much a relative error in b can grow in the solution of A x = b: you can lose about log10 of it in decimal digits.",
                    show(&big), show(&small)
                ));
                Ok(c)
            }),
            "posdef" | "isposdef" => self.one_matrix(f, args).map(|a| {
                let r = decomp::cholesky(&a);
                let yes = r.is_ok();
                match r {
                    Ok((_, d)) => self.note(format!(
                        "Positive definite: symmetric, and elimination gives the pivots {}, all positive (so chol(A) exists, every eigenvalue is positive and xᵀAx > 0 for every x ≠ 0).",
                        list(&d)
                    )),
                    Err(e) => self.note(if e.contains("symmetric") {
                        "Not positive definite: only symmetric matrices (A = transpose(A)) are called positive definite.".to_string()
                    } else {
                        format!("Not positive definite: {}.", e.trim_start_matches("chol: the matrix is not positive definite ").trim_start_matches('(').replace("); every", ", and every"))
                    }),
                }
                Expr::Bool(yes)
            }),
            "kron" => {
                if args.len() != 2 {
                    return Some(Err("usage: kron(A, B)".into()));
                }
                let a = match self.matrix_or_scalar(&args[0]) {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                };
                let b = match self.matrix_or_scalar(&args[1]) {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                };
                let (p, q) = dims(&a);
                let (r, s) = dims(&b);
                self.note(format!(
                    "The Kronecker product replaces each entry a_ij of A by the block a_ij*B, so a {}×{} and a {}×{} matrix give a {}×{} matrix. It describes linear maps on pairs (tensor products): kron(A, B)*kron(x, y) = kron(A*x, B*y).",
                    p, q, r, s, p * r, q * s
                ));
                Ok(m(decomp::kron(&a, &b)))
            }
            "diag" => self.one_matrix(f, args).map(|a| {
                let (r, c) = dims(&a);
                if r == 1 || c == 1 {
                    let v: Vec<Expr> = a.into_iter().flatten().collect();
                    let n = v.len();
                    m((0..n)
                        .map(|i| (0..n).map(|j| if i == j { v[i].clone() } else { Expr::zero() }).collect())
                        .collect())
                } else {
                    Expr::Matrix(vec![(0..r.min(c)).map(|i| a[i][i].clone()).collect()])
                }
            }),
            _ => return None,
        };
        Some(r)
    }

    fn one_matrix(&mut self, f: &str, args: &[Node]) -> R<Mat> {
        let [a] = args else {
            return Err(format!("{} takes one matrix", f));
        };
        self.matrix_arg(a, f)
    }

    fn matrix_or_scalar(&mut self, n: &Node) -> R<Mat> {
        Ok(match self.eval(n)? {
            Expr::Matrix(x) => x,
            s => vec![vec![s]],
        })
    }

    /// Several outputs: [Q,R] = qr(A), [U,S,V] = svd(A), [P,J] = jordan(A)
    pub(crate) fn la_multi(&mut self, f: &str, args: &[Node], outs: usize) -> Option<R<Vec<Expr>>> {
        let r = match (f, outs) {
            ("qr", 2) => self.one_matrix(f, args).and_then(|a| {
                let (q, r) = decomp::qr(&a)?;
                self.qr_note(&a, &q, &r);
                Ok(vec![m(q), m(r)])
            }),
            ("svd", 3) => self.one_matrix(f, args).and_then(|a| {
                let r = decomp::svd(&a)?;
                self.svd_note(&r);
                Ok(vec![m(r.u), m(r.s), m(r.v)])
            }),
            ("jordan", 2) => self.one_matrix(f, args).and_then(|a| {
                let (p, j, blocks) = decomp::jordan(&a)?;
                self.jordan_note(&blocks);
                Ok(vec![m(p), m(j)])
            }),
            ("qr", _) | ("svd", _) | ("jordan", _) => {
                Err("use [Q, R] = qr(A), [U, S, V] = svd(A) or [P, J] = jordan(A)".into())
            }
            _ => return None,
        };
        Some(r)
    }

    fn expm(&mut self, args: &[Node]) -> R<Expr> {
        let (a, t) = match args {
            [a] => (a, None),
            [a, t] => (a, Some(ident(t, "t")?)),
            _ => return Err("usage: expm(A) or expm(A, t) for e^(A t)".into()),
        };
        let mat = self.matrix_arg(a, "expm")?;
        let (r, c) = dims(&mat);
        if r != c {
            return Err("expm needs a square matrix".into());
        }
        if !decomp::is_numeric(&mat) {
            return Err("expm needs a matrix of numbers; for e^(A t) write expm(A, t)".into());
        }
        if !decomp::has_float(&mat) {
            match decomp::expm_exact(&mat, t.as_deref()) {
                Ok(e) => {
                    self.note(match &t {
                        Some(t) => format!("e^(A{t}) = I + A{t} + (A{t})²/2! + … is computed from the Jordan form A = P*J*P⁻¹: e^(A{t}) = P*e^(J{t})*P⁻¹. Each eigenvalue λ contributes e^(λ{t}), times powers of {t} for a Jordan block bigger than 1×1; a complex pair a ± bi contributes e^(a{t})*cos(b{t}) and e^(a{t})*sin(b{t}). x({t}) = expm(A, {t})*x0 solves x' = A x with x(0) = x0."),
                        None => "e^A = I + A + A²/2! + … is computed from the Jordan form A = P*J*P⁻¹: e^A = P*e^J*P⁻¹, and e^J has e^λ for each eigenvalue λ on its diagonal. expm(A, t) gives e^(A t).".to_string(),
                    });
                    return Ok(m(e));
                }
                Err(e) if t.is_some() => return Err(e),
                Err(_) => {}
            }
        }
        if t.is_some() {
            return Err("expm(A, t) needs exact entries; for numbers use expm(A*t0) at a value t0".into());
        }
        self.note("e^A = I + A + A²/2! + A³/3! + …, computed numerically by scaling and squaring: e^A = (e^(A/2^s))^(2^s), with the series for the small matrix A/2^s.");
        Ok(m(decomp::expm_numeric(&mat)?))
    }

    fn gs_note(&mut self, a: &Mat, q: &Mat) {
        let (_, c) = dims(a);
        let k = dims(q).1;
        self.note(format!(
            "Gram–Schmidt takes the columns one at a time, subtracts their projections on the directions found so far, and divides by the length: q₁ = a₁/|a₁|, then q₂ is a₂ − (q₁·a₂)q₁ normalized, and so on. {} of the {} columns were independent, so the columns of the result are an orthonormal basis of the column space (dimension {}).",
            k, c, k
        ));
    }

    fn qr_note(&mut self, a: &Mat, q: &Mat, r: &Mat) {
        if self.teaching() {
            let (_, c) = dims(a);
            let diag: Vec<Expr> = (0..c).map(|i| r[i][i].clone()).collect();
            self.note(format!(
                "Gram–Schmidt on the columns of A: each column minus its projections on the earlier q's, then divided by its length. The lengths become the diagonal of R: {}. Above the diagonal, R[i, j] = q_i · a_j records how much of column j lies along q_i. Q has orthonormal columns (QᵀQ = I), so A = QR turns least squares A x ≈ b into R x = Qᵀ b.",
                list(&diag)
            ));
            let _ = q;
        } else {
            self.note("A = Q*R: Q has orthonormal columns (Gram–Schmidt on the columns of A, QᵀQ = I) and R is upper triangular with R[i, j] = q_i · a_j. Least squares A x ≈ b becomes R x = Qᵀ b.");
        }
    }

    fn chol_note(&mut self, l: &Mat, d: &[Expr]) {
        let _ = l;
        self.note(format!(
            "A = L*Lᵀ with L lower triangular. It exists exactly when A is symmetric positive definite, which elimination shows: the pivots are {}, all positive, and the diagonal of L holds their square roots. It is LU for symmetric matrices, at half the cost.",
            list(d)
        ));
    }

    fn svd_note(&mut self, r: &decomp::Svd) {
        self.note(format!(
            "A = U*S*Vᵀ: the columns of V are orthonormal eigenvectors of AᵀA (the input directions), S holds the singular values {} (square roots of the eigenvalues of AᵀA), and the columns of U are A*v/σ (the output directions), completed to an orthonormal basis. So A maps v_i to σ_i u_i: every matrix is a rotation, a stretch along the axes, and another rotation.{}",
            list(&r.sigma),
            if r.exact { "" } else { " Computed numerically (Jacobi rotations), since AᵀA has no rational eigenvalues." }
        ));
    }

    fn jordan_note(&mut self, blocks: &[(Expr, usize)]) {
        let b: Vec<String> = blocks
            .iter()
            .map(|(l, s)| format!("{}×{} for λ = {}", s, s, show(l)))
            .collect();
        let diag = blocks.iter().all(|b| b.1 == 1);
        self.note(format!(
            "A = P*J*P⁻¹ with J block diagonal; blocks: {}. {}",
            b.join(", "),
            if diag {
                "All blocks are 1×1: A is diagonalizable and the columns of P are eigenvectors."
            } else {
                "A block of size k means only one eigenvector for k repeats of λ; the other columns of P are generalized eigenvectors, with (A − λI)v_j = v_(j−1). A is not diagonalizable."
            }
        ));
    }
}


