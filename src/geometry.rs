//! Differential geometry: metrics, curvature, geodesic equations.
//!
//! Every intermediate result is tidied with [`crate::ratfun::tidy`], so derivatives
//! are taken of simplified Christoffel symbols rather than of raw expressions.
//! That keeps results readable and the computation fast.

use crate::build::*;
use crate::calc::deriv;
use crate::expr::{Expr, Func};
use crate::latex;
use crate::linalg::{self, Mat};
use crate::print::show;
use crate::ratfun::tidy;
use std::cell::OnceCell;
use std::rc::Rc;

// ---------- metrics ----------

#[derive(Clone, Debug, PartialEq)]
pub struct Metric {
    pub coords: Vec<String>,
    /// components g_ab (or g^ab when `upper`)
    pub g: Mat,
    pub upper: bool,
}

fn is_diagonal(m: &Mat) -> bool {
    m.iter()
        .enumerate()
        .all(|(i, r)| r.iter().enumerate().all(|(j, x)| i == j || x.is_zero()))
}

/// Inverse of a symbolic matrix, tidied
pub fn inverse(m: &Mat) -> Result<Mat, String> {
    if is_diagonal(m) {
        let n = m.len();
        let mut out = linalg::filled(n, n, Expr::zero());
        for i in 0..n {
            if m[i][i].is_zero() {
                return Err("the metric is singular".into());
            }
            out[i][i] = tidy(&pow(m[i][i].clone(), Expr::int(-1)));
        }
        return Ok(out);
    }
    let inv = linalg::inverse(m).map_err(|_| "the metric is singular".to_string())?;
    Ok(inv.iter().map(|r| r.iter().map(tidy).collect()).collect())
}

impl Metric {
    pub fn new(coords: Vec<String>, g: Mat) -> Result<Metric, String> {
        let n = coords.len();
        if g.len() != n || g.iter().any(|r| r.len() != n) {
            return Err("the metric must be a square matrix with one row per coordinate".into());
        }
        for i in 0..n {
            for j in 0..i {
                if !crate::ratfun::is_zero(&sub(g[i][j].clone(), g[j][i].clone())) {
                    return Err("the metric must be symmetric".into());
                }
            }
        }
        Ok(Metric {
            coords,
            g: g.iter().map(|r| r.iter().map(tidy).collect()).collect(),
            upper: false,
        })
    }

    pub fn dim(&self) -> usize {
        self.coords.len()
    }

    pub fn lower(&self) -> Result<Mat, String> {
        if self.upper {
            inverse(&self.g)
        } else {
            Ok(self.g.clone())
        }
    }

    pub fn raised(&self) -> Result<Mat, String> {
        if self.upper {
            Ok(self.g.clone())
        } else {
            inverse(&self.g)
        }
    }

    pub fn inverse(&self) -> Result<Metric, String> {
        Ok(Metric {
            coords: self.coords.clone(),
            g: inverse(&self.g)?,
            upper: !self.upper,
        })
    }

    fn terms(&self) -> Vec<(Expr, usize, usize)> {
        let n = self.dim();
        let mut out = vec![];
        for a in 0..n {
            for b in a..n {
                let g = &self.g[a][b];
                let c = if a == b {
                    g.clone()
                } else {
                    tidy(&mul2(Expr::int(2), g.clone()))
                };
                if !c.is_zero() {
                    out.push((c, a, b));
                }
            }
        }
        out
    }

    pub fn text(&self) -> String {
        if self.upper {
            let rows: Vec<String> = self
                .g
                .iter()
                .map(|r| format!("[{}]", r.iter().map(show).collect::<Vec<_>>().join(", ")))
                .collect();
            return format!(
                "Inverse metric g^ab (coordinates {}): [{}]",
                self.coords.join(", "),
                rows.join(", ")
            );
        }
        let ts: Vec<String> = self
            .terms()
            .iter()
            .enumerate()
            .map(|(i, (c, a, b))| {
                let d = if a == b {
                    format!("d{}^2", self.coords[*a])
                } else {
                    format!("d{} d{}", self.coords[*a], self.coords[*b])
                };
                let (negative, c) = if c.is_negative_term() {
                    (true, neg(c.clone()))
                } else {
                    (false, c.clone())
                };
                let body = match &c {
                    x if x.is_one() => d,
                    Expr::Add(_) => format!("({})*{}", show(&c), d),
                    _ => format!("{}*{}", show(&c), d),
                };
                match (i, negative) {
                    (0, true) => format!("-{}", body),
                    (0, false) => body,
                    (_, true) => format!(" - {}", body),
                    (_, false) => format!(" + {}", body),
                }
            })
            .collect();
        format!(
            "ds^2 = {}",
            if ts.is_empty() {
                "0".into()
            } else {
                ts.concat()
            }
        )
    }

    pub fn latex(&self) -> String {
        if self.upper {
            return format!(
                "$$g^{{ab}} = {}$$",
                latex::latex(&Expr::Matrix(self.g.clone()))
            );
        }
        let name = |i: usize| latex::name(&self.coords[i]);
        let ts: Vec<String> = self
            .terms()
            .iter()
            .enumerate()
            .map(|(i, (c, a, b))| {
                let d = if a == b {
                    format!("d{}^2", name(*a))
                } else {
                    format!("d{}\\, d{}", name(*a), name(*b))
                };
                let (negative, c) = if c.is_negative_term() {
                    (true, neg(c.clone()))
                } else {
                    (false, c.clone())
                };
                let cl = match &c {
                    x if x.is_one() => String::new(),
                    Expr::Add(_) => format!("\\left({}\\right)\\, ", latex::latex(&c)),
                    _ => format!("{}\\, ", latex::latex(&c)),
                };
                let sign = if negative {
                    if i == 0 { "-" } else { " - " }
                } else if i == 0 {
                    ""
                } else {
                    " + "
                };
                format!("{}{}{}", sign, cl, d)
            })
            .collect();
        format!(
            "$$ds^2 = {}$$",
            if ts.is_empty() {
                "0".into()
            } else {
                ts.concat()
            }
        )
    }
}

fn sym(s: &str) -> Expr {
    Expr::sym(s)
}
fn sq(e: Expr) -> Expr {
    pow(e, Expr::int(2))
}

fn diag(coords: &[&str], d: Vec<Expr>) -> Metric {
    let n = d.len();
    let mut g = linalg::filled(n, n, Expr::zero());
    for (i, x) in d.into_iter().enumerate() {
        g[i][i] = x;
    }
    Metric {
        coords: coords.iter().map(|s| s.to_string()).collect(),
        g,
        upper: false,
    }
}

pub const TEMPLATES: [&str; 9] = [
    "minkowski",
    "polar",
    "euclidean",
    "spherical",
    "two-sphere",
    "three-sphere",
    "schwarzschild",
    "torus",
    "hyperbolic",
];

/// Ready-made metrics: metric.generate(name)
pub fn template(name: &str) -> Option<Metric> {
    let sin = |x: &str| func(Func::Sin, sym(x));
    let cos = |x: &str| func(Func::Cos, sym(x));
    let r = || sym("r");
    Some(match name.trim() {
        "minkowski" => diag(
            &["t", "x", "y", "z"],
            vec![Expr::int(-1), Expr::one(), Expr::one(), Expr::one()],
        ),
        "polar" => diag(&["r", "phi"], vec![Expr::one(), sq(r())]),
        "euclidean" => diag(
            &["x", "y", "z"],
            vec![Expr::one(), Expr::one(), Expr::one()],
        ),
        "spherical" => diag(
            &["r", "theta", "phi"],
            vec![Expr::one(), sq(r()), mul2(sq(r()), sq(sin("theta")))],
        ),
        "two-sphere" => diag(
            &["theta", "phi"],
            vec![sq(r()), mul2(sq(r()), sq(sin("theta")))],
        ),
        "three-sphere" => diag(
            &["psi", "theta", "phi"],
            vec![
                sq(r()),
                mul2(sq(r()), sq(sin("psi"))),
                mul(vec![sq(r()), sq(sin("psi")), sq(sin("theta"))]),
            ],
        ),
        "schwarzschild" => {
            let f = div(sub(r(), sym("r_s")), r());
            diag(
                &["t", "r", "theta", "phi"],
                vec![
                    neg(f.clone()),
                    pow(f, Expr::int(-1)),
                    sq(r()),
                    mul2(sq(r()), sq(sin("theta"))),
                ],
            )
        }
        "torus" => diag(
            &["theta", "phi"],
            vec![
                sq(sym("a")),
                sq(add2(sym("R"), mul2(sym("a"), cos("theta")))),
            ],
        ),
        "hyperbolic" => diag(
            &["x", "y"],
            vec![pow(sym("y"), Expr::int(-2)), pow(sym("y"), Expr::int(-2))],
        ),
        _ => return None,
    })
}

/// The metric induced on a parametrised surface by the Euclidean metric of the
/// space it lives in (the first fundamental form): g_ij = sum_k dX_k/du_i dX_k/du_j
pub fn induced(embedding: &[Expr], coords: &[String]) -> Metric {
    let jac: Vec<Vec<Expr>> = coords
        .iter()
        .map(|u| embedding.iter().map(|x| deriv(x, u)).collect())
        .collect();
    let n = coords.len();
    let mut g = linalg::filled(n, n, Expr::zero());
    for i in 0..n {
        for j in i..n {
            let s = tidy(&add(jac[i]
                .iter()
                .zip(&jac[j])
                .map(|(a, b)| mul2(a.clone(), b.clone()))
                .collect()));
            g[i][j] = s.clone();
            g[j][i] = s;
        }
    }
    Metric {
        coords: coords.to_vec(),
        g,
        upper: false,
    }
}

// ---------- curvature ----------

/// Curvature of a metric, computed on demand
pub struct Curvature {
    pub n: usize,
    xs: Vec<String>,
    gl: Mat,
    gu: Mat,
    first: OnceCell<Vec<Expr>>,
    second: OnceCell<Vec<Expr>>,
    riemann: OnceCell<Vec<Expr>>,
    riemann_lower: OnceCell<Vec<Expr>>,
    ricci: OnceCell<Vec<Expr>>,
    ricci_scalar: OnceCell<Expr>,
    kretschmann: OnceCell<Expr>,
}

fn sum(ts: Vec<Expr>) -> Expr {
    let ts: Vec<Expr> = ts.into_iter().filter(|t| !t.is_zero()).collect();
    match ts.len() {
        0 => Expr::zero(),
        _ => tidy(&add(ts)),
    }
}

fn prod(a: &Expr, b: &Expr) -> Expr {
    if a.is_zero() || b.is_zero() {
        Expr::zero()
    } else {
        mul2(a.clone(), b.clone())
    }
}

fn scale(c: Expr, e: Expr) -> Expr {
    if e.is_zero() { e } else { mul2(c, e) }
}

impl Curvature {
    pub fn new(m: &Metric) -> Result<Curvature, String> {
        Ok(Curvature {
            n: m.dim(),
            xs: m.coords.clone(),
            gl: m.lower()?,
            gu: m.raised()?,
            first: OnceCell::new(),
            second: OnceCell::new(),
            riemann: OnceCell::new(),
            riemann_lower: OnceCell::new(),
            ricci: OnceCell::new(),
            ricci_scalar: OnceCell::new(),
            kretschmann: OnceCell::new(),
        })
    }

    fn i3(&self, a: usize, b: usize, c: usize) -> usize {
        (a * self.n + b) * self.n + c
    }
    fn i4(&self, a: usize, b: usize, c: usize, d: usize) -> usize {
        ((a * self.n + b) * self.n + c) * self.n + d
    }

    fn d(&self, e: &Expr, i: usize) -> Expr {
        if e.is_zero() {
            Expr::zero()
        } else {
            tidy(&deriv(e, &self.xs[i]))
        }
    }

    /// Gamma_abc = 1/2 (g_ab,c + g_ac,b - g_bc,a)
    pub fn first(&self) -> &Vec<Expr> {
        self.first.get_or_init(|| {
            let n = self.n;
            let mut dg = vec![Expr::zero(); n * n * n];
            for a in 0..n {
                for b in 0..n {
                    for c in 0..n {
                        dg[self.i3(a, b, c)] = self.d(&self.gl[a][b], c);
                    }
                }
            }
            let half = rat(1, 2);
            let mut out = vec![Expr::zero(); n * n * n];
            for a in 0..n {
                for b in 0..n {
                    for c in 0..n {
                        out[self.i3(a, b, c)] = sum(vec![
                            scale(half.clone(), dg[self.i3(a, b, c)].clone()),
                            scale(half.clone(), dg[self.i3(a, c, b)].clone()),
                            scale(rat(-1, 2), dg[self.i3(b, c, a)].clone()),
                        ]);
                    }
                }
            }
            out
        })
    }

    /// Gamma^a_bc = g^ad Gamma_dbc
    pub fn second(&self) -> &Vec<Expr> {
        self.second.get_or_init(|| {
            let n = self.n;
            let first = self.first();
            let mut out = vec![Expr::zero(); n * n * n];
            for a in 0..n {
                for b in 0..n {
                    for c in b..n {
                        let v = sum((0..n)
                            .map(|k| prod(&self.gu[a][k], &first[self.i3(k, b, c)]))
                            .collect());
                        out[self.i3(a, b, c)] = v.clone();
                        out[self.i3(a, c, b)] = v;
                    }
                }
            }
            out
        })
    }

    pub fn gamma(&self, a: usize, b: usize, c: usize) -> &Expr {
        &self.second()[self.i3(a, b, c)]
    }

    /// R^a_bcd = d_c Gamma^a_bd - d_d Gamma^a_bc + Gamma^a_ce Gamma^e_bd - Gamma^a_de Gamma^e_bc
    pub fn riemann(&self) -> &Vec<Expr> {
        self.riemann.get_or_init(|| {
            let n = self.n;
            let mut r = vec![Expr::zero(); n * n * n * n];
            for a in 0..n {
                for b in 0..n {
                    for c in 0..n {
                        for d in c + 1..n {
                            let mut ts = vec![
                                self.d(self.gamma(a, b, d), c),
                                neg(self.d(self.gamma(a, b, c), d)),
                            ];
                            for e in 0..n {
                                ts.push(prod(self.gamma(a, c, e), self.gamma(e, b, d)));
                                let t = prod(self.gamma(a, d, e), self.gamma(e, b, c));
                                ts.push(if t.is_zero() { t } else { neg(t) });
                            }
                            let v = sum(ts);
                            r[self.i4(a, b, d, c)] = if v.is_zero() {
                                v.clone()
                            } else {
                                tidy(&neg(v.clone()))
                            };
                            r[self.i4(a, b, c, d)] = v;
                        }
                    }
                }
            }
            r
        })
    }

    /// R_abcd = g_ae R^e_bcd
    pub fn riemann_lower(&self) -> &Vec<Expr> {
        self.riemann_lower.get_or_init(|| {
            let n = self.n;
            let r = self.riemann();
            let mut out = vec![Expr::zero(); n * n * n * n];
            for a in 0..n {
                for b in 0..n {
                    for c in 0..n {
                        for d in 0..n {
                            out[self.i4(a, b, c, d)] = sum((0..n)
                                .map(|e| prod(&self.gl[a][e], &r[self.i4(e, b, c, d)]))
                                .collect());
                        }
                    }
                }
            }
            out
        })
    }

    /// R_bd = R^a_bad
    pub fn ricci(&self) -> &Vec<Expr> {
        self.ricci.get_or_init(|| {
            let n = self.n;
            let r = self.riemann();
            let mut out = vec![Expr::zero(); n * n];
            for b in 0..n {
                for d in b..n {
                    let v = sum((0..n).map(|a| r[self.i4(a, b, a, d)].clone()).collect());
                    out[b * n + d] = v.clone();
                    out[d * n + b] = v;
                }
            }
            out
        })
    }

    pub fn ricci_scalar(&self) -> &Expr {
        self.ricci_scalar.get_or_init(|| {
            let n = self.n;
            let ric = self.ricci();
            let mut ts = vec![];
            for a in 0..n {
                for b in 0..n {
                    ts.push(prod(&self.gu[a][b], &ric[a * n + b]));
                }
            }
            sum(ts)
        })
    }

    /// G_ab = R_ab - 1/2 g_ab R
    pub fn einstein(&self) -> Vec<Expr> {
        let n = self.n;
        let ric = self.ricci();
        let s = self.ricci_scalar();
        let mut out = vec![Expr::zero(); n * n];
        for a in 0..n {
            for b in 0..n {
                out[a * n + b] = sum(vec![
                    ric[a * n + b].clone(),
                    scale(rat(-1, 2), prod(&self.gl[a][b], s)),
                ]);
            }
        }
        out
    }

    /// Schouten tensor P_ab = (R_ab - R g_ab / (2(n-1))) / (n-2)
    pub fn schouten(&self) -> Result<Vec<Expr>, String> {
        let n = self.n;
        if n < 3 {
            return Err("the Schouten tensor needs at least 3 dimensions".into());
        }
        let ric = self.ricci();
        let s = self.ricci_scalar();
        let k = n as i128;
        let mut out = vec![Expr::zero(); n * n];
        for a in 0..n {
            for b in 0..n {
                out[a * n + b] = sum(vec![
                    scale(rat(1, k - 2), ric[a * n + b].clone()),
                    scale(rat(-1, 2 * (k - 1) * (k - 2)), prod(&self.gl[a][b], s)),
                ]);
            }
        }
        Ok(out)
    }

    /// Weyl tensor C_iklm
    pub fn weyl(&self) -> Result<Vec<Expr>, String> {
        let n = self.n;
        if n < 3 {
            return Err("the Weyl tensor needs at least 3 dimensions".into());
        }
        let (rl, ric, s, g) = (
            self.riemann_lower(),
            self.ricci(),
            self.ricci_scalar(),
            &self.gl,
        );
        let k = n as i128;
        let r = |a: usize, b: usize| &ric[a * n + b];
        let mut out = vec![Expr::zero(); n * n * n * n];
        for i in 0..n {
            for kk in 0..n {
                for l in 0..n {
                    for m in 0..n {
                        let t2 = add(vec![
                            prod(r(i, m), &g[kk][l]),
                            neg(prod(r(i, l), &g[kk][m])),
                            prod(r(kk, l), &g[i][m]),
                            neg(prod(r(kk, m), &g[i][l])),
                        ]);
                        let t3 = mul(vec![
                            rat(1, (k - 1) * (k - 2)),
                            s.clone(),
                            add(vec![
                                prod(&g[i][l], &g[kk][m]),
                                neg(prod(&g[i][m], &g[kk][l])),
                            ]),
                        ]);
                        out[self.i4(i, kk, l, m)] = sum(vec![
                            rl[self.i4(i, kk, l, m)].clone(),
                            scale(rat(1, k - 2), t2),
                            t3,
                        ]);
                    }
                }
            }
        }
        Ok(out)
    }

    /// R_abcd R^abcd
    pub fn kretschmann(&self) -> &Expr {
        self.kretschmann.get_or_init(|| {
            let n = self.n;
            let rl = self.riemann_lower();
            let mut cur = rl.clone();
            for p in 0..4 {
                let prev = cur.clone();
                for a in 0..n {
                    for b in 0..n {
                        for c in 0..n {
                            for d in 0..n {
                                let ix = [a, b, c, d];
                                cur[self.i4(a, b, c, d)] = sum((0..n)
                                    .map(|e| {
                                        let mut jx = ix;
                                        jx[p] = e;
                                        prod(
                                            &self.gu[ix[p]][e],
                                            &prev[self.i4(jx[0], jx[1], jx[2], jx[3])],
                                        )
                                    })
                                    .collect());
                            }
                        }
                    }
                }
            }
            sum((0..rl.len()).map(|i| prod(&rl[i], &cur[i])).collect())
        })
    }

    pub fn gaussian(&self) -> Result<Expr, String> {
        if self.n != 2 {
            return Err("Gaussian curvature needs a 2D metric (a surface)".into());
        }
        Ok(tidy(&mul2(rat(1, 2), self.ricci_scalar().clone())))
    }

    /// right-hand sides of the geodesic equations x''^a = -Gamma^a_bc x'^b x'^c
    pub fn geodesic_rhs(&self) -> Vec<Expr> {
        let n = self.n;
        (0..n)
            .map(|a| {
                let mut ts = vec![];
                for b in 0..n {
                    for c in 0..n {
                        let g = self.gamma(a, b, c);
                        if !g.is_zero() {
                            ts.push(mul(vec![
                                Expr::int(-1),
                                g.clone(),
                                dot(&self.xs[b]),
                                dot(&self.xs[c]),
                            ]));
                        }
                    }
                }
                sum(ts)
            })
            .collect()
    }
}

pub fn dot(x: &str) -> Expr {
    Expr::Sym(format!("{}'", x))
}
pub fn ddot(x: &str) -> Expr {
    Expr::Sym(format!("{}''", x))
}

thread_local! {
    static CACHE: std::cell::RefCell<Vec<(Metric, Rc<Curvature>)>> = const { std::cell::RefCell::new(vec![]) };
}

/// Curvature of a metric; recent results are kept, since notebooks usually ask
/// several questions about one metric
pub fn curvature(m: &Metric) -> Result<Rc<Curvature>, String> {
    if let Some(c) = CACHE.with(|c| {
        c.borrow()
            .iter()
            .find(|(k, _)| k == m)
            .map(|(_, v)| v.clone())
    }) {
        return Ok(c);
    }
    let c = Rc::new(Curvature::new(m)?);
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.push((m.clone(), c.clone()));
        if cache.len() > 8 {
            cache.remove(0);
        }
    });
    Ok(c)
}

// ---------- displays ----------

/// Index symmetries, used to list each independent component once
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Symmetry {
    None,
    /// symmetric in the last two indices (Christoffel symbols)
    LastPair,
    /// symmetric rank 2 (metric, Ricci, Einstein)
    Pair,
    /// antisymmetric in the last two (R^a_bcd)
    AntiLastPair,
    /// R_abcd: antisymmetric pairs, symmetric under pair exchange
    RiemannLike,
}

impl Symmetry {
    fn keep(self, ix: &[usize]) -> bool {
        let k = ix.len();
        match self {
            Symmetry::None => true,
            Symmetry::LastPair => ix[k - 2] <= ix[k - 1],
            Symmetry::Pair => ix[0] <= ix[1],
            Symmetry::AntiLastPair => ix[2] < ix[3],
            Symmetry::RiemannLike => {
                ix[0] < ix[1]
                    && ix[2] < ix[3]
                    && (ix[0] < ix[2] || (ix[0] == ix[2] && ix[1] <= ix[3]))
            }
        }
    }
}

/// A named set of components (a tensor or Christoffel symbols), shown as the
/// list of its non-zero independent components
#[derive(Clone, Debug, PartialEq)]
pub struct Components {
    pub title: String,
    pub symbol: String,
    pub latex_symbol: String,
    /// true for an upper index
    pub upper: Vec<bool>,
    pub coords: Vec<String>,
    pub values: Vec<Expr>,
    pub symmetry: Symmetry,
}

impl Components {
    pub fn rank(&self) -> usize {
        self.upper.len()
    }

    fn location(&self, mut address: usize) -> Vec<usize> {
        let n = self.coords.len();
        let mut ix = vec![0; self.rank()];
        for k in (0..self.rank()).rev() {
            ix[k] = address % n;
            address /= n;
        }
        ix
    }

    pub fn get(&self, ix: &[usize]) -> Result<Expr, String> {
        let n = self.coords.len();
        if ix.len() != self.rank() {
            return Err(format!("{} needs {} indices", self.symbol, self.rank()));
        }
        if ix.iter().any(|&i| i >= n) {
            return Err("index out of bounds".into());
        }
        Ok(self.values[ix.iter().fold(0, |acc, i| acc * n + i)].clone())
    }

    fn non_zero(&self) -> Vec<(Vec<usize>, &Expr)> {
        self.values
            .iter()
            .enumerate()
            .map(|(a, v)| (self.location(a), v))
            .filter(|(ix, v)| self.symmetry.keep(ix) && !v.is_zero())
            .collect()
    }

    fn text_indices(&self, ix: &[usize]) -> String {
        let mut s = String::new();
        let mut last: Option<bool> = None;
        for (i, up) in ix.iter().zip(&self.upper) {
            if last == Some(*up) {
                s.push(',');
            } else {
                s.push(if *up { '^' } else { '_' });
            }
            s.push_str(&self.coords[*i]);
            last = Some(*up);
        }
        s
    }

    pub fn text(&self) -> String {
        let head = format!(
            "{} ({}, coordinates {})",
            self.title,
            self.symbol,
            self.coords.join(", ")
        );
        let nz = self.non_zero();
        if nz.is_empty() {
            return format!("{}: all components are zero", head);
        }
        let mut lines = vec![format!("{}, non-zero components:", head)];
        for (ix, v) in nz {
            lines.push(format!(
                "  {}{} = {}",
                self.symbol,
                self.text_indices(&ix),
                show(v)
            ));
        }
        lines.join("\n")
    }

    pub fn latex(&self) -> String {
        let nz = self.non_zero();
        let body = if nz.is_empty() {
            format!("{} = 0 \\quad\\text{{(all components)}}", self.latex_symbol)
        } else {
            nz.iter()
                .map(|(ix, v)| {
                    let idx: String = ix
                        .iter()
                        .zip(&self.upper)
                        .map(|(i, up)| {
                            format!(
                                "{}{{{}}}",
                                if *up { "{}^" } else { "{}_" },
                                latex::name(&self.coords[*i])
                            )
                        })
                        .collect();
                    format!("{}{} &= {}", self.latex_symbol, idx, latex::latex(v))
                })
                .collect::<Vec<_>>()
                .join(" \\\\\n")
        };
        format!("$$\\begin{{aligned}}\n{}\n\\end{{aligned}}$$", body)
    }

    pub fn markdown(&self) -> String {
        let names: Vec<String> = self
            .coords
            .iter()
            .map(|c| format!("${}$", latex::name(c)))
            .collect();
        format!(
            "**{}** (coordinates {})\n\n{}",
            self.title,
            names.join(", "),
            self.latex()
        )
    }
}

/// A list of equations lhs = rhs
#[derive(Clone, Debug, PartialEq)]
pub struct Equations {
    pub title: String,
    pub eqs: Vec<(Expr, Expr)>,
}

impl Equations {
    pub fn text(&self) -> String {
        let mut lines = vec![self.title.clone()];
        lines.extend(
            self.eqs
                .iter()
                .map(|(l, r)| format!("  {} = {}", show(l), show(r))),
        );
        lines.join("\n")
    }
    pub fn latex(&self) -> String {
        let body: Vec<String> = self
            .eqs
            .iter()
            .map(|(l, r)| format!("{} &= {}", latex::latex(l), latex::latex(r)))
            .collect();
        format!(
            "$$\\begin{{aligned}}\n{}\n\\end{{aligned}}$$",
            body.join(" \\\\\n")
        )
    }
    pub fn markdown(&self) -> String {
        format!("**{}**\n\n{}", self.title, self.latex())
    }
}

/// The curvature commands that return components or scalars
pub fn named(kind: &str, m: &Metric) -> Result<Expr, String> {
    use crate::expr::Obj;
    let cv = curvature(m)?;
    let n = m.dim();
    let comps =
        |title: &str, sym: &str, lsym: &str, upper: Vec<bool>, values: Vec<Expr>, s: Symmetry| {
            Expr::Obj(Rc::new(Obj::Components(Components {
                title: title.into(),
                symbol: sym.into(),
                latex_symbol: lsym.into(),
                upper,
                coords: m.coords.clone(),
                values,
                symmetry: s,
            })))
        };
    let (u, l) = (true, false);
    Ok(match kind {
        "christoffelfirst" => comps(
            "Christoffel symbols of the first kind",
            "Gamma",
            "\\Gamma",
            vec![l, l, l],
            cv.first().clone(),
            Symmetry::LastPair,
        ),
        "christoffel" | "christoffelsecond" => comps(
            "Christoffel symbols of the second kind",
            "Gamma",
            "\\Gamma",
            vec![u, l, l],
            cv.second().clone(),
            Symmetry::LastPair,
        ),
        "riemannfirst" => comps(
            "Riemann curvature tensor",
            "R",
            "R",
            vec![l, l, l, l],
            cv.riemann_lower().clone(),
            Symmetry::RiemannLike,
        ),
        "riemannsecond" | "riemann" => comps(
            "Riemann curvature tensor",
            "R",
            "R",
            vec![u, l, l, l],
            cv.riemann().clone(),
            Symmetry::AntiLastPair,
        ),
        "riccitensor" => comps(
            "Ricci tensor",
            "R",
            "R",
            vec![l, l],
            cv.ricci().clone(),
            Symmetry::Pair,
        ),
        "einsteintensor" => comps(
            "Einstein tensor",
            "G",
            "G",
            vec![l, l],
            cv.einstein(),
            Symmetry::Pair,
        ),
        "schoutentensor" => comps(
            "Schouten tensor",
            "P",
            "P",
            vec![l, l],
            cv.schouten()?,
            Symmetry::Pair,
        ),
        "weyltensor" => comps(
            "Weyl tensor",
            "C",
            "C",
            vec![l, l, l, l],
            cv.weyl()?,
            Symmetry::RiemannLike,
        ),
        "ricciscalar" => cv.ricci_scalar().clone(),
        "einsteinscalar" => tidy(&mul2(rat(2 - n as i128, 2), cv.ricci_scalar().clone())),
        "gausscurvature" => cv.gaussian()?,
        "kretschmann" => cv.kretschmann().clone(),
        "geodesics" => {
            let rhs = cv.geodesic_rhs();
            Expr::Obj(Rc::new(Obj::Equations(Equations {
                title: "Geodesic equations (' = d/ds along the curve)".into(),
                eqs: m
                    .coords
                    .iter()
                    .zip(rhs)
                    .map(|(x, r)| (ddot(x), r))
                    .collect(),
            })))
        }
        "inverse" => Expr::Obj(Rc::new(Obj::Metric(m.inverse()?))),
        _ => return Err(format!("unknown function {}", kind)),
    })
}

pub const NAMED: [&str; 16] = [
    "christoffel",
    "christoffelfirst",
    "christoffelsecond",
    "riemann",
    "riemannfirst",
    "riemannsecond",
    "riccitensor",
    "ricciscalar",
    "einsteintensor",
    "einsteinscalar",
    "schoutentensor",
    "weyltensor",
    "gausscurvature",
    "kretschmann",
    "geodesics",
    "inverse",
];
