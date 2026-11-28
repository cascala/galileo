//! Finding laws in data and checking them.
//!
//! * `discover(data, vars)` searches for the simplest formula that explains the
//!   last variable from the others (symbolic regression): power laws with
//!   simple rational exponents, and sparse sums of terms from a library
//!   (the SINDy idea). Candidates are ranked by fit against simplicity.
//! * `check(law, data, vars)` tests a formula (or several) against
//!   observations, fitting any free constants.
//! * `scatter(data, vars)` draws observations as points, `odedata` turns an ODE
//!   solution into a data table.

use crate::build::*;
use crate::calc;
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::latex::{latex, name as lname};
use crate::numeric::{self, compile, run};
use crate::parser::Node;
use crate::plot::{Figure, Series};
use crate::print::show;
use std::rc::Rc;

/// Result of discover or check: text for terminals, Markdown for notebooks
#[derive(Clone, Debug, PartialEq)]
pub struct Report {
    pub kind: &'static str,
    pub text: String,
    pub markdown: String,
    /// the formulas found or checked; r[k] picks one
    pub formulas: Vec<Expr>,
    pub best: usize,
    /// column names and the derivative order of the last one
    pub vars: Vec<String>,
    pub order: usize,
    pub target: usize,
    /// what the result shows (plain Galileo); None: the formulas
    pub value: Option<Expr>,
}

/// the same report showing `v` as its value
pub fn with_value(e: Expr, v: Expr) -> Expr {
    if let Expr::Obj(o) = &e {
        if let Obj::Report(r) = &**o {
            let mut r = r.clone();
            r.value = Some(v);
            return Expr::Obj(Rc::new(Obj::Report(r)));
        }
    }
    e
}

impl Report {
    /// The value a report shows: its own, else its formulas (one, or a column)
    pub fn shown_value(&self) -> Option<Expr> {
        if let Some(v) = &self.value {
            return Some(v.clone());
        }
        match self.formulas.len() {
            0 => None,
            1 => Some(self.formulas[0].clone()),
            _ => Some(Expr::Matrix(
                self.formulas.iter().map(|f| vec![f.clone()]).collect(),
            )),
        }
    }
}

fn report(r: Report) -> Expr {
    Expr::Obj(Rc::new(Obj::Report(r)))
}

// ---------- small numeric helpers ----------

/// Weighted least squares: minimise sum w_i (y_i - A_i . c)^2.
/// Returns the coefficients and the diagonal of (A^T W A)^-1.
fn wls(a: &[Vec<f64>], y: &[f64], w: &[f64]) -> Option<(Vec<f64>, Vec<f64>)> {
    wls_full(a, y, w).map(|(c, inv)| {
        let d = (0..c.len()).map(|j| inv[j][j]).collect();
        (c, d)
    })
}

/// Like wls, with the whole matrix (A^T W A)^-1
fn wls_full(a: &[Vec<f64>], y: &[f64], w: &[f64]) -> Option<(Vec<f64>, Vec<Vec<f64>>)> {
    let n = a.len();
    let p = a.first()?.len();
    if p == 0 || n < p {
        return None;
    }
    // scale columns for conditioning
    let scale: Vec<f64> = (0..p)
        .map(|j| {
            let s =
                (a.iter().zip(w).map(|(r, wi)| wi * r[j] * r[j]).sum::<f64>() / n as f64).sqrt();
            if s > 0.0 && s.is_finite() { s } else { 1.0 }
        })
        .collect();
    let mut m = vec![vec![0.0; 2 * p + 1]; p];
    for (i, r) in a.iter().enumerate() {
        for j in 0..p {
            let aj = r[j] / scale[j];
            for k in 0..p {
                m[j][k] += w[i] * aj * r[k] / scale[k];
            }
            m[j][2 * p] += w[i] * aj * y[i];
        }
    }
    for j in 0..p {
        m[j][p + j] = 1.0;
    }
    // Gauss-Jordan with partial pivoting on [M | I | b]
    let norm = (0..p).map(|j| m[j][j].abs()).fold(0.0, f64::max);
    for col in 0..p {
        let piv = (col..p).max_by(|&x, &y| m[x][col].abs().total_cmp(&m[y][col].abs()))?;
        if m[piv][col].abs() <= 1e-11 * norm.max(1e-300) {
            return None;
        }
        m.swap(col, piv);
        let d = m[col][col];
        for v in m[col].iter_mut() {
            *v /= d;
        }
        for r in 0..p {
            if r != col {
                let f = m[r][col];
                if f != 0.0 {
                    for k in 0..=2 * p {
                        m[r][k] -= f * m[col][k];
                    }
                }
            }
        }
    }
    let c: Vec<f64> = (0..p).map(|j| m[j][2 * p] / scale[j]).collect();
    let inv: Vec<Vec<f64>> = (0..p)
        .map(|j| {
            (0..p)
                .map(|k| m[j][p + k] / (scale[j] * scale[k]))
                .collect()
        })
        .collect();
    if c.iter().all(|v| v.is_finite()) {
        Some((c, inv))
    } else {
        None
    }
}

/// Round to the number of significant digits the uncertainty supports
fn round_sig(v: f64, sd: f64) -> f64 {
    if v == 0.0 || !v.is_finite() {
        return v;
    }
    let digits = if sd > 0.0 && sd.is_finite() {
        ((v.abs() / sd).log10().ceil() as i32 + 1).clamp(2, 10)
    } else {
        10
    };
    let s = format!("{:.*e}", (digits - 1) as usize, v);
    s.parse().unwrap_or(v)
}

fn gcd(a: i128, b: i128) -> i128 {
    if b == 0 { a.abs() } else { gcd(b, a % b) }
}

fn digits(n: i128) -> usize {
    n.abs().to_string().len()
}

/// A simple exact constant close to c (within 2 standard deviations), or c rounded
fn recognize(c: f64, sd: f64) -> (Expr, bool) {
    let tol = (2.0 * sd).max(1e-8 * c.abs());
    let rel = tol / c.abs();
    if c != 0.0 && rel < 0.05 {
        // fractions only when the constant is known well (otherwise 0.6627 "is" 2/3)
        let qmax = if rel < 1e-3 { 12 } else { 1 };
        let mut ks: Vec<(f64, Expr, usize)> = vec![(1.0, Expr::one(), 0)];
        if rel < 1e-3 {
            ks.push((std::f64::consts::PI, pi(), 2));
            ks.push((std::f64::consts::PI.powi(2), pow(pi(), Expr::int(2)), 2));
            ks.push((2f64.sqrt(), sqrt(Expr::int(2)), 3));
            ks.push((3f64.sqrt(), sqrt(Expr::int(3)), 3));
        }
        let allowed = 2 + (-rel.log10()).floor().max(0.0) as usize;
        let mut best: Option<(usize, Expr)> = None;
        for (k, ke, kc) in &ks {
            for q in 1..=qmax {
                let p = (c * q as f64 / k).round();
                if p == 0.0 || p.abs() > 1e4 {
                    continue;
                }
                let p = p as i128;
                if gcd(p, q) != 1 || (p as f64 / q as f64 * k - c).abs() > tol {
                    continue;
                }
                let cost = 3 * digits(p) - 1 + if q > 1 { digits(q) } else { 0 } + kc;
                if cost <= allowed && best.as_ref().is_none_or(|b| cost < b.0) {
                    best = Some((cost, mul(vec![rat(p, q), ke.clone()])));
                }
            }
        }
        if let Some((_, e)) = best {
            return (e, true);
        }
    }
    (Expr::float(round_sig(c, sd)), false)
}

/// Rational exponents p/q (q <= 4) within 2 sd of a, simplest first (at most 3)
fn exponent_options(a: f64, sd: f64) -> Vec<Expr> {
    let tol = (2.0 * sd).max(0.01);
    let mut opts: Vec<(i128, f64, i128)> = vec![];
    for q in 1..=4i128 {
        let lo = ((a - tol) * q as f64).ceil() as i128;
        let hi = ((a + tol) * q as f64).floor() as i128;
        for p in lo..=hi {
            if gcd(p, q) == 1 || p == 0 && q == 1 {
                opts.push((q, (p as f64 / q as f64 - a).abs(), p));
            }
        }
    }
    opts.sort_by(|x, y| x.0.cmp(&y.0).then(x.1.total_cmp(&y.1)));
    opts.dedup_by_key(|o| (o.2 * 12) / o.0);
    opts.into_iter()
        .take(3)
        .map(|(q, _, p)| rat(p, q))
        .collect()
}

fn fmt(v: f64) -> String {
    if v == 0.0 {
        return "0".into();
    }
    let a = v.abs();
    if !(1e-4..1e6).contains(&a) {
        return format!("{:.3e}", v);
    }
    let s = format!("{:.4}", round_sig(v, a * 1e-4));
    let s = if s.contains('.') {
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    } else {
        s
    };
    if s == "-0" { "0".into() } else { s }
}

fn pct(v: f64) -> String {
    let p = v * 100.0;
    let a = p.abs();
    if a >= 10.0 {
        format!("{:.0}%", p)
    } else if a >= 1.0 {
        format!("{:.1}%", p)
    } else if a >= 0.001 {
        let d = (2 - a.log10().floor() as i32 - 1).max(0) as usize;
        format!("{:.*}%", d, p)
    } else if a >= 1e-9 {
        format!("{:.0e}%", p)
    } else {
        "0%".into()
    }
}

// ---------- data tables ----------

/// The observations, ready for fitting
struct Table {
    /// names of the input columns, in the order of `x`
    inputs: Vec<String>,
    /// the target as written, e.g. T or u''
    target: String,
    x: Vec<Vec<f64>>,
    y: Vec<f64>,
    sigma: Option<Vec<f64>>,
    names: Vec<String>,
    order: usize,
    target_col: usize,
    /// typical error of the estimated derivatives (0 without derivatives)
    fd_err: f64,
}

/// A column name, or a derivative d(u, t) / d(u, t, 2) of one
fn var_spec(n: &Node) -> R<(String, usize)> {
    match n {
        Node::Ident(s) => Ok((s.clone(), 0)),
        Node::Call(f, a) if f == "d" && (2..=3).contains(&a.len()) => {
            let Node::Ident(s) = &a[0] else {
                return Err("write a derivative column as d(u, t) or d(u, t, 2)".into());
            };
            let k = match a.get(2) {
                None => 1,
                Some(Node::Num(k)) => k.parse::<usize>().ok().filter(|k| (1..=4).contains(k)).ok_or("d(u, t, k): k is 1 to 4")?,
                _ => return Err("d(u, t, k): k is a number".into()),
            };
            Ok((s.clone(), k))
        }
        Node::Transpose(_) => Err("write a derivative column as d(u, t) or d(u, t, 2); ' is the transpose".into()),
        _ => Err("the variables must be names such as [a T], one per column of the data".into()),
    }
}

/// (names, derivative order of the target, index of the target column):
/// the target is the name written with primes, otherwise the last one
fn var_specs(n: &Node) -> R<(Vec<String>, usize, usize)> {
    let items: Vec<&Node> = match n {
        Node::Matrix(rows) => rows.iter().flatten().collect(),
        other => vec![other],
    };
    let mut names = vec![];
    let mut order = 0;
    let mut target = items.len().saturating_sub(1);
    for (i, it) in items.iter().enumerate() {
        let (s, k) = var_spec(it)?;
        if k > 0 {
            if order > 0 {
                return Err("only one variable can be a derivative such as d(u, t, 2)".into());
            }
            if i == 0 {
                return Err(
                    "the first column is the independent variable, so it can't be the derivative"
                        .into(),
                );
            }
            order = k;
            target = i;
        }
        names.push(s);
    }
    Ok((names, order, target))
}

fn numbers_of(e: &Expr, what: &str) -> R<Vec<Vec<f64>>> {
    let rows = match e {
        Expr::Matrix(m) => m.clone(),
        other if other.is_scalar() => vec![vec![other.clone()]],
        _ => return Err(format!("{} must be a matrix of numbers", what)),
    };
    rows.iter()
        .map(|r| {
            r.iter()
                .map(|v| {
                    let x = numeric::value(v);
                    if x.is_finite() {
                        Ok(x)
                    } else {
                        Err(format!(
                            "{} must contain only numbers, found {}",
                            what,
                            show(v)
                        ))
                    }
                })
                .collect()
        })
        .collect()
}

/// Derivatives of y with respect to t (sorted), dropping the end points
fn differentiate(t: &[f64], y: &[f64]) -> Vec<f64> {
    (1..t.len() - 1)
        .map(|i| {
            let (h1, h2) = (t[i] - t[i - 1], t[i + 1] - t[i]);
            (-h2 / (h1 * (h1 + h2))) * y[i - 1]
                + ((h2 - h1) / (h1 * h2)) * y[i]
                + (h1 / (h2 * (h1 + h2))) * y[i + 1]
        })
        .collect()
}

fn second_derivative(t: &[f64], y: &[f64]) -> Vec<f64> {
    (1..t.len() - 1)
        .map(|i| {
            let (h1, h2) = (t[i] - t[i - 1], t[i + 1] - t[i]);
            2.0 * (y[i - 1] / (h1 * (h1 + h2)) - y[i] / (h1 * h2) + y[i + 1] / (h2 * (h1 + h2)))
        })
        .collect()
}

impl Session {
    fn data_table(&mut self, data: &Node, vars: &Node, sigma: Option<&Node>, f: &str) -> R<Table> {
        let (names, order, last) = var_specs(vars)?;
        let d = self.eval(data)?;
        let mut rows = numbers_of(&d, &format!("{}: the data", f))?;
        if rows.len() == 1
            && names.len() > 1
            && rows[0].len() % names.len() == 0
            && rows[0].len() > names.len()
        {
            return Err(format!(
                "{}: the data must have one row per observation and one column per variable",
                f
            ));
        }
        let ncol = rows.first().map_or(0, |r| r.len());
        if ncol != names.len() {
            return Err(format!(
                "{}: the data has {} columns but {} variable names were given ({}); give one name per column",
                f,
                ncol,
                names.len(),
                names.join(", ")
            ));
        }
        if names.len() < 2 && order == 0 {
            return Err(format!(
                "{}: give at least two columns: the inputs and, last, the quantity to explain",
                f
            ));
        }
        let mut sig = match sigma {
            None => None,
            Some(s) => {
                let v: Vec<f64> = numbers_of(&self.eval(s)?, &format!("{}: the uncertainties", f))?
                    .into_iter()
                    .flatten()
                    .collect();
                if v.len() != rows.len() {
                    return Err(format!(
                        "{}: give one uncertainty per row of data ({} rows, {} uncertainties)",
                        f,
                        rows.len(),
                        v.len()
                    ));
                }
                if v.iter().any(|s| *s <= 0.0) {
                    return Err(format!("{}: uncertainties must be positive", f));
                }
                Some(v)
            }
        };
        let mut fd_err = 0.0;
        let (inputs, x, y): (Vec<String>, Vec<Vec<f64>>, Vec<f64>) = if order == 0 {
            let keep = |r: &Vec<f64>| -> Vec<f64> {
                r.iter()
                    .enumerate()
                    .filter(|(j, _)| *j != last)
                    .map(|(_, v)| *v)
                    .collect()
            };
            let nm: Vec<String> = names
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != last)
                .map(|(_, v)| v.clone())
                .collect();
            (
                nm,
                rows.iter().map(keep).collect(),
                rows.iter().map(|r| r[last]).collect(),
            )
        } else {
            if names.len() < 2 {
                return Err(format!(
                    "{}: a derivative needs the independent variable as the first column, e.g. [t d(x, t, 2)]",
                    f
                ));
            }
            if order > 2 {
                return Err(format!(
                    "{}: derivatives up to second order (d(u, t, 2)) are supported",
                    f
                ));
            }
            rows.sort_by(|a, b| a[0].total_cmp(&b[0]));
            if rows.len() < 5 || rows.windows(2).any(|w| w[1][0] <= w[0][0]) {
                return Err(format!(
                    "{}: to estimate {} the first column must have at least 5 different values",
                    f,
                    dname(&names[last], &names[0], order)
                ));
            }
            let t: Vec<f64> = rows.iter().map(|r| r[0]).collect();
            let u: Vec<f64> = rows.iter().map(|r| r[last]).collect();
            // five-point estimates (an interpolating quartic through each row and its
            // two neighbours on each side, error ~ h^3), three points for short tables
            let (dy, cut) = if t.len() >= 7 {
                let d: Vec<f64> = (2..t.len() - 2)
                    .map(|i| {
                        let a: Vec<Vec<f64>> = (i - 2..=i + 2)
                            .map(|j| (0..5).map(|k| (t[j] - t[i]).powi(k)).collect())
                            .collect();
                        let yy: Vec<f64> = (i - 2..=i + 2).map(|j| u[j]).collect();
                        match wls(&a, &yy, &[1.0; 5]) {
                            Some((c, _)) => {
                                if order == 1 {
                                    c[1]
                                } else {
                                    2.0 * c[2]
                                }
                            }
                            None => f64::NAN,
                        }
                    })
                    .collect();
                (d, 2)
            } else if order == 1 {
                (differentiate(&t, &u), 1)
            } else {
                (second_derivative(&t, &u), 1)
            };
            // compare with the three-point estimate to judge the derivative error
            let d3 = if order == 1 {
                differentiate(&t, &u)
            } else {
                second_derivative(&t, &u)
            };
            fd_err = if cut == 2 {
                let m = dy.len();
                ((0..m).map(|i| (dy[i] - d3[i + 1]).powi(2)).sum::<f64>() / m as f64).sqrt()
            } else {
                0.0
            };
            let inner = &rows[cut..rows.len() - cut];
            sig = sig.map(|s| s[cut..s.len() - cut].to_vec());
            (names.clone(), inner.to_vec(), dy)
        };
        Ok(Table {
            inputs,
            target: dname(&names[last], &names[0], order),
            x,
            y,
            sigma: sig,
            names,
            order,
            target_col: last,
            fd_err,
        })
    }
}

impl Table {
    fn weights(&self) -> Vec<f64> {
        match &self.sigma {
            Some(s) => s.iter().map(|v| 1.0 / (v * v)).collect(),
            None if self.relative() => self.y.iter().map(|v| 1.0 / (v * v)).collect(),
            None => vec![1.0; self.y.len()],
        }
    }
    /// measure misfit relative to each value when no value is (nearly) zero
    fn relative(&self) -> bool {
        let m = self.y.iter().map(|v| v.abs()).fold(0.0, f64::max);
        let same_sign = self.y.iter().all(|v| *v > 0.0) || self.y.iter().all(|v| *v < 0.0);
        same_sign && self.y.iter().all(|v| v.abs() > 1e-6 * m)
    }
    /// misfit: chi^2 per degree of freedom with uncertainties, else an RMS relative error
    fn misfit(&self, pred: &[f64], params: usize) -> f64 {
        let n = self.y.len();
        if pred.iter().any(|p| !p.is_finite()) {
            return f64::INFINITY;
        }
        match &self.sigma {
            Some(s) => {
                let chi: f64 = (0..n).map(|i| ((self.y[i] - pred[i]) / s[i]).powi(2)).sum();
                chi / (n.saturating_sub(params).max(1)) as f64
            }
            None if self.relative() => ((0..n)
                .map(|i| ((self.y[i] - pred[i]) / self.y[i]).powi(2))
                .sum::<f64>()
                / n as f64)
                .sqrt(),
            None => {
                let scale = (self.y.iter().map(|v| v * v).sum::<f64>() / n as f64)
                    .sqrt()
                    .max(1e-300);
                ((0..n).map(|i| (self.y[i] - pred[i]).powi(2)).sum::<f64>() / n as f64).sqrt()
                    / scale
            }
        }
    }
    fn predict(&self, e: &Expr) -> Vec<f64> {
        let nm: Vec<&str> = self.inputs.iter().map(|s| s.as_str()).collect();
        let c = compile(e, &nm);
        self.x.iter().map(|r| run(&c, r)).collect()
    }
    fn misfit_name(&self) -> &'static str {
        if self.sigma.is_some() {
            "chi2/dof"
        } else if self.relative() {
            "rel. error"
        } else {
            "error"
        }
    }
    fn fmt_misfit(&self, m: f64) -> String {
        if self.sigma.is_some() {
            format!("{:.3}", m)
        } else {
            pct(m)
        }
    }
}

// ---------- discover ----------

#[derive(Clone)]
struct Cand {
    formula: Expr,
    complexity: usize,
    err: f64,
    /// leave-one-out error: how well the formula predicts each row from the others
    cv: f64,
    /// number of fitted constants
    params: usize,
}

fn default_library(t: &Table) -> Vec<Expr> {
    let xs: Vec<Expr> = t.inputs.iter().map(|s| Expr::sym(s)).collect();
    let mut lib = vec![Expr::one()];
    let pw = |x: &Expr, p: i128, q: i128| pow(x.clone(), rat(p, q));
    if xs.len() == 1 {
        let x = &xs[0];
        for (p, q) in [
            (1, 1),
            (2, 1),
            (3, 1),
            (-1, 1),
            (-2, 1),
            (1, 2),
            (3, 2),
            (-1, 2),
        ] {
            lib.push(pw(x, p, q));
        }
    } else {
        for x in &xs {
            for p in [1, 2, -1, -2] {
                lib.push(pw(x, p, 1));
            }
        }
        for i in 0..xs.len() {
            for j in i + 1..xs.len() {
                lib.push(mul2(xs[i].clone(), xs[j].clone()));
                lib.push(div(xs[i].clone(), xs[j].clone()));
                lib.push(div(xs[j].clone(), xs[i].clone()));
            }
        }
    }
    lib
}

/// columns of term values; terms that are not finite on all rows are dropped
fn term_columns(t: &Table, lib: Vec<Expr>) -> (Vec<Expr>, Vec<Vec<f64>>) {
    let mut terms = vec![];
    let mut cols: Vec<Vec<f64>> = vec![];
    for e in lib {
        let v = t.predict(&e);
        if v.iter().all(|x| x.is_finite() && x.abs() < 1e150) && !cols.iter().any(|c| c == &v) {
            terms.push(e);
            cols.push(v);
        }
    }
    (terms, cols)
}

fn subsets(n: usize, k: usize) -> Vec<Vec<usize>> {
    let mut out = vec![];
    let mut cur = vec![];
    fn go(start: usize, n: usize, k: usize, cur: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
        if cur.len() == k {
            out.push(cur.clone());
            return;
        }
        for i in start..n {
            cur.push(i);
            go(i + 1, n, k, cur, out);
            cur.pop();
        }
    }
    go(0, n, k, &mut cur, &mut out);
    out
}

fn n_floats(e: &Expr) -> usize {
    match e {
        Expr::Num(n) if !n.is_exact() => 1,
        Expr::Add(v) | Expr::Mul(v) => v.iter().map(n_floats).sum(),
        Expr::Pow(a, b) => n_floats(a) + 2 * n_floats(b),
        Expr::Func(_, a) => n_floats(a),
        _ => 0,
    }
}

/// size of a formula: every constant counts as one node (two if it is a decimal
/// that could not be recognised), fractional exponents count one more
fn complexity(e: &Expr) -> usize {
    if e.symbols().is_empty() {
        return 1 + n_floats(e).min(1);
    }
    match e {
        Expr::Add(v) | Expr::Mul(v) => 1 + v.iter().map(complexity).sum::<usize>(),
        // x^2 or 1/x counts like a function of x; a fractional power one more
        Expr::Pow(a, b) if b.num().is_some_and(|n| n.is_exact()) => {
            1 + complexity(a) + b.num().is_some_and(|n| !n.is_integer()) as usize
        }
        Expr::Pow(a, b) => 1 + complexity(a) + complexity(b),
        Expr::Func(_, a) => 1 + complexity(a),
        _ => 1,
    }
}

fn linear_candidates(
    t: &Table,
    terms: &[Expr],
    cols: &[Vec<f64>],
    kmax: usize,
    out: &mut Vec<Cand>,
) {
    let n = t.y.len();
    let w = t.weights();
    for k in 1..=kmax.min(terms.len()) {
        if k >= n && k > 1 {
            break;
        }
        for s in subsets(terms.len(), k) {
            let a: Vec<Vec<f64>> = (0..n)
                .map(|i| s.iter().map(|&j| cols[j][i]).collect())
                .collect();
            let Some((c, inv)) = wls_full(&a, &t.y, &w) else {
                continue;
            };
            let var: Vec<f64> = (0..k).map(|j| inv[j][j]).collect();
            let pred: Vec<f64> = (0..n)
                .map(|i| (0..k).map(|j| c[j] * a[i][j]).sum())
                .collect();
            // leave-one-out predictions from the hat matrix: y_i - r_i / (1 - h_ii)
            let loo: Vec<f64> = (0..n)
                .map(|i| {
                    let h: f64 = w[i]
                        * (0..k)
                            .map(|j| (0..k).map(|l| a[i][j] * inv[j][l] * a[i][l]).sum::<f64>())
                            .sum::<f64>();
                    if h < 1.0 - 1e-9 {
                        t.y[i] - (t.y[i] - pred[i]) / (1.0 - h)
                    } else {
                        f64::NAN
                    }
                })
                .collect();
            let cv = if loo.iter().all(|v| v.is_finite()) {
                t.misfit(&loo, 0)
            } else {
                f64::INFINITY
            };
            let chi: f64 = (0..n).map(|i| w[i] * (t.y[i] - pred[i]).powi(2)).sum();
            let s2 = if t.sigma.is_some() {
                (chi / (n.saturating_sub(k).max(1)) as f64).max(1.0)
            } else {
                chi / (n.saturating_sub(k).max(1)) as f64
            };
            // without uncertainties, the misfit itself bounds how well a coefficient is known
            let e0 = if t.sigma.is_some() {
                0.0
            } else {
                t.misfit(&pred, k)
            };
            let yrms = (t.y.iter().map(|v| v * v).sum::<f64>() / n as f64).sqrt();
            let mut floats = vec![];
            let mut exact: Vec<Option<Expr>> = vec![];
            let mut sds = vec![];
            for j in 0..k {
                let crms = ((0..n).map(|i| a[i][j] * a[i][j]).sum::<f64>() / n as f64)
                    .sqrt()
                    .max(1e-300);
                let sd0 = (var[j] * s2).sqrt();
                let scale = if t.relative() {
                    c[j].abs()
                } else {
                    yrms / crms
                };
                let sd = if t.sigma.is_some() {
                    sd0
                } else {
                    (3.0 * sd0)
                        .max(3.0 * e0 * scale / (n as f64).sqrt())
                        .max(t.fd_err / crms)
                };
                let (ce, ok) = recognize(c[j], sd);
                exact.push(if ok { Some(ce) } else { None });
                sds.push(sd);
                floats.push(mul2(
                    Expr::float(round_sig(c[j], sd0.max(1e-12 * c[j].abs()))),
                    terms[s[j]].clone(),
                ));
            }
            // fix a subset of the recognised constants and refit the others; keep the largest
            // subset that doesn't spoil the fit
            let ff = add(floats);
            let ef = t.misfit(&t.predict(&ff), k);
            let rec: Vec<usize> = (0..k).filter(|&j| exact[j].is_some()).collect();
            let mut best_f: (usize, f64, Expr) = (0, ef, ff);
            for mask in 1u32..(1u32 << rec.len()) {
                let fixed: Vec<usize> = (0..rec.len())
                    .filter(|b| mask & (1 << b) != 0)
                    .map(|b| rec[b])
                    .collect();
                if fixed.len() < best_f.0 {
                    continue;
                }
                let free: Vec<usize> = (0..k).filter(|j| !fixed.contains(j)).collect();
                let mut cf = c.clone();
                if !free.is_empty() {
                    let yy: Vec<f64> = (0..n)
                        .map(|i| {
                            t.y[i]
                                - fixed
                                    .iter()
                                    .map(|&j| numeric::value(exact[j].as_ref().unwrap()) * a[i][j])
                                    .sum::<f64>()
                        })
                        .collect();
                    let aa: Vec<Vec<f64>> = (0..n)
                        .map(|i| free.iter().map(|&j| a[i][j]).collect())
                        .collect();
                    let Some((c2, _)) = wls(&aa, &yy, &w) else {
                        continue;
                    };
                    for (m, &j) in free.iter().enumerate() {
                        cf[j] = c2[m];
                    }
                }
                let mut parts = vec![];
                for j in 0..k {
                    let ce = if fixed.contains(&j) {
                        exact[j].clone().unwrap()
                    } else {
                        Expr::float(round_sig(cf[j], sds[j].max(1e-12 * cf[j].abs())))
                    };
                    if !ce.is_zero() {
                        parts.push(mul2(ce, terms[s[j]].clone()));
                    }
                }
                let f = add(parts);
                let e = t.misfit(&t.predict(&f), k);
                if e <= (1.5 * ef).max(1e-6) && (fixed.len() > best_f.0 || e < best_f.1) {
                    best_f = (fixed.len(), e, f);
                }
            }
            let (nfixed, err, f) = best_f;
            let cv = if nfixed > 0 { cv.max(err) } else { cv };
            out.push(Cand {
                complexity: complexity(&f),
                formula: f,
                err,
                cv,
                params: k,
            });
        }
    }
}

/// y = C * prod x_i^a_i, fitted on logarithms
fn power_candidates(t: &Table, out: &mut Vec<Cand>, notes: &mut Vec<String>) {
    let n = t.y.len();
    let sign = if t.y.iter().all(|v| *v > 0.0) {
        1.0
    } else if t.y.iter().all(|v| *v < 0.0) {
        -1.0
    } else {
        return;
    };
    let use_: Vec<usize> = (0..t.inputs.len())
        .filter(|&j| t.x.iter().all(|r| r[j] > 0.0))
        .collect();
    if use_.is_empty() || n < use_.len() + 2 {
        return;
    }
    let ly: Vec<f64> = t.y.iter().map(|v| (v * sign).ln()).collect();
    let lw: Vec<f64> = match &t.sigma {
        Some(s) => (0..n).map(|i| (t.y[i] / s[i]).powi(2)).collect(),
        None => vec![1.0; n],
    };
    let a: Vec<Vec<f64>> =
        t.x.iter()
            .map(|r| {
                std::iter::once(1.0)
                    .chain(use_.iter().map(|&j| r[j].ln()))
                    .collect()
            })
            .collect();
    let Some((c, var)) = wls(&a, &ly, &lw) else {
        return;
    };
    let p = use_.len() + 1;
    let chi: f64 = (0..n)
        .map(|i| lw[i] * (ly[i] - (0..p).map(|j| c[j] * a[i][j]).sum::<f64>()).powi(2))
        .sum();
    let s2 = if t.sigma.is_some() {
        (chi / (n - p).max(1) as f64).max(1.0)
    } else {
        chi / (n - p).max(1) as f64
    };
    let sds: Vec<f64> = (0..p).map(|j| (var[j] * s2).sqrt()).collect();
    let fitted: Vec<String> = use_
        .iter()
        .enumerate()
        .map(|(k, &j)| format!("{}: {} ± {}", t.inputs[j], fmt(c[k + 1]), fmt(sds[k + 1])))
        .collect();
    notes.push(format!(
        "power-law exponents fitted freely: {}",
        fitted.join(", ")
    ));
    // rational exponent combinations
    let opts: Vec<Vec<Expr>> = (0..use_.len())
        .map(|k| exponent_options(c[k + 1], sds[k + 1]))
        .collect();
    let mut combos: Vec<Vec<Expr>> = vec![vec![]];
    for o in &opts {
        let mut next = vec![];
        for cmb in &combos {
            for e in o {
                let mut v = cmb.clone();
                v.push(e.clone());
                next.push(v);
            }
        }
        combos = next;
    }
    let build = |exps: &[Expr], fitted_coef: Option<(f64, f64)>| -> Option<Expr> {
        let ex: Vec<f64> = exps.iter().map(numeric::value).collect();
        let (lc, sd) = match fitted_coef {
            Some(x) => x,
            None => {
                let r: Vec<f64> = (0..n)
                    .map(|i| {
                        ly[i]
                            - (0..use_.len())
                                .map(|k| ex[k] * t.x[i][use_[k]].ln())
                                .sum::<f64>()
                    })
                    .collect();
                let sw: f64 = lw.iter().sum();
                let m = (0..n).map(|i| lw[i] * r[i]).sum::<f64>() / sw;
                let v = (0..n).map(|i| lw[i] * (r[i] - m).powi(2)).sum::<f64>() / sw;
                let sd = if t.sigma.is_some() {
                    (1.0 / sw)
                        .sqrt()
                        .max((v / (n as f64 - 1.0).max(1.0)).sqrt())
                } else {
                    (v / (n as f64 - 1.0).max(1.0)).sqrt()
                };
                (m, sd)
            }
        };
        let cval = sign * lc.exp();
        let (ce, _) = recognize(cval, cval.abs() * sd);
        let mut f = vec![ce];
        for (k, e) in exps.iter().enumerate() {
            if !e.is_zero() {
                f.push(pow(Expr::sym(&t.inputs[use_[k]]), e.clone()));
            }
        }
        Some(mul(f))
    };
    for cmb in combos.iter().take(27) {
        if let Some(f) = build(cmb, None) {
            let err = t.misfit(&t.predict(&f), 1);
            let cv = err * (n as f64 / (n - 1) as f64).sqrt();
            out.push(Cand {
                complexity: complexity(&f),
                formula: f,
                err,
                cv,
                params: 1,
            });
        }
    }
    // the free fit, exponents as decimals
    let free: Vec<Expr> = (0..use_.len())
        .map(|k| Expr::float(round_sig(c[k + 1], sds[k + 1])))
        .collect();
    if let Some(f) = build(&free, None) {
        let err = t.misfit(&t.predict(&f), p);
        let cv = err * (n as f64 / (n - p).max(1) as f64).sqrt();
        out.push(Cand {
            complexity: complexity(&f),
            formula: f,
            err,
            cv,
            params: p,
        });
    }
}

/// u, d(u, t) or d(u, t, 2)
fn dname(u: &str, t: &str, order: usize) -> String {
    match order {
        0 => u.to_string(),
        1 => format!("d({}, {})", u, t),
        k => format!("d({}, {}, {})", u, t, k),
    }
}

/// u, d(u, t, 2) shown as u, u'' in formulas
fn md_name(t: &str) -> String {
    if let Some(inner) = t.strip_prefix("d(").and_then(|r| r.strip_suffix(')')) {
        let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
        let k = parts.get(2).and_then(|k| k.parse::<usize>().ok()).unwrap_or(1);
        return format!("{}{}", lname(parts[0]), "'".repeat(k));
    }
    lname(t)
}

impl Session {
    pub(crate) fn discover_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "discover" => self.discover(args),
            "check" => self.check(args),
            "scatter" => self.scatter(args),
            "odedata" => self.odedata(args),
            "column" => self.column(args),
            "transform" => self.transform(args),
            "best" => match args {
                [d] => match self.eval(d) {
                    Ok(Expr::Obj(o)) => match &*o {
                        Obj::Report(r) if !r.formulas.is_empty() => Ok(r.formulas[r.best].clone()),
                        _ => Err("best(d) needs the result of discover".into()),
                    },
                    Ok(_) => Err("best(d) needs the result of discover".into()),
                    Err(e) => Err(e),
                },
                _ => Err("usage: best(d) gives the formula discover marked as best".into()),
            },
            _ => return None,
        };
        Some(r)
    }

    /// is this argument a list of numbers with one entry per row (uncertainties)?
    fn is_sigma(&mut self, n: &Node, rows: usize) -> bool {
        match self.eval(n) {
            Ok(e) => {
                let v: Vec<Expr> = match e {
                    Expr::Matrix(m) => m.into_iter().flatten().collect(),
                    other => vec![other],
                };
                rows > 1 && v.len() == rows && v.iter().all(|x| numeric::value(x).is_finite())
            }
            Err(_) => false,
        }
    }

    fn data_rows(&mut self, data: &Node) -> usize {
        match self.eval(data) {
            Ok(Expr::Matrix(m)) => m.len(),
            _ => 0,
        }
    }

    fn discover(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: discover(data, [x y]) finds y as a formula in x; discover(data, [x y], [term1 term2 ...]) searches sums of the given terms; add a last argument with one uncertainty per row to weigh the data";
        if args.len() < 2 || args.len() > 4 {
            return Err(usage.into());
        }
        let rows = self.data_rows(&args[0]);
        let (mut lib_node, mut sig_node) = (None, None);
        for a in &args[2..] {
            if sig_node.is_none() && self.is_sigma(a, rows) {
                sig_node = Some(a);
            } else if lib_node.is_none() {
                lib_node = Some(a);
            } else {
                return Err(usage.into());
            }
        }
        let t = self.data_table(&args[0], &args[1], sig_node, "discover")?;
        let mut cands = vec![];
        let mut notes = vec![];
        let user_lib = lib_node.is_some();
        let lib = match lib_node {
            Some(l) => {
                let e = self.eval_hidden(l, &t.inputs)?;
                let v: Vec<Expr> = match e {
                    Expr::Matrix(m) => m.into_iter().flatten().collect(),
                    other => vec![other],
                };
                let nm: Vec<&str> = t.inputs.iter().map(|s| s.as_str()).collect();
                for term in &v {
                    let free = numeric::free(term, &nm);
                    if !free.is_empty() {
                        return Err(format!(
                            "discover: the term {} uses {}, which is not a column; terms may use {}",
                            show(term),
                            free.join(", "),
                            t.inputs.join(", ")
                        ));
                    }
                }
                v
            }
            None => default_library(&t),
        };
        let (terms, cols) = term_columns(&t, lib);
        if terms.is_empty() {
            return Err("discover: none of the terms can be evaluated on all of the data".into());
        }
        let kmax = if user_lib && terms.len() <= 10 {
            4
        } else if terms.len() <= 24 {
            3
        } else {
            2
        };
        linear_candidates(&t, &terms, &cols, kmax, &mut cands);
        if !user_lib {
            power_candidates(&t, &mut cands, &mut notes);
        }
        cands.retain(|c| c.err.is_finite());
        if cands.is_empty() {
            return Err("discover: no candidate formula could be fitted to the data".into());
        }
        // Pareto front: each simpler formula must be beaten on error by a more complex one
        cands.sort_by(|a, b| {
            a.complexity
                .cmp(&b.complexity)
                .then(a.err.total_cmp(&b.err))
        });
        let mut seen: Vec<String> = vec![];
        let mut front: Vec<Cand> = vec![];
        for c in cands {
            let key = show(&c.formula);
            if seen.contains(&key) {
                continue;
            }
            seen.push(key);
            if front.last().is_none_or(|l| c.err < l.err * 0.98 - 1e-15) {
                front.push(c);
            }
        }
        front.truncate(10);
        // only formulas with at least two observations per fitted constant can be "best"
        let n_obs = t.y.len();
        let allowed = |c: &Cand| 2 * c.params < n_obs || c.params <= 1;
        let emin = front
            .iter()
            .filter(|c| allowed(c))
            .map(|c| c.err)
            .fold(f64::INFINITY, f64::min);
        let cvmin = front
            .iter()
            .filter(|c| allowed(c))
            .map(|c| c.cv)
            .fold(f64::INFINITY, f64::min);
        let ok = |c: &Cand| {
            allowed(c)
                && if t.sigma.is_some() {
                    c.err <= (1.5 * emin).max(emin + 1.0)
                } else {
                    c.cv <= (2.0 * cvmin).max(1e-7)
                }
        };
        let best = front.iter().position(ok).unwrap_or(0);

        // text
        let n = t.y.len();
        let mut tx = vec![format!(
            "discover: {} in terms of {}, {} observations{}",
            t.target,
            t.inputs.join(", "),
            n,
            if t.order > 0 {
                format!(" ({} estimated from neighbouring rows)", t.target)
            } else {
                String::new()
            }
        )];
        tx.push(format!(
            "  #  complexity  {:>10}   formula",
            t.misfit_name()
        ));
        for (i, c) in front.iter().enumerate() {
            tx.push(format!(
                "{}{:>2}  {:>10}  {:>10}   {} = {}",
                if i == best { "*" } else { " " },
                i,
                c.complexity,
                t.fmt_misfit(c.err),
                t.target,
                show(&c.formula)
            ));
        }
        let rule = if t.sigma.is_some() {
            "  * best: the simplest formula whose chi2/dof is close to the smallest one (at least two observations per fitted constant)".to_string()
        } else {
            "  * best: the simplest formula that predicts left-out rows at most twice as badly as the best one (leave-one-out test, at least two observations per fitted constant)".to_string()
        };
        tx.push(rule.clone());
        for nt in &notes {
            tx.push(format!("  {}", nt));
        }
        // markdown
        let tn = md_name(&t.target);
        let mut md = vec![format!(
            "**discover**: ${}$ in terms of {}, {} observations{}\n",
            tn,
            t.inputs
                .iter()
                .map(|s| format!("${}$", lname(s)))
                .collect::<Vec<_>>()
                .join(", "),
            n,
            if t.order > 0 {
                format!(" (${}$ estimated from neighbouring rows)", tn)
            } else {
                String::new()
            }
        )];
        md.push(format!(
            "| # | complexity | {} | formula |\n|---|---|---|---|",
            t.misfit_name()
        ));
        for (i, c) in front.iter().enumerate() {
            let f = format!("${} = {}$", tn, latex(&c.formula));
            md.push(format!(
                "| {} | {} | {} | {} |",
                if i == best {
                    format!("**{}** ★", i)
                } else {
                    i.to_string()
                },
                c.complexity,
                t.fmt_misfit(c.err),
                if i == best { format!("**{}**", f) } else { f }
            ));
        }
        md.push(format!("\n★ {}", &rule[4..]));
        for nt in &notes {
            md.push(format!("\n{}", nt));
        }
        Ok(report(Report {
            kind: "Discovery",
            text: tx.join("\n"),
            markdown: md.join("\n"),
            formulas: front.into_iter().map(|c| c.formula).collect(),
            best,
            vars: t.names.clone(),
            order: t.order,
            target: t.target_col,
            value: None,
        }))
    }

    // ---------- check ----------

    fn check(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: check(law, data, [x y]) or check(law, data, [x y], sigma); law is a formula for the last variable (free constants are fitted), a list of formulas to compare, or the result of discover";
        if args.len() < 2 || args.len() > 4 {
            return Err(usage.into());
        }
        // a discover result carries its own variable names
        let law0 = self.eval_hidden(&args[0], &[]).ok();
        let from_report = match &law0 {
            Some(Expr::Obj(o)) => match &**o {
                Obj::Report(r) if !r.formulas.is_empty() => Some(r.clone()),
                _ => None,
            },
            _ => None,
        };
        let rows = self.data_rows(&args[1]);
        let (vars_node, sig_node): (Node, Option<&Node>) = match (&from_report, args.len()) {
            (Some(r), 2) => (vars_node_of(r), None),
            (Some(r), 3) if self.is_sigma(&args[2], rows) => (vars_node_of(r), Some(&args[2])),
            (_, 3) => (args[2].clone(), None),
            (_, 4) => (args[2].clone(), Some(&args[3])),
            _ => return Err(usage.into()),
        };
        let t = self.data_table(&args[1], &vars_node, sig_node, "check")?;
        let laws: Vec<Expr> = match &from_report {
            Some(r) => vec![r.formulas[r.best].clone()],
            None => match self.eval_hidden(&args[0], &t.inputs)? {
                Expr::Matrix(m) => m.into_iter().flatten().collect(),
                e => vec![e],
            },
        };
        let mut results = vec![];
        for law in &laws {
            results.push(fit_law(&t, law)?);
        }
        let tn = md_name(&t.target);
        let n = t.y.len();
        let mut tx = vec![];
        let mut md = vec![];
        if results.len() == 1 {
            let r = &results[0];
            tx.push(format!(
                "check: {} = {} against {} observations",
                t.target,
                show(&r.formula),
                n
            ));
            md.push(format!(
                "**check**: ${} = {}$ against {} observations\n",
                tn,
                latex(&r.formula),
                n
            ));
            for (nm, v, sd) in &r.params {
                tx.push(format!("  fitted {} = {} ± {}", nm, fmt(*v), fmt(*sd)));
                md.push(format!(
                    "fitted ${} = {} \\pm {}$\n",
                    lname(nm),
                    fmt(*v),
                    fmt(*sd)
                ));
            }
            let diff_head = if t.sigma.is_some() {
                "off by"
            } else {
                "difference"
            };
            let mut head: Vec<String> = t.inputs.clone();
            head.push(format!("{} observed", t.target));
            head.push("predicted".into());
            head.push(diff_head.into());
            tx.push(format!(
                "  {}",
                head.iter()
                    .map(|h| format!("{:>12}", h))
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
            let mdh: Vec<String> = t
                .inputs
                .iter()
                .map(|s| format!("${}$", lname(s)))
                .chain([
                    format!("${}$ observed", tn),
                    "predicted".into(),
                    diff_head.into(),
                ])
                .collect();
            md.push(format!(
                "| {} |\n|{}",
                mdh.join(" | "),
                "---|".repeat(mdh.len())
            ));
            let shown_rows = if n > 30 { 30 } else { n };
            for i in 0..shown_rows {
                let d = match &t.sigma {
                    Some(s) => format!("{:+.1}σ", (t.y[i] - r.pred[i]) / s[i]),
                    None if t.relative() => {
                        format!("{:+}", pct((t.y[i] - r.pred[i]) / t.y[i])).replace("+-", "-")
                    }
                    None => fmt(t.y[i] - r.pred[i]),
                };
                let mut cells: Vec<String> = t.x[i].iter().map(|v| fmt(*v)).collect();
                cells.push(fmt(t.y[i]));
                cells.push(fmt(r.pred[i]));
                cells.push(d);
                tx.push(format!(
                    "  {}",
                    cells
                        .iter()
                        .map(|h| format!("{:>12}", h))
                        .collect::<Vec<_>>()
                        .join(" ")
                ));
                md.push(format!("| {} |", cells.join(" | ")));
            }
            if n > shown_rows {
                tx.push(format!("  ... {} more rows", n - shown_rows));
                md.push(format!("\n... {} more rows", n - shown_rows));
            }
            tx.push(format!("  {}", r.verdict));
            md.push(format!("\n{}", r.verdict));
        } else {
            tx.push(format!(
                "check: {} formulas for {} against {} observations",
                results.len(),
                t.target,
                n
            ));
            md.push(format!(
                "**check**: {} formulas for ${}$ against {} observations\n",
                results.len(),
                tn,
                n
            ));
            tx.push(format!(
                "  {:<36} {:>10}  verdict",
                "formula",
                t.misfit_name()
            ));
            md.push(format!(
                "| formula | fitted constants | {} | verdict |\n|---|---|---|---|",
                t.misfit_name()
            ));
            for r in &results {
                let params: Vec<String> = r
                    .params
                    .iter()
                    .map(|(nm, v, sd)| format!("{} = {} ± {}", nm, fmt(*v), fmt(*sd)))
                    .collect();
                tx.push(format!(
                    "  {:<36} {:>10}  {}{}",
                    format!("{} = {}", t.target, show(&r.formula)),
                    t.fmt_misfit(r.err),
                    r.short,
                    if params.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", params.join(", "))
                    }
                ));
                let mparams: Vec<String> = r
                    .params
                    .iter()
                    .map(|(nm, v, sd)| format!("${} = {} \\pm {}$", lname(nm), fmt(*v), fmt(*sd)))
                    .collect();
                md.push(format!(
                    "| ${} = {}$ | {} | {} | {} |",
                    tn,
                    latex(&r.formula),
                    if mparams.is_empty() {
                        "none".into()
                    } else {
                        mparams.join(", ")
                    },
                    t.fmt_misfit(r.err),
                    r.short
                ));
            }
        }
        Ok(report(Report {
            kind: "Check",
            text: tx.join("\n"),
            markdown: md.join("\n"),
            formulas: results.iter().map(|r| r.fitted.clone()).collect(),
            best: 0,
            vars: t.names.clone(),
            order: t.order,
            target: t.target_col,
            value: None,
        }))
    }
}

fn vars_node_of(r: &Report) -> Node {
    let items: Vec<Node> = r
        .vars
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let mut l = Node::Ident(s.clone());
            if i == r.target {
                for _ in 0..r.order {
                    l = Node::Transpose(Box::new(l));
                }
            }
            l
        })
        .collect();
    Node::Matrix(vec![items])
}

struct Fit {
    formula: Expr,
    fitted: Expr,
    params: Vec<(String, f64, f64)>,
    pred: Vec<f64>,
    err: f64,
    verdict: String,
    short: String,
}

/// Fit the free constants of a law (Levenberg-Marquardt) and judge the result
fn fit_law(t: &Table, law: &Expr) -> R<Fit> {
    let nm: Vec<&str> = t.inputs.iter().map(|s| s.as_str()).collect();
    let pnames = numeric::free(law, &nm);
    let mut all: Vec<&str> = nm.clone();
    all.extend(pnames.iter().map(|s| s.as_str()));
    let code = compile(law, &all);
    let n = t.y.len();
    let w = t.weights();
    let k = pnames.len();
    if k >= n {
        return Err(format!(
            "check: {} has {} free constants ({}) but there are only {} observations",
            show(law),
            k,
            pnames.join(", "),
            n
        ));
    }
    let eval = |th: &[f64]| -> Vec<f64> {
        t.x.iter()
            .map(|r| {
                let mut a = r.clone();
                a.extend_from_slice(th);
                run(&code, &a)
            })
            .collect()
    };
    let cost = |p: &[f64]| -> f64 { (0..n).map(|i| w[i] * (t.y[i] - p[i]).powi(2)).sum() };
    let mut th = vec![1.0; k];
    let mut var = vec![0.0; k];
    if k > 0 {
        let mut lambda = 1e-3;
        let mut c = cost(&eval(&th));
        for _ in 0..300 {
            let p0 = eval(&th);
            // numeric Jacobian
            let mut jac = vec![vec![0.0; k]; n];
            for j in 0..k {
                let h = 1e-6 * th[j].abs().max(1e-6);
                let mut t2 = th.clone();
                t2[j] += h;
                let p1 = eval(&t2);
                for i in 0..n {
                    jac[i][j] = (p1[i] - p0[i]) / h;
                }
            }
            let r: Vec<f64> = (0..n).map(|i| t.y[i] - p0[i]).collect();
            // (J^T W J + lambda diag) d = J^T W r, via an augmented least squares problem
            let mut a = jac.clone();
            let mut yy = r.clone();
            let mut ww = w.clone();
            for j in 0..k {
                let d: f64 = (0..n)
                    .map(|i| w[i] * jac[i][j] * jac[i][j])
                    .sum::<f64>()
                    .max(1e-300);
                let mut row = vec![0.0; k];
                row[j] = 1.0;
                a.push(row);
                yy.push(0.0);
                ww.push(lambda * d);
            }
            let Some((d, _)) = wls(&a, &yy, &ww) else {
                break;
            };
            let cand: Vec<f64> = th.iter().zip(&d).map(|(a, b)| a + b).collect();
            let cc = cost(&eval(&cand));
            if cc.is_finite() && cc < c {
                let done = (c - cc) <= 1e-15 * c.max(1e-300);
                th = cand;
                c = cc;
                lambda = (lambda / 10.0).max(1e-12);
                if done {
                    break;
                }
            } else {
                lambda *= 10.0;
                if lambda > 1e12 {
                    break;
                }
            }
        }
        // uncertainties from the final Jacobian
        let p0 = eval(&th);
        let mut jac = vec![vec![0.0; k]; n];
        for j in 0..k {
            let h = 1e-6 * th[j].abs().max(1e-6);
            let mut t2 = th.clone();
            t2[j] += h;
            let p1 = eval(&t2);
            for i in 0..n {
                jac[i][j] = (p1[i] - p0[i]) / h;
            }
        }
        if let Some((_, v)) = wls(&jac, &vec![0.0; n], &w) {
            let s2 = c / (n - k).max(1) as f64;
            let s2 = if t.sigma.is_some() { s2.max(1.0) } else { s2 };
            var = v.iter().map(|x| x * s2).collect();
        }
    }
    let pred = eval(&th);
    if pred.iter().any(|p| !p.is_finite()) {
        return Err(format!(
            "check: {} can't be evaluated on all of the data",
            show(law)
        ));
    }
    let err = t.misfit(&pred, k);
    let params: Vec<(String, f64, f64)> = pnames
        .iter()
        .enumerate()
        .map(|(j, s)| (s.clone(), th[j], var[j].sqrt()))
        .collect();
    let mut fitted = law.clone();
    for (s, v, sd) in &params {
        fitted = calc::subst(&fitted, s, &Expr::float(round_sig(*v, *sd)));
    }
    let (verdict, short) = match &t.sigma {
        Some(s) => {
            let (worst, z) = (0..n)
                .map(|i| (i, ((t.y[i] - pred[i]) / s[i]).abs()))
                .max_by(|a, b| a.1.total_cmp(&b.1))
                .unwrap();
            if err <= 3.0 && z < 3.0 {
                (
                    format!(
                        "chi2/dof = {:.2}, worst point {:.1}σ off (row {}): consistent with the data",
                        err,
                        z,
                        worst + 1
                    ),
                    "consistent".to_string(),
                )
            } else {
                (
                    format!(
                        "chi2/dof = {:.2}, worst point {:.1}σ off (row {}): NOT consistent with the data",
                        err,
                        z,
                        worst + 1
                    ),
                    format!("ruled out (row {} is {:.1}σ off)", worst + 1, z),
                )
            }
        }
        None => {
            let rel = t.relative();
            let scale = t.y.iter().map(|v| v.abs()).fold(0.0, f64::max);
            let (worst, d) = (0..n)
                .map(|i| {
                    (
                        i,
                        if rel {
                            ((t.y[i] - pred[i]) / t.y[i]).abs()
                        } else {
                            (t.y[i] - pred[i]).abs() / scale
                        },
                    )
                })
                .max_by(|a, b| a.1.total_cmp(&b.1))
                .unwrap();
            let what = if rel {
                "difference"
            } else {
                "difference (relative to the largest value)"
            };
            (
                format!(
                    "largest {} {} (row {}); typical {}",
                    what,
                    pct(d),
                    worst + 1,
                    pct(err)
                ),
                format!("within {}", pct(d)),
            )
        }
    };
    Ok(Fit {
        formula: law.clone(),
        fitted,
        params,
        pred,
        err,
        verdict,
        short,
    })
}

// ---------- scatter, odedata, column ----------

impl Session {
    fn scatter(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: scatter(data, [x y]) plots the two columns as points; scatter(data, [names...], [X Y]) plots expressions X, Y of the columns; add uncertainties of the last column as a final argument for error bars";
        if args.len() < 2 || args.len() > 4 {
            return Err(usage.into());
        }
        if args.len() == 2 {
            if let Some((xs, ys)) = self.two_lists(&args[0], &args[1], "scatter")? {
                self.note(format!("scatter: the {} points (x, y) of the two lists.", xs.len()));
                let s = Series { label: "data".into(), points: xs.into_iter().zip(ys).collect(), markers: true, ..Default::default() };
                return Ok(Expr::Obj(std::rc::Rc::new(Obj::Figure(Figure::new(vec![s], "", "", false)))));
            }
        }
        let (names, _, _) = var_specs(&args[1])?;
        let d = self.eval(&args[0])?;
        let rows = numbers_of(&d, "scatter: the data")?;
        if rows.first().map_or(0, |r| r.len()) != names.len() {
            return Err(format!(
                "scatter: give one name per column of the data ({} columns)",
                rows.first().map_or(0, |r| r.len())
            ));
        }
        let (mut map, mut sig) = (None, None);
        for a in &args[2..] {
            if sig.is_none() && self.is_sigma(a, rows.len()) {
                sig = Some(a);
            } else if map.is_none() {
                map = Some(a);
            } else {
                return Err(usage.into());
            }
        }
        let ms: Vec<Expr> = match map {
            Some(m) => match self.eval_hidden(m, &names)? {
                Expr::Matrix(v) => v.into_iter().flatten().collect(),
                other => vec![other],
            },
            None if names.len() == 2 => names.iter().map(|s| Expr::sym(s)).collect(),
            None => return Err("scatter: with more than two columns, say what to plot, e.g. scatter(data, [a e T], [a T])".into()),
        };
        if ms.len() != 2 {
            return Err("scatter: the plot map must have two entries [X Y]".into());
        }
        let sigma: Option<Vec<f64>> = match sig {
            Some(s) => Some(
                numbers_of(&self.eval(s)?, "scatter: the uncertainties")?
                    .into_iter()
                    .flatten()
                    .collect(),
            ),
            None => None,
        };
        let nm: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        let free: Vec<String> = ms.iter().flat_map(|m| numeric::free(m, &nm)).collect();
        if !free.is_empty() {
            return Err(format!(
                "scatter: {} is not a column; the columns are {}",
                free[0],
                names.join(", ")
            ));
        }
        let (cx, cy) = (compile(&ms[0], &nm), compile(&ms[1], &nm));
        let last = names.len() - 1;
        let mut pts = vec![];
        let mut errs = vec![];
        for (i, r) in rows.iter().enumerate() {
            pts.push((run(&cx, r), run(&cy, r)));
            if let Some(s) = &sigma {
                // propagate the uncertainty of the last column through Y
                let (mut up, mut dn) = (r.clone(), r.clone());
                up[last] += s[i];
                dn[last] -= s[i];
                let e = ((run(&cy, &up) - run(&cy, &dn)) / 2.0).abs();
                errs.push(if e.is_finite() { e } else { 0.0 });
            }
        }
        let series = Series {
            label: "data".into(),
            points: pts,
            markers: true,
            errors: errs,
            ..Default::default()
        };
        // x = r cos, y = r sin: a picture of positions, same scale on both axes
        let polar = show(&ms[0]).contains("cos(") && show(&ms[1]).contains("sin(");
        Ok(Expr::Obj(Rc::new(Obj::Figure(Figure::new(
            vec![series],
            &show(&ms[0]),
            &show(&ms[1]),
            polar,
        )))))
    }

    /// odedata([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1, n): a table of n+1 rows [t y1 y2]
    fn odedata(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: odedata([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1, n) gives a table with n+1 rows [t y1 y2]";
        let [f, ys, t, y0, t0, t1, n] = args else {
            return Err(usage.into());
        };
        let vars: Vec<String> = match ys {
            Node::Matrix(rows) => rows
                .iter()
                .flatten()
                .map(|x| var_spec(x).map(|v| v.0))
                .collect::<R<_>>()?,
            other => vec![var_spec(other)?.0],
        };
        let tv = var_spec(t)?.0;
        let mut all = vars.clone();
        all.push(tv);
        let rhs: Vec<Expr> = match self.eval_hidden(f, &all)? {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            e => vec![e],
        };
        let start: Vec<f64> = numbers_of(&self.eval(y0)?, "odedata: the start values")?
            .into_iter()
            .flatten()
            .collect();
        if rhs.len() != vars.len() || start.len() != vars.len() {
            return Err("odedata: give one equation and one start value per unknown".into());
        }
        let num = |s: &mut Session, n: &Node, w: &str| -> R<f64> {
            let v = numeric::value(&s.eval(n)?);
            if v.is_finite() {
                Ok(v)
            } else {
                Err(format!("odedata: {} must be a number", w))
            }
        };
        let (a, b) = (num(self, t0, "t0")?, num(self, t1, "t1")?);
        let k = num(self, n, "n")?;
        if !(1.0..=100000.0).contains(&k) {
            return Err("odedata: n must be between 1 and 100000".into());
        }
        let k = k as usize;
        let nm: Vec<&str> = all.iter().map(|s| s.as_str()).collect();
        let free: Vec<String> = rhs.iter().flat_map(|r| numeric::free(r, &nm)).collect();
        if !free.is_empty() {
            return Err(format!("odedata: give {} a value first", free[0]));
        }
        let fs: Vec<numeric::Code> = rhs.iter().map(|r| compile(r, &nm)).collect();
        let func = |tt: f64, y: &[f64]| -> Vec<f64> {
            let mut a = y.to_vec();
            a.push(tt);
            fs.iter().map(|g| run(g, &a)).collect()
        };
        let sub = (crate::ode::DEFAULT_STEPS / k).max(20);
        let traj = crate::ode::rk4(&func, &start, a, b, k * sub);
        let mut out = vec![];
        for (i, (tt, y)) in traj.iter().enumerate() {
            if i % sub == 0 {
                let mut row = vec![Expr::float(*tt)];
                row.extend(y.iter().map(|v| Expr::float(*v)));
                out.push(row);
            }
        }
        if out.len() < k + 1 {
            return Err(format!(
                "odedata: the solution breaks down at t = {}",
                fmt(traj.last().map_or(a, |p| p.0))
            ));
        }
        Ok(Expr::Matrix(out))
    }

    /// transform(data, [a T], [a/149.6 T^2]): a new table, one column per expression of the named columns
    fn transform(&mut self, args: &[Node]) -> R<Expr> {
        let [d, vars, exprs] = args else {
            return Err("usage: transform(data, [names], [expr1 expr2 ...]) makes a new table whose columns are the expressions, computed row by row".into());
        };
        let (names, _, _) = var_specs(vars)?;
        let rows = numbers_of(&self.eval(d)?, "transform: the data")?;
        if rows.first().map_or(0, |r| r.len()) != names.len() {
            return Err(format!(
                "transform: give one name per column of the data ({} columns)",
                rows.first().map_or(0, |r| r.len())
            ));
        }
        let es: Vec<Expr> = match self.eval_hidden(exprs, &names)? {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            e => vec![e],
        };
        let nm: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        for e in &es {
            let free = numeric::free(e, &nm);
            if !free.is_empty() {
                return Err(format!(
                    "transform: {} is not a column; the columns are {}",
                    free[0],
                    names.join(", ")
                ));
            }
        }
        let codes: Vec<numeric::Code> = es.iter().map(|e| compile(e, &nm)).collect();
        let out: Vec<Vec<Expr>> = rows
            .iter()
            .map(|r| codes.iter().map(|c| Expr::float(run(c, r))).collect())
            .collect();
        Ok(Expr::Matrix(out))
    }

    /// column(data, k): the k-th column (counting from 1) as a column vector
    fn column(&mut self, args: &[Node]) -> R<Expr> {
        let [d, k] = args else {
            return Err("usage: column(data, k) gives column k (counting from 1)".into());
        };
        let m = match self.eval(d)? {
            Expr::Matrix(m) => m,
            _ => return Err("column: the first argument must be a matrix".into()),
        };
        let kv = self.eval(k)?;
        let k = kv
            .num()
            .and_then(|n| n.as_int())
            .ok_or("column: k must be a whole number")?;
        let ncol = m.first().map_or(0, |r| r.len()) as i128;
        if k < 1 || k > ncol {
            return Err(format!("column: k must be between 1 and {}", ncol));
        }
        Ok(Expr::Matrix(
            m.iter()
                .map(|r| vec![r[(k - 1) as usize].clone()])
                .collect(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::Session;

    /// values, with a report replaced by its plain text
    fn run(src: &str) -> String {
        run_in(&mut Session::tidy(), src)
    }

    fn run_in(s: &mut Session, src: &str) -> String {
        s.run(src)
            .into_iter()
            .map(|r| match r {
                Ok(crate::eval::Output::Value(crate::expr::Expr::Obj(o))) => match &*o {
                    crate::expr::Obj::Report(r) => r.text.clone(),
                    _ => o.text(),
                },
                Ok(o) => o.render(),
                Err(e) => format!("Error: {}", e),
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn kepler_third_law() {
        let out = run(
            "d = [0.387 0.2408; 0.723 0.6152; 1 1; 1.524 1.881; 5.203 11.86; 9.537 29.46; 19.19 84.01; 30.07 164.8]\ndiscover(d, [a T])",
        );
        let best = out.lines().find(|l| l.starts_with('*')).unwrap();
        assert!(best.contains("T = a^(3/2)"), "{}", out);
    }

    #[test]
    fn inverse_square_with_four_pi_squared() {
        let mut s = Session::tidy();
        s.run_to_string("a = [0.387; 0.723; 1; 1.524; 5.203; 9.537]; T = [0.2408; 0.6152; 1; 1.881; 11.86; 29.46]");
        // centripetal acceleration 4 pi^2 a / T^2 of exact Kepler data
        let out = run_in(
            &mut s,
            "d = [1 39.4784176; 2 9.8696044; 4 2.4674011; 0.5 157.9136704]\ndiscover(d, [r g])",
        );
        let best = out.lines().find(|l| l.starts_with('*')).unwrap();
        assert!(
            best.contains("4*pi^2*r^-2") || best.contains("4*pi^2/r^2"),
            "{}",
            out
        );
    }

    #[test]
    fn sparse_ode() {
        let out = run(
            "tab = odedata([v, 1 - u + 0.01*u^2], [u v], phi, [1.5 0], 0, 12, 400);\ndiscover(tab, [phi d(u, phi, 2) v], [1 u u^2 u^3])",
        );
        let best = out.lines().find(|l| l.starts_with('*')).unwrap();
        assert!(best.contains("u^2-u+1"), "{}", out);
    }

    #[test]
    fn check_compares_laws() {
        let out = run(
            "d = [0.3707 0.1038; 0.7232 0.0517; 0.9997 0.050; 0.3409 0.1098]; s = [0.0011; 0.0295; 0.012; 0.009];\ncheck([k/p, k/p^2, k], d, [p w], s)",
        );
        assert!(
            out.contains("consistent") && out.contains("ruled out"),
            "{}",
            out
        );
    }
}
