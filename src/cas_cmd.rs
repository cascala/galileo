//! Commands for the symbolic tools in [`crate::cas`], integer functions,
//! more matrix functions, and plot labels.

use crate::build::*;
use crate::calc::{self, simplify};
use crate::cas::{self, Point, Side};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::linalg;
use crate::num::Number;
use crate::numeric;
use crate::parser::Node;
use crate::print::show;
use num_bigint::BigInt;
use num_traits::{Signed, Zero};
use std::rc::Rc;

fn name(n: &Node, what: &str) -> R<String> {
    match n {
        Node::Ident(s) => Ok(s.clone()),
        _ => Err(format!("{} must be a variable name", what)),
    }
}

fn names(n: &Node, what: &str) -> R<Vec<String>> {
    match n {
        Node::Matrix(rows) => rows.iter().flatten().map(|x| name(x, what)).collect(),
        Node::Ident(s) => Ok(vec![s.clone()]),
        _ => Err(format!("{} must be a list of names such as [x y]", what)),
    }
}

fn row(v: Vec<Expr>) -> Expr {
    if v.len() == 1 {
        v.into_iter().next().unwrap()
    } else {
        Expr::Matrix(vec![v])
    }
}

fn is_inf(e: &Expr) -> i32 {
    match e {
        Expr::Sym(s) if s == "inf" || s == "Inf" => 1,
        Expr::Mul(f)
            if f.len() == 2
                && f[0].num().is_some_and(|n| n.is_minus_one())
                && is_inf(&f[1]) == 1 =>
        {
            -1
        }
        Expr::Num(n) if !n.is_exact() && n.to_f64().is_infinite() => {
            if n.to_f64() > 0.0 {
                1
            } else {
                -1
            }
        }
        _ => 0,
    }
}

fn point(e: &Expr, side: Side) -> Point {
    match is_inf(e) {
        1 => Point::PosInf,
        -1 => Point::NegInf,
        _ => Point::At(e.clone(), side),
    }
}

fn integer(e: &Expr, f: &str) -> R<i128> {
    if let Some(n) = exact_int(e) {
        return Ok(n);
    }
    let orig = e;
    let e = calc::evalf(e);
    let v = e
        .num()
        .map(|n| n.to_f64())
        .ok_or_else(|| crate::extra_cmd::not_whole(orig, f))?;
    if v.fract() != 0.0 || v.abs() > 1e30 {
        return Err(crate::extra_cmd::not_whole(orig, f));
    }
    Ok(v as i128)
}

fn big_integer(e: &Expr, f: &str) -> R<BigInt> {
    match exact_big(e) {
        Some(n) => Ok(n),
        None => integer(e, f).map(BigInt::from),
    }
}

fn modulo(va: &Expr, vb: &Expr) -> R<Expr> {
    if let (Some(x), Some(y)) = (exact_int(va), exact_int(vb)) {
        if y == 0 {
            return Err("mod: division by zero".into());
        }
        return Ok(Expr::int(x.rem_euclid(y)));
    }
    if let (Some(x), Some(y)) = (exact_big(va), exact_big(vb)) {
        if y.is_zero() {
            return Err("mod: division by zero".into());
        }
        let r = x % &y;
        let r = if r.is_negative() { r + y.abs() } else { r };
        return Ok(crate::extra_cmd::bnum(r));
    }
    for v in [va, vb] {
        if crate::extra_cmd::is_huge(v) {
            return Err(crate::extra_cmd::not_whole(v, "mod"));
        }
    }
    let (x, y) = (real(va, "mod")?, real(vb, "mod")?);
    Ok(Expr::float(x - y * (x / y).floor()))
}

/// c/(a + b*sqrt(k)) -> c*(a - b*sqrt(k))/(a^2 - b^2*k), when that is simpler
pub(crate) fn rationalize_root(e: &Expr) -> Expr {
    let (n, d) = calc::num_den(e);
    let Expr::Add(terms) = &d else {
        return e.clone();
    };
    let is_root = |t: &Expr| format!("{:?}", t).contains("Pow") && show(t).contains("sqrt");
    if terms.len() != 2 || terms.iter().filter(|t| is_root(t)).count() != 1 {
        return e.clone();
    }
    let conj = add(terms
        .iter()
        .map(|t| {
            if is_root(t) {
                neg(t.clone())
            } else {
                t.clone()
            }
        })
        .collect());
    let top = calc::expand(&mul2(n, conj.clone()));
    let bottom = simplify(&calc::expand(&mul2(d.clone(), conj)));
    if bottom.num().is_none() {
        return e.clone();
    }
    let r = simplify(&calc::expand(&div(top, bottom)));
    if r.size() <= e.size() + 2 {
        r
    } else {
        e.clone()
    }
}

/// A real symmetric matrix of numbers that is better diagonalized numerically:
/// it has a decimal entry, or is too large for exact roots (5 or more rows).
/// Returns the eigenvalues in increasing order and unit eigenvectors as columns.
pub(crate) fn numeric_symmetric_eig(m: &linalg::Mat) -> Option<(Vec<f64>, Vec<Vec<f64>>)> {
    let n = m.len();
    if n == 0 || m.iter().any(|r| r.len() != n) {
        return None;
    }
    let nums: Vec<Number> = m.iter().flatten().map(|e| e.num()).collect::<Option<_>>()?;
    if !(n >= 5 || nums.iter().any(|x| !x.is_exact())) {
        return None;
    }
    let a: Vec<Vec<f64>> = m.iter().map(|r| r.iter().map(numeric::value).collect()).collect();
    if a.iter().flatten().any(|x| !x.is_finite()) {
        return None;
    }
    let scale = a.iter().flatten().fold(0.0f64, |s, x| s.max(x.abs())).max(1e-300);
    for i in 0..n {
        for j in 0..i {
            if (a[i][j] - a[j][i]).abs() > 1e-12 * scale {
                return None;
            }
        }
    }
    let (vals, vecs) = jacobi(a);
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by(|&i, &j| vals[i].total_cmp(&vals[j]));
    // 12 significant digits, and rounding noise below 1e-12 of the largest entry set to 0
    let round = |x: f64| format!("{:.11e}", x).parse().unwrap_or(x);
    let tidy = |x: f64| if x.abs() < 1e-12 * scale { 0.0 } else { round(x) };
    let values = order.iter().map(|&k| tidy(vals[k])).collect();
    // columns, each with its first clearly nonzero entry positive
    let mut cols: Vec<Vec<f64>> = order.iter().map(|&k| (0..n).map(|i| vecs[i][k]).collect()).collect();
    for c in &mut cols {
        if let Some(f) = c.iter().find(|x| x.abs() > 1e-9) {
            if *f < 0.0 {
                c.iter_mut().for_each(|x| *x = -*x);
            }
        }
        c.iter_mut().for_each(|x| *x = if x.abs() < 1e-12 { 0.0 } else { round(*x) });
    }
    let rows = (0..n).map(|i| cols.iter().map(|c| c[i]).collect()).collect();
    Some((values, rows))
}

/// Cyclic Jacobi rotations: eigenvalues and eigenvectors (columns) of a symmetric matrix
fn jacobi(mut a: Vec<Vec<f64>>) -> (Vec<f64>, Vec<Vec<f64>>) {
    let n = a.len();
    let mut v: Vec<Vec<f64>> = (0..n).map(|i| (0..n).map(|j| if i == j { 1.0 } else { 0.0 }).collect()).collect();
    for _sweep in 0..100 {
        let off: f64 = (0..n).flat_map(|i| (0..n).filter(move |&j| j != i).map(move |j| (i, j))).map(|(i, j)| a[i][j] * a[i][j]).sum();
        let diag: f64 = (0..n).map(|i| a[i][i] * a[i][i]).sum();
        if off <= 1e-30 * diag.max(1e-300) {
            break;
        }
        for p in 0..n {
            for q in p + 1..n {
                if a[p][q] == 0.0 {
                    continue;
                }
                let theta = (a[q][q] - a[p][p]) / (2.0 * a[p][q]);
                let t = theta.signum() / (theta.abs() + (theta * theta + 1.0).sqrt());
                let t = if theta == 0.0 { 1.0 } else { t };
                let c = 1.0 / (t * t + 1.0).sqrt();
                let s = t * c;
                for k in 0..n {
                    let (akp, akq) = (a[k][p], a[k][q]);
                    a[k][p] = c * akp - s * akq;
                    a[k][q] = s * akp + c * akq;
                }
                for k in 0..n {
                    let (apk, aqk) = (a[p][k], a[q][k]);
                    a[p][k] = c * apk - s * aqk;
                    a[q][k] = s * apk + c * aqk;
                }
                for k in 0..n {
                    let (vkp, vkq) = (v[k][p], v[k][q]);
                    v[k][p] = c * vkp - s * vkq;
                    v[k][q] = s * vkp + c * vkq;
                }
            }
        }
    }
    ((0..n).map(|i| a[i][i]).collect(), v)
}

/// Eigenvectors (columns) and the matching eigenvalues, from the null space
/// of A - λI for each distinct exact eigenvalue.
pub(crate) fn eigvec(m: &linalg::Mat) -> R<(linalg::Mat, Vec<Expr>)> {
    let (r, c) = linalg::dims(m);
    if r != c {
        return Err("eigvec needs a square matrix".into());
    }
    if let Some((vals, vecs)) = numeric_symmetric_eig(m) {
        let cols = vecs.iter().map(|r| r.iter().map(|&x| Expr::float(x)).collect()).collect();
        return Ok((cols, vals.into_iter().map(Expr::float).collect()));
    }
    let p = charpoly(m, "λ")?;
    let co = cas::poly_coeffs(&p, "λ").ok_or("eigvec: no characteristic polynomial")?;
    let Expr::Matrix(roots) = cas::poly_roots(&co).map(|v| Expr::Matrix(vec![v]))? else {
        unreachable!()
    };
    let mut distinct: Vec<Expr> = vec![];
    for l in roots.into_iter().flatten() {
        if !distinct
            .iter()
            .any(|d| simplify(&sub(d.clone(), l.clone())).is_zero())
        {
            distinct.push(l);
        }
    }
    let mut cols: Vec<Vec<Expr>> = vec![];
    let mut vals = vec![];
    for l in distinct {
        if !l.num().map_or(true, |n| n.is_exact()) {
            return Err("eigvec: the eigenvalues are only known numerically".into());
        }
        let shifted: linalg::Mat = (0..r)
            .map(|i| {
                (0..r)
                    .map(|j| {
                        if i == j {
                            simplify(&sub(m[i][j].clone(), l.clone()))
                        } else {
                            m[i][j].clone()
                        }
                    })
                    .collect()
            })
            .collect();
        let n = linalg::nullspace(&shifted);
        for k in 0..linalg::dims(&n).1 {
            // scale so the entries are as simple as possible: last nonzero entry 1
            cols.push((0..r).map(|i| rationalize_root(&n[i][k])).collect());
            vals.push(l.clone());
        }
    }
    if cols.is_empty() {
        return Err("eigvec: no exact eigenvectors found".into());
    }
    Ok((linalg::transpose(&cols), vals))
}

fn exact_int(e: &Expr) -> Option<i128> {
    e.num().filter(|n| n.is_exact()).and_then(|n| n.as_int())
}

fn exact_big(e: &Expr) -> Option<BigInt> {
    e.num().and_then(|n| n.as_bigint())
}

/// floor, ceil or round, exact for exact numbers of any size
fn round_exact(v: &Expr, f: &str) -> Option<R<Expr>> {
    match v {
        Expr::Matrix(m) => {
            let mut out = vec![];
            for r in m {
                let mut o = vec![];
                for e in r {
                    o.push(round_exact(e, f)?);
                }
                out.push(o.into_iter().collect::<R<Vec<_>>>());
            }
            Some(out.into_iter().collect::<R<Vec<_>>>().map(Expr::Matrix))
        }
        Expr::Num(n) if n.is_exact() => {
            let r = n.big()?;
            let r = match f {
                "floor" => r.floor(),
                "ceil" => r.ceil(),
                _ => r.round(),
            };
            Some(Ok(Expr::Num(Number::from_big(r))))
        }
        _ => None,
    }
}

fn real(e: &Expr, f: &str) -> R<f64> {
    if is_inf(e) != 0 {
        return Ok(is_inf(e) as f64 * f64::INFINITY);
    }
    let v = numeric::value(e);
    if v.is_finite() {
        Ok(v)
    } else {
        Err(format!("{} needs a number, got {}", f, show(e)))
    }
}

fn label_text(n: &Node, s: &mut Session) -> R<String> {
    match n {
        Node::Str(t) => Ok(t.clone()),
        Node::Ident(t) => Ok(t.clone()),
        other => Ok(show(&s.eval(other)?)),
    }
}

impl Session {
    /// Some(result) if f is one of the commands here
    pub(crate) fn cas_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "integrate" | "int" => self.integrate(args),
            "nintegrate" => self.nintegrate(args),
            "limit" => self.limit(args),
            "taylor" | "series" => self.taylor(args),
            "solve" => self.solve(args),
            "fsolve" => self.fsolve(args),
            "sum" | "prod" | "product" => self.sum(f != "sum", args),
            "factorial" | "floor" | "ceil" | "round" | "numer" | "denom" | "trace" | "size"
            | "eig" | "rref" | "rank" | "null" | "eigvec" => self.unary(f, args),
            "binomial" | "mod" | "gcd" | "lcm" | "dot" | "cross" | "charpoly" | "degree" => {
                self.binary(f, args)
            }
            "coeff" => self.coeff(args),
            "polarplot" => self.polarplot(args),
            "title" | "xlabel" | "ylabel" | "zlabel" | "labels" | "legend" => self.labels(f, args),
            "window" => self.window(args),
            _ => return None,
        };
        Some(r)
    }

    fn integrate(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: integrate(f, x) or integrate(f, x, a, b)";
        if args.len() != 2 && args.len() != 4 {
            return Err(usage.into());
        }
        let x = name(&args[1], "integrate(f, x): x")?;
        let fx = self.eval_hidden(&args[0], std::slice::from_ref(&x))?;
        if let Expr::Matrix(m) = &fx {
            // element by element
            let mut out = vec![];
            for r in m {
                let mut o = vec![];
                for e in r {
                    o.push(self.integrate_one(e, &x, args)?);
                }
                out.push(o);
            }
            return Ok(Expr::Matrix(out));
        }
        self.integrate_one(&fx, &x, args)
    }

    fn integrate_one(&mut self, fx: &Expr, x: &str, args: &[Node]) -> R<Expr> {
        if args.len() == 2 {
            return cas::antiderivative(fx, x)
                .ok_or_else(|| format!("integrate: can't find an antiderivative of {}; for a number use integrate(f, {}, a, b)", show(fx), x));
        }
        let a = self.eval(&args[2])?;
        let b = self.eval(&args[3])?;
        let r = definite(fx, x, &a, &b, 0);
        // a complex integrand over a real interval: integrate the real and imaginary parts
        if r.is_err() && crate::extra_cmd::has_i(fx) && !crate::extra_cmd::has_i(&a) && !crate::extra_cmd::has_i(&b) {
            if let Ok((re, im)) = crate::extra_cmd::parts(fx) {
                if !crate::extra_cmd::has_i(&re) && !crate::extra_cmd::has_i(&im) {
                    let (p, q) = (definite(&re, x, &a, &b, 0), definite(&im, x, &a, &b, 0));
                    if let (Ok(p), Ok(q)) = (p, q) {
                        return Ok(calc::simplify(&add2(p, mul2(q, Expr::Const(crate::expr::Const::I)))));
                    }
                }
            }
        }
        r
    }

    fn nintegrate(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x, a, b] = args else {
            return Err("usage: nintegrate(f, x, a, b)".into());
        };
        let x = name(x, "nintegrate: x")?;
        let fx = self.eval_hidden(f, std::slice::from_ref(&x))?;
        let free = numeric::free(&fx, &[x.as_str()]);
        if !free.is_empty() {
            return Err(format!(
                "nintegrate: give values to {} first",
                free.join(", ")
            ));
        }
        let (a, b) = (self.eval(a)?, self.eval(b)?);
        let v = cas::nintegrate(&fx, &x, real(&a, "nintegrate")?, real(&b, "nintegrate")?)
            .map_err(|e| format!("nintegrate: {}", e))?;
        Ok(Expr::float(v))
    }

    fn limit(&mut self, args: &[Node]) -> R<Expr> {
        if args.len() != 3 && args.len() != 4 {
            return Err("usage: limit(f, x, a) or limit(f, x, a, left) / limit(f, x, a, right); a can be inf or -inf".into());
        }
        let x = name(&args[1], "limit(f, x, a): x")?;
        let fx = self.eval_hidden(&args[0], std::slice::from_ref(&x))?;
        let a = self.eval(&args[2])?;
        let side = match args.get(3) {
            None => Side::Both,
            Some(Node::Ident(s) | Node::Str(s)) if s == "left" || s == "-" => Side::Left,
            Some(Node::Ident(s) | Node::Str(s)) if s == "right" || s == "+" => Side::Right,
            Some(_) => return Err("limit: the fourth argument is left or right".into()),
        };
        cas::limit(&fx, &x, &point(&a, side))
    }

    fn taylor(&mut self, args: &[Node]) -> R<Expr> {
        let usage =
            "usage: taylor(f, x, a, n): the Taylor polynomial of f around x = a up to (x-a)^n";
        let (f, x, a, n) = match args {
            [f, x, a, n] => (f, x, Some(a), n),
            [f, x, n] => (f, x, None, n),
            _ => return Err(usage.into()),
        };
        let x = name(x, "taylor: x")?;
        let fx = self.eval_hidden(f, std::slice::from_ref(&x))?;
        let a = match a {
            Some(a) => self.eval(a)?,
            None => Expr::zero(),
        };
        let n = self.eval(n)?;
        let n = exact_int(&n)
            .filter(|k| (0..=40).contains(k))
            .ok_or("taylor: the order n must be a whole number from 0 to 40")?;
        cas::taylor(&fx, &x, &a, n as usize)
    }

    fn solve(&mut self, args: &[Node]) -> R<Expr> {
        if let [f, x, n] = args {
            return self.solve_mod(f, x, n);
        }
        let [f, x] = args else {
            return Err("usage: solve(f, x) solves f = 0; solve([f1 f2], [x y]) solves a linear system; A \\ b solves A*x = b".into());
        };
        let xs = names(x, "solve: the unknowns")?;
        let fx = self.eval_hidden(f, &xs)?;
        let eqs: Vec<Expr> = match fx {
            Expr::Matrix(m) => m
                .into_iter()
                .flatten()
                .map(|e| crate::eval::equation_lhs(&e))
                .collect(),
            e => vec![crate::eval::equation_lhs(&e)],
        };
        if eqs.len() == 1 && xs.len() == 1 {
            crate::radicals::begin();
            let r = cas::solve(&eqs[0], &xs[0]);
            let traces = crate::radicals::take();
            if r.is_ok() {
                if let Some(md) = crate::radicals::notes(&traces, &xs[0], self.teaching()) {
                    self.note2(crate::teach::plain(&md), md);
                }
            }
            if r.is_err() {
                // sin(x) = 1/2 and friends: the solutions in [0, 2 pi)
                if let Some(t) = crate::extra_cmd::solve_trig(&eqs[0], &xs[0]) {
                    return t.map(row);
                }
                if let Some(t) = crate::extra_cmd::solve_inverse(&eqs[0], &xs[0]) {
                    return t.map(row);
                }
            }
            return r.map(row);
        }
        match cas::solve_linear(&eqs, &xs) {
            Ok(v) => {
                let free: Vec<&String> = xs.iter().zip(&v).filter(|(x, e)| **e == Expr::sym(x)).map(|p| p.0).collect();
                if !free.is_empty() {
                    let names: Vec<&str> = free.iter().map(|s| s.as_str()).collect();
                    self.note(format!(
                        "Infinitely many solutions: {} {} free (any value works) and the other unknowns are given in terms of {}. Elimination left {} independent equation(s) for {} unknowns.",
                        names.join(", "),
                        if names.len() == 1 { "is" } else { "are" },
                        if names.len() == 1 { "it" } else { "them" },
                        xs.len() - names.len(),
                        xs.len()
                    ));
                }
                Ok(row(v))
            }
            Err(e) if e.contains("linear") => {
                // polynomial systems: elimination, real solutions
                let sols = crate::vcalc::solve_system(&eqs, &xs).ok_or_else(|| {
                    "solve: can't solve this system exactly; try fsolve([f1 f2], [x y], [x0 y0]) from a guess".to_string()
                })?;
                if sols.is_empty() {
                    return Err("solve: no real solution".into());
                }
                self.note(format!(
                    "Real solutions of the system, one per row ({}). Found by eliminating unknowns one at a time (substitution, factoring and resultants); complex solutions are left out.{}",
                    xs.join(", "),
                    crate::vcalc::family_note(&sols)
                ));
                if sols.len() == 1 {
                    return Ok(row(sols.into_iter().next().unwrap()));
                }
                Ok(Expr::Matrix(sols))
            }
            Err(e) => Err(e),
        }
    }

    /// solve(f, x, N): the roots of a polynomial in Z/N
    fn ff_eigvec(&mut self, m: &linalg::Mat, r: &std::rc::Rc<crate::ffield::Ring>) -> R<Expr> {
        let a = crate::fmat::lift(m, r)?;
        let c = crate::fmat::charpoly(&a, r)?;
        let n = a.len();
        let mut cols: Vec<Vec<Expr>> = vec![vec![]; n];
        let mut vals = vec![];
        for l in crate::fmat::roots(&c, r)? {
            let shifted: crate::fmat::FM = (0..n)
                .map(|i| {
                    (0..n)
                        .map(|j| if i == j { a[i][j].add(&l.neg()) } else { a[i][j].clone() })
                        .collect()
                })
                .collect();
            let ns = crate::fmat::nullspace(&shifted, r)?;
            for k in 0..ns.first().map_or(0, |row| row.len()) {
                for i in 0..n {
                    cols[i].push(crate::ffield::value(ns[i][k].clone()));
                }
                vals.push(l.text());
            }
        }
        if vals.is_empty() {
            self.note(&format!("No eigenvalues in {}, so no eigenvectors there.", r.name()));
            return Ok(Expr::Matrix(vec![]));
        }
        self.note(&format!(
            "Eigenvectors over {}, one per column, for the eigenvalues {} (a basis of the null space of A - λI for each).",
            r.name(),
            vals.join(", ")
        ));
        Ok(Expr::Matrix(cols))
    }

    /// solve(f, x, F) and solve([f1, f2], [x, y], F) over a finite ring F
    fn solve_ring(&mut self, f: &Node, x: &Node, r: std::rc::Rc<crate::ffield::Ring>) -> R<Expr> {
        use crate::ffield as ff;
        let xs = names(x, "solve: the unknowns")?;
        let fx = self.eval_hidden(f, &xs)?;
        let eqs: Vec<Expr> = match fx {
            Expr::Matrix(m) => m.into_iter().flatten().map(|e| crate::eval::equation_lhs(&e)).collect(),
            Expr::Obj(o) if matches!(&*o, crate::expr::Obj::List(_)) => {
                let crate::expr::Obj::List(v) = &*o else { unreachable!() };
                v.iter().map(crate::eval::equation_lhs).collect()
            }
            e => vec![crate::eval::equation_lhs(&e)],
        };
        let exact = |e: &Expr| -> R<ff::Elem> {
            r.from_expr(&simplify(e)).map_err(|_| {
                format!(
                    "solve over {}: the coefficients must be whole numbers or fractions, got {}",
                    r.name(),
                    show(e)
                )
            })
        };
        // linear: coefficient matrix and right-hand side
        let mut linear = true;
        let mut a = vec![];
        let mut b = vec![];
        'eq: for e in &eqs {
            let mut rowv = vec![];
            for v in &xs {
                let c = simplify(&calc::deriv(e, v));
                if xs.iter().any(|w| c.contains_sym(w)) {
                    linear = false;
                    break 'eq;
                }
                rowv.push(exact(&c)?);
            }
            let mut rest = e.clone();
            for v in &xs {
                rest = calc::subst(&rest, v, &Expr::zero());
            }
            a.push(rowv);
            b.push(exact(&rest)?.neg());
        }
        if linear && !(eqs.len() == 1 && xs.len() == 1) {
            let sol = crate::fmat::system(&a, &b, &xs, &r)?;
            let free: Vec<&String> = xs.iter().zip(&sol).filter(|(x, e)| **e == Expr::sym(x)).map(|p| p.0).collect();
            if free.is_empty() {
                self.note(&format!("The one solution in {} ({}), by Gaussian elimination in the field.", r.name(), xs.join(", ")));
            } else {
                let names: Vec<&str> = free.iter().map(|s| s.as_str()).collect();
                self.note(&format!(
                    "{}^{} solutions in {}: {} free (any element of {}) and the others given by representatives of their coefficients.",
                    r.size(),
                    names.len(),
                    r.name(),
                    names.join(", "),
                    r.name()
                ));
            }
            return Ok(row(sol));
        }
        // polynomial equations: one unknown by roots, several by trying every point
        let polys: Vec<Vec<(ff::Elem, Vec<u32>)>> = eqs
            .iter()
            .map(|e| poly_terms(&calc::expand(e), &xs, &r))
            .collect::<Option<_>>()
            .ok_or_else(|| format!("solve over {}: the equations must be polynomials in {}", r.name(), xs.join(", ")))?;
        let q = r.size();
        let nv = xs.len() as u32;
        let points = num_traits::pow(q.clone(), nv as usize);
        if xs.len() == 1 && eqs.len() == 1 {
            let deg = polys[0].iter().map(|(_, e)| e[0]).max().unwrap_or(0) as usize;
            let mut c = vec![r.int(0.into()); deg + 1];
            for (co, e) in &polys[0] {
                c[e[0] as usize] = c[e[0] as usize].add(co);
            }
            if r.is_field() {
                let roots = crate::fmat::roots(&c, &r)?;
                self.note(&format!("The roots of {} in {}.", show(&eqs[0]), r.name()));
                return Ok(if roots.is_empty() { Expr::Matrix(vec![]) } else { row(roots.into_iter().map(ff::value).collect()) });
            }
        }
        if points > num_bigint::BigInt::from(2_000_000) {
            return Err(format!(
                "solve over {}: a nonlinear system is solved by trying all {} points, which stops at two million",
                r.name(),
                points
            ));
        }
        let qn = num_traits::ToPrimitive::to_u64(&q).unwrap();
        let mut sols: Vec<Vec<Expr>> = vec![];
        let mut pt: Vec<ff::Elem> = vec![r.int(0.into()); xs.len()];
        let all: Vec<ff::Elem> = (0..qn).map(|t| r.elem(crate::fmat::digits(t, &r.n, r.k))).collect();
        let total = num_traits::ToPrimitive::to_u64(&points).unwrap();
        for t in 0..total {
            let mut u = t;
            for slot in pt.iter_mut().rev() {
                *slot = all[(u % qn) as usize].clone();
                u /= qn;
            }
            let ok = polys.iter().all(|p| {
                p.iter()
                    .fold(r.int(0.into()), |acc, (c, e)| {
                        let mut term = c.clone();
                        for (x, k) in pt.iter().zip(e) {
                            term = term.mul(&x.pow(&num_bigint::BigInt::from(*k)).unwrap());
                        }
                        acc.add(&term)
                    })
                    .is_zero()
            });
            if ok {
                sols.push(pt.iter().cloned().map(ff::value).collect());
                if sols.len() > 10_000 {
                    return Err("solve: more than 10000 solutions".into());
                }
            }
        }
        self.note(&format!(
            "All {} solutions in {}, one per row ({}), found by trying each of the {} points.",
            sols.len(),
            r.name(),
            xs.join(", "),
            points
        ));
        Ok(match sols.len() {
            0 => Expr::Matrix(vec![]),
            1 => row(sols.pop().unwrap()),
            _ => Expr::Matrix(sols),
        })
    }

    fn solve_mod(&mut self, f: &Node, x: &Node, n: &Node) -> R<Expr> {
        let nv = self.eval(n)?;
        if let Some(r) = crate::ffield::as_ring(&nv) {
            return self.solve_ring(f, x, r);
        }
        if matches!(x, Node::Matrix(_)) {
            let n = nv.num().and_then(|v| v.as_bigint()).ok_or("solve(eqs, vars, N): N must be an integer or a ring such as gf(4)")?;
            let r = if crate::numtheory::is_prime(&n) { crate::ffield::gf(&n)? } else { crate::ffield::zmod(&n)? };
            return self.solve_ring(f, x, r);
        }
        let x = name(x, "solve: x")?;
        let n = match nv {
            Expr::Num(v) => v.as_bigint(),
            _ => None,
        }
        .ok_or("solve(f, x, N): N must be an integer or a ring such as gf(4)")?;
        let fx = crate::eval::equation_lhs(&self.eval_hidden(f, std::slice::from_ref(&x))?);
        let c = cas::poly_coeffs(&fx, &x)
            .ok_or_else(|| format!("solve(f, x, N): {} is not a polynomial in {}", show(&fx), x))?;
        let mut ints = vec![];
        for e in &c {
            let r = e
                .num()
                .and_then(|v| v.big())
                .ok_or("solve(f, x, N): the coefficients must be integers or fractions")?;
            let inv = crate::numtheory::modinv(&r.denom().clone(), &n).ok_or_else(|| {
                format!(
                    "solve(f, x, N): the denominator {} has no inverse mod {}",
                    r.denom(),
                    n
                )
            })?;
            ints.push(r.numer() * inv);
        }
        let t = crate::modsolve::solve(&ints, &n)?;
        let md = crate::modsolve::notes(&t, &show(&fx), &x, self.teaching());
        self.note2(crate::teach::plain(&md), md);
        let roots: Vec<Expr> = t
            .roots
            .iter()
            .cloned()
            .map(crate::extra_cmd::bnum)
            .collect();
        Ok(if roots.is_empty() {
            Expr::Matrix(vec![])
        } else {
            row(roots)
        })
    }

    fn fsolve(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x, g] = args else {
            return Err(
                "usage: fsolve(f, x, guess): a numeric root of f = 0 near the guess".into(),
            );
        };
        if let Node::Matrix(_) = x {
            // a system: fsolve([f1 f2], [x y], [x0 y0])
            let xs = names(x, "fsolve: the unknowns")?;
            let fx: Vec<Expr> = match self.eval_hidden(f, &xs)? {
                Expr::Matrix(m) => m.into_iter().flatten().map(|e| crate::eval::equation_lhs(&e)).collect(),
                e => vec![crate::eval::equation_lhs(&e)],
            };
            let g: Vec<f64> = match self.eval(g)? {
                Expr::Matrix(m) => m.into_iter().flatten().map(|e| numeric::value(&e)).collect(),
                e => vec![numeric::value(&e)],
            };
            if g.len() != xs.len() || g.iter().any(|v| !v.is_finite()) {
                return Err("fsolve: the guess needs one number per unknown".into());
            }
            let free: Vec<String> = fx.iter().flat_map(|e| numeric::free(e, &xs.iter().map(|s| s.as_str()).collect::<Vec<_>>())).collect();
            if !free.is_empty() {
                return Err(format!("fsolve: give values to {} first", free.join(", ")));
            }
            let r = crate::vcalc::newton(&fx, &xs, &g).map_err(|e| format!("fsolve: {}", e))?;
            self.note("Newton's method for a system: at each step it replaces the equations by their linear approximation (the Jacobian) and solves that; it converges fast from a good guess. Another guess can give another solution.");
            return Ok(row(r.into_iter().map(Expr::float).collect()));
        }
        let x = name(x, "fsolve: x")?;
        let fx = crate::eval::equation_lhs(&self.eval_hidden(f, std::slice::from_ref(&x))?);
        let free = numeric::free(&fx, &[x.as_str()]);
        if !free.is_empty() {
            return Err(format!("fsolve: give values to {} first", free.join(", ")));
        }
        let g = self.eval(g)?;
        Ok(Expr::float(cas::fsolve(&fx, &x, real(&g, "fsolve")?)?))
    }

    fn sum(&mut self, product: bool, args: &[Node]) -> R<Expr> {
        let fname = if product { "prod" } else { "sum" };
        let [f, k, a, b] = args else {
            // sum(A): sum of the entries of a matrix
            if let [m] = args {
                if let Expr::Matrix(rows) = self.eval(m)? {
                    let all: Vec<Expr> = rows.into_iter().flatten().collect();
                    return Ok(if product { mul(all) } else { add(all) });
                }
            }
            return Err(format!(
                "usage: {}(f, k, a, b) for k = a, a+1, ..., b, or {}(A) over the entries of a matrix",
                fname, fname
            ));
        };
        let k = name(k, &format!("{}: k", fname))?;
        let (a, b) = (self.eval(a)?, self.eval(b)?);
        if !product && (is_inf(&b) == 1 || !b.symbols().is_empty()) {
            // closed forms: sum(k^2, k, 1, n), sum(1/2^k, k, 0, inf)
            let fk = self.eval_hidden(f, std::slice::from_ref(&k))?;
            return crate::extra_cmd::symbolic_sum(&fk, &k, &a, &b, is_inf(&b) == 1);
        }
        let (a, b) = (integer(&a, fname)?, integer(&b, fname)?);
        if b - a > 100_000 {
            return Err(format!("{}: at most 100000 terms", fname));
        }
        // evaluate the body with k = a, a+1, ..., b, so factorial(k) etc. work
        let saved = self.vars.remove(&k);
        let mut terms: Vec<Expr> = vec![];
        let mut err = None;
        for i in a..=b {
            self.vars.insert(k.clone(), Expr::int(i));
            match self.eval(f) {
                Ok(v) => terms.push(v),
                Err(e) => {
                    err = Some(e);
                    break;
                }
            }
        }
        match saved {
            Some(v) => {
                self.vars.insert(k.clone(), v);
            }
            None => {
                self.vars.remove(&k);
            }
        }
        if let Some(e) = err {
            return Err(e);
        }
        let r = if product { mul(terms) } else { add(terms) };
        Ok(if r.size() < 200 { simplify(&r) } else { r })
    }

    fn unary(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        if let ("round", [a, d]) = (f, args) {
            // round(x, d): d decimals, entry by entry
            let v = self.eval(a)?;
            let d = self.eval(d)?.num().and_then(|k| k.as_int()).filter(|k| (0..=15).contains(k)).ok_or("round: the number of decimals must be a whole number from 0 to 15")?;
            let k = 10f64.powi(d as i32);
            // a quantity: round its number, keep its units (round(194.04 m, 1) = 194.0 m)
            if crate::units::has_units(&v) && !matches!(v, Expr::Matrix(_)) {
                if let Some((c, u)) = crate::units::split(&v).filter(|(c, _)| !crate::units::has_units(c)) {
                    let c = elementwise_num(&c, f, &|x| Expr::float((x * k).round() / k + 0.0))?;
                    return Ok(crate::build::mul2(c, crate::units::expr_of(&u)));
                }
            }
            return elementwise_num(&v, f, &|x| Expr::float((x * k).round() / k + 0.0));
        }
        let [a] = args else {
            return Err(format!("{} takes 1 argument", f));
        };
        let v = self.eval(a)?;
        let round = |g: fn(f64) -> f64| -> R<Expr> {
            elementwise_num(&v, f, &|x| {
                let r = g(x);
                if r.abs() < 1e30 {
                    Expr::int(r as i128)
                } else {
                    Expr::float(r)
                }
            })
        };
        match f {
            // a symbolic argument stays as factorial(n), so that
            // simplify(factorial(n + 2)/factorial(n)) gives (n + 1)(n + 2)
            "factorial" if exact_int(&v).is_none()
                && !matches!(v, Expr::Matrix(_))
                && calc::evalf(&v).num().is_none() =>
            {
                Ok(crate::special::apply("factorial", vec![v]))
            }
            "factorial" => cas::factorial(integer(&v, f)?),
            "floor" | "ceil" | "round" if round_exact(&v, f).is_some() => {
                round_exact(&v, f).unwrap()
            }
            "floor" => round(f64::floor),
            "ceil" => round(f64::ceil),
            "round" => round(f64::round),
            "numer" | "denom" => {
                let (n, d) = calc::num_den(&calc::together(&v));
                Ok(if f == "numer" { n } else { d })
            }
            "rref" | "rank" | "null" => {
                let Expr::Matrix(m) = v else {
                    return Err(format!("{} needs a matrix", f));
                };
                if let Some(r) = crate::fmat::ring_of(&m)? {
                    let a = crate::fmat::lift(&m, &r)?;
                    return Ok(match f {
                        "rref" => crate::fmat::expr(&crate::fmat::rref(&a, &r)?.0),
                        "rank" => Expr::int(crate::fmat::rref(&a, &r)?.1.len() as i128),
                        _ => crate::fmat::expr(&crate::fmat::nullspace(&a, &r)?),
                    });
                }
                match f {
                    "rref" => Ok(Expr::Matrix(linalg::rref(&m).0)),
                    "rank" => Ok(Expr::int(linalg::rref(&m).1.len() as i128)),
                    _ => {
                        let n = linalg::nullspace(&m);
                        Ok(if n.is_empty() {
                            Expr::Matrix(vec![])
                        } else {
                            Expr::Matrix(n)
                        })
                    }
                }
            }
            "eigvec" => {
                let Expr::Matrix(m) = v else {
                    return Err("eigvec needs a square matrix".into());
                };
                if let Some(r) = crate::fmat::ring_of(&m)? {
                    return self.ff_eigvec(&m, &r);
                }
                eigvec(&m).map(|(vecs, _)| Expr::Matrix(vecs))
            }
            "trace" | "size" | "eig" => {
                if let (Some(x), "trace") = (crate::qring::as_elem(&v), f) {
                    self.note("The field trace: the trace of multiplication by the element (the sum of its conjugates).");
                    return Ok(crate::qring::qnum(x.trace()));
                }
                let m = match v {
                    Expr::Matrix(m) => m,
                    _ if f == "size" => {
                        return Ok(Expr::Matrix(vec![vec![Expr::one(), Expr::one()]]));
                    }
                    s => vec![vec![s]],
                };
                let (r, c) = linalg::dims(&m);
                match f {
                    "size" => Ok(Expr::Matrix(vec![vec![
                        Expr::int(r as i128),
                        Expr::int(c as i128),
                    ]])),
                    _ if r != c => Err(format!("{} needs a square matrix", f)),
                    _ if crate::fmat::ring_of(&m)?.is_some() => {
                        let ring = crate::fmat::ring_of(&m)?.unwrap();
                        let a = crate::fmat::lift(&m, &ring)?;
                        if f == "trace" {
                            let t = (0..r).fold(ring.int(0.into()), |acc, i| acc.add(&a[i][i]));
                            return Ok(crate::ffield::value(t));
                        }
                        let c = crate::fmat::charpoly(&a, &ring)?;
                        let roots = crate::fmat::roots(&c, &ring)?;
                        self.note(&format!(
                            "eig over {}: the roots in the field of det(A - x I) = {}; eigenvalues outside {} (in a bigger field) are not listed.",
                            ring.name(),
                            show(&ff_poly(&c, "x")),
                            ring.name()
                        ));
                        Ok(if roots.is_empty() {
                            Expr::Matrix(vec![])
                        } else {
                            row(roots.into_iter().map(crate::ffield::value).collect())
                        })
                    }
                    "trace" => Ok(simplify(&add((0..r).map(|i| m[i][i].clone()).collect()))),
                    _ => {
                        if let Some((vals, _)) = numeric_symmetric_eig(&m) {
                            self.note("eig: a symmetric matrix of numbers, diagonalized numerically (Jacobi rotations); eigenvalues in increasing order.");
                            return Ok(row(vals.into_iter().map(Expr::float).collect()));
                        }
                        let p = charpoly(&m, "λ")?;
                        let c =
                            cas::poly_coeffs(&p, "λ").ok_or("eig: no characteristic polynomial")?;
                        cas::poly_roots(&c).map(row)
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    /// gcd of two polynomials in one variable over Q, or mod a prime p: monic
    fn poly_gcd(&mut self, a: &Expr, b: &Expr, p: Option<num_bigint::BigInt>) -> R<Expr> {
        use num_rational::BigRational;
        let mut vars: Vec<String> = a.symbols();
        for v in b.symbols() {
            if !vars.contains(&v) {
                vars.push(v);
            }
        }
        if vars.len() > 1 {
            return Err(format!("gcd: polynomials in one variable only, got {}", vars.join(", ")));
        }
        let x = vars.pop().unwrap_or_else(|| "x".to_string());
        let coeffs = |e: &Expr| -> R<Vec<BigRational>> {
            let c = cas::poly_coeffs(&calc::expand(e), &x)
                .ok_or_else(|| format!("gcd: {} is not a polynomial", show(e)))?;
            c.iter()
                .map(|c| simplify(c).num().and_then(|n| n.rational()).ok_or_else(|| "gcd: the coefficients must be rational numbers".to_string()))
                .collect()
        };
        let (ca, cb) = (coeffs(a)?, coeffs(b)?);
        let trim = |mut v: Vec<BigRational>| {
            while v.last().is_some_and(|c| c.is_zero()) {
                v.pop();
            }
            v
        };
        let g: Vec<BigRational> = if let Some(p) = p {
            let ints = |v: &[BigRational]| -> R<Vec<num_bigint::BigInt>> {
                v.iter()
                    .map(|c| {
                        let d = c.denom().clone();
                        let inv = crate::numtheory::modinv(&num_integer::Integer::mod_floor(&d, &p), &p)
                            .ok_or("gcd: a denominator is divisible by p")?;
                        Ok(num_integer::Integer::mod_floor(&(c.numer() * inv), &p))
                    })
                    .collect()
            };
            crate::modsolve::gcd(&ints(&ca)?, &ints(&cb)?, &p)
                .into_iter()
                .map(BigRational::from_integer)
                .collect()
        } else {
            let (mut u, mut v) = (trim(ca), trim(cb));
            while !v.is_empty() {
                // u mod v
                let mut r = u.clone();
                let m = v.len() - 1;
                while r.len() > m && !r.is_empty() {
                    let lead = r[r.len() - 1].clone() / v[m].clone();
                    let k = r.len() - 1 - m;
                    for j in 0..=m {
                        r[k + j] = r[k + j].clone() - lead.clone() * v[j].clone();
                    }
                    r.pop();
                    r = trim(r);
                }
                u = v;
                v = r;
            }
            match u.last().cloned() {
                Some(l) => u.into_iter().map(|c| c / l.clone()).collect(),
                None => u,
            }
        };
        let xs = Expr::sym(&x);
        let terms: Vec<Expr> = g
            .into_iter()
            .enumerate()
            .filter(|(_, c)| !c.is_zero())
            .map(|(k, c)| mul2(crate::qring::qnum(c), pow(xs.clone(), Expr::int(k as i128))))
            .collect();
        let note = match &terms[..] {
            [] => "gcd: both polynomials are 0.".to_string(),
            _ => "gcd of polynomials: Euclid's algorithm (divide, keep the remainder, repeat until it is 0), made monic.".to_string(),
        };
        self.note(note);
        Ok(simplify(&add(terms)))
    }

    fn binary(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        if f == "gcd" && args.len() == 3 {
            let (a, b) = (self.eval(&args[0])?, self.eval(&args[1])?);
            let p = big_integer(&self.eval(&args[2])?, "gcd")?;
            if p < num_bigint::BigInt::from(2) || !crate::numtheory::is_prime(&p) {
                return Err("gcd(f, g, p): p must be a prime".into());
            }
            return self.poly_gcd(&a, &b, Some(p));
        }
        let [a, b] = args else {
            return Err(format!("{} takes 2 arguments", f));
        };
        if f == "charpoly" || f == "degree" {
            let x = name(b, &format!("{}: the second argument", f))?;
            let v = self.eval_hidden(a, std::slice::from_ref(&x))?;
            if f == "degree" {
                let c = cas::poly_coeffs(&v, &x)
                    .ok_or_else(|| format!("degree: {} is not a polynomial in {}", show(&v), x))?;
                return Ok(Expr::int(c.len() as i128 - 1));
            }
            let Expr::Matrix(m) = v else {
                return Err("charpoly needs a matrix".into());
            };
            if let Some(r) = crate::fmat::ring_of(&m)? {
                let a = crate::fmat::lift(&m, &r)?;
                let c = crate::fmat::charpoly(&a, &r)?;
                self.note(&format!(
                    "det(A - {} I) with coefficients in {} (shown by their representatives).",
                    x,
                    r.name()
                ));
                return Ok(ff_poly(&c, &x));
            }
            return charpoly(&m, &x);
        }
        let (va, vb) = (self.eval(a)?, self.eval(b)?);
        match f {
            "binomial" => Ok(cas::binomial(integer(&va, f)?, integer(&vb, f)?)),
            "gcd" | "lcm" => {
                if f == "gcd" && !(va.num().is_some_and(|n| n.is_integer()) && vb.num().is_some_and(|n| n.is_integer())) {
                    return self.poly_gcd(&va, &vb, None);
                }
                let (x, y) = (big_integer(&va, f)?, big_integer(&vb, f)?);
                let g = num_integer::Integer::gcd(&x, &y);
                Ok(crate::extra_cmd::bnum(if f == "gcd" {
                    g
                } else if g.is_zero() {
                    g
                } else {
                    (x / &g * y).abs()
                }))
            }
            "mod" => match (&va, &vb) {
                (Expr::Matrix(m), b) => Ok(Expr::Matrix(
                    m.iter()
                        .map(|r| r.iter().map(|x| modulo(x, b)).collect::<R<_>>())
                        .collect::<R<_>>()?,
                )),
                (a, Expr::Matrix(m)) => Ok(Expr::Matrix(
                    m.iter()
                        .map(|r| r.iter().map(|y| modulo(a, y)).collect::<R<_>>())
                        .collect::<R<_>>()?,
                )),
                (a, b) => modulo(a, b),
            },
            "dot" | "cross" => {
                let flat = |e: Expr| -> Vec<Expr> {
                    match e {
                        Expr::Matrix(m) => m.into_iter().flatten().collect(),
                        o => vec![o],
                    }
                };
                let (u, v) = (flat(va), flat(vb));
                if u.len() != v.len() {
                    return Err(format!("{}: the vectors must have the same length", f));
                }
                if f == "dot" {
                    return Ok(simplify(&add(u
                        .iter()
                        .zip(&v)
                        .map(|(p, q)| mul2(p.clone(), q.clone()))
                        .collect())));
                }
                if u.len() != 3 {
                    return Err("cross needs two vectors of length 3".into());
                }
                let c = |i: usize, j: usize| {
                    simplify(&sub(
                        mul2(u[i].clone(), v[j].clone()),
                        mul2(u[j].clone(), v[i].clone()),
                    ))
                };
                Ok(Expr::Matrix(vec![vec![c(1, 2), c(2, 0), c(0, 1)]]))
            }
            _ => unreachable!(),
        }
    }

    fn coeff(&mut self, args: &[Node]) -> R<Expr> {
        let [p, x, n] = args else {
            return Err("usage: coeff(p, x, n): the coefficient of x^n in p".into());
        };
        let x = name(x, "coeff: x")?;
        let v = self.eval_hidden(p, std::slice::from_ref(&x))?;
        let n = self.eval(n)?;
        let n = exact_int(&n)
            .filter(|k| *k >= 0)
            .ok_or("coeff: n must be a whole number")? as usize;
        let c = cas::poly_coeffs(&v, &x)
            .ok_or_else(|| format!("coeff: {} is not a polynomial in {}", show(&v), x))?;
        Ok(c.get(n).map(simplify).unwrap_or_else(Expr::zero))
    }

    fn polarplot(&mut self, args: &[Node]) -> R<Expr> {
        let [r, t, a, b] = args else {
            return Err(
                "usage: polarplot(r, phi, from, to): the curve at distance r(phi) from the origin"
                    .into(),
            );
        };
        let tn = name(t, "polarplot: the angle")?;
        let rv = self.eval_hidden(r, std::slice::from_ref(&tn))?;
        let (a, b) = (self.eval(a)?, self.eval(b)?);
        let (a, b) = (real(&a, "polarplot")?, real(&b, "polarplot")?);
        let free = numeric::free(&rv, &[tn.as_str()]);
        if !free.is_empty() {
            return Err(format!(
                "polarplot: give values to {} first",
                free.join(", ")
            ));
        }
        let c = numeric::compile(&rv, &[tn.as_str()]);
        let pts = crate::plot::steps(a, b, crate::plot::SAMPLES)
            .iter()
            .map(|&s| {
                let rr = numeric::run(&c, &[s]);
                (rr * s.cos(), rr * s.sin())
            })
            .collect();
        let s = crate::plot::Series {
            label: format!("r = {}", crate::parser::source(r)),
            points: pts,
            ..Default::default()
        };
        Ok(Expr::Obj(Rc::new(Obj::Figure(crate::plot::Figure::new(
            vec![s],
            "x",
            "y",
            true,
        )))))
    }

    /// window(p, x0, x1, y0, y1): show exactly that part of a 2D plot
    fn window(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: window(p, x0, x1, y0, y1): show x from x0 to x1 and y from y0 to y1";
        if args.len() != 5 {
            return Err(usage.into());
        }
        let p = self.eval(&args[0])?;
        let mut v = vec![];
        for a in &args[1..] {
            v.push(real(&self.eval(a)?, "window")?);
        }
        if !(v[0] < v[1] && v[2] < v[3]) || v.iter().any(|x| !x.is_finite()) {
            return Err("window: need x0 < x1 and y0 < y1".into());
        }
        let Expr::Obj(o) = &p else { return Err("window: the first argument must be a plot".into()) };
        let Obj::Figure(fig) = &**o else { return Err("window: only 2D plots have a window".into()) };
        let mut g = fig.clone();
        g.window = Some((v[0], v[1], v[2], v[3]));
        Ok(Expr::Obj(Rc::new(Obj::Figure(g))))
    }

    fn labels(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let usage = match f {
            "labels" => {
                "usage: labels(p, \"x label\", \"y label\") (and a third for z on 3D plots)"
            }
            "legend" => "usage: legend(p, \"first curve\", \"second curve\", ...)",
            _ => {
                "usage: title(p, \"text\"), xlabel(p, \"text\"), ylabel(p, \"text\"), zlabel(p, \"text\")"
            }
        };
        if args.len() < 2 {
            return Err(usage.into());
        }
        let p = self.eval(&args[0])?;
        let mut texts = vec![];
        for a in &args[1..] {
            texts.push(label_text(a, self)?);
        }
        let Expr::Obj(o) = &p else {
            return Err(format!("{}: the first argument must be a plot", f));
        };
        let out = match &**o {
            Obj::Figure(fig) => {
                let mut g = fig.clone();
                match f {
                    "title" => g.title = texts.join(" "),
                    "xlabel" => g.x_label = texts.join(" "),
                    "ylabel" => g.y_label = texts.join(" "),
                    "labels" => {
                        g.x_label = texts[0].clone();
                        if let Some(y) = texts.get(1) {
                            g.y_label = y.clone();
                        }
                    }
                    "legend" => {
                        for (s, t) in g.series.iter_mut().zip(&texts) {
                            s.label = t.clone();
                        }
                    }
                    _ => return Err("zlabel needs a 3D plot".into()),
                }
                Obj::Figure(g)
            }
            Obj::Figure3D(fig) => {
                let mut g = fig.clone();
                match f {
                    "title" => g.title = texts.join(" "),
                    "xlabel" => g.labels[0] = texts.join(" "),
                    "ylabel" => g.labels[1] = texts.join(" "),
                    "zlabel" => g.labels[2] = texts.join(" "),
                    "labels" => {
                        for (l, t) in g.labels.iter_mut().zip(&texts) {
                            *l = t.clone();
                        }
                    }
                    "legend" => {
                        let mut it = texts.iter();
                        for s in g.surfaces.iter_mut() {
                            if let Some(t) = it.next() {
                                s.label = t.clone();
                            }
                        }
                        for c in g.curves.iter_mut() {
                            if let Some(t) = it.next() {
                                c.label = t.clone();
                            }
                        }
                    }
                    _ => unreachable!(),
                }
                Obj::Figure3D(g)
            }
            _ => return Err(format!("{}: the first argument must be a plot", f)),
        };
        Ok(Expr::Obj(Rc::new(out)))
    }
}

fn elementwise_num(v: &Expr, f: &str, g: &dyn Fn(f64) -> Expr) -> R<Expr> {
    match v {
        Expr::Matrix(m) => {
            let mut out = vec![];
            for r in m {
                let mut o = vec![];
                for e in r {
                    o.push(elementwise_num(e, f, g)?);
                }
                out.push(o);
            }
            Ok(Expr::Matrix(out))
        }
        e => Ok(g(real(e, f)?)),
    }
}

/// A polynomial with ring coefficients (low to high), written with their
/// representatives
pub(crate) fn ff_poly(c: &[crate::ffield::Elem], x: &str) -> Expr {
    let xv = Expr::sym(x);
    let terms: Vec<Expr> = c
        .iter()
        .enumerate()
        .rev()
        .filter(|(_, e)| !e.is_zero())
        .map(|(k, e)| {
            let co = e.lift();
            match k {
                0 => co,
                1 => mul2(co, xv.clone()),
                _ => mul2(co, pow(xv.clone(), Expr::int(k as i128))),
            }
        })
        .collect();
    if terms.is_empty() {
        Expr::zero()
    } else {
        simplify(&add(terms))
    }
}

/// det(A - x I), expanded
pub(crate) fn charpoly(m: &[Vec<Expr>], x: &str) -> R<Expr> {
    let (r, c) = linalg::dims(&m.to_vec());
    if r != c {
        return Err("charpoly needs a square matrix".into());
    }
    let xv = Expr::sym(x);
    let a: Vec<Vec<Expr>> = (0..r)
        .map(|i| {
            (0..r)
                .map(|j| {
                    if i == j {
                        sub(m[i][j].clone(), xv.clone())
                    } else {
                        m[i][j].clone()
                    }
                })
                .collect()
        })
        .collect();
    let d = linalg::det(&a)?;
    let p = calc::expand(&d);
    Ok(match cas::poly_coeffs(&p, x) {
        Some(c) => add(c
            .iter()
            .enumerate()
            .map(|(k, v)| mul2(simplify(v), pow(xv.clone(), Expr::int(k as i128))))
            .collect()),
        None => p,
    })
}

/// definite integral; splits at singular points inside the interval
/// c x^p e^(-k x) integrated from 0 to inf = c gamma(p+1) / k^(p+1)
/// The integral over the whole line of P(x) e^(Q(x)), P a polynomial and Q
/// quadratic with negative leading coefficient (symbolic leading coefficients
/// are taken to make it so): with Q = -a x^2 + b x + c, it is
/// e^(c + b^2/(4a)) sqrt(pi/a) E[P(m + Z)], m = b/(2a), Z normal with variance 1/(2a).
fn gaussian_integral(fx: &Expr, x: &str, a: &Expr, b: &Expr) -> Option<Expr> {
    if is_inf(a) != -1 || is_inf(b) != 1 {
        return None;
    }
    let fs = match fx {
        Expr::Mul(v) => v.clone(),
        o => vec![o.clone()],
    };
    let mut q = vec![];
    let mut rest = vec![];
    for f in fs {
        match &f {
            Expr::Pow(base, e) if **base == Expr::Const(crate::expr::Const::E) && e.contains_sym(x) => q.push((**e).clone()),
            _ => rest.push(f),
        }
    }
    if q.is_empty() {
        return None;
    }
    let co = cas::poly_coeffs(&calc::expand(&add(q)), x)?;
    if co.len() != 3 {
        return None;
    }
    let aa = calc::simplify(&neg(co[2].clone()));
    let av = numeric::value(&aa);
    if av.is_finite() && av <= 0.0 || crate::assume::sign(&aa) == Some(-1) {
        return None;
    }
    let pc = cas::poly_coeffs(&calc::expand(&mul(rest)), x)?;
    let m = div(co[1].clone(), mul2(Expr::int(2), aa.clone()));
    let var = div(Expr::one(), mul2(Expr::int(2), aa.clone()));
    // E[(m + Z)^n] = sum_j C(n, j) m^(n-j) E[Z^j], E[Z^(2i)] = (2i-1)!! var^i
    let mut terms = vec![];
    for (n, c) in pc.iter().enumerate() {
        if cas::is_zero(c) {
            continue;
        }
        let mut binom: i128 = 1;
        for j in 0..=n {
            if j > 0 {
                binom = binom * (n + 1 - j) as i128 / j as i128;
            }
            if j % 2 == 1 {
                continue;
            }
            let dfact: i128 = (1..j as i128).step_by(2).product();
            terms.push(mul(vec![
                c.clone(),
                Expr::int(binom * dfact.max(1)),
                pow(m.clone(), Expr::int((n - j) as i128)),
                pow(var.clone(), Expr::int(j as i128 / 2)),
            ]));
        }
    }
    let front = mul2(
        exp(add2(co[0].clone(), div(pow(co[1].clone(), Expr::int(2)), mul2(Expr::int(4), aa.clone())))),
        sqrt(div(Expr::Const(crate::expr::Const::Pi), aa)),
    );
    let r = calc::simplify(&calc::expand(&mul2(front, add(terms))));
    let f = calc::factor(&r);
    Some(if f.size() < r.size() { f } else { r })
}

fn gamma_integral(fx: &Expr, x: &str, a: &Expr, b: &Expr) -> Option<Expr> {
    if !a.is_zero() || is_inf(b) != 1 {
        return None;
    }
    let fs = match fx {
        Expr::Mul(v) => v.clone(),
        o => vec![o.clone()],
    };
    let (mut p, mut k) = (None, None);
    let mut c = vec![];
    for f in fs {
        match &f {
            Expr::Sym(s) if s == x && p.is_none() => p = Some(Expr::one()),
            Expr::Pow(base, e) if matches!(&**base, Expr::Sym(s) if s == x) && !e.contains_sym(x) && p.is_none() => {
                p = Some((**e).clone())
            }
            Expr::Pow(base, q) if **base == Expr::Const(crate::expr::Const::E) && q.contains_sym(x) && k.is_none() => {
                let co = cas::poly_coeffs(q, x)?;
                if co.len() != 2 || !co[0].is_zero() {
                    return None;
                }
                k = Some(neg(co[1].clone()));
            }
            _ if !f.contains_sym(x) => c.push(f),
            _ => return None,
        }
    }
    let (p, k) = (p.unwrap_or_else(Expr::zero), k?);
    let pv = numeric::value(&p);
    let kv = numeric::value(&k);
    // whole p: the antiderivative already gives p!; need p > -1 and k > 0
    // (a symbolic p is taken to be > -1, the condition for the integral to exist)
    if pv.is_finite() && (pv <= -1.0 || pv.fract() == 0.0) || kv.is_finite() && kv <= 0.0 {
        return None;
    }
    let p1 = add2(p, Expr::one());
    Some(crate::ratfun::tidy(&mul(vec![
        mul(c),
        crate::special::apply("gamma", vec![p1.clone()]),
        pow(k, neg(p1)),
    ])))
}

/// u = p x + q with numbers p != 0, q: the root -q/p
fn linear_root(u: &Expr, x: &str) -> Option<(f64, Expr, Expr)> {
    let c = cas::poly_coeffs(u, x)?;
    if c.len() != 2 || !c[0].symbols().is_empty() || !c[1].symbols().is_empty() || c[1].is_zero() {
        return None;
    }
    let r = simplify(&neg(div(c[0].clone(), c[1].clone())));
    Some((numeric::value(&r), r, c[1].clone()))
}

/// arguments of heaviside, sign and abs (kinks) and of dirac (spikes)
fn kinks(e: &Expr, x: &str, out: &mut Vec<Expr>, spikes: &mut Vec<Expr>) {
    match e {
        Expr::Apply(f, a) if a.len() == 1 && (f == "heaviside" || f == "sign") && a[0].contains_sym(x) => {
            out.push(a[0].clone())
        }
        Expr::Apply(f, a) if a.len() == 1 && f == "dirac" && a[0].contains_sym(x) => spikes.push(a[0].clone()),
        Expr::Func(crate::expr::Func::Abs, a) if a.contains_sym(x) => {
            out.push((**a).clone());
            kinks(a, x, out, spikes)
        }
        Expr::Add(v) | Expr::Mul(v) | Expr::Apply(_, v) => v.iter().for_each(|t| kinks(t, x, out, spikes)),
        Expr::Pow(p, q) => {
            kinks(p, x, out, spikes);
            kinks(q, x, out, spikes)
        }
        Expr::Func(_, a) => kinks(a, x, out, spikes),
        _ => {}
    }
}

/// replace heaviside(u), sign(u), abs(u) by their values where u has sign sg
fn fix_signs(e: &Expr, x: &str, at: f64) -> Expr {
    let sg = |u: &Expr| -> f64 {
        let v = numeric::value(&calc::subst(u, x, &Expr::float(at)));
        if v > 0.0 { 1.0 } else { -1.0 }
    };
    match e {
        Expr::Apply(f, a) if a.len() == 1 && (f == "heaviside" || f == "sign") && a[0].contains_sym(x) => {
            let s = sg(&a[0]);
            match (f.as_str(), s > 0.0) {
                ("heaviside", true) => Expr::one(),
                ("heaviside", false) => Expr::zero(),
                (_, true) => Expr::one(),
                _ => Expr::int(-1),
            }
        }
        Expr::Func(crate::expr::Func::Abs, a) if a.contains_sym(x) => {
            let inner = fix_signs(a, x, at);
            if sg(a) > 0.0 { inner } else { neg(inner) }
        }
        Expr::Add(v) => add(v.iter().map(|t| fix_signs(t, x, at)).collect()),
        Expr::Mul(v) => mul(v.iter().map(|t| fix_signs(t, x, at)).collect()),
        Expr::Pow(p, q) => pow(fix_signs(p, x, at), fix_signs(q, x, at)),
        Expr::Func(f, a) => func(*f, fix_signs(a, x, at)),
        Expr::Apply(f, v) => Expr::Apply(f.clone(), v.iter().map(|t| fix_signs(t, x, at)).collect()),
        o => o.clone(),
    }
}

/// piecewise integrands: split at the kinks of heaviside, sign and abs, and
/// sift out dirac(u): the integral of h(x) dirac(p x + q) is h(x0) / |p|
fn piecewise_definite(fx: &Expr, x: &str, a: &Expr, b: &Expr, depth: usize) -> Option<R<Expr>> {
    let (mut ks, mut spikes) = (vec![], vec![]);
    kinks(fx, x, &mut ks, &mut spikes);
    if ks.is_empty() && spikes.is_empty() {
        return None;
    }
    let (lo, hi) = (real(a, "integrate").ok()?, real(b, "integrate").ok()?);
    if lo > hi {
        return Some(definite(fx, x, b, a, depth).map(neg));
    }
    if let Some(u) = spikes.first() {
        let (x0, r, p) = linear_root(u, x)?;
        let d = Expr::sym("dirac__");
        let marked = replace_apply(fx, "dirac", u, &d);
        let h = simplify(&calc::deriv(&marked, "dirac__"));
        if h.contains_sym("dirac__") {
            return Some(Err("integrate: dirac must appear linearly (h(x) dirac(x - a))".into()));
        }
        let rest = simplify(&calc::subst(&marked, "dirac__", &Expr::zero()));
        let w = if x0 > lo && x0 < hi {
            Expr::one()
        } else if x0 == lo || x0 == hi {
            Expr::Num(Number::rat(1, 2).unwrap())
        } else {
            Expr::zero()
        };
        let pick = simplify(&mul(vec![w, calc::subst(&h, x, &r), pow(func(crate::expr::Func::Abs, p), Expr::int(-1))]));
        let base = if cas::is_zero(&rest) { Ok(Expr::zero()) } else { definite(&rest, x, a, b, depth + 1) };
        return Some(base.map(|v| simplify(&add2(v, pick))));
    }
    let mut cuts: Vec<(f64, Expr)> = ks
        .iter()
        .filter_map(|u| linear_root(u, x))
        .filter(|(v, _, _)| *v > lo && *v < hi)
        .map(|(v, r, _)| (v, r))
        .collect();
    if cuts.len() < ks.len() && ks.iter().any(|u| linear_root(u, x).is_none()) {
        return None;
    }
    cuts.sort_by(|p, q| p.0.partial_cmp(&q.0).unwrap());
    cuts.dedup_by(|p, q| (p.0 - q.0).abs() < 1e-12);
    if depth > 6 {
        return None;
    }
    let mut pts = vec![(lo, a.clone())];
    pts.extend(cuts);
    pts.push((hi, b.clone()));
    let mut parts = vec![];
    for w in pts.windows(2) {
        let (l, h) = (w[0].0, w[1].0);
        let mid = match (l.is_finite(), h.is_finite()) {
            (true, true) => (l + h) / 2.0,
            (true, false) => l + 1.0,
            (false, true) => h - 1.0,
            _ => 0.0,
        };
        let piece = simplify(&fix_signs(fx, x, mid));
        match definite(&piece, x, &w[0].1, &w[1].1, depth + 1) {
            Ok(v) => parts.push(v),
            Err(e) => return Some(Err(e)),
        }
    }
    Some(Ok(simplify(&add(parts))))
}

fn replace_apply(e: &Expr, name: &str, arg: &Expr, by: &Expr) -> Expr {
    match e {
        Expr::Apply(f, a) if f == name && a.len() == 1 && a[0] == *arg => by.clone(),
        Expr::Add(v) => add(v.iter().map(|t| replace_apply(t, name, arg, by)).collect()),
        Expr::Mul(v) => mul(v.iter().map(|t| replace_apply(t, name, arg, by)).collect()),
        Expr::Pow(p, q) => pow(replace_apply(p, name, arg, by), replace_apply(q, name, arg, by)),
        Expr::Func(f, a) => func(*f, replace_apply(a, name, arg, by)),
        o => o.clone(),
    }
}

pub(crate) fn definite(fx: &Expr, x: &str, a: &Expr, b: &Expr, depth: usize) -> R<Expr> {
    if let Some(g) = gamma_integral(fx, x, a, b) {
        return Ok(g);
    }
    if let Some(g) = gaussian_integral(fx, x, a, b) {
        return Ok(g);
    }
    // a full period of a rational function of cos and sin
    if let (Ok(lo), Ok(hi)) = (real(a, "integrate"), real(b, "integrate")) {
        if (hi - lo - 2.0 * std::f64::consts::PI).abs() < 1e-12 {
            if let Some(g) = crate::laurent::period_integral(fx, x) {
                return Ok(g);
            }
        }
    }
    if is_inf(b) == 1 && (is_inf(a) == -1 || a.is_zero()) {
        if let Some(g) = crate::laurent::fourier_integral(fx, x, a.is_zero()) {
            return Ok(g);
        }
    }
    if let Some(r) = piecewise_definite(fx, x, a, b, depth) {
        return r;
    }
    let free = numeric::free(fx, &[x]);
    let (an, bn) = (real(a, "integrate").ok(), real(b, "integrate").ok());
    let numeric_ok = free.is_empty() && an.is_some() && bn.is_some();
    // singularities strictly inside: zeros of the denominator
    if let (Some(lo), Some(hi)) = (an, bn) {
        if depth < 4 {
            let (_, d) = calc::num_den(&calc::together(fx));
            if d.contains_sym(x) {
                if let Ok(roots) = cas::solve(&d, x) {
                    let (l, h) = if lo < hi { (lo, hi) } else { (hi, lo) };
                    let mut inside: Vec<(f64, Expr)> = roots
                        .into_iter()
                        .filter(|r| r.symbols().is_empty())
                        .filter_map(|r| {
                            let v = numeric::value(&r);
                            (v.is_finite() && v > l && v < h).then_some((v, r))
                        })
                        .collect();
                    inside.sort_by(|p, q| p.0.partial_cmp(&q.0).unwrap());
                    if !inside.is_empty() {
                        let mut cuts = vec![if lo < hi { a.clone() } else { b.clone() }];
                        cuts.extend(inside.into_iter().map(|p| p.1));
                        cuts.push(if lo < hi { b.clone() } else { a.clone() });
                        let mut parts = vec![];
                        for w in cuts.windows(2) {
                            parts.push(definite(fx, x, &w[0], &w[1], depth + 1)?);
                        }
                        let s = simplify(&add(parts));
                        return Ok(if lo < hi { s } else { neg(s) });
                    }
                }
            }
        }
    }
    let quad = || -> R<f64> { cas::nintegrate(fx, x, an.unwrap(), bn.unwrap()) };
    let forward = match (an, bn) {
        (Some(p), Some(q)) => p <= q,
        _ => true,
    };
    if let Some(big_f) = cas::antiderivative(fx, x) {
        let (sa, sb) = if forward {
            (Side::Right, Side::Left)
        } else {
            (Side::Left, Side::Right)
        };
        let fa = cas::limit(&big_f, x, &point(a, sa));
        let fb = cas::limit(&big_f, x, &point(b, sb));
        if let (Ok(fa), Ok(fb)) = (fa, fb) {
            if is_inf(&fa) != 0 || is_inf(&fb) != 0 {
                return Err("integrate: the integral diverges".into());
            }
            let res = simplify(&sub(fb, fa));
            if numeric_ok {
                if let Ok(q) = quad() {
                    let v = numeric::value(&res);
                    if !v.is_finite() || (v - q).abs() > 1e-6 * (1.0 + q.abs()) {
                        return Ok(Expr::float(q));
                    }
                }
            }
            return Ok(res);
        }
    }
    if numeric_ok {
        return quad()
            .map(Expr::float)
            .map_err(|e| format!("integrate: {}", e));
    }
    if !free.is_empty() {
        return Err(format!(
            "integrate: can't integrate {} symbolically; give values to {} to get a number",
            show(fx),
            free.join(", ")
        ));
    }
    Err(format!(
        "integrate: can't integrate {} from {} to {}",
        show(fx),
        show(a),
        show(b)
    ))
}

/// The terms (coefficient, exponents) of an expanded polynomial in vars with
/// coefficients in the ring r
fn poly_terms(
    e: &Expr,
    vars: &[String],
    r: &std::rc::Rc<crate::ffield::Ring>,
) -> Option<Vec<(crate::ffield::Elem, Vec<u32>)>> {
    let terms: Vec<&Expr> = match e {
        Expr::Add(v) => v.iter().collect(),
        o => vec![o],
    };
    let mut out = vec![];
    for t in terms {
        let factors: Vec<&Expr> = match t {
            Expr::Mul(v) => v.iter().collect(),
            o => vec![o],
        };
        let mut c = r.int(1.into());
        let mut ex = vec![0u32; vars.len()];
        for f in factors {
            let (base, k) = match f {
                Expr::Pow(b, k) => (&**b, k.num().filter(|n| n.is_integer()).and_then(|n| n.as_int())?),
                o => (o, 1),
            };
            match base {
                Expr::Sym(s) if k >= 0 => {
                    let i = vars.iter().position(|v| v == s)?;
                    ex[i] += k as u32;
                }
                _ => {
                    let v = r.from_expr(f).ok()?;
                    c = c.mul(&v);
                }
            }
        }
        out.push((c, ex));
    }
    Some(out)
}
