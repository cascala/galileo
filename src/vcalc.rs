//! Multivariable calculus: gradient, divergence, curl and Laplacian in
//! Cartesian, polar, cylindrical and spherical coordinates; Jacobian and
//! Hessian matrices; scalar potentials; real solutions of polynomial systems
//! (for critical points and Lagrange multipliers); numeric minimization.

use crate::build::*;
use crate::calc::{deriv, expand, simplify, subst};
use crate::cas;
use crate::eval::R;
use crate::expr::{Expr, Func};
use crate::numeric;
use crate::ratfun::tidy;


fn clean(e: &Expr) -> Expr {
    tidy(&simplify(e))
}

// ---------- coordinate systems ----------

pub const SYSTEMS: &str = "\"cartesian\", \"polar\", \"cylindrical\" or \"spherical\"";

/// Scale factors h_i of an orthogonal coordinate system, so that
/// ds^2 = sum (h_i dq_i)^2. Spherical is (r, theta, phi) with theta measured
/// from the z axis (the physics convention).
pub fn scale_factors(system: &str, vars: &[String]) -> R<Vec<Expr>> {
    let v = |k: usize| Expr::sym(&vars[k]);
    let need = |n: usize| -> R<()> {
        if vars.len() == n {
            Ok(())
        } else {
            Err(format!(
                "{} coordinates need {} variables, e.g. {}",
                system,
                n,
                match system {
                    "polar" => "[r theta]",
                    "cylindrical" => "[r theta z]",
                    _ => "[r theta phi]",
                }
            ))
        }
    };
    Ok(match system {
        "cartesian" => vec![Expr::one(); vars.len()],
        "polar" => {
            need(2)?;
            vec![Expr::one(), v(0)]
        }
        "cylindrical" => {
            need(3)?;
            vec![Expr::one(), v(0), Expr::one()]
        }
        "spherical" => {
            need(3)?;
            vec![Expr::one(), v(0), mul2(v(0), func(Func::Sin, v(1)))]
        }
        other => return Err(format!("unknown coordinate system \"{}\"; use {}", other, SYSTEMS)),
    })
}

pub fn grad(f: &Expr, vars: &[String], h: &[Expr]) -> Vec<Expr> {
    vars.iter()
        .zip(h)
        .map(|(x, hi)| clean(&div(deriv(f, x), hi.clone())))
        .collect()
}

pub fn divergence(fs: &[Expr], vars: &[String], h: &[Expr]) -> Expr {
    let big_h = mul(h.to_vec());
    let terms = (0..vars.len())
        .map(|i| {
            let w = div(mul2(fs[i].clone(), big_h.clone()), h[i].clone());
            deriv(&w, &vars[i])
        })
        .collect();
    clean(&div(add(terms), big_h))
}

/// Curl of a 3D field; for a 2D field [P Q] the scalar dQ/dx - dP/dy
pub fn curl(fs: &[Expr], vars: &[String], h: &[Expr]) -> R<Expr> {
    match vars.len() {
        2 => {
            let t = sub(
                deriv(&mul2(h[1].clone(), fs[1].clone()), &vars[0]),
                deriv(&mul2(h[0].clone(), fs[0].clone()), &vars[1]),
            );
            Ok(clean(&div(t, mul2(h[0].clone(), h[1].clone()))))
        }
        3 => {
            let c = |_i: usize, j: usize, k: usize| {
                let t = sub(
                    deriv(&mul2(h[k].clone(), fs[k].clone()), &vars[j]),
                    deriv(&mul2(h[j].clone(), fs[j].clone()), &vars[k]),
                );
                clean(&div(t, mul2(h[j].clone(), h[k].clone())))
            };
            Ok(Expr::Matrix(vec![vec![c(0, 1, 2), c(1, 2, 0), c(2, 0, 1)]]))
        }
        _ => Err("curl needs a field in 2 or 3 variables".into()),
    }
}

pub fn laplacian(f: &Expr, vars: &[String], h: &[Expr]) -> Expr {
    let g = grad(f, vars, h);
    divergence(&g, vars, h)
}

pub fn jacobian(fs: &[Expr], vars: &[String]) -> Vec<Vec<Expr>> {
    fs.iter()
        .map(|f| vars.iter().map(|x| clean(&deriv(f, x))).collect())
        .collect()
}

pub fn hessian(f: &Expr, vars: &[String]) -> Vec<Vec<Expr>> {
    let g: Vec<Expr> = vars.iter().map(|x| deriv(f, x)).collect();
    jacobian(&g, vars)
}

/// A scalar potential phi with grad phi = F (Cartesian), or None when F is
/// not conservative (its Jacobian is not symmetric) or the integrals fail
pub fn potential(fs: &[Expr], vars: &[String]) -> Option<Expr> {
    let n = vars.len();
    for i in 0..n {
        for j in i + 1..n {
            if !cas::is_zero(&sub(deriv(&fs[i], &vars[j]), deriv(&fs[j], &vars[i]))) {
                return None;
            }
        }
    }
    let mut phi = Expr::zero();
    for i in 0..n {
        // what phi still misses in direction i depends only on vars[i..]
        let g = clean(&sub(fs[i].clone(), deriv(&phi, &vars[i])));
        if g.is_zero() {
            continue;
        }
        let gi = cas::antiderivative(&g, &vars[i])?;
        phi = add2(phi, gi);
    }
    let phi = clean(&phi);
    let ok = (0..n).all(|i| cas::is_zero(&sub(deriv(&phi, &vars[i]), fs[i].clone())));
    ok.then_some(phi)
}

// ---------- polynomial systems ----------

/// numerator of e over a common denominator, expanded, without factors e^u
/// (which are never 0)
fn numerator(e: &Expr) -> Expr {
    let t = crate::calc::together(e);
    let (n, _) = crate::calc::num_den(&t);
    let is_exp = |f: &Expr| matches!(f, Expr::Pow(b, _) if **b == Expr::Const(crate::expr::Const::E));
    let has_exp = |e: &Expr| {
        let mut found = false;
        let mut stack = vec![e.clone()];
        while let Some(x) = stack.pop() {
            if is_exp(&x) {
                found = true;
                break;
            }
            match x {
                Expr::Add(v) | Expr::Mul(v) => stack.extend(v),
                Expr::Pow(a, b) => {
                    stack.push(*a);
                    stack.push(*b);
                }
                _ => {}
            }
        }
        found
    };
    let n = if has_exp(&n) { crate::calc::factor(&n) } else { n };
    let n = match n {
        Expr::Mul(fs) => mul(fs.into_iter().filter(|f| !is_exp(f)).collect()),
        f if is_exp(&f) => Expr::one(),
        other => other,
    };
    expand(&n)
}

fn degree_in(e: &Expr, x: &str) -> Option<usize> {
    if !e.contains_sym(x) {
        return Some(0);
    }
    cas::poly_coeffs(e, x).map(|c| c.len() - 1)
}

fn real_value(e: &Expr) -> Option<f64> {
    let v = numeric::value(e);
    v.is_finite().then_some(v)
}

/// Distinct non-constant factors of a polynomial (or just itself)
fn factors(e: &Expr) -> Vec<Expr> {
    if e.size() > 300 {
        return vec![e.clone()];
    }
    match crate::factor_cmd::factor_parts(e) {
        Some((_, fs)) if fs.len() >= 2 => fs
            .into_iter()
            .map(|(f, _)| f)
            .filter(|f| !f.symbols().is_empty())
            .collect(),
        Some((_, fs)) if fs.len() == 1 && fs[0].1 > 1 => vec![fs[0].0.clone()],
        _ => vec![e.clone()],
    }
}

/// Sylvester resultant of two polynomials in x
pub fn resultant(p: &Expr, q: &Expr, x: &str) -> Option<Expr> {
    let a = cas::poly_coeffs(p, x)?;
    let b = cas::poly_coeffs(q, x)?;
    let (m, n) = (a.len() - 1, b.len() - 1);
    if m + n == 0 {
        return Some(Expr::one());
    }
    if m + n > 12 {
        return None;
    }
    let size = m + n;
    let mut s = vec![vec![Expr::zero(); size]; size];
    // coefficients from the highest power down
    for r in 0..n {
        for (k, c) in a.iter().rev().enumerate() {
            s[r][r + k] = c.clone();
        }
    }
    for r in 0..m {
        for (k, c) in b.iter().rev().enumerate() {
            s[n + r][r + k] = c.clone();
        }
    }
    crate::linalg::det(&s).ok().map(|d| expand(&d))
}

const MAX_SOLUTIONS: usize = 200;

/// Real roots of a polynomial in x with numeric coefficients: exact when the
/// solver finds them, else floats
fn real_roots(p: &Expr, x: &str) -> Vec<Expr> {
    if let Ok(rs) = cas::solve(p, x) {
        let reals: Vec<Expr> = rs
            .into_iter()
            .filter(|r| r.symbols().is_empty() && real_value(r).is_some())
            .map(|r| if r.size() > 80 { Expr::float(numeric::value(&r)) } else { r })
            .collect();
        return reals;
    }
    let Some(c) = cas::poly_coeffs(p, x) else {
        return vec![];
    };
    let cf: Vec<f64> = c.iter().map(numeric::value).collect();
    if cf.iter().any(|v| !v.is_finite()) {
        return vec![];
    }
    let scale = cf.iter().fold(0.0f64, |m, v| m.max(v.abs()));
    cas::numeric_roots(&cf)
        .into_iter()
        .filter(|(re, im)| im.abs() <= 1e-9 * (1.0 + re.abs()))
        .map(|(re, _)| Expr::float(re))
        .filter(|_| scale > 0.0)
        .collect()
}

/// all eqs vanish (numerically) at the point
fn satisfies(eqs: &[Expr], vars: &[String], point: &[Expr]) -> bool {
    let names: Vec<&str> = vars.iter().map(|s| s.as_str()).collect();
    let vals: Vec<f64> = point.iter().map(numeric::value).collect();
    eqs.iter().all(|e| {
        let c = numeric::compile(e, &names);
        let v = numeric::run(&c, &vals);
        // scale by the size of the terms so big coefficients don't fail the test
        let mag = match e {
            Expr::Add(ts) => ts
                .iter()
                .map(|t| numeric::run(&numeric::compile(t, &names), &vals).abs())
                .fold(1.0, f64::max),
            _ => 1.0,
        };
        v.is_finite() && v.abs() <= 1e-7 * mag
    })
}

/// decimals as exact fractions: 2.1 becomes 21/10 (from the shortest decimal
/// that prints as the same float)
pub(crate) fn exactify(e: &Expr) -> Expr {
    match e {
        Expr::Num(n) if !n.is_exact() => {
            let v = n.to_f64();
            let t = format!("{}", v);
            if !t.contains('e') && v.is_finite() {
                let (ip, fp) = t.split_once('.').unwrap_or((&t, ""));
                if fp.len() <= 18 {
                    if let Ok(num) = format!("{}{}", ip, fp).parse::<i128>() {
                        if let Some(q) = crate::num::Number::rat(num, 10i128.pow(fp.len() as u32)) {
                            return Expr::Num(q);
                        }
                    }
                }
            }
            e.clone()
        }
        _ => e.map_children(&mut |x| exactify(x)),
    }
}

pub fn has_float(e: &Expr) -> bool {
    match e {
        Expr::Num(n) => !n.is_exact(),
        Expr::Add(v) | Expr::Mul(v) | Expr::Apply(_, v) => v.iter().any(has_float),
        Expr::Pow(a, b) => has_float(a) || has_float(b),
        Expr::Func(_, a) => has_float(a),
        Expr::Matrix(m) => m.iter().flatten().any(has_float),
        _ => false,
    }
}

/// a number that is not real (contains i); symbolic values are not complex
fn complex(e: &Expr) -> bool {
    e.symbols().is_empty() && real_value(e).is_none()
}

/// a solution with free unknowns left in it, checked symbolically
fn satisfies_family(eqs: &[Expr], vars: &[String], point: &[Expr]) -> bool {
    eqs.iter().all(|e| {
        let mut x = e.clone();
        for (v, p) in vars.iter().zip(point) {
            if *p != Expr::sym(v) {
                x = subst(&x, v, p);
            }
        }
        cas::is_zero(&simplify(&x))
    })
}

fn push_unique(out: &mut Vec<Vec<Expr>>, s: Vec<Expr>) {
    if s.iter().any(|x| !x.symbols().is_empty()) {
        if !out.contains(&s) && out.len() < MAX_SOLUTIONS {
            out.push(s);
        }
        return;
    }
    let key: Vec<f64> = s.iter().map(numeric::value).collect();
    let same = out.iter().any(|o| {
        o.iter()
            .zip(&key)
            .all(|(a, b)| (numeric::value(a) - b).abs() <= 1e-9 * (1.0 + b.abs()))
    });
    if !same && out.len() < MAX_SOLUTIONS {
        out.push(s);
    }
}

/// Real solutions of a system of polynomial equations (each = 0); each
/// solution lists the values of vars in order. None when the system is not
/// polynomial or too big for elimination.
pub fn solve_system(eqs: &[Expr], vars: &[String]) -> Option<Vec<Vec<Expr>>> {
    // decimals become exact fractions (2.1 = 21/10), and the answers floats again
    let floats = eqs.iter().any(has_float);
    let eqs: Vec<Expr> = eqs
        .iter()
        .map(|e| if floats { exactify(e) } else { e.clone() })
        .map(|e| numerator(&e))
        .collect();
    for e in &eqs {
        for v in vars {
            degree_in(e, v)?;
        }
    }
    let mut sols = sys(eqs.clone(), vars.to_vec(), 0)?;
    sols.retain(|s| {
        if s.iter().any(|x| !x.symbols().is_empty()) {
            satisfies_family(&eqs, vars, s)
        } else {
            satisfies(&eqs, vars, s)
        }
    });
    if floats {
        for s in sols.iter_mut() {
            for x in s.iter_mut() {
                *x = crate::calc::evalf(x);
            }
        }
    }
    sols.sort_by(|a, b| {
        for (x, y) in a.iter().zip(b) {
            let o = numeric::value(x).total_cmp(&numeric::value(y));
            if o.is_ne() {
                return o;
            }
        }
        std::cmp::Ordering::Equal
    });
    Some(sols)
}

fn sys(eqs: Vec<Expr>, vars: Vec<String>, depth: usize) -> Option<Vec<Vec<Expr>>> {
    if depth > 24 {
        return None;
    }
    let mut eqs: Vec<Expr> = eqs
        .into_iter()
        .map(|e| expand(&e))
        .filter(|e| !cas::is_zero(e))
        .collect();
    // a nonzero constant can't vanish
    if eqs.iter().any(|e| vars.iter().all(|v| !e.contains_sym(v))) {
        let all_const_ok = eqs
            .iter()
            .filter(|e| vars.iter().all(|v| !e.contains_sym(v)))
            .all(|e| e.symbols().is_empty() && real_value(e).is_some_and(|v| v.abs() < 1e-12));
        if !all_const_ok {
            return Some(vec![]);
        }
        eqs.retain(|e| vars.iter().any(|v| e.contains_sym(v)));
    }
    if vars.is_empty() {
        return Some(vec![vec![]]);
    }
    if eqs.is_empty() {
        // underdetermined: the remaining unknowns are free (a curve or
        // surface of solutions); they stay as symbols
        return Some(vec![vars.iter().map(|v| Expr::sym(v)).collect()]);
    }
    // split a factored equation: x*(y - 1) = 0 means x = 0 or y = 1
    for (i, e) in eqs.iter().enumerate() {
        let fs = factors(e);
        if fs.len() >= 2 || (fs.len() == 1 && fs[0] != *e) {
            let mut out = vec![];
            for f in fs {
                let mut sub_eqs = eqs.clone();
                sub_eqs[i] = f;
                for s in sys(sub_eqs, vars.clone(), depth + 1)? {
                    push_unique(&mut out, s);
                }
            }
            return Some(out);
        }
    }
    // an equation linear in some variable with a constant coefficient: substitute
    for (i, e) in eqs.iter().enumerate() {
        for (k, v) in vars.iter().enumerate() {
            let Some(c) = cas::poly_coeffs(e, v) else { continue };
            if c.len() != 2 || vars.iter().any(|w| c[1].contains_sym(w)) || cas::is_zero(&c[1]) {
                continue;
            }
            let val = clean(&div(neg(c[0].clone()), c[1].clone()));
            let rest: Vec<Expr> = eqs
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, o)| numerator(&subst(o, v, &val)))
                .collect();
            let mut others = vars.clone();
            others.remove(k);
            let mut out = vec![];
            for s in sys(rest, others.clone(), depth + 1)? {
                let mut x = val.clone();
                for (w, sv) in others.iter().zip(&s) {
                    x = subst(&x, w, sv);
                }
                let x = clean(&x);
                if complex(&x) {
                    continue;
                }
                let mut full = s.clone();
                full.insert(k, x);
                push_unique(&mut out, full);
            }
            return Some(out);
        }
    }
    // an equation linear in v with a coefficient c that depends on the other
    // unknowns: either c != 0 and v = -rest/c, or c = 0 and rest = 0
    let mut best: Option<(usize, usize, Expr, Expr)> = None;
    for (i, e) in eqs.iter().enumerate() {
        for (k, v) in vars.iter().enumerate() {
            let Some(c) = cas::poly_coeffs(e, v) else { continue };
            if c.len() != 2 || cas::is_zero(&c[1]) {
                continue;
            }
            if best.as_ref().is_none_or(|b| c[1].size() < b.3.size()) {
                best = Some((i, k, c[0].clone(), c[1].clone()));
            }
        }
    }
    if let Some((i, k, c0, c1)) = best {
        let v = vars[k].clone();
        let val = div(neg(c0.clone()), c1.clone());
        let rest: Vec<Expr> = eqs
            .iter()
            .enumerate()
            .filter(|(j, _)| *j != i)
            .map(|(_, o)| numerator(&subst(o, &v, &val)))
            .collect();
        let mut others = vars.clone();
        others.remove(k);
        let mut out = vec![];
        for s in sys(rest, others.clone(), depth + 1)? {
            let mut c = c1.clone();
            let mut x = val.clone();
            for (w, sv) in others.iter().zip(&s) {
                c = subst(&c, w, sv);
                x = subst(&x, w, sv);
            }
            // c = 0 belongs to the other branch
            if cas::is_zero(&c) || real_value(&c).is_some_and(|cv| cv.abs() < 1e-12) || complex(&c) {
                continue;
            }
            let x = clean(&x);
            if complex(&x) {
                continue;
            }
            let mut full = s.clone();
            full.insert(k, x);
            push_unique(&mut out, full);
        }
        let mut zero = eqs.clone();
        zero[i] = c1;
        zero.push(c0);
        for s in sys(zero, vars.clone(), depth + 1)? {
            push_unique(&mut out, s);
        }
        return Some(out);
    }
    // an equation in one variable: solve it and substitute
    for (i, e) in eqs.iter().enumerate() {
        let present: Vec<usize> = (0..vars.len()).filter(|&k| e.contains_sym(&vars[k])).collect();
        if present.len() != 1 {
            continue;
        }
        let k = present[0];
        let v = &vars[k];
        let mut others = vars.clone();
        others.remove(k);
        let mut out = vec![];
        for r in real_roots(e, v) {
            let rest: Vec<Expr> = eqs
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, o)| numerator(&subst(o, v, &r)))
                .collect();
            for s in sys(rest, others.clone(), depth + 1)? {
                let mut full = s;
                full.insert(k, r.clone());
                push_unique(&mut out, full);
            }
        }
        return Some(out);
    }
    // eliminate a variable with resultants
    let (k, _) = vars
        .iter()
        .enumerate()
        .map(|(k, v)| (k, eqs.iter().filter_map(|e| degree_in(e, v)).max().unwrap_or(0)))
        .filter(|(_, d)| *d > 0)
        .min_by_key(|(_, d)| *d)?;
    let v = vars[k].clone();
    let with: Vec<&Expr> = eqs.iter().filter(|e| e.contains_sym(&v)).collect();
    let without: Vec<Expr> = eqs.iter().filter(|e| !e.contains_sym(&v)).cloned().collect();
    let pivot = with
        .iter()
        .min_by_key(|e| degree_in(e, &v).unwrap_or(99))
        .copied()?
        .clone();
    let mut reduced = without;
    for e in &with {
        if **e == pivot {
            continue;
        }
        let r = resultant(&pivot, e, &v)?;
        if !cas::is_zero(&r) {
            reduced.push(r);
        }
    }
    let mut others = vars.clone();
    others.remove(k);
    if reduced.is_empty() {
        return None;
    }
    let mut out = vec![];
    for s in sys(reduced, others.clone(), depth + 1)? {
        // back-substitute and solve for v
        let mut here: Vec<Expr> = eqs.clone();
        for (w, sv) in others.iter().zip(&s) {
            here = here.iter().map(|e| subst(e, w, sv)).collect();
        }
        let here: Vec<Expr> = here.into_iter().map(|e| expand(&e)).filter(|e| !cas::is_zero(e)).collect();
        let Some(p) = here
            .iter()
            .filter(|e| e.contains_sym(&v))
            .min_by_key(|e| degree_in(e, &v).unwrap_or(99))
        else {
            continue;
        };
        for r in real_roots(p, &v) {
            let mut full = s.clone();
            full.insert(k, r);
            let mut names = others.clone();
            names.insert(k, v.clone());
            if satisfies(&eqs, &names, &full) {
                push_unique(&mut out, full);
            }
        }
    }
    Some(out)
}

/// Newton's method for a square system from a starting point
pub fn newton(eqs: &[Expr], vars: &[String], x0: &[f64]) -> R<Vec<f64>> {
    let names: Vec<&str> = vars.iter().map(|s| s.as_str()).collect();
    let f: Vec<numeric::Code> = eqs.iter().map(|e| numeric::compile(e, &names)).collect();
    let jac: Vec<Vec<numeric::Code>> = eqs
        .iter()
        .map(|e| vars.iter().map(|v| numeric::compile(&deriv(e, v), &names)).collect())
        .collect();
    let n = vars.len();
    if eqs.len() != n {
        return Err("give as many equations as unknowns".into());
    }
    let mut x = x0.to_vec();
    for _ in 0..200 {
        let fv: Vec<f64> = f.iter().map(|c| numeric::run(c, &x)).collect();
        let norm = fv.iter().map(|v| v * v).sum::<f64>().sqrt();
        if !norm.is_finite() {
            return Err("the equations have no real value near the guess".into());
        }
        if norm < 1e-14 {
            return Ok(x);
        }
        let j: Vec<Vec<f64>> = jac
            .iter()
            .map(|r| r.iter().map(|c| numeric::run(c, &x)).collect())
            .collect();
        let dx = lin_solve(j, fv.iter().map(|v| -v).collect()).ok_or("the Jacobian is singular at the guess; try another one")?;
        // damped step: halve until the residual goes down
        let mut t = 1.0;
        loop {
            let xn: Vec<f64> = x.iter().zip(&dx).map(|(a, d)| a + t * d).collect();
            let nn = f
                .iter()
                .map(|c| numeric::run(c, &xn).powi(2))
                .sum::<f64>()
                .sqrt();
            if nn.is_finite() && (nn < norm || t < 1e-6) {
                x = xn;
                break;
            }
            t /= 2.0;
        }
        if dx.iter().map(|d| d.abs()).fold(0.0, f64::max) < 1e-15 {
            break;
        }
    }
    let fv: Vec<f64> = f.iter().map(|c| numeric::run(c, &x)).collect();
    if fv.iter().all(|v| v.abs() < 1e-9) {
        Ok(x)
    } else {
        Err("Newton's method did not converge from this guess".into())
    }
}

/// Gaussian elimination with partial pivoting
fn lin_solve(mut a: Vec<Vec<f64>>, mut b: Vec<f64>) -> Option<Vec<f64>> {
    let n = b.len();
    for c in 0..n {
        let p = (c..n).max_by(|&i, &j| a[i][c].abs().total_cmp(&a[j][c].abs()))?;
        if a[p][c].abs() < 1e-300 {
            return None;
        }
        a.swap(c, p);
        b.swap(c, p);
        for r in c + 1..n {
            let f = a[r][c] / a[c][c];
            for k in c..n {
                a[r][k] -= f * a[c][k];
            }
            b[r] -= f * b[c];
        }
    }
    let mut x = vec![0.0; n];
    for r in (0..n).rev() {
        let s: f64 = (r + 1..n).map(|k| a[r][k] * x[k]).sum();
        x[r] = (b[r] - s) / a[r][r];
    }
    Some(x)
}

/// Eigenvalues of a symmetric matrix (Jacobi rotations), ascending
pub fn sym_eigenvalues(m: &[Vec<f64>]) -> Vec<f64> {
    let n = m.len();
    let mut a = m.to_vec();
    for _ in 0..100 {
        let off: f64 = (0..n)
            .flat_map(|i| (0..n).filter(move |&j| j != i).map(move |j| (i, j)))
            .map(|(i, j)| a[i][j] * a[i][j])
            .sum();
        if off < 1e-30 {
            break;
        }
        for p in 0..n {
            for q in p + 1..n {
                if a[p][q].abs() < 1e-300 {
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
            }
        }
    }
    let mut ev: Vec<f64> = (0..n).map(|i| a[i][i]).collect();
    ev.sort_by(|x, y| x.total_cmp(y));
    ev
}

/// "min", "max", "saddle" or "inconclusive" from the Hessian's eigenvalues
pub fn classify(h: &[Vec<f64>]) -> &'static str {
    let ev = sym_eigenvalues(h);
    let scale = ev.iter().fold(1e-300f64, |m, v| m.max(v.abs()));
    let tol = 1e-10 * scale.max(1.0);
    if ev.iter().any(|v| !v.is_finite()) {
        "inconclusive"
    } else if ev.iter().all(|v| *v > tol) {
        "min"
    } else if ev.iter().all(|v| *v < -tol) {
        "max"
    } else if ev.iter().any(|v| *v > tol) && ev.iter().any(|v| *v < -tol) {
        "saddle"
    } else {
        "inconclusive"
    }
}

/// BFGS with a backtracking line search, from x0; returns (point, value, iterations)
pub fn bfgs(f: &Expr, vars: &[String], x0: &[f64]) -> R<(Vec<f64>, f64, usize)> {
    let names: Vec<&str> = vars.iter().map(|s| s.as_str()).collect();
    let fc = numeric::compile(f, &names);
    let gc: Vec<numeric::Code> = vars.iter().map(|v| numeric::compile(&deriv(f, v), &names)).collect();
    let n = vars.len();
    let val = |x: &[f64]| numeric::run(&fc, x);
    let grad = |x: &[f64]| gc.iter().map(|c| numeric::run(c, x)).collect::<Vec<f64>>();
    let mut x = x0.to_vec();
    let mut fx = val(&x);
    if !fx.is_finite() {
        return Err("the function has no real value at the starting point".into());
    }
    let mut g = grad(&x);
    let mut h: Vec<Vec<f64>> = (0..n).map(|i| (0..n).map(|j| if i == j { 1.0 } else { 0.0 }).collect()).collect();
    for it in 0..2000 {
        let gn = g.iter().map(|v| v * v).sum::<f64>().sqrt();
        if gn < 1e-10 * (1.0 + fx.abs()) {
            return Ok((x, fx, it));
        }
        let mut d: Vec<f64> = (0..n).map(|i| -(0..n).map(|j| h[i][j] * g[j]).sum::<f64>()).collect();
        let mut slope: f64 = d.iter().zip(&g).map(|(a, b)| a * b).sum();
        if slope >= 0.0 {
            // not a descent direction: reset to steepest descent
            d = g.iter().map(|v| -v).collect();
            slope = -gn * gn;
            for (i, row) in h.iter_mut().enumerate() {
                for (j, v) in row.iter_mut().enumerate() {
                    *v = if i == j { 1.0 } else { 0.0 };
                }
            }
        }
        let mut t = 1.0;
        let (xn, fnew) = loop {
            let xn: Vec<f64> = x.iter().zip(&d).map(|(a, b)| a + t * b).collect();
            let fv = val(&xn);
            if fv.is_finite() && fv <= fx + 1e-4 * t * slope {
                break (xn, fv);
            }
            t /= 2.0;
            if t < 1e-20 {
                return Ok((x, fx, it));
            }
        };
        let gnew = grad(&xn);
        let s: Vec<f64> = xn.iter().zip(&x).map(|(a, b)| a - b).collect();
        let y: Vec<f64> = gnew.iter().zip(&g).map(|(a, b)| a - b).collect();
        let sy: f64 = s.iter().zip(&y).map(|(a, b)| a * b).sum();
        if sy > 1e-300 {
            let hy: Vec<f64> = (0..n).map(|i| (0..n).map(|j| h[i][j] * y[j]).sum()).collect();
            let yhy: f64 = y.iter().zip(&hy).map(|(a, b)| a * b).sum();
            for i in 0..n {
                for j in 0..n {
                    h[i][j] += (sy + yhy) * s[i] * s[j] / (sy * sy) - (hy[i] * s[j] + s[i] * hy[j]) / sy;
                }
            }
        }
        if (fx - fnew).abs() < 1e-16 * (1.0 + fx.abs()) && s.iter().all(|v| v.abs() < 1e-14) {
            return Ok((xn, fnew, it));
        }
        x = xn;
        fx = fnew;
        g = gnew;
        if !fx.is_finite() || fx < -1e300 {
            return Err("the function decreases without bound".into());
        }
    }
    Ok((x, fx, 2000))
}

/// The value of f at a point given as expressions
pub fn at(f: &Expr, vars: &[String], point: &[Expr]) -> Expr {
    let mut e = f.clone();
    for (v, p) in vars.iter().zip(point) {
        e = subst(&e, v, p);
    }
    clean(&e)
}

pub fn hessian_at(f: &Expr, vars: &[String], point: &[f64]) -> Vec<Vec<f64>> {
    let names: Vec<&str> = vars.iter().map(|s| s.as_str()).collect();
    hessian(f, vars)
        .iter()
        .map(|r| r.iter().map(|e| numeric::run(&numeric::compile(e, &names), point)).collect())
        .collect()
}

/// The sentence for notes when some solutions have free unknowns
pub fn family_note(sols: &[Vec<Expr>]) -> &'static str {
    if sols.iter().any(|s| s.iter().any(|x| !x.symbols().is_empty())) {
        " A row that still contains an unknown is a whole family of solutions: that unknown can take any value (for example [y, y] means every point with x = y)."
    } else {
        ""
    }
}
