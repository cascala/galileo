//! More exact ODE solving: separable first-order equations y' = f(x) g(y)
//! and linear systems X' = A X + b with constant coefficients.

use crate::build::*;
use crate::calc::{num_den, simplify, subst, together};
use crate::cas;
use crate::expr::{Const, Expr, Func};
use crate::linalg::{self, Mat};
use crate::print::show;

type R<T> = Result<T, String>;

fn free(e: &Expr, v: &str) -> bool {
    !e.contains_sym(v)
}

fn shortest(c: Vec<Expr>) -> Expr {
    c.into_iter().min_by_key(|e| show(e).len()).unwrap()
}

fn tidy(e: &Expr) -> Expr {
    let a = simplify(e);
    let b = crate::ratfun::tidy(&a);
    shortest(vec![b, a])
}

/// r(x, y) = f(x) g(y): Some((f, g))
pub fn split_separable(r: &Expr, y: &str, x: &str) -> Option<(Expr, Expr)> {
    let r = simplify(r);
    if free(&r, x) {
        return Some((Expr::one(), r));
    }
    if free(&r, y) {
        return Some((r, Expr::one()));
    }
    for x0 in [Expr::zero(), Expr::one(), Expr::int(2), Expr::int(3)] {
        let g = simplify(&subst(&r, x, &x0));
        if cas::is_zero(&g) || g.contains_sym(x) {
            continue;
        }
        let f = div(r.clone(), g.clone());
        for cand in [simplify(&f), together(&f), crate::ratfun::tidy(&f)] {
            if free(&cand, y) {
                return Some((tidy(&cand), g));
            }
        }
        return None;
    }
    None
}

/// all y-terms of an Add are c*log(u): one log of the product, with the
/// common size of the coefficients pulled out (2 log a - 2 log b = 2 log(a/b))
fn combine_logs(v: &[Expr]) -> Option<Expr> {
    let mut parts = vec![];
    for t in v {
        let (c, f) = match t {
            Expr::Mul(w) if w.len() == 2 && w[0].num().is_some() => (w[0].num().unwrap(), w[1].clone()),
            o => (crate::num::Number::one(), o.clone()),
        };
        let Expr::Func(Func::Log, u) = f else { return None };
        parts.push((c, (*u).clone()));
    }
    let c0 = parts[0].0.abs();
    if parts.iter().all(|(c, _)| c.abs() == c0) {
        let inner = mul(parts.iter().map(|(c, u)| pow(u.clone(), Expr::int(if c.is_negative() { -1 } else { 1 }))).collect());
        return Some(mul2(Expr::Num(c0), func(Func::Log, inner)));
    }
    Some(func(Func::Log, mul(parts.into_iter().map(|(c, u)| pow(u, Expr::Num(c))).collect())))
}

/// solve lhs(y) = rhs for y by peeling off one function at a time
pub fn invert(lhs: &Expr, rhs: &Expr, y: &str) -> Option<Expr> {
    let (mut l, mut r) = (lhs.clone(), rhs.clone());
    for _ in 0..12 {
        match &l {
            Expr::Sym(s) if s == y => return Some(tidy(&r)),
            Expr::Add(v) => {
                let (c, ys): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| free(t, y));
                if !c.is_empty() {
                    r = sub(r, add(c));
                    l = add(ys);
                } else if let Some(g) = combine_logs(&ys) {
                    l = g;
                } else {
                    break;
                }
            }
            Expr::Mul(v) => {
                let (c, ys): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| free(t, y));
                if c.is_empty() {
                    break;
                }
                r = div(r, mul(c));
                l = mul(ys);
            }
            Expr::Func(Func::Log, u) => {
                r = exp(r);
                l = (**u).clone();
            }
            // |u| = R: the sign goes into the constant
            Expr::Func(Func::Abs, u) => l = (**u).clone(),
            Expr::Func(f, u) => {
                let inv = match f {
                    Func::Atan => Func::Tan,
                    Func::Tan => Func::Atan,
                    Func::Asin => Func::Sin,
                    Func::Sin => Func::Asin,
                    Func::Atanh => Func::Tanh,
                    Func::Tanh => Func::Atanh,
                    Func::Sinh => Func::Asinh,
                    Func::Asinh => Func::Sinh,
                    _ => break,
                };
                r = func(inv, r);
                l = (**u).clone();
            }
            Expr::Pow(b, u) if **b == Expr::Const(Const::E) => {
                r = func(Func::Log, r);
                l = (**u).clone();
            }
            Expr::Pow(b, k) if free(k, y) && k.num().is_some() => {
                // u^k = R -> u = R^(1/k) (the principal root)
                r = pow(r, pow((**k).clone(), Expr::int(-1)));
                l = (**b).clone();
            }
            _ => break,
        }
    }
    // a rational equation in y: the polynomial solver
    let (n, _) = num_den(&together(&sub(l, r)));
    let roots = cas::solve(&simplify(&n), y).ok()?;
    if roots.len() == 1 {
        Some(tidy(&roots[0]))
    } else {
        None
    }
}

/// exp(C1 + ...) -> C1 exp(...): rename e^C1 as the constant
fn absorb(sol: &Expr, c: &str) -> Expr {
    fn go(e: &Expr, c: &Expr) -> Expr {
        match e {
            Expr::Pow(b, u) if **b == Expr::Const(Const::E) => match &**u {
                // exp(a C1 + rest) with a constant -> C1 exp(rest)
                o if o.contains_sym(&show(c)) && !matches!(o, Expr::Add(v) if v.contains(c)) => {
                    let a = simplify(&crate::calc::deriv(o, &show(c)));
                    if a.symbols().is_empty() && !cas::is_zero(&a) {
                        mul2(c.clone(), exp(simplify(&subst(o, &show(c), &Expr::zero()))))
                    } else {
                        exp(go(o, c))
                    }
                }
                Expr::Add(v) if v.contains(c) => {
                    let rest: Vec<Expr> = v.iter().filter(|t| *t != c).cloned().collect();
                    mul2(c.clone(), exp(add(rest)))
                }
                o if o == c => c.clone(),
                o => exp(go(o, c)),
            },
            Expr::Add(v) => add(v.iter().map(|t| go(t, c)).collect()),
            Expr::Mul(v) => mul(v.iter().map(|t| go(t, c)).collect()),
            Expr::Pow(b, k) => pow(go(b, c), go(k, c)),
            Expr::Func(f, a) => func(*f, go(a, c)),
            o => o.clone(),
        }
    }
    let alt = go(sol, &Expr::sym(c));
    if alt == *sol {
        return sol.clone();
    }
    let alt = tidy(&alt);
    // every C1 must have been inside an exponential
    let still = go(&alt, &Expr::sym(c));
    if still != alt { sol.clone() } else { alt }
}

/// the sum-of-terms form when it is not much longer
pub fn prefer_expanded(e: &Expr) -> Expr {
    let a = tidy(e);
    let ex = crate::calc::expand;
    let b = shortest(vec![simplify(&ex(e)), ex(&simplify(e)), ex(&simplify(&ex(e)))]);
    if show(&b).len() * 10 <= show(&a).len() * 13 { b } else { a }
}

fn has_i(e: &Expr) -> bool {
    match e {
        Expr::Const(Const::I) => true,
        Expr::Add(v) | Expr::Mul(v) => v.iter().any(has_i),
        Expr::Pow(a, b) => has_i(a) || has_i(b),
        Expr::Func(_, a) => has_i(a),
        _ => false,
    }
}

pub enum Separable {
    Explicit(Expr),
    Implicit(Expr, Expr),
}

/// y' = r(x, y) with r = f(x) g(y): the general solution with constant C1,
/// or the particular one through (x0, y0)
pub fn separable(r: &Expr, y: &str, x: &str, ic: Option<(&Expr, &Expr)>) -> R<(Separable, Vec<String>)> {
    let (f, g) = split_separable(r, y, x).ok_or(
        "dsolve: this first-order equation is neither linear nor separable (y' = f(x) g(y)); use odeplot for a numeric solution",
    )?;
    let mut notes = vec![format!(
        "Separable: {y}' = f({x}) g({y}) with f({x}) = {} and g({y}) = {}.",
        show(&f),
        show(&g)
    )];
    let gi = cas::antiderivative(&simplify(&pow(g.clone(), Expr::int(-1))), y)
        .ok_or_else(|| format!("dsolve: can't integrate 1/({})", show(&g)))?;
    let fi = cas::antiderivative(&f, x).ok_or_else(|| format!("dsolve: can't integrate {}", show(&f)))?;
    let (gi, fi) = (tidy(&gi), tidy(&fi));
    notes.push(format!(
        "Divide by g({y}) and integrate both sides: ∫ d{y}/g({y}) = ∫ f({x}) d{x}, so {} = {} + C1.",
        show(&gi),
        show(&fi)
    ));
    // constant solutions g(y) = 0 are lost when dividing
    if let Ok(zs) = cas::solve(&g, y) {
        let zs: Vec<Expr> = zs.into_iter().filter(|z| !has_i(z)).collect();
        if !zs.is_empty() && !free(&g, y) {
            notes.push(format!(
                "Dividing by g({y}) loses the constant solutions where g({y}) = 0: {y} = {}.",
                zs.iter().map(show).collect::<Vec<_>>().join(&format!(", {} = ", y))
            ));
        }
    }
    let c1 = Expr::sym("C1");
    let raw = invert(&gi, &add2(fi.clone(), c1.clone()), y);
    let explicit = raw.as_ref().map(|s| absorb(s, "C1"));
    let Some((x0, y0)) = ic else {
        return Ok(match explicit {
            Some(s) => {
                notes.push(format!("Solved for {y}; C1 is an arbitrary constant (e^C1 is renamed C1).").into());
                (Separable::Explicit(s), notes)
            }
            None => {
                notes.push(format!("This can't be solved for {y} in closed form; the answer is the implicit equation.").into());
                (Separable::Implicit(gi, add2(fi, c1)), notes)
            }
        });
    };
    // the start value, and the equation itself just after x0 (picks the right branch of a root)
    let fits = |s: &Expr| {
        if !cas::is_zero(&simplify(&sub(subst(s, x, x0), y0.clone()))) {
            return false;
        }
        let ds = crate::calc::deriv(s, x);
        [0.05, 0.1].iter().all(|h| {
            let xv = add2(x0.clone(), Expr::float(*h));
            let yv = subst(s, x, &xv);
            let lhs = crate::numeric::value(&subst(&ds, x, &xv));
            let rhs = crate::numeric::value(&subst(&subst(r, y, &yv), x, &xv));
            lhs.is_finite() && rhs.is_finite() && (lhs - rhs).abs() <= 1e-6 * (1.0 + rhs.abs())
        })
    };
    // 1. solve the explicit form for its constant
    if let Some(s) = &explicit {
        let (n, _) = num_den(&together(&simplify(&sub(subst(s, x, x0), y0.clone()))));
        if let Ok(cs) = cas::solve(&simplify(&n), "C1") {
            for c in cs.into_iter().filter(|v| v.symbols().is_empty()) {
                let cand = tidy(&subst(s, "C1", &c));
                if fits(&cand) {
                    notes.push(format!("The start value {y}({}) = {} fixes the constant: C1 = {}.", show(x0), show(y0), show(&c)));
                    return Ok((Separable::Explicit(cand), notes));
                }
            }
        }
    }
    // 2. the implicit form is linear in C1
    let c = simplify(&sub(subst(&gi, y, y0), subst(&fi, x, x0)));
    if let Some(s) = &raw {
        let cand = tidy(&subst(s, "C1", &c));
        for cand in [cand.clone(), tidy(&neg(cand))] {
            if fits(&cand) {
                notes.push(format!("The start value {y}({}) = {} fixes C1 = {}.", show(x0), show(y0), show(&c)));
                return Ok((Separable::Explicit(cand), notes));
            }
        }
    }
    notes.push(format!("The start value fixes C1 = {}; the answer stays implicit.", show(&c)));
    Ok((Separable::Implicit(gi, tidy(&add2(fi, c))), notes))
}

/// X' = A X + b with constant A and b: X(t) = e^(A (t - t0)) (X0 - Xp) + Xp
/// where Xp = -A^(-1) b; without a start the constants are C1, C2, ...
pub fn linear_system(a: &Mat, b: &[Expr], t: &str, ic: Option<(&Expr, &[Expr])>) -> R<(Vec<Expr>, Vec<String>)> {
    let n = a.len();
    let tv = Expr::sym(t);
    let mut notes = vec![];
    let xp: Vec<Expr> = if b.iter().all(cas::is_zero) {
        vec![Expr::zero(); n]
    } else {
        let bm: Mat = b.iter().map(|v| vec![neg(v.clone())]).collect();
        let s = linalg::solve(a, &bm).map_err(|_| "dsolve: A is singular, so the forced system has no constant equilibrium; only X' = A X + b with invertible A is supported")?;
        let xp: Vec<Expr> = s.into_iter().map(|r| simplify(&r[0])).collect();
        notes.push(format!(
            "Equilibrium: A Xp + b = 0 gives Xp = [{}]; the deviation X - Xp solves the homogeneous system.",
            xp.iter().map(show).collect::<Vec<_>>().join(", ")
        ));
        xp
    };
    let (t0, start): (Expr, Vec<Expr>) = match ic {
        Some((t0, x0)) => {
            if x0.len() != n {
                return Err(format!("dsolve: the system has {} unknowns, so it needs {} start values", n, n));
            }
            (t0.clone(), x0.iter().zip(&xp).map(|(v, p)| sub(v.clone(), p.clone())).collect())
        }
        None => (Expr::zero(), (1..=n).map(|k| Expr::sym(&format!("C{}", k))).collect()),
    };
    let e = crate::decomp::expm_exact(a, Some(t))?;
    notes.push(format!(
        "The solution is X(t) = e^(A t) X(0){}, with e^(A t) from expm(A, t).",
        if xp.iter().all(cas::is_zero) { "" } else { " for the deviation" }
    ));
    let shifted = if t0.is_zero() { tv.clone() } else { sub(tv.clone(), t0) };
    let mut out = vec![];
    for i in 0..n {
        let mut acc = vec![xp[i].clone()];
        for j in 0..n {
            acc.push(mul2(subst(&e[i][j], t, &shifted), start[j].clone()));
        }
        let v = add(acc);
        out.push(prefer_expanded(&v));
    }
    Ok((out, notes))
}
