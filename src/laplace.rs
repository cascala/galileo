//! Partial fractions, the Laplace transform and its inverse.

use crate::build::*;
use crate::calc::{deriv, evalf, expand, simplify, subst};
use crate::expr::{Const, Expr, Func};
use crate::num::Number;
use num_bigint::BigInt;
use num_traits::One;

pub type R<T> = Result<T, String>;
type Poly = Vec<Number>;

fn trim(mut p: Poly) -> Poly {
    while p.len() > 1 && p.last().is_some_and(|c| c.is_zero()) {
        p.pop();
    }
    p
}

fn pmul(a: &[Number], b: &[Number]) -> Poly {
    let mut r = vec![Number::zero(); a.len() + b.len() - 1];
    for (i, x) in a.iter().enumerate() {
        for (j, y) in b.iter().enumerate() {
            r[i + j] = r[i + j].add(x.mul(y));
        }
    }
    trim(r)
}

fn pdivmod(a: &[Number], b: &[Number]) -> (Poly, Poly) {
    let b = trim(b.to_vec());
    let mut r = trim(a.to_vec());
    if r.len() < b.len() {
        return (vec![Number::zero()], r);
    }
    let mut q = vec![Number::zero(); r.len() - b.len() + 1];
    let lead = b.last().unwrap().clone();
    while r.len() >= b.len() && !(r.len() == 1 && r[0].is_zero()) {
        let k = r.len() - b.len();
        let c = r.last().unwrap().div(&lead).unwrap();
        q[k] = c.clone();
        for (i, bi) in b.iter().enumerate() {
            r[k + i] = r[k + i].sub(c.mul(bi));
        }
        r.pop();
        if r.is_empty() {
            r.push(Number::zero());
        }
        r = trim(r);
        if r.len() < b.len() {
            break;
        }
    }
    (trim(q), r)
}

fn to_expr(p: &[Number], x: &Expr) -> Expr {
    add(p
        .iter()
        .enumerate()
        .filter(|(_, c)| !c.is_zero())
        .map(|(k, c)| mul2(Expr::Num(c.clone()), pow(x.clone(), Expr::int(k as i128))))
        .collect())
}

fn exact_coeffs(e: &Expr, x: &str) -> Option<Poly> {
    let c = crate::cas::poly_coeffs(e, x)?;
    c.iter()
        .map(|v| simplify(v).num().filter(|n| n.is_exact() && n.big().is_some()))
        .collect::<Option<Vec<_>>>()
        .map(trim)
}

/// One partial fraction: numerator (degree < factor degree) over factor^power
pub struct Piece {
    pub num: Poly,
    pub factor: Poly,
    pub power: u32,
}

/// F = poly + sum of pieces, over the rationals
pub fn partial_fractions(f: &Expr, x: &str) -> R<(Poly, Vec<Piece>)> {
    let (n, d) = crate::calc::num_den(&crate::calc::together(f));
    let pn = exact_coeffs(&n, x).ok_or("partfrac: the numerator must be a polynomial with exact coefficients")?;
    let pd = exact_coeffs(&d, x).ok_or("partfrac: the denominator must be a polynomial with exact coefficients")?;
    if pd.len() <= 1 {
        return Ok((pdivmod(&pn, &pd).0, vec![]));
    }
    let (q, r) = pdivmod(&pn, &pd);
    // factor the denominator over the integers
    let lcm = pd.iter().fold(BigInt::one(), |acc, c| {
        let d = c.big().unwrap().denom().clone();
        num_integer::Integer::lcm(&acc, &d)
    });
    let zd: Vec<BigInt> = pd
        .iter()
        .map(|c| (c.big().unwrap() * num_rational::BigRational::from_integer(lcm.clone())).to_integer())
        .collect();
    let (_, facs) = crate::algfactor::factor_zpoly(&zd).ok_or("partfrac: the denominator is too large to factor")?;
    let facs: Vec<(Poly, u32)> = facs
        .into_iter()
        .map(|(p, m)| (p.into_iter().map(Number::from_bigint).collect(), m))
        .collect();
    // basis: x^k * D / f^j for each factor f, 1 <= j <= m, 0 <= k < deg f
    let deg = pd.len() - 1;
    let mut basis: Vec<Poly> = vec![];
    let mut idx: Vec<(usize, u32, usize)> = vec![];
    for (fi, (fp, m)) in facs.iter().enumerate() {
        let mut fpow = vec![Number::one()];
        for j in 1..=*m {
            fpow = pmul(&fpow, fp);
            let quot = pdivmod(&pd, &fpow).0;
            for k in 0..fp.len() - 1 {
                let mut xk = vec![Number::zero(); k];
                xk.push(Number::one());
                basis.push(pmul(&quot, &xk));
                idx.push((fi, j, k));
            }
        }
    }
    if basis.len() != deg {
        return Err("partfrac: could not split the denominator".into());
    }
    let mat: Vec<Vec<Expr>> = (0..deg)
        .map(|i| basis.iter().map(|b| Expr::Num(b.get(i).cloned().unwrap_or(Number::zero()))).collect())
        .collect();
    let rhs: Vec<Vec<Expr>> = (0..deg).map(|i| vec![Expr::Num(r.get(i).cloned().unwrap_or(Number::zero()))]).collect();
    let sol = crate::linalg::solve(&mat, &rhs).map_err(|_| "partfrac: no decomposition found".to_string())?;
    let mut pieces: Vec<Piece> = vec![];
    for (fi, (fp, m)) in facs.iter().enumerate() {
        for j in 1..=*m {
            let num: Poly = idx
                .iter()
                .zip(&sol)
                .filter(|((f, jj, _), _)| *f == fi && *jj == j)
                .map(|(_, v)| v[0].num().unwrap_or(Number::zero()))
                .collect();
            if num.iter().any(|c| !c.is_zero()) {
                pieces.push(Piece { num: trim(num), factor: fp.clone(), power: j });
            }
        }
    }
    Ok((q, pieces))
}

pub fn partfrac_expr(f: &Expr, x: &str) -> R<Expr> {
    let xv = Expr::sym(x);
    let (q, pieces) = partial_fractions(f, x)?;
    let mut terms = vec![to_expr(&q, &xv)];
    for p in &pieces {
        terms.push(div(to_expr(&p.num, &xv), pow(to_expr(&p.factor, &xv), Expr::int(p.power as i128))));
    }
    Ok(add(terms))
}

// ---------- the Laplace transform ----------

fn linear_in(u: &Expr, t: &str) -> Option<(Expr, Expr)> {
    let a = simplify(&deriv(u, t));
    if a.contains_sym(t) {
        return None;
    }
    let b = simplify(&subst(u, t, &Expr::zero()));
    Some((a, b))
}

fn factors(e: &Expr) -> Vec<Expr> {
    match e {
        Expr::Mul(v) => v.clone(),
        o => vec![o.clone()],
    }
}

/// y(t), y'(t), y''(t) as (name, order)
fn unknown(e: &Expr, t: &str) -> Option<(String, usize)> {
    if let Expr::Apply(n, a) = e {
        if a.len() == 1 && a[0] == Expr::sym(t) && !crate::special::is_special(n, 1) {
            let order = n.chars().rev().take_while(|c| *c == '\'').count();
            return Some((n[..n.len() - order].to_string(), order));
        }
    }
    None
}

/// L{f}(s), by the rules of the table
pub fn laplace(f: &Expr, t: &str, s: &str) -> R<Expr> {
    let e = expand(f);
    let terms = match &e {
        Expr::Add(v) => v.clone(),
        o => vec![o.clone()],
    };
    let mut out = vec![];
    for term in terms {
        out.push(laplace_term(&term, t, s, 0)?);
    }
    Ok(tidy(&add(out)))
}

fn tidy(e: &Expr) -> Expr {
    let a = simplify(e);
    let b = crate::ratfun::tidy(&a);
    [b, a, e.clone()].into_iter().min_by_key(|c| crate::print::show(c).len()).unwrap()
}

fn laplace_term(term: &Expr, t: &str, s: &str, depth: usize) -> R<Expr> {
    if depth > 12 {
        return Err("laplace: the expression is too complicated".into());
    }
    let sv = Expr::sym(s);
    let (consts, rest): (Vec<Expr>, Vec<Expr>) = factors(term).into_iter().partition(|f| !f.contains_sym(t));
    let c = mul(consts);
    let g = mul(rest.clone());
    let again = |h: &Expr, s2: &Expr| -> R<Expr> {
        let r = laplace_term(&expand(h), t, s, depth + 1)?;
        Ok(subst(&r, s, s2))
    };
    // sums inside (after a shift): transform term by term
    if let Expr::Add(v) = &g {
        let mut o = vec![];
        for x in v {
            o.push(laplace_term(x, t, s, depth + 1)?);
        }
        return Ok(mul2(c, add(o)));
    }
    if g.is_one() {
        return Ok(div(c, sv));
    }
    // derivatives of an unknown function: L{y^(n)} = s^n Y - s^(n-1) y(0) - ... - y^(n-1)(0)
    if rest.len() == 1 {
        if let Some((name, n)) = unknown(&g, t) {
            let big = Expr::Apply(name.to_uppercase(), vec![sv.clone()]);
            let mut r = vec![mul2(pow(sv.clone(), Expr::int(n as i128)), big)];
            for k in 0..n {
                let d = format!("{}{}", name, "'".repeat(k));
                r.push(neg(mul2(
                    pow(sv.clone(), Expr::int((n - 1 - k) as i128)),
                    Expr::Apply(d, vec![Expr::zero()]),
                )));
            }
            return Ok(mul2(c, add(r)));
        }
    }
    // heaviside(t - a) h(t) -> e^(-a s) L{h(t + a)}
    for (i, f) in rest.iter().enumerate() {
        if let Expr::Apply(n, a) = f {
            if n == "heaviside" || n == "dirac" {
                let (k, b) = linear_in(&a[0], t).ok_or("laplace: heaviside and dirac need an argument t - a")?;
                if !k.is_one() {
                    return Err("laplace: write heaviside(t - a) with t alone (coefficient 1)".into());
                }
                let a0 = neg(b);
                let others: Vec<Expr> = rest.iter().enumerate().filter(|(j, _)| *j != i).map(|p| p.1.clone()).collect();
                let h = mul(others);
                let shift = exp(neg(mul2(a0.clone(), sv.clone())));
                if n == "dirac" {
                    return Ok(simplify(&mul(vec![c, shift, subst(&h, t, &a0)])));
                }
                let moved = subst(&h, t, &add2(Expr::sym(t), a0));
                return Ok(mul(vec![c, shift, again(&moved, &sv)?]));
            }
        }
    }
    // e^(a t + b) h(t) -> e^b L{h}(s - a)
    for (i, f) in rest.iter().enumerate() {
        if let Expr::Pow(b, u) = f {
            if **b == Expr::Const(Const::E) {
                if let Some((a, b0)) = linear_in(u, t) {
                    let others: Vec<Expr> = rest.iter().enumerate().filter(|(j, _)| *j != i).map(|p| p.1.clone()).collect();
                    let h = mul(others);
                    return Ok(mul(vec![c, exp(b0), again(&h, &sub(sv.clone(), a))?]));
                }
            }
        }
    }
    // t^n h(t) -> (-1)^n d^n/ds^n L{h}
    for (i, f) in rest.iter().enumerate() {
        let (b, k) = f.base_exp();
        if b == Expr::sym(t) {
            let others: Vec<Expr> = rest.iter().enumerate().filter(|(j, _)| *j != i).map(|p| p.1.clone()).collect();
            if others.is_empty() {
                // t^p = Gamma(p + 1)/s^(p + 1)
                let p1 = add2(k.clone(), Expr::one());
                if k.num().is_some_and(|n| n.is_negative() && n.to_f64() <= -1.0) {
                    return Err("laplace: t^p needs p > -1".into());
                }
                return Ok(mul(vec![c, crate::special::apply("gamma", vec![p1.clone()]), pow(sv.clone(), neg(p1))]));
            }
            let n = k.num().and_then(|n| n.as_int()).filter(|n| *n > 0).ok_or("laplace: can't transform this power of t")?;
            let mut r = laplace_term(&mul(others), t, s, depth + 1)?;
            for _ in 0..n {
                r = deriv(&r, s);
            }
            let sign = if n % 2 == 0 { Expr::one() } else { Expr::int(-1) };
            return Ok(mul(vec![c, sign, r]));
        }
    }
    // sin, cos, sinh, cosh of b t (+ phase)
    if rest.len() == 1 {
        if let Expr::Func(fun, u) = &g {
            if let Some((b, ph)) = linear_in(u, t) {
                if !ph.is_zero() && matches!(fun, Func::Sin | Func::Cos) {
                    // sin(bt + c) = sin(bt)cos(c) + cos(bt)sin(c)
                    let bt = mul2(b.clone(), Expr::sym(t));
                    let split = match fun {
                        Func::Sin => add2(mul2(func(Func::Sin, bt.clone()), func(Func::Cos, ph.clone())), mul2(func(Func::Cos, bt), func(Func::Sin, ph))),
                        _ => sub(mul2(func(Func::Cos, bt.clone()), func(Func::Cos, ph.clone())), mul2(func(Func::Sin, bt), func(Func::Sin, ph))),
                    };
                    return Ok(mul2(c, laplace(&split, t, s)?));
                }
                let s2 = pow(sv.clone(), Expr::int(2));
                let b2 = pow(b.clone(), Expr::int(2));
                let r = match fun {
                    Func::Sin => Some(div(b.clone(), add2(s2, b2))),
                    Func::Cos => Some(div(sv.clone(), add2(s2, b2))),
                    Func::Sinh => Some(div(b.clone(), sub(s2, b2))),
                    Func::Cosh => Some(div(sv.clone(), sub(s2, b2))),
                    _ => None,
                };
                if let Some(r) = r {
                    return Ok(mul2(c, r));
                }
            }
        }
    }
    // products and powers of sines and cosines: reduce to sums first
    let red = crate::extra_cmd::trig_reduce(&g);
    if red != g && depth < 6 {
        let e = expand(&red);
        if matches!(e, Expr::Add(_)) || e != g {
            return Ok(mul2(c, laplace(&e, t, s)?));
        }
    }
    Err(format!(
        "laplace: no rule for {}; the table covers t^n, e^(at), sin, cos, sinh, cosh, heaviside(t - a), dirac(t - a), products with e^(at) and t^n, and y(t), d(y, t), d(y, t, 2), ...",
        crate::print::show(&g)
    ))
}

// ---------- the inverse transform ----------

/// L^-1{F}(t) for F a rational function of s, times e^(-a s) factors
pub fn ilaplace(f: &Expr, s: &str, t: &str) -> R<Expr> {
    let e = expand(&simplify(f));
    let terms = match &e {
        Expr::Add(v) => v.clone(),
        o => vec![o.clone()],
    };
    // group by the delay a in e^(-a s)
    let mut groups: Vec<(Expr, Vec<Expr>)> = vec![];
    for term in terms {
        let mut delay = Expr::zero();
        let mut rest = vec![];
        for f in factors(&term) {
            match &f {
                Expr::Pow(b, u) if **b == Expr::Const(Const::E) && u.contains_sym(s) => {
                    let (k, b0) = linear_in(u, s).ok_or("ilaplace: e^(...) must be e^(-a s)")?;
                    if !b0.is_zero() {
                        rest.push(exp(b0));
                    }
                    delay = add2(delay, neg(k));
                }
                _ => rest.push(f),
            }
        }
        let delay = simplify(&delay);
        match groups.iter_mut().find(|g| g.0 == delay) {
            Some(g) => g.1.push(mul(rest)),
            None => groups.push((delay, vec![mul(rest)])),
        }
    }
    let tv = Expr::sym(t);
    let mut out = vec![];
    for (a, parts) in groups {
        let g = add(parts);
        let h = ilaplace_rational(&g, s, t)?;
        if a.is_zero() {
            out.push(h);
        } else {
            let shifted = subst(&h, t, &sub(tv.clone(), a.clone()));
            out.push(mul2(crate::special::apply("heaviside", vec![sub(tv.clone(), a)]), shifted));
        }
    }
    Ok(tidy(&add(out)))
}

fn ilaplace_rational(f: &Expr, s: &str, t: &str) -> R<Expr> {
    // symbolic constants: pull them out when F = c * G(s)
    let (consts, rest): (Vec<Expr>, Vec<Expr>) = factors(f).into_iter().partition(|x| !x.contains_sym(s));
    let c = mul(consts);
    let g = mul(rest);
    let (q, pieces) = partial_fractions(&g, s).map_err(|e| e.replace("partfrac", "ilaplace"))?;
    let tv = Expr::sym(t);
    let mut out = vec![];
    for (k, a) in q.iter().enumerate() {
        if !a.is_zero() {
            // s^k -> the k-th derivative of dirac
            if k == 0 {
                out.push(mul2(Expr::Num(a.clone()), crate::special::apply("dirac", vec![tv.clone()])));
            } else {
                return Err("ilaplace: F(s) grows like a power of s; its inverse is not a function".into());
            }
        }
    }
    // simple poles at r and -r: A e^(rt) + B e^(-rt) = (A+B) cosh(rt) + (A-B) sinh(rt)
    let simple = |p: &Piece| -> Option<(Number, Number)> {
        if p.power != 1 || p.factor.len() != 2 {
            return None;
        }
        let r = p.factor[0].neg().div(&p.factor[1])?;
        let a = p.num.first().cloned().unwrap_or(Number::zero()).div(&p.factor[1])?;
        Some((r, a))
    };
    let mut used = vec![false; pieces.len()];
    for i in 0..pieces.len() {
        let Some((r, a)) = simple(&pieces[i]) else { continue };
        if r.is_zero() || used[i] {
            continue;
        }
        if let Some(j) = (i + 1..pieces.len()).find(|&j| !used[j] && simple(&pieces[j]).is_some_and(|(r2, _)| r2 == r.neg())) {
            let (_, b) = simple(&pieces[j]).unwrap();
            used[i] = true;
            used[j] = true;
            let rt = mul2(Expr::Num(r.clone()), tv.clone());
            out.push(add2(
                mul2(Expr::Num(a.add(&b)), func(Func::Cosh, rt.clone())),
                mul2(Expr::Num(a.sub(&b)), func(Func::Sinh, rt)),
            ));
        }
    }
    for (p, u) in pieces.iter().zip(&used) {
        if !u {
            out.push(inverse_piece(p, &tv)?);
        }
    }
    Ok(mul2(c, add(out)))
}

fn fact(n: u32) -> Expr {
    Expr::int((1..=n as i128).product::<i128>().max(1))
}

fn inverse_piece(p: &Piece, t: &Expr) -> R<Expr> {
    let lead = p.factor.last().unwrap().clone();
    let monic: Vec<Number> = p.factor.iter().map(|c| c.div(&lead).unwrap()).collect();
    // num / (lead^power * monic^power)
    let scale = Expr::Num(Number::one().div(lead.pow(Number::int(p.power as i128)).unwrap()).unwrap());
    let n = p.power;
    match monic.len() {
        2 => {
            // c / (s - r)^n -> c t^(n-1) e^(r t) / (n-1)!
            let r = monic[0].neg();
            let c = Expr::Num(p.num[0].clone());
            Ok(simplify(&mul(vec![
                scale,
                c,
                pow(t.clone(), Expr::int(n as i128 - 1)),
                exp(mul2(Expr::Num(r), t.clone())),
                pow(fact(n - 1), Expr::int(-1)),
            ])))
        }
        3 => {
            // (B s + C) / ((s - al)^2 + D)^n with D = q0 - al^2
            let al = monic[1].neg().div(Number::int(2)).unwrap();
            let dd = monic[0].sub(al.mul(&al));
            let b = p.num.get(1).cloned().unwrap_or(Number::zero());
            let c0 = p.num.first().cloned().unwrap_or(Number::zero());
            // B (s - al) + (C + B al)
            let cc = c0.add(b.mul(&al));
            let (bv, cv, alv) = (Expr::Num(b), Expr::Num(cc), Expr::Num(al));
            let ea = exp(mul2(alv, t.clone()));
            let beta = crate::cas::rsqrt(&Expr::Num(dd.abs()));
            let bt = mul2(beta.clone(), t.clone());
            let r = if dd.is_negative() {
                // real roots: cosh and sinh
                match n {
                    1 => add2(mul2(bv, func(Func::Cosh, bt.clone())), mul2(div(cv, beta), func(Func::Sinh, bt))),
                    _ => return Err("ilaplace: repeated real quadratic factors are not supported".into()),
                }
            } else {
                let (sn, cs) = (func(Func::Sin, bt.clone()), func(Func::Cos, bt.clone()));
                match n {
                    1 => add2(mul2(bv, cs), mul2(div(cv, beta.clone()), sn)),
                    2 => {
                        // (s-al)/((s-al)^2+b^2)^2 -> t sin(bt)/(2b); 1/(...)^2 -> (sin(bt) - bt cos(bt))/(2b^3)
                        let two = Expr::int(2);
                        add2(
                            mul(vec![bv, t.clone(), sn.clone(), pow(mul2(two.clone(), beta.clone()), Expr::int(-1))]),
                            mul(vec![
                                cv,
                                sub(sn, mul2(bt, cs)),
                                pow(mul2(two, pow(beta.clone(), Expr::int(3))), Expr::int(-1)),
                            ]),
                        )
                    }
                    _ => return Err("ilaplace: factors repeated three or more times are not supported".into()),
                }
            };
            Ok(mul(vec![scale, ea, r]))
        }
        _ => Err("ilaplace: the denominator has an irreducible factor of degree 3 or more".into()),
    }
}

/// numeric check helper for tests
pub fn approx(e: &Expr) -> f64 {
    evalf(e).num().map(|n| n.to_f64()).unwrap_or(f64::NAN)
}

