//! Calculus and algebraic manipulation: deriv, expand, factor, simplify,
//! substitution and numeric evaluation.

use crate::build::*;
use crate::expr::{Const, Expr, Func};
use crate::num::Number;
use Expr::*;

/// d e / d x
pub fn deriv(e: &Expr, x: &str) -> Expr {
    if !e.contains_sym(x) {
        return match e {
            Matrix(rows) => Matrix(
                rows.iter()
                    .map(|r| r.iter().map(|_| Expr::zero()).collect())
                    .collect(),
            ),
            _ => Expr::zero(),
        };
    }
    match e {
        Sym(_) => Expr::one(), // contains x, so it is x
        Add(terms) => add(terms.iter().map(|t| deriv(t, x)).collect()),
        Mul(f) => add((0..f.len())
            .map(|i| {
                let mut g = f.clone();
                g[i] = deriv(&f[i], x);
                mul(g)
            })
            .collect()),
        Pow(b, p) => {
            if !p.contains_sym(x) {
                // p * b^(p-1) * b'
                mul(vec![
                    (**p).clone(),
                    pow((**b).clone(), sub((**p).clone(), Expr::one())),
                    deriv(b, x),
                ])
            } else {
                // b^p * (p' log b + p b'/b)
                mul(vec![
                    e.clone(),
                    add(vec![
                        mul(vec![deriv(p, x), func(Func::Log, (**b).clone())]),
                        mul(vec![
                            (**p).clone(),
                            deriv(b, x),
                            pow((**b).clone(), Expr::int(-1)),
                        ]),
                    ]),
                ])
            }
        }
        Func(f, a) => {
            let a = (**a).clone();
            let da = deriv(&a, x);
            let outer = match f {
                Func::Sin => func(Func::Cos, a),
                Func::Cos => neg(func(Func::Sin, a)),
                Func::Tan => pow(func(Func::Cos, a), Expr::int(-2)),
                Func::Asin => pow(sub(Expr::one(), pow(a, Expr::int(2))), rat(-1, 2)),
                Func::Acos => neg(pow(sub(Expr::one(), pow(a, Expr::int(2))), rat(-1, 2))),
                Func::Atan => pow(add2(Expr::one(), pow(a, Expr::int(2))), Expr::int(-1)),
                Func::Log => pow(a, Expr::int(-1)),
                Func::Abs => div(a.clone(), func(Func::Abs, a)),
                Func::Sinh => func(Func::Cosh, a),
                Func::Cosh => func(Func::Sinh, a),
                Func::Tanh => pow(func(Func::Cosh, a), Expr::int(-2)),
                Func::Asinh => pow(add2(pow(a, Expr::int(2)), Expr::one()), rat(-1, 2)),
                Func::Acosh => pow(sub(pow(a, Expr::int(2)), Expr::one()), rat(-1, 2)),
                Func::Atanh => pow(sub(Expr::one(), pow(a, Expr::int(2))), Expr::int(-1)),
            };
            mul(vec![outer, da])
        }
        Apply(f, args) if f == "atan2" && args.len() == 2 => {
            // d atan2(y, x) = (x dy - y dx) / (x^2 + y^2)
            let (y, xx) = (&args[0], &args[1]);
            div(
                sub(mul2(xx.clone(), deriv(y, x)), mul2(y.clone(), deriv(xx, x))),
                add2(pow(xx.clone(), Expr::int(2)), pow(y.clone(), Expr::int(2))),
            )
        }
        Apply(f, args)
            if crate::special::is_special(f, args.len())
                && crate::special::deriv(f, args, x).is_some() =>
        {
            crate::special::deriv(f, args, x).unwrap()
        }
        Apply(f, args) if args.len() == 1 => {
            // chain rule with a new function f'
            mul2(Apply(format!("{}'", f), args.clone()), deriv(&args[0], x))
        }
        Apply(f, args) => {
            // partial derivatives: f_{,k} written as f_k with k the argument position (1-based)
            add(args
                .iter()
                .enumerate()
                .filter(|(_, a)| a.contains_sym(x))
                .map(|(k, a)| mul2(Apply(partial_name(f, k), args.clone()), deriv(a, x)))
                .collect())
        }
        Matrix(rows) => Matrix(
            rows.iter()
                .map(|r| r.iter().map(|v| deriv(v, x)).collect())
                .collect(),
        ),
        _ => Expr::zero(),
    }
}

/// name of the derivative of f with respect to its k-th argument: f_1, f_12, ...
fn partial_name(f: &str, k: usize) -> String {
    let (base, idx) = match f.rsplit_once(',') {
        Some((b, i)) => (b.to_string(), i.to_string()),
        None => (f.to_string(), String::new()),
    };
    format!("{},{}{}", base, idx, k + 1)
}

/// Multiply out products and positive integer powers of sums
pub fn expand(e: &Expr) -> Expr {
    match e {
        Add(t) => add(t.iter().map(expand).collect()),
        Mul(f) => {
            let mut terms = vec![Expr::one()];
            for factor in f {
                let fx = expand(factor);
                let parts = match fx {
                    Add(t) => t,
                    other => vec![other],
                };
                terms = terms
                    .iter()
                    .flat_map(|a| parts.iter().map(move |b| mul2(a.clone(), b.clone())))
                    .collect();
            }
            add(terms)
        }
        Pow(b, p) => {
            let bx = expand(b);
            match (
                p.num()
                    .filter(|n| n.is_integer())
                    .map(|n| n.to_f64() as i128),
                &bx,
            ) {
                (Some(n), Add(_)) if n > 1 && n <= 64 => {
                    let mut acc = bx.clone();
                    for _ in 1..n {
                        acc = distribute(&acc, &bx);
                    }
                    acc
                }
                _ => pow(bx, expand(p)),
            }
        }
        Func(f, a) => func(*f, expand(a)),
        Matrix(_) => e.map_children(&mut |x| expand(x)),
        _ => e.clone(),
    }
}

/// Multiply two expanded expressions term by term
fn distribute(a: &Expr, b: &Expr) -> Expr {
    let terms = |e: &Expr| match e {
        Add(t) => t.clone(),
        other => vec![other.clone()],
    };
    let (ta, tb) = (terms(a), terms(b));
    add(ta
        .iter()
        .flat_map(|x| tb.iter().map(move |y| expand(&mul2(x.clone(), y.clone()))))
        .collect())
}

/// Replace a symbol by a value
pub fn subst(e: &Expr, name: &str, val: &Expr) -> Expr {
    match e {
        Sym(s) if s == name => val.clone(),
        _ => e.map_children(&mut |c| subst(c, name, val)),
    }
}

/// Replace every occurrence of the subexpression `target` (such as d(y, t)) by val
pub fn subst_expr(e: &Expr, target: &Expr, val: &Expr) -> Expr {
    if e == target {
        return val.clone();
    }
    e.map_children(&mut |c| subst_expr(c, target, val))
}

/// Numeric evaluation: exact numbers and constants become floats
pub fn evalf(e: &Expr) -> Expr {
    match e {
        Num(n) => Expr::float(n.to_f64()),
        Const(Const::Pi) => Expr::float(std::f64::consts::PI),
        Const(Const::E) => Expr::float(std::f64::consts::E),
        Pow(b, p) if **b == Const(Const::E) => {
            let pe = evalf(p);
            match pe {
                Num(n) => Expr::float(n.to_f64().exp()),
                other => exp(other),
            }
        }
        Apply(f, _) => {
            // atan2 and special functions of numbers give a float
            let a = e.map_children(&mut |c| evalf(c));
            if let Apply(_, args) = &a {
                if (f == "atan2" || crate::special::is_special(f, args.len()))
                    && args.iter().all(|x| matches!(x, Num(_)))
                {
                    let v = crate::numeric::value(&a);
                    if v.is_finite() {
                        return Expr::float(v);
                    }
                }
            }
            a
        }
        _ => e.map_children(&mut |c| evalf(c)),
    }
}

// ---------- rational functions ----------

/// Split into (numerator, denominator); the denominator collects factors
/// with negative numeric exponents and the coefficient's denominator.
pub fn num_den(e: &Expr) -> (Expr, Expr) {
    let factors = match e {
        Mul(f) => f.clone(),
        other => vec![other.clone()],
    };
    let mut n = vec![];
    let mut d = vec![];
    for f in factors {
        match &f {
            Num(Number::Rat(a, b)) => {
                n.push(Expr::int(*a));
                d.push(Expr::int(*b));
            }
            Pow(b, p) if p.num().is_some_and(|x| x.is_negative()) => {
                d.push(pow((**b).clone(), neg((**p).clone())))
            }
            _ => n.push(f),
        }
    }
    (mul(n), mul(d))
}

/// Factors of a product as (base, numeric exponent) pairs plus coefficient
fn monomial_parts(e: &Expr) -> (Number, Vec<(Expr, Number)>) {
    let (c, rest) = e.coeff_and_rest();
    let fs = match rest {
        Mul(f) => f,
        Num(_) => vec![],
        other => vec![other],
    };
    let mut parts = vec![];
    for f in fs {
        let (b, p) = f.base_exp();
        match p {
            Num(n) => parts.push((b, n)),
            other => parts.push((pow(b, other), Number::one())),
        }
    }
    (c, parts)
}

fn igcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a
}

/// Greatest common monomial of a list of terms: gcd of coefficients times
/// the common bases with their smallest positive exponent.
fn content(terms: &[Expr]) -> Expr {
    let mut g: Option<i128> = Some(0);
    let mut common: Option<Vec<(Expr, Number)>> = None;
    for t in terms {
        let (c, parts) = monomial_parts(t);
        g = match (g, c.as_int()) {
            (Some(a), Some(b)) => Some(igcd(a, b)),
            _ => None,
        };
        let parts: Vec<(Expr, Number)> = parts
            .into_iter()
            .filter(|(_, p)| p.is_integer() && !p.is_negative())
            .collect();
        common = Some(match common {
            None => parts,
            Some(cs) => cs
                .into_iter()
                .filter_map(|(b, p)| {
                    parts.iter().find(|(b2, _)| *b2 == b).map(|(_, p2)| {
                        (
                            b,
                            if p.cmp_value(p2).is_le() {
                                p
                            } else {
                                p2.clone()
                            },
                        )
                    })
                })
                .collect(),
        });
    }
    let mut f = vec![];
    if let Some(g) = g {
        if g > 1 {
            f.push(Expr::int(g));
        }
    }
    // pull out a minus sign when every term is negative
    if terms.iter().all(|t| t.is_negative_term()) {
        f.push(Expr::int(-1));
    }
    for (b, p) in common.unwrap_or_default() {
        f.push(pow(b, Num(p)));
    }
    mul(f)
}

/// Put a sum over a common denominator and cancel common factors
pub fn together(e: &Expr) -> Expr {
    let terms = match e {
        Add(t) => t.clone(),
        other => vec![other.clone()],
    };
    // common denominator: each base with its largest exponent
    let mut dparts: Vec<(Expr, Number)> = vec![];
    let mut dcoef: i128 = 1;
    for t in &terms {
        let (_, d) = num_den(t);
        let (c, parts) = monomial_parts(&d);
        if let Some(c) = c.as_int() {
            dcoef = dcoef / igcd(dcoef, c) * c;
        }
        for (b, p) in parts {
            match dparts.iter_mut().find(|(b2, _)| *b2 == b) {
                Some(x) => {
                    if p.cmp_value(&x.1).is_gt() {
                        x.1 = p
                    }
                }
                None => dparts.push((b, p)),
            }
        }
    }
    let mut dfactors: Vec<Expr> = dparts
        .iter()
        .map(|(b, p)| pow(b.clone(), Num(p.clone())))
        .collect();
    dfactors.push(Expr::int(dcoef));
    let den = mul(dfactors);
    if den.is_one() {
        return e.clone();
    }
    let numer = expand(&add(terms
        .iter()
        .map(|t| mul2(t.clone(), den.clone()))
        .collect()));
    cancel(&numer, &den)
}

fn cancel(numer: &Expr, den: &Expr) -> Expr {
    if numer.is_zero() {
        return Expr::zero();
    }
    if expand(numer) == expand(den) {
        return Expr::one();
    }
    let nterms = match numer {
        Add(t) => t.clone(),
        other => vec![other.clone()],
    };
    let c = content(&nterms);
    // the part of the content that also divides the denominator
    let (dc, dparts) = monomial_parts(den);
    let (cc, cparts) = monomial_parts(&c);
    let mut cancel_f = vec![];
    if let (Some(a), Some(b)) = (dc.as_int(), cc.as_int()) {
        let g = igcd(a, b);
        if g > 1 {
            cancel_f.push(Expr::int(g));
        }
    }
    for (b, p) in &cparts {
        if let Some((_, q)) = dparts.iter().find(|(b2, _)| b2 == b) {
            let m = if p.cmp_value(q).is_le() {
                p.clone()
            } else {
                q.clone()
            };
            cancel_f.push(pow(b.clone(), Num(m)));
        }
    }
    let g = mul(cancel_f);
    let n2 = expand(&div(numer.clone(), g.clone()));
    let d2 = div(den.clone(), g);
    div(n2, d2)
}

/// Pull common factors out of sums
pub fn factor(e: &Expr) -> Expr {
    match e {
        Add(t) => {
            let t: Vec<Expr> = t.iter().map(factor).collect();
            let c = content(&t);
            if c.is_one() {
                return add(t);
            }
            let inner = add(t.iter().map(|x| div(x.clone(), c.clone())).collect());
            mul(vec![c, inner])
        }
        Mul(_) | Pow(..) | Func(..) | Matrix(_) => e.map_children(&mut |x| factor(x)),
        _ => e.clone(),
    }
}

/// Rewrite sin(a)^n (n >= 2) as sin(a)^(n-2) * (1 - cos(a)^2), or the
/// reverse when `sin_to_cos` is false.
fn pythagoras(e: &Expr, sin_to_cos: bool) -> Expr {
    let (from, to) = if sin_to_cos {
        (Func::Sin, Func::Cos)
    } else {
        (Func::Cos, Func::Sin)
    };
    if let Pow(b, p) = e {
        if let (Func(f, a), Some(n)) = (
            &**b,
            p.num()
                .filter(|n| n.is_integer())
                .map(|n| n.to_f64() as i128),
        ) {
            if *f == from && n >= 2 {
                let a2 = pythagoras(a, sin_to_cos);
                return mul(vec![
                    pow(func(from, a2.clone()), Expr::int(n - 2)),
                    sub(Expr::one(), pow(func(to, a2), Expr::int(2))),
                ]);
            }
        }
    }
    e.map_children(&mut |c| pythagoras(c, sin_to_cos))
}

/// Find a simpler equivalent form: combine fractions, cancel common factors
/// and apply sin^2 + cos^2 = 1, keeping whichever form is smallest.
/// The `simplify` command: every expression to one target form (see
/// ratfun::normal_form), entry by entry for matrices; expressions that are not
/// rational functions of kernels fall back to the smallest of several forms.
/// Numbers keep the look of the input: when a decimal took part, results with
/// a finite decimal form print as decimals.
pub fn simplify_target(e: &Expr) -> Expr {
    match e {
        Matrix(_) => return e.map_children(&mut |x| simplify_target(x)),
        Num(_) | Sym(_) | Const(_) | Bool(_) | Str(_) | Obj(_) => return e.clone(),
        _ => {}
    }
    if crate::units::has_units(e) {
        return simplify(e);
    }
    let e = &no_root_denominators(&cancel_ratios(e));
    let r = crate::ratfun::normal_form(e).unwrap_or_else(|| simplify(e));
    if has_decimal(e) {
        decimals(&r)
    } else {
        r
    }
}

/// factorial(n + 2)/factorial(n) = (n + 1)(n + 2), everywhere in e
fn cancel_ratios(e: &Expr) -> Expr {
    let inner = e.map_children(&mut |x| cancel_ratios(x));
    crate::special::ratios(&inner).unwrap_or(inner)
}

/// square roots of fractions with the root in the numerator: sqrt(1/2) = sqrt(2)/2
fn no_root_denominators(e: &Expr) -> Expr {
    if let Pow(b, x) = e {
        if let (Num(r), Num(h)) = (&**b, &**x) {
            let half = crate::num::Number::rat(1, 2).unwrap();
            let r = if *h == half.neg() && r.is_exact() && !r.is_zero() { r.recip().unwrap() } else { r.clone() };
            let h = if h.is_negative() { half.clone() } else { h.clone() };
            if h == half && r.is_exact() && !r.is_integer() && !r.is_negative() {
                if let (Some(n), Some(d)) = (r.to_exact().numer(), r.to_exact().denom()) {
                    if let Some(nd) = n.checked_mul(d) {
                        return mul(vec![
                            Num(crate::num::Number::rat(1, d).unwrap()),
                            crate::build::pow(Expr::int(nd), Num(half)),
                        ]);
                    }
                }
            }
        }
    }
    e.map_children(&mut |c| no_root_denominators(c))
}

pub fn has_decimal(e: &Expr) -> bool {
    match e {
        Num(n) => n.is_xd(),
        Add(v) | Mul(v) | Apply(_, v) => v.iter().any(has_decimal),
        Pow(a, b) => has_decimal(a) || has_decimal(b),
        Func(_, a) => has_decimal(a),
        Held(_, v) => v.iter().any(has_decimal),
        Matrix(m) => m.iter().flatten().any(has_decimal),
        _ => false,
    }
}

/// exact fractions with a finite decimal form, written as decimals
pub fn decimals(e: &Expr) -> Expr {
    match e {
        Num(n) if n.is_exact() && !n.is_integer() => Num(n.to_xd()),
        Num(_) | Bool(_) | Str(_) | Obj(_) | Sym(_) | Const(_) => e.clone(),
        // exponents stay as they are: x^(1/2), not x^0.5
        Pow(b, x) if **b != Const(Const::E) => crate::build::pow(decimals(b), (**x).clone()),
        _ => e.map_children(&mut |x| decimals(x)),
    }
}

pub fn simplify(e: &Expr) -> Expr {
    match e {
        Matrix(_) => return e.map_children(&mut |x| simplify(x)),
        Num(_) | Sym(_) | Const(_) | Bool(_) | Str(_) => return e.clone(),
        _ => {}
    }
    // factorial(n + 2)/factorial(n) = (n + 1)(n + 2): cancel before the
    // factorials are worked out, and again once the factors are simplified
    if let Some(r) = crate::special::ratios(e) {
        return simplify(&r);
    }
    let inner = e.map_children(&mut |x| simplify(x));
    let inner = crate::special::ratios(&inner).map(|r| simplify(&r)).unwrap_or(inner);
    let mut best = inner.clone();
    let mut consider = |c: Expr| {
        if cost(&c) < cost(&best) {
            best = c;
        }
    };
    let r = rationalize(&inner);
    consider(r.clone());
    consider(crate::ratfun::tidy(&inner));
    consider(together(&inner));
    let ex = expand(&inner);
    consider(together(&ex));
    for s2c in [true, false] {
        let p = expand(&pythagoras(&r, s2c));
        consider(together(&p));
    }
    best
}

/// Size, with a penalty for fractions nested inside sums (d - b*c/a)
fn cost(e: &Expr) -> usize {
    fn nested(e: &Expr, in_sum: bool) -> usize {
        match e {
            Pow(b, p) => {
                let here = (in_sum && p.num().is_some_and(|n| n.is_negative())) as usize;
                here + nested(b, in_sum) + nested(p, false)
            }
            Add(v) => v.iter().map(|x| nested(x, true)).sum(),
            Mul(v) => v.iter().map(|x| nested(x, in_sum)).sum(),
            Func(_, a) => nested(a, false),
            _ => 0,
        }
    }
    e.size() + 3 * nested(e, false)
}

/// Bring every sum, bottom-up, over a common denominator. Locally this can
/// make things bigger, but it lets outer fractions cancel:
/// d/(a*(d - b*c/a)) becomes d/(a*d - b*c).
pub fn rationalize(e: &Expr) -> Expr {
    let inner = e.map_children(&mut |x| rationalize(x));
    match inner {
        Add(_) => together(&inner),
        Mul(_) => {
            let (n, d) = num_den(&inner);
            if d.is_one() {
                inner
            } else {
                cancel(&expand(&n), &d)
            }
        }
        _ => inner,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print::show;
    fn x() -> Expr {
        Expr::sym("x")
    }
    #[test]
    fn derivative_of_quotient() {
        let f = div(func(Func::Sin, x()), pow(x(), Expr::int(2)));
        let d = simplify(&deriv(&f, "x"));
        assert_eq!(show(&d), "(x*cos(x)-2*sin(x))/x^3");
    }
    #[test]
    fn expand_difference_of_squares() {
        let a = Expr::sym("a");
        let b = Expr::sym("b");
        let e = mul(vec![add2(a.clone(), b.clone()), sub(a, b)]);
        assert_eq!(show(&expand(&e)), "a^2-b^2");
    }
    #[test]
    fn pythagorean_identity() {
        let s = pow(func(Func::Sin, x()), Expr::int(2));
        let c = pow(func(Func::Cos, x()), Expr::int(2));
        assert_eq!(simplify(&add2(s, c)), Expr::one());
    }
    #[test]
    fn factor_common_terms() {
        let a = Expr::sym("a");
        let e = add(vec![
            mul2(a.clone(), Expr::sym("b")),
            mul2(a, Expr::sym("c")),
        ]);
        assert_eq!(show(&factor(&e)), "a*(b+c)");
    }
}
