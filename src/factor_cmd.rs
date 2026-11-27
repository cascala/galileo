//! Commands for algebraic factoring: factor() on polynomials over the
//! integers and on whole numbers of the form a^n ± b^n, cyclotomic(n),
//! algfactor(n) (the algebraic pieces of a number), factorsearch (hunt for
//! factorizations in a family of polynomials) and digits(n).

use crate::algfactor::{self as af, ZPoly};
use crate::build::*;
use crate::calc;
use crate::discover::Report;
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::latex::latex;
use crate::num::Number;
use crate::parser::{Node, Op};
use crate::print::show;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::BTreeMap;
use std::rc::Rc;

fn report(kind: &'static str, text: String, markdown: String, formulas: Vec<Expr>) -> Expr {
    Expr::Obj(Rc::new(Obj::Report(Report {
        kind,
        text,
        markdown,
        formulas,
        best: 0,
        vars: vec![],
        order: 0,
        target: 0,
        value: None,
    })))
}

fn int(n: i128) -> Expr {
    Expr::int(n)
}

/// A factorization as Galileo returns it everywhere: rows [p, e], smallest
/// first, equal parts merged into an exponent. Parts need not be prime (a
/// composite part that did not split gets a row of its own); a negative part
/// becomes a row [-1, 1] in front. No parts gives [].
pub(crate) fn factor_rows(parts: Vec<BigInt>) -> Expr {
    let mut sign = false;
    let mut m: BTreeMap<BigInt, u32> = BTreeMap::new();
    for p in parts {
        if p.is_negative() {
            sign = !sign;
        }
        let a = p.abs();
        if !a.is_one() {
            *m.entry(a).or_insert(0) += 1;
        }
    }
    let mut rows = vec![];
    if sign {
        rows.push(vec![Expr::int(-1), Expr::int(1)]);
    }
    for (p, e) in m {
        rows.push(vec![bnum(p), Expr::int(e as i128)]);
    }
    Expr::Matrix(rows)
}

/// The split n = f * (n / f) as factorization rows; [] when there is no split
pub(crate) fn split_rows(n: &BigInt, f: Option<&BigInt>) -> Expr {
    match f {
        Some(f) if !f.is_zero() && !f.abs().is_one() && f.abs() != n.abs() => {
            factor_rows(vec![f.abs(), (n / f).abs()])
        }
        _ => Expr::Matrix(vec![]),
    }
}

fn bnum(n: BigInt) -> Expr {
    Expr::Num(Number::from_bigint(n))
}

// ---------- expressions <-> integer polynomials ----------

/// the terms of a polynomial in vars as exponent vectors with rational coefficients
pub(crate) fn terms_of(e: &Expr, vars: &[String]) -> Option<Vec<(Vec<usize>, BigRational)>> {
    let ex = calc::expand(e);
    let ts = match &ex {
        Expr::Add(t) => t.clone(),
        o => vec![o.clone()],
    };
    let mut out = vec![];
    for t in ts {
        let (c, rest) = t.coeff_and_rest();
        let c = c.big().filter(|_| c.is_exact())?;
        let mut ex = vec![0usize; vars.len()];
        let fs = match rest {
            Expr::Mul(f) => f,
            o => vec![o],
        };
        for f in fs {
            match &f {
                Expr::Num(n) if n.is_one() => {}
                Expr::Sym(s) => ex[vars.iter().position(|v| v == s)?] += 1,
                Expr::Pow(b, k) => {
                    let Expr::Sym(s) = &**b else { return None };
                    let k = k
                        .num()
                        .filter(|k| k.is_exact() && k.is_integer() && !k.is_negative())?
                        .as_int()?;
                    if k > 5000 {
                        return None;
                    }
                    ex[vars.iter().position(|v| v == s)?] += k as usize;
                }
                _ => return None,
            }
        }
        out.push((ex, c));
    }
    Some(out)
}

fn upoly_expr(p: &[BigInt], x: &Expr) -> Expr {
    add(p
        .iter()
        .enumerate()
        .filter(|(_, c)| !c.is_zero())
        .map(|(k, c)| mul2(bnum(c.clone()), pow(x.clone(), int(k as i128))))
        .collect())
}

/// homogeneous version of p: sum p_i x^i y^(d - i)
fn hpoly_expr(p: &[BigInt], x: &Expr, y: &Expr) -> Expr {
    let d = p.len() - 1;
    add(p
        .iter()
        .enumerate()
        .filter(|(_, c)| !c.is_zero())
        .map(|(k, c)| {
            mul(vec![
                bnum(c.clone()),
                pow(x.clone(), int(k as i128)),
                pow(y.clone(), int((d - k) as i128)),
            ])
        })
        .collect())
}

/// Factorization of a polynomial with rational coefficients in one variable,
/// or homogeneous in two: (constant, [(factor, multiplicity)]).
pub fn factor_parts(e: &Expr) -> Option<(Number, Vec<(Expr, u32)>)> {
    let vars = e.symbols();
    if vars.is_empty() || vars.len() > 8 {
        return None;
    }
    let ts = terms_of(e, &vars)?;
    let den = ts
        .iter()
        .fold(BigInt::one(), |acc, (_, c)| acc.lcm(c.denom()));
    let total: Vec<usize> = ts
        .iter()
        .filter(|(_, c)| !c.is_zero())
        .map(|(ex, _)| ex.iter().sum())
        .collect();
    let dtot = *total.first()?;
    if vars.len() > 2 || (vars.len() == 2 && total.iter().any(|&d| d != dtot)) {
        // several variables: Kronecker substitution
        let mt: Vec<af::MTerm> = ts
            .iter()
            .map(|(ex, c)| {
                (
                    ex.iter().map(|&k| k as u32).collect(),
                    (c * BigRational::from_integer(den.clone())).to_integer(),
                )
            })
            .collect();
        let (c, fs) = af::factor_multi(&mt, vars.len())?;
        let syms: Vec<Expr> = vars.iter().map(|v| Expr::sym(v)).collect();
        let out = fs
            .into_iter()
            .map(|(f, m)| {
                let e = add(f
                    .into_iter()
                    .map(|(ex, c)| {
                        let mut v = vec![bnum(c)];
                        for (s, k) in syms.iter().zip(ex) {
                            v.push(pow(s.clone(), int(k as i128)));
                        }
                        mul(v)
                    })
                    .collect());
                (e, m)
            })
            .collect();
        return Some((Number::from_big(BigRational::new(c, den)), out));
    }
    // coefficients in the first variable (the second is set to 1)
    let dmax = ts.iter().map(|(ex, _)| ex[0]).max()?;
    let mut p: ZPoly = vec![BigInt::zero(); dmax + 1];
    for (ex, c) in &ts {
        p[ex[0]] += (c * BigRational::from_integer(den.clone())).to_integer();
    }
    let (c, fs) = af::factor_zpoly(&p)?;
    let x = Expr::sym(&vars[0]);
    let mut out: Vec<(Expr, u32)> = vec![];
    if vars.len() == 2 {
        let y = Expr::sym(&vars[1]);
        let degf = af::trim(p.clone()).len().saturating_sub(1);
        if dtot > degf {
            out.push((y.clone(), (dtot - degf) as u32));
        }
        for (f, m) in fs {
            out.push((hpoly_expr(&f, &x, &y), m));
        }
    } else {
        for (f, m) in fs {
            out.push((upoly_expr(&f, &x), m));
        }
    }
    let k = Number::from_big(BigRational::new(c, den));
    Some((k, out))
}

fn assemble(k: Number, fs: &[(Expr, u32)]) -> Expr {
    let mut v = vec![Expr::Num(k)];
    for (f, m) in fs {
        v.push(pow(f.clone(), int(*m as i128)));
    }
    mul(v)
}

/// factor() on a polynomial: Some when it is a polynomial we can handle
pub fn factor_poly(e: &Expr) -> Option<Expr> {
    let (k, fs) = factor_parts(e)?;
    let nontrivial = fs.iter().map(|(_, m)| *m as usize).sum::<usize>() >= 2 || !k.is_one();
    if !nontrivial {
        return Some(e.clone());
    }
    Some(assemble(k, &fs))
}

// ---------- whole numbers ----------

fn digits(n: &BigInt) -> usize {
    n.abs().to_string().len()
}

struct IntFactors {
    sign: bool,
    primes: BTreeMap<BigInt, u32>,
    composites: Vec<BigInt>,
}

impl IntFactors {
    /// rows [p, e] (prime and exponent), [-1, 1] first for a negative number,
    /// and [c, 1] for a composite part that did not split
    fn matrix(&self) -> Expr {
        let big = |n: &BigInt| Expr::Num(Number::from_bigint(n.clone()));
        let mut rows = vec![];
        if self.sign {
            rows.push(vec![Expr::int(-1), Expr::int(1)]);
        }
        for (p, e) in &self.primes {
            rows.push(vec![big(p), Expr::int(*e as i128)]);
        }
        for c in &self.composites {
            rows.push(vec![big(c), Expr::int(1)]);
        }
        Expr::Matrix(rows)
    }
    fn text(&self) -> String {
        let mut parts = vec![];
        if self.sign {
            parts.push("-1".to_string());
        }
        for (p, e) in &self.primes {
            parts.push(if *e == 1 {
                p.to_string()
            } else {
                format!("{}^{}", p, e)
            });
        }
        let labels = self.labels();
        parts.extend(labels.iter().cloned());
        let mut s = parts.join(" * ");
        for (l, c) in labels.iter().zip(&self.composites) {
            s.push_str(&format!(
                "\n{} = {} (composite: no factor found in reasonable time)",
                l, c
            ));
        }
        s
    }
    fn labels(&self) -> Vec<String> {
        let mut seen: BTreeMap<usize, usize> = BTreeMap::new();
        self.composites
            .iter()
            .map(|c| {
                let d = digits(c);
                let k = seen.entry(d).or_insert(0);
                *k += 1;
                if *k == 1 {
                    format!("C{}", d)
                } else {
                    format!("C{}.{}", d, k)
                }
            })
            .collect()
    }
}

/// a^n ± b^n with a, b >= 1, read from the expression as typed: (a, b, n, plus, negative)
fn binomial_from_node(s: &mut Session, node: &Node) -> Option<(BigInt, BigInt, u64, bool, bool)> {
    let Node::Bin(op, l, r) = node else {
        return None;
    };
    let plus = match op {
        Op::Add => true,
        Op::Sub => false,
        _ => return None,
    };
    let mut side = |n: &Node| -> Option<(BigInt, u64)> {
        match n {
            Node::Bin(Op::Pow, b, e) => {
                let b = s.eval(b).ok()?.num()?.as_bigint()?;
                let e = s.eval(e).ok()?.num()?.as_bigint()?.to_u64()?;
                Some((b, e))
            }
            _ => Some((s.eval(n).ok()?.num()?.as_bigint()?, 1)),
        }
    };
    let (a, n1) = side(l)?;
    let (b, n2) = side(r)?;
    if a.is_negative() || b.is_negative() || a.is_zero() || b.is_zero() {
        return None;
    }
    let (n1, n2) = match (a.is_one(), b.is_one()) {
        (true, false) => (n2, n2),
        (false, true) => (n1, n1),
        _ => (n1, n2),
    };
    let k = n1.gcd(&n2);
    if k < 2 {
        return None;
    }
    let a = num_traits::pow(a, (n1 / k) as usize);
    let b = num_traits::pow(b, (n2 / k) as usize);
    if !plus && a < b {
        return Some((b, a, k, false, true));
    }
    Some((a, b, k, plus, false))
}

/// the algebraic form of n: from the expression typed, or recognized as b^k ± 1
fn int_form(
    s: &mut Session,
    node: Option<&Node>,
    n: &BigInt,
    detect: bool,
) -> Option<(BigInt, BigInt, u64, bool, bool)> {
    let check = |a: &BigInt, b: &BigInt, k: u64, plus: bool, neg: bool| {
        let v = if plus {
            num_traits::pow(a.clone(), k as usize) + num_traits::pow(b.clone(), k as usize)
        } else {
            num_traits::pow(a.clone(), k as usize) - num_traits::pow(b.clone(), k as usize)
        };
        if neg { -v == *n } else { v == *n }
    };
    if let Some(f) = node.and_then(|nd| binomial_from_node(s, nd)) {
        if check(&f.0, &f.1, f.2, f.3, f.4) {
            return Some(f);
        }
    }
    if detect {
        let (b, k, plus) = af::detect_binomial(&n.abs())?;
        return Some((b, BigInt::one(), k, plus, n.is_negative()));
    }
    None
}

fn form_latex(a: &BigInt, b: &BigInt, k: u64, plus: bool) -> String {
    let op = if plus { "+" } else { "-" };
    if b.is_one() {
        format!("{}^{{{}}} {} 1", a, k, op)
    } else {
        format!("{}^{{{}}} {} {}^{{{}}}", a, k, op, b, k)
    }
}

impl Session {
    /// factor(n) for a whole number: split algebraically first when n = a^n ± b^n
    /// The factorization of n as rows [p, e], with its text (2^3 * 3^2 * 5)
    pub(crate) fn factor_integer(&mut self, node: &Node, n: &BigInt) -> R<(Expr, String)> {
        let detect = n.bits() > 100;
        if let Some((a, b, k, plus, neg)) = int_form(self, Some(node), n, detect) {
            if let Some(pieces) = af::algebraic_pieces(&a, &b, k, plus) {
                let mut f = IntFactors {
                    sign: neg,
                    primes: BTreeMap::new(),
                    composites: vec![],
                };
                for p in &pieces {
                    let (ps, cs) = af::factor_partial_known(&p.value, p.d);
                    for q in ps {
                        *f.primes.entry(q).or_insert(0) += 1;
                    }
                    f.composites.extend(cs);
                }
                return Ok((f.matrix(), f.text()));
            }
        }
        if n.bits() <= 120 {
            let mut f = IntFactors {
                sign: n.is_negative(),
                primes: BTreeMap::new(),
                composites: vec![],
            };
            for (p, e) in crate::numtheory::factorize(&n.abs())? {
                f.primes.insert(p, e as u32);
            }
            return Ok((f.matrix(), f.text()));
        }
        let (ps, cs) = af::factor_partial(n);
        if ps.is_empty() {
            return Err(format!(
                "could not find a factor of this {}-digit number in reasonable time",
                digits(n)
            ));
        }
        let mut f = IntFactors {
            sign: n.is_negative(),
            primes: BTreeMap::new(),
            composites: cs,
        };
        for q in ps {
            *f.primes.entry(q).or_insert(0) += 1;
        }
        Ok((f.matrix(), f.text()))
    }

    /// Some(result) if f is one of the commands here
    pub(crate) fn factor_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        Some(match f {
            "cyclotomic" => self.cyclotomic_cmd(args),
            "addfamily" => (|| {
                let [f] = args else {
                    return Err("addfamily(f) adds a polynomial family like x^5 + x + 1 to the list algfactor tries".into());
                };
                let v = self.eval(f)?;
                let (x, y, p, den) = one_var_form(&v)?;
                if y.is_some() || !den.is_one() {
                    return Err("addfamily(f): f must be a polynomial in one variable with whole-number coefficients".into());
                }
                let splits = af::factor_zpoly(&p)
                    .is_some_and(|(_, fs)| fs.iter().map(|(_, m)| *m).sum::<u32>() >= 2);
                let added = af::add_family(&show(&x), p);
                self.note(match (added, splits) {
                    (false, _) => "already in the list".into(),
                    (true, true) => {
                        format!("added {}; algfactor now tries it on every number", show(&v))
                    }
                    (true, false) => format!(
                        "added {}; note that it does not factor over the integers, so it can recognize a number but not split it",
                        show(&v)
                    ),
                });
                Ok(Expr::Bool(added))
            })(),
            "families" => (|| {
                let (nb, all) = af::families();
                let mut md = vec![
                    format!(
                        "algfactor tests every number against a^k ± b^k (any a, b up to 12·effort), Sophie Germain's a^4 + 4b^4, divisors of b^k ± 1, these {} polynomial families ({} built in, {} added with `addfamily`), and finally every sparse polynomial with small coefficients, by writing N in base x for each degree (trinomials, quadrinomials, ... of any degree):",
                        all.len(),
                        nb,
                        all.len() - nb
                    ),
                    String::new(),
                ];
                let mut tx = vec![md[0].replace('`', "")];
                for (i, (name, _)) in all.iter().enumerate() {
                    let tag = if i < nb { "" } else { " (added)" };
                    md.push(format!("* {}{}", name, tag));
                    tx.push(format!("  {}{}", name, tag));
                }
                let value = Expr::Matrix(
                    all.iter()
                        .map(|(_, f)| vec![crate::eval::value_of(&af::show_zpoly(f, "x"))])
                        .collect(),
                );
                Ok(crate::discover::with_value(
                    report("Families", tx.join("\n"), md.join("\n"), vec![]),
                    value,
                ))
            })(),
            "algfactor" => self.algfactor_cmd(args),
            "factorsearch" => self.factorsearch_cmd(args),
            "digits" => (|| {
                let [a] = args else {
                    return Err("digits takes 1 argument: digits(n)".into());
                };
                let v = self.eval(a)?;
                let n = v
                    .num()
                    .and_then(|n| n.as_bigint())
                    .ok_or_else(|| format!("digits needs a whole number, got {}", show(&v)))?;
                Ok(int(digits(&n) as i128))
            })(),
            _ => return None,
        })
    }

    fn cyclotomic_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: cyclotomic(n) is the polynomial Phi_n(x); cyclotomic(n, v) puts v (a number or an expression) in place of x";
        let (n, v) = match args {
            [n] => (self.eval(n)?, Expr::sym("x")),
            [n, v] => (self.eval(n)?, self.eval(v)?),
            _ => return Err(usage.into()),
        };
        let n = n
            .num()
            .and_then(|n| n.as_int())
            .filter(|&n| (1..=20_000).contains(&n))
            .ok_or("cyclotomic: n must be a whole number from 1 to 20000")? as u64;
        if let Some(b) = v.num().and_then(|x| x.as_bigint()) {
            return Ok(bnum(af::cyclotomic_value(n, &b, &BigInt::one())));
        }
        let p = af::cyclotomic(n);
        let x = Expr::sym("x");
        let e = upoly_expr(&p, &x);
        Ok(if v == x { e } else { calc::subst(&e, "x", &v) })
    }

    fn algfactor_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: algfactor(n), algfactor(n, effort), algfactor(n, f) or algfactor(n, f, effort), with f a polynomial family like x^6 + 27: the algebraic pieces of n and their factors; effort 1 (the default) is about a second of searching, larger numbers search more bases and exponents and factor longer";
        let (a, rest) = match args {
            [a, rest @ ..] if rest.len() <= 2 => (a, rest),
            _ => return Err(usage.into()),
        };
        let mut effort = 1u64;
        let mut family: Option<(String, ZPoly)> = None;
        for r in rest {
            let v = self.eval(r)?;
            if let Some(e) = v.num() {
                effort = e
                    .as_int()
                    .filter(|&e| (1..=10_000).contains(&e))
                    .ok_or("algfactor(n, effort): effort is a whole number from 1 to 10000")?
                    as u64;
            } else {
                let (x, y, p, den) = one_var_form(&v)?;
                if y.is_some() || !den.is_one() {
                    return Err("algfactor(n, f): f must be a polynomial in one variable with whole-number coefficients, like x^6 + 27".into());
                }
                family = Some((show(&x), p));
            }
        }
        let v = self.eval(a)?;
        let n = v
            .num()
            .and_then(|n| n.as_bigint())
            .filter(|n| n.abs() > BigInt::one())
            .ok_or("algfactor needs a whole number of 2 or more")?;
        crate::numtheory::set_effort(effort);
        let r = self.algfactor_run(a, &n, effort, family);
        crate::numtheory::set_effort(1);
        r
    }

    fn algfactor_run(
        &mut self,
        a: &Node,
        n: &BigInt,
        effort: u64,
        family: Option<(String, ZPoly)>,
    ) -> R<Expr> {
        let typed = int_form(self, Some(a), n, false);
        let mut var = "x".to_string();
        let mut no_fit = None;
        let mut family_name: Option<String> = None;
        let form = if let Some((v, f)) = family {
            var = v.clone();
            match af::int_root(&f, n) {
                Some(x) => Some(af::Form::Poly { f, x }),
                None => {
                    no_fit = Some(af::show_zpoly(&f, &v));
                    None
                }
            }
        } else {
            match typed {
                Some((a, b, k, plus, _)) => Some(af::Form::Exact { a, b, k, plus }),
                None => af::find_form(&n.abs(), effort)
                    .or_else(|| {
                        let (name, f, x) = af::catalogue_match(&n.abs())?;
                        family_name = Some(format!("from the list of families: {}", name));
                        Some(af::Form::Poly { f, x })
                    })
                    .or_else(|| {
                        let (name, f, x, y) = af::catalogue_match2(&n.abs(), effort)?;
                        family_name = Some(format!(
                            "from the list of families, with a second variable: {}",
                            name
                        ));
                        Some(af::Form::Poly2 { f, x, y })
                    })
                    .or_else(|| {
                        let (f, x) = af::base_search(&n.abs(), effort)?;
                        family_name = Some("found by writing N in base x with small digits".into());
                        Some(af::Form::Poly { f, x })
                    })
                    .or_else(|| {
                        let (a, b) = af::fermat(&n.abs(), 20_000 * effort)?;
                        Some(af::Form::Squares { a, b })
                    }),
            }
        };
        let nd = digits(n);
        let (pieces, head_md, head_tx) = match &form {
            Some(af::Form::Exact { a, b, k, plus }) => {
                let f = form_latex(a, b, *k, *plus);
                let ft = f.replace(['{', '}'], "");
                match af::algebraic_pieces(a, b, *k, *plus) {
                    Some(p) => {
                        let n_au = p.iter().filter(|x| x.text.starts_with('L')).count();
                        let extra = if n_au > 0 {
                            format!(", {} of them Aurifeuillean pairs $L \\cdot M$", n_au)
                        } else {
                            String::new()
                        };
                        (
                            p.clone(),
                            format!(
                                "$${}$$ splits algebraically into {} pieces{}:",
                                f,
                                p.len(),
                                extra
                            ),
                            format!("{} splits algebraically into {} pieces:", ft, p.len()),
                        )
                    }
                    None => (
                        vec![],
                        format!(
                            "This number is $${}$$ but that form does not split, so it is one piece:",
                            f
                        ),
                        format!(
                            "This number is {} but that form does not split, so it is one piece:",
                            ft
                        ),
                    ),
                }
            }
            Some(af::Form::Divides { b, k, plus, .. }) => {
                let op = if *plus { "+" } else { "-" };
                let p = af::divisor_pieces(n, *b, *k, *plus);
                let s = if p.len() == 1 { "" } else { "s" };
                (
                    p.clone(),
                    format!(
                        "This {}-digit number $N$ divides $${}^{{{}}} {} 1$$ ({} is the smallest such exponent for base {}), so it splits along the algebraic pieces of that number into {} part{}:",
                        nd,
                        b,
                        k,
                        op,
                        k,
                        b,
                        p.len(),
                        s
                    ),
                    format!(
                        "This {}-digit number N divides {}^{} {} 1, so it splits along the pieces of that number into {} part{}:",
                        nd,
                        b,
                        k,
                        op,
                        p.len(),
                        s
                    ),
                )
            }
            Some(af::Form::Poly { f, x }) => {
                let fs = af::show_zpoly(f, &var);
                let p = af::poly_pieces(f, x, &var).unwrap_or_default();
                if p.len() <= 1 {
                    let m = format!(
                        "This number is {} with {} = {}, but that polynomial does not factor over the integers, so it is one piece:",
                        fs, var, x
                    );
                    (vec![], m.clone(), m)
                } else {
                    let from = family_name
                        .as_ref()
                        .map(|n| format!(" ({})", n))
                        .unwrap_or_default();
                    let m = format!(
                        "This number is {} with {} = {}{}. The polynomial factors, so the number splits into {} pieces:",
                        fs,
                        var,
                        x,
                        from,
                        p.len()
                    );
                    (p, m.clone(), m)
                }
            }
            Some(af::Form::Poly2 { f, x, y }) => {
                let fs = af::show_hzpoly(f, "x", "y");
                let p = af::poly2_pieces(f, x, y).unwrap_or_default();
                let from = family_name
                    .as_ref()
                    .map(|n| format!(" ({})", n))
                    .unwrap_or_default();
                let m = format!(
                    "This number is {} with x = {} and y = {}{}. The polynomial factors, so the number splits into {} pieces:",
                    fs,
                    x,
                    y,
                    from,
                    p.len()
                );
                (p, m.clone(), m)
            }
            Some(af::Form::Squares { a, b }) => {
                let p = vec![
                    af::Piece {
                        text: "a - b".into(),
                        latex: "a - b".into(),
                        value: a - b,
                        d: 0,
                    },
                    af::Piece {
                        text: "a + b".into(),
                        latex: "a + b".into(),
                        value: a + b,
                        d: 0,
                    },
                ];
                let m = format!(
                    "This number is a difference of two squares, a^2 - b^2 = (a - b)(a + b), with a = {} and b = {} (found by Fermat's method, which works when two factors are close to the square root of N), so it splits into 2 pieces:",
                    a, b
                );
                (p, m.clone(), m)
            }
            None => match &no_fit {
                Some(fs) => {
                    let m = format!(
                        "This number is not {} for any whole number {}, so it is one piece:",
                        fs, var
                    );
                    (vec![], m.clone(), m)
                }
                None => (vec![], String::new(), String::new()),
            },
        };
        let pieces = if pieces.is_empty() {
            vec![af::Piece {
                text: n.to_string(),
                latex: n.to_string(),
                value: n.abs(),
                d: 0,
            }]
        } else {
            pieces
        };
        let (head_md, head_tx) = if head_md.is_empty() {
            let (b1, b2, k2) = (
                (12 * effort).min(10_000),
                (12 * effort).min(1000),
                (3000 * effort).min(10_000_000),
            );
            (
                format!(
                    "No algebraic form found: this {}-digit number is not $a^k \\pm 1$ for any $a$, not $a^k \\pm b^k$ for $b \\le {}$, and does not divide $b^k \\pm 1$ for $b \\le {}$, $k \\le {}$; it fits none of the listed families, in no base $x$ is it a sparse polynomial with digits up to {} that factors, and it is not $a^2 - b^2$ with $a$ within {} of $\\sqrt{{N}}$. It is one piece:",
                    nd,
                    b1,
                    b2,
                    k2,
                    (8 * effort).min(100_000),
                    20_000 * effort
                ),
                format!(
                    "No algebraic form found: this {}-digit number is not a^k ± 1 for any a, not a^k ± b^k for b <= {}, and does not divide b^k ± 1 for b <= {}, k <= {}; it fits none of the listed families, in no base x is it a sparse polynomial with digits up to {} that factors, and it is not a^2 - b^2 with a within {} of sqrt(N). It is one piece:",
                    nd,
                    b1,
                    b2,
                    k2,
                    (8 * effort).min(100_000),
                    20_000 * effort
                ),
            )
        } else {
            (head_md, head_tx)
        };
        let mut md = vec![
            head_md,
            String::new(),
            "| piece | digits | factors |".into(),
            "|---|---|---|".into(),
        ];
        let mut tx = vec![head_tx];
        let (mut nprime, mut ncomp, mut compdig) = (0, 0, 0);
        let mut formulas = vec![];
        let mut allf: Vec<BigInt> = vec![];
        for p in &pieces {
            let (ps, cs) = af::factor_partial_known(&p.value, p.d);
            allf.extend(ps.iter().cloned());
            allf.extend(cs.iter().cloned());
            nprime += ps.len();
            ncomp += cs.len();
            compdig += cs.iter().map(digits).sum::<usize>();
            let mut grouped: BTreeMap<BigInt, u32> = BTreeMap::new();
            for q in ps {
                *grouped.entry(q).or_insert(0) += 1;
            }
            let mut fac: Vec<String> = grouped
                .iter()
                .map(|(q, e)| {
                    if *e == 1 {
                        q.to_string()
                    } else {
                        format!("{}^{}", q, e)
                    }
                })
                .collect();
            let mut fac_md: Vec<String> = grouped
                .iter()
                .map(|(q, e)| {
                    if *e == 1 {
                        q.to_string()
                    } else {
                        format!("{}<sup>{}</sup>", q, e)
                    }
                })
                .collect();
            for c in &cs {
                fac.push(format!("C{} (composite, no factor found)", digits(c)));
                fac_md.push(format!("**C{}** (composite, no factor found)", digits(c)));
            }
            let single_prime =
                grouped.len() == 1 && cs.is_empty() && grouped.values().all(|e| *e == 1);
            let cell = if single_prime {
                "prime".to_string()
            } else {
                fac_md.join(" · ")
            };
            md.push(format!(
                "| ${}$ | {} | {} |",
                p.latex,
                digits(&p.value),
                cell
            ));
            let shown = if digits(&p.value) <= 40 {
                format!(" = {}", p.value)
            } else {
                format!(" ({} digits)", digits(&p.value))
            };
            tx.push(format!(
                "  {}{}: {}",
                p.text,
                shown,
                if single_prime {
                    "prime".into()
                } else {
                    fac.join(" * ")
                }
            ));
            formulas.push(bnum(p.value.clone()));
        }
        let summary = if ncomp == 0 {
            format!("All {} prime factors found.", nprime)
        } else {
            format!(
                "{} prime factors found; {} composite part{} with {} digits in all did not split in reasonable time.",
                nprime,
                ncomp,
                if ncomp == 1 { "" } else { "s" },
                compdig
            )
        };
        md.push(String::new());
        md.push(summary.clone());
        tx.push(summary);
        allf.sort();
        md.push(String::new());
        md.push("The value has one row per factor found, like `factor`: the prime (or a composite part left over) and its exponent.".into());
        let value = factor_rows(allf);
        Ok(crate::discover::with_value(
            report("Algebraic factors", tx.join("\n"), md.join("\n"), formulas),
            value,
        ))
    }

    fn factorsearch_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: factorsearch(expr, k, a, b) factors expr over the integers for k = a, a+1, ..., b and lists the k for which it splits; expr must be a polynomial in one other variable, or homogeneous in two";
        let [body, kn, a, b, rest @ ..] = args else {
            return Err(usage.into());
        };
        let Node::Ident(k) = kn else {
            return Err(usage.into());
        };
        let get = |s: &mut Session, n: &Node| -> R<i128> {
            s.eval(n)?
                .num()
                .and_then(|x| x.as_int())
                .ok_or_else(|| usage.to_string())
        };
        let (a, b) = (get(self, a)?, get(self, b)?);
        let step = match rest {
            [s] => get(self, s)?,
            [] => 1,
            _ => return Err(usage.into()),
        };
        if step <= 0 || b < a || (b - a) / step > 5000 {
            return Err(
                "factorsearch: give a <= b, a positive step and at most 5000 values".into(),
            );
        }
        let mut vals = vec![];
        let mut kv = a;
        while kv <= b {
            vals.push(int(kv));
            kv += step;
        }
        let polys = self.with_values(body, k, &vals)?;
        let mut md = vec![
            format!("| {} | polynomial | factors |", k),
            "|---|---|---|".into(),
        ];
        let mut tx = vec![];
        let mut formulas = vec![];
        let (total, mut hits) = (vals.len(), 0);
        for (kv, e) in vals.iter().zip(polys) {
            let ek = calc::expand(&e);
            if let Some((c, fs)) = factor_parts(&ek) {
                let count: u32 = fs.iter().map(|(_, m)| *m).sum();
                let monomial = fs.iter().all(|(f, _)| matches!(f, Expr::Sym(_)));
                if count >= 2 && !monomial {
                    hits += 1;
                    let fe = assemble(c, &fs);
                    md.push(format!(
                        "| {} | ${}$ | ${}$ |",
                        show(kv),
                        latex(&ek),
                        latex(&fe)
                    ));
                    tx.push(format!(
                        "{} = {}: {} = {}",
                        k,
                        show(kv),
                        show(&ek),
                        show(&fe)
                    ));
                    formulas.push(fe);
                }
            }
        }
        let summary = format!(
            "{} of {} values of {} give a polynomial that factors over the integers.",
            hits, total, k
        );
        if hits == 0 {
            md.clear();
        }
        md.push(String::new());
        md.push(summary.clone());
        tx.push(summary);
        Ok(report(
            "Factor search",
            tx.join("\n"),
            md.join("\n"),
            formulas,
        ))
    }
}

// ---------- factoring over other fields ----------

/// a polynomial in one variable, or homogeneous in two: (x, y and total degree when homogeneous, integer coefficients in x, common denominator)
fn one_var_form(e: &Expr) -> R<(Expr, Option<(Expr, usize)>, ZPoly, BigInt)> {
    let vars = e.symbols();
    let bad = || {
        "factor(p, field): p must be a polynomial in one variable, or homogeneous in two (like x^2 + y^2)".to_string()
    };
    if vars.is_empty() || vars.len() > 2 {
        return Err(bad());
    }
    let ts = terms_of(e, &vars).ok_or_else(bad)?;
    let den = ts
        .iter()
        .fold(BigInt::one(), |acc, (_, c)| acc.lcm(c.denom()));
    let total: Vec<usize> = ts
        .iter()
        .filter(|(_, c)| !c.is_zero())
        .map(|(ex, _)| ex.iter().sum())
        .collect();
    let dtot = *total.first().ok_or_else(bad)?;
    if vars.len() == 2 && total.iter().any(|&d| d != dtot) {
        return Err(bad());
    }
    let dmax = ts.iter().map(|(ex, _)| ex[0]).max().unwrap_or(0);
    let mut p: ZPoly = vec![BigInt::zero(); dmax + 1];
    for (ex, c) in &ts {
        p[ex[0]] += (c * BigRational::from_integer(den.clone())).to_integer();
    }
    let y = if vars.len() == 2 {
        Some((Expr::sym(&vars[1]), dtot))
    } else {
        None
    };
    Ok((Expr::sym(&vars[0]), y, af::trim(p), den))
}

/// integer coefficients (low first) of a polynomial in one variable
pub(crate) fn int_poly(e: &Expr) -> Option<ZPoly> {
    let (_, y, p, den) = one_var_form(e).ok()?;
    if y.is_some() || !den.is_one() {
        None
    } else {
        Some(p)
    }
}

/// sum c_k x^k, homogenized with y when given
fn coeffs_expr(cs: &[Expr], x: &Expr, y: Option<&Expr>) -> Expr {
    let d = cs.len() - 1;
    add(cs
        .iter()
        .enumerate()
        .map(|(k, c)| {
            let mut v = vec![c.clone(), pow(x.clone(), int(k as i128))];
            if let Some(y) = y {
                v.push(pow(y.clone(), int((d - k) as i128)));
            }
            mul(v)
        })
        .collect())
}

/// the squarefree d with Q(v) = Q(sqrt(d)), when v is a square root of a rational
fn quadratic_field(v: &Expr) -> Option<BigInt> {
    let sq = calc::expand(&pow(v.clone(), int(2)));
    let q = sq.num().filter(|n| n.is_exact())?.big()?;
    let t = q.numer() * q.denom();
    if t.is_zero() {
        return None;
    }
    let mut d = if t.sign() == num_bigint::Sign::Minus {
        -BigInt::one()
    } else {
        BigInt::one()
    };
    for (p, e) in crate::numtheory::factorize(&t.magnitude().clone().into()).ok()? {
        if e % 2 == 1 {
            d *= p;
        }
    }
    if d.is_one() { None } else { Some(d) }
}

impl Session {
    /// factor(p, field): field is a prime (mod p) or a square root such as sqrt(2), i or sqrt(-3)
    pub(crate) fn factor_over(&mut self, e: &Expr, field: &Expr) -> R<Expr> {
        let (x, y, p, den) = one_var_form(e)?;
        let yref = y.as_ref().map(|(y, _)| y);
        let extra_y = |degf: usize| -> Option<(Expr, u32)> {
            match &y {
                Some((y, dt)) if *dt > degf => Some((y.clone(), (*dt - degf) as u32)),
                _ => None,
            }
        };
        let degf = p.len().saturating_sub(1);
        if let Some(n) = field.num().filter(|n| n.is_exact() && n.is_integer()) {
            let pr = n
                .as_int()
                .filter(|&q| q >= 2 && q < 1 << 31 && crate::numtheory::small::is_prime(q as u128))
                .ok_or("factor(p, n) factors modulo a prime n: give a prime below 2^31")?
                as u64;
            let pb = BigInt::from(pr);
            let dinv = (&den % &pb)
                .modinv(&pb)
                .ok_or("factor(p, n): a denominator of p is divisible by n")?;
            let (lead, fs) = af::factor_mod_p(&p, pr).ok_or("factor: polynomial too large")?;
            let lead = (BigInt::from(lead) * dinv) % &pb;
            let mut v = vec![bnum(lead)];
            if let Some((yy, m)) = extra_y(af::trim(p.clone()).len().saturating_sub(1)) {
                v.push(pow(yy, int(m as i128)));
            }
            for (h, m) in fs {
                let cs: Vec<Expr> = h.iter().map(|&c| int(c as i128)).collect();
                v.push(pow(coeffs_expr(&cs, &x, yref), int(m as i128)));
            }
            return Ok(mul(v));
        }
        let d = quadratic_field(field).ok_or("factor(p, field): the field is a prime (factor mod p), or a square root like sqrt(2), i or sqrt(-3)")?;
        let (c, fs) = crate::algext::factor_ext(&p, &d).ok_or("factor: polynomial too large")?;
        let root = if d == -BigInt::one() {
            Expr::Const(crate::expr::Const::I)
        } else {
            crate::build::sqrt(bnum(d.clone()))
        };
        let mut v = vec![Expr::Num(Number::from_big(
            c / BigRational::from_integer(den),
        ))];
        if let Some((yy, m)) = extra_y(degf) {
            v.push(pow(yy, int(m as i128)));
        }
        for (h, m) in fs {
            let cs: Vec<Expr> = h
                .iter()
                .map(|q| {
                    add2(
                        Expr::Num(Number::from_big(q.a.clone())),
                        mul2(Expr::Num(Number::from_big(q.b.clone())), root.clone()),
                    )
                })
                .collect();
            v.push(pow(coeffs_expr(&cs, &x, yref), int(m as i128)));
        }
        Ok(mul(v))
    }
}
