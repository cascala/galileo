//! Rational-function normal form, used to tidy results (curvature in particular).
//!
//! An expression is written as `numerator / (f1^k1 * f2^k2 * ...)`, where the
//! numerator and the factors `fi` are expanded polynomials in "kernels": symbols,
//! constants, function applications such as `sin(theta)`, and roots such as
//! `sqrt(r)`. Sums are put over a common denominator, the numerator is divided
//! exactly by the denominator factors wherever possible, and `sin^2 + cos^2 = 1`
//! is applied in one direction. That decides zero (the Ricci tensor of the
//! Schwarzschild metric is zero) and cancels common factors such as `r - r_s`,
//! which the plain `together`/`cancel` in [`crate::calc`] cannot do.

use crate::build::*;
use crate::expr::{Expr, Func};
use crate::num::Number;
use crate::print::show;
use std::collections::BTreeMap;

/// sorted (kernel id, exponent) pairs
type Mono = Vec<(usize, u32)>;
type Poly = BTreeMap<Mono, Number>;

#[derive(Clone, Copy, PartialEq)]
enum Trig {
    None,
    SinToCos,
    CosToSin,
}

#[derive(Clone)]
struct Rat {
    num: Poly,
    den: Vec<(Poly, u32)>,
}

struct Ctx {
    trig: Trig,
    /// per-argument directions that override `trig`, e.g. sin(phi)^2 -> 1 - cos(phi)^2 only
    trig_args: Vec<(Expr, Trig)>,
    kernels: Vec<Expr>,
    /// polynomials seen as sums in the input: candidate factors of denominators
    candidates: Vec<Poly>,
    /// work budget, so pathological inputs give up instead of hanging
    budget: usize,
}

struct GiveUp;
type R<T> = Result<T, GiveUp>;

fn mono_mul(a: &Mono, b: &Mono) -> Mono {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0, 0);
    while i < a.len() || j < b.len() {
        if j >= b.len() || (i < a.len() && a[i].0 < b[j].0) {
            out.push(a[i]);
            i += 1;
        } else if i >= a.len() || b[j].0 < a[i].0 {
            out.push(b[j]);
            j += 1;
        } else {
            out.push((a[i].0, a[i].1 + b[j].1));
            i += 1;
            j += 1;
        }
    }
    out
}

fn mono_div(a: &Mono, b: &Mono) -> Option<Mono> {
    let mut out = a.clone();
    for &(k, e) in b {
        let pos = out.iter().position(|x| x.0 == k)?;
        if out[pos].1 < e {
            return None;
        }
        out[pos].1 -= e;
        if out[pos].1 == 0 {
            out.remove(pos);
        }
    }
    Some(out)
}

/// Monomial order: lexicographic, smaller kernel ids are "bigger"
fn mono_cmp(a: &Mono, b: &Mono) -> std::cmp::Ordering {
    use std::cmp::Ordering::*;
    for i in 0..a.len().max(b.len()) {
        if i >= a.len() {
            return Less;
        }
        if i >= b.len() {
            return Greater;
        }
        let ((ka, ea), (kb, eb)) = (a[i], b[i]);
        if ka != kb {
            return if ka < kb { Greater } else { Less };
        }
        if ea != eb {
            return ea.cmp(&eb);
        }
    }
    Equal
}

fn leading(p: &Poly) -> (&Mono, Number) {
    let (m, c) = p.iter().max_by(|a, b| mono_cmp(a.0, b.0)).unwrap();
    (m, c.clone())
}

fn constant(c: Number) -> Poly {
    let mut p = Poly::new();
    if !c.is_zero() {
        p.insert(vec![], c);
    }
    p
}

fn as_constant(p: &Poly) -> Option<Number> {
    match p.len() {
        0 => Some(Number::zero()),
        1 => p.get(&vec![]).cloned(),
        _ => None,
    }
}

/// Round a float to 12 significant digits, so equal values computed in
/// different ways compare equal; exact numbers are left alone.
fn round(c: Number) -> Number {
    match c {
        Number::Float(f) if f != 0.0 && f.is_finite() => {
            let mag = 10f64.powf(11.0 - f.abs().log10().floor());
            Number::Float((f * mag).round() / mag)
        }
        _ => c,
    }
}

/// drop zeros (and float noise) and round
fn clean(p: Poly) -> Poly {
    let scale = p.values().map(|c| c.to_f64().abs()).fold(0.0, f64::max);
    p.into_iter()
        .filter(|(_, c)| match c {
            Number::Rat(..) | Number::Big(_) | Number::Dec(..) => !c.is_zero(),
            Number::Float(f) => f.abs() > 1e-11 * scale && *f != 0.0,
        })
        .map(|(m, c)| (m, round(c)))
        .collect()
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

/// p = c * q with q primitive: integer coefficients without common factor and a
/// positive leading coefficient (or a leading coefficient 1 if there are floats)
fn normalize(p: &Poly) -> (Number, Poly) {
    let (_, lc) = leading(p);
    let exact = p.values().all(|c| c.is_exact());
    let c = if exact {
        let mut g = 0i128;
        let mut l = 1i128;
        let mut ok = true;
        for v in p.values() {
            let (Some(n), Some(d)) = (v.numer(), v.denom()) else {
                ok = false;
                break;
            };
            g = igcd(g, n);
            match (l / igcd(l, d)).checked_mul(d) {
                Some(x) => l = x,
                None => ok = false,
            }
        }
        match (ok, Number::rat(g, l)) {
            (true, Some(c)) if !c.is_zero() => {
                if lc.is_negative() {
                    c.neg()
                } else {
                    c
                }
            }
            _ => lc,
        }
    } else {
        lc
    };
    let inv = c.recip().unwrap_or(Number::one());
    (
        c,
        clean(p.iter().map(|(m, v)| (m.clone(), v.mul(&inv))).collect()),
    )
}

impl Ctx {
    fn new(trig: Trig) -> Ctx {
        Ctx {
            trig,
            trig_args: vec![],
            kernels: vec![],
            candidates: vec![],
            budget: 50_000_000,
        }
    }

    fn spend(&mut self, n: usize) -> R<()> {
        if self.budget < n {
            return Err(GiveUp);
        }
        self.budget -= n;
        Ok(())
    }

    fn kernel(&mut self, e: Expr) -> usize {
        if let Some(i) = self.kernels.iter().position(|k| *k == e) {
            return i;
        }
        self.kernels.push(e);
        self.kernels.len() - 1
    }

    fn kpoly(&mut self, e: Expr) -> Poly {
        let k = self.kernel(e);
        let mut p = Poly::new();
        p.insert(vec![(k, 1)], Number::one());
        p
    }

    fn add(&mut self, a: &Poly, b: &Poly, s: Number) -> R<Poly> {
        self.spend(a.len() + b.len())?;
        let mut m = a.clone();
        for (mo, c) in b {
            let e = m.entry(mo.clone()).or_insert(Number::zero());
            *e = e.add(c.mul(&s));
        }
        self.reduce_poly(clean(m))
    }

    fn mul(&mut self, a: &Poly, b: &Poly) -> R<Poly> {
        self.spend(a.len() * b.len() + 1)?;
        let mut m = Poly::new();
        for (ma, ca) in a {
            for (mb, cb) in b {
                let e = m.entry(mono_mul(ma, mb)).or_insert(Number::zero());
                *e = e.add(ca.mul(cb));
            }
        }
        self.reduce_poly(clean(m))
    }

    fn pow(&mut self, a: &Poly, k: u32) -> R<Poly> {
        let mut acc = constant(Number::one());
        for _ in 0..k {
            acc = self.mul(&acc, a)?;
        }
        Ok(acc)
    }

    /// the kernel that sin^2 (or cos^2) is rewritten into
    fn partner(&mut self, k: usize) -> Option<usize> {
        let dir = match &self.kernels[k] {
            Expr::Func(Func::Sin | Func::Cos, a) => self
                .trig_args
                .iter()
                .find(|(x, _)| *x == **a)
                .map_or(self.trig, |(_, t)| *t),
            _ => return None,
        };
        let (from, to) = match dir {
            Trig::None => return None,
            Trig::SinToCos => (Func::Sin, Func::Cos),
            Trig::CosToSin => (Func::Cos, Func::Sin),
        };
        match &self.kernels[k] {
            Expr::Func(f, a) if *f == from => {
                let e = Expr::Func(to, a.clone());
                Some(self.kernel(e))
            }
            _ => None,
        }
    }

    /// root kernels b^(1/q): (b as a polynomial, q)
    fn root(&self, k: usize) -> Option<(Expr, u32)> {
        if let Expr::Pow(b, p) = &self.kernels[k] {
            if let Some(Number::Rat(1, q)) = p.num() {
                if q > 1 && q < 64 {
                    return Some(((**b).clone(), q as u32));
                }
            }
        }
        None
    }

    /// sin(x)^2 -> 1 - cos(x)^2 (or the reverse) and sqrt(b)^2 -> b
    fn reduce_poly(&mut self, p: Poly) -> R<Poly> {
        let mut changed = false;
        let mut out = Poly::new();
        let put = |out: &mut Poly, mo: Mono, c: Number| {
            let e = out.entry(mo).or_insert(Number::zero());
            *e = e.add(c);
        };
        for (mo, c) in p {
            let mut hit = None;
            for &(k, e) in &mo {
                if e >= 2 {
                    if let Some(q) = self.partner(k) {
                        hit = Some((k, e, Ok(q)));
                        break;
                    }
                }
                if let Some((b, q)) = self.root(k) {
                    if e >= q {
                        hit = Some((k, e, Err((b, q))));
                        break;
                    }
                }
            }
            match hit {
                None => put(&mut out, mo, c),
                Some((k, e, Ok(q))) => {
                    changed = true;
                    let mut rest: Mono = mo.iter().filter(|x| x.0 != k).copied().collect();
                    if e > 2 {
                        rest = mono_mul(&rest, &vec![(k, e - 2)]);
                    }
                    put(&mut out, rest.clone(), c.clone());
                    put(&mut out, mono_mul(&rest, &vec![(q, 2)]), c.neg());
                }
                Some((k, e, Err((b, q)))) => {
                    // b must be a polynomial for the rewrite to stay polynomial
                    let r = self.rat_of(&b)?;
                    if !r.den.is_empty() {
                        put(&mut out, mo, c);
                        continue;
                    }
                    changed = true;
                    let mut rest: Mono = mo.iter().filter(|x| x.0 != k).copied().collect();
                    if e % q > 0 {
                        rest = mono_mul(&rest, &vec![(k, e % q)]);
                    }
                    let bp = self.pow(&r.num, e / q)?;
                    let mut term = Poly::new();
                    term.insert(rest, c);
                    let t = self.mul(&term, &bp)?;
                    for (m2, c2) in t {
                        put(&mut out, m2, c2);
                    }
                }
            }
        }
        let out = clean(out);
        if changed {
            self.reduce_poly(out)
        } else {
            Ok(out)
        }
    }

    /// p / d if d divides p exactly
    fn divide(&mut self, p: &Poly, d: &Poly) -> R<Option<Poly>> {
        if d.is_empty() {
            return Ok(None);
        }
        let (ld, cd) = leading(d);
        let (ld, cd) = (ld.clone(), cd);
        let scale = p.values().map(|c| c.to_f64().abs()).fold(0.0, f64::max);
        let mut rem = p.clone();
        let mut q = Poly::new();
        while !rem.is_empty() {
            self.spend(d.len())?;
            let (lr, cr) = leading(&rem);
            let Some(mo) = mono_div(lr, &ld) else {
                return Ok(None);
            };
            let Some(c) = cr.div(&cd) else {
                return Ok(None);
            };
            let e = q.entry(mo.clone()).or_insert(Number::zero());
            *e = e.add(&c);
            for (md, cdd) in d {
                let x = mono_mul(&mo, md);
                let e = rem.entry(x).or_insert(Number::zero());
                *e = e.sub(c.mul(cdd));
            }
            rem.retain(|_, v| match v {
                Number::Rat(..) | Number::Big(_) | Number::Dec(..) => !v.is_zero(),
                Number::Float(f) => f.abs() > 1e-9 * scale,
            });
        }
        Ok(Some(clean(q)))
    }

    /// split off monomial and numeric content: p = c * f1^k1 * ...
    fn factor_den(&mut self, p: &Poly) -> R<(Number, Vec<(Poly, u32)>)> {
        if p.is_empty() {
            return Err(GiveUp); // division by zero
        }
        let mut content: Mono = p.keys().next().unwrap().clone();
        for mo in p.keys() {
            content = content
                .into_iter()
                .filter_map(|(k, e)| mo.iter().find(|x| x.0 == k).map(|x| (k, e.min(x.1))))
                .collect();
        }
        let rest: Poly = p
            .iter()
            .map(|(m, c)| (mono_div(m, &content).unwrap(), c.clone()))
            .collect();
        let (c, rest) = normalize(&rest);
        let mut fs: Vec<(Poly, u32)> = content
            .iter()
            .map(|&(k, e)| {
                let mut kp = Poly::new();
                kp.insert(vec![(k, 1)], Number::one());
                (kp, e)
            })
            .collect();
        if as_constant(&rest).is_none_or(|x| !x.is_one()) {
            fs.push((rest, 1));
        }
        Ok((c, fs))
    }

    fn rat_mul(&mut self, a: &Rat, b: &Rat) -> R<Rat> {
        Ok(Rat {
            num: self.mul(&a.num, &b.num)?,
            den: merge(&a.den, &b.den),
        })
    }

    fn rat_add(&mut self, a: &Rat, b: &Rat) -> R<Rat> {
        let mut common: Vec<(Poly, u32)> = vec![];
        for (p, k) in a.den.iter().chain(&b.den) {
            match common.iter_mut().find(|x| x.0 == *p) {
                Some(x) => x.1 = x.1.max(*k),
                None => common.push((p.clone(), *k)),
            }
        }
        let lift = |r: &Rat, cx: &mut Ctx| -> R<Poly> {
            let mut acc = r.num.clone();
            for (p, k) in &common {
                let have = r.den.iter().find(|x| x.0 == *p).map_or(0, |x| x.1);
                let pk = cx.pow(p, k - have)?;
                acc = cx.mul(&acc, &pk)?;
            }
            Ok(acc)
        };
        let la = lift(a, self)?;
        let lb = lift(b, self)?;
        Ok(Rat {
            num: self.add(&la, &lb, Number::one())?,
            den: common,
        })
    }

    fn rat_inv(&mut self, a: &Rat) -> R<Rat> {
        let (c, fs) = self.factor_den(&a.num)?;
        let mut num = constant(c.recip().ok_or(GiveUp)?);
        for (p, k) in &a.den {
            let pk = self.pow(p, *k)?;
            num = self.mul(&num, &pk)?;
        }
        Ok(Rat { num, den: fs })
    }

    fn rat_pow(&mut self, a: &Rat, k: i128) -> R<Rat> {
        let mut acc = Rat {
            num: constant(Number::one()),
            den: vec![],
        };
        for _ in 0..k.abs() {
            acc = self.rat_mul(&acc, a)?;
        }
        if k < 0 { self.rat_inv(&acc) } else { Ok(acc) }
    }

    fn atom(&mut self, e: Expr) -> Rat {
        Rat {
            num: self.kpoly(e),
            den: vec![],
        }
    }

    fn remember(&mut self, r: Rat) -> R<Rat> {
        if r.den.is_empty() && r.num.len() > 1 {
            let (_, fs) = self.factor_den(&r.num)?;
            for (p, _) in fs {
                if p.len() > 1 && !self.candidates.contains(&p) {
                    self.candidates.push(p);
                }
            }
        }
        Ok(r)
    }

    fn rat_of(&mut self, e: &Expr) -> R<Rat> {
        use Expr::*;
        self.spend(1)?;
        Ok(match e {
            Num(n) => Rat {
                num: constant(n.clone()),
                den: vec![],
            },
            Add(ts) => {
                let mut acc = Rat {
                    num: Poly::new(),
                    den: vec![],
                };
                for t in ts {
                    let r = self.rat_of(t)?;
                    acc = self.rat_add(&acc, &r)?;
                }
                self.remember(acc)?
            }
            Mul(fs) => {
                let mut acc = Rat {
                    num: constant(Number::one()),
                    den: vec![],
                };
                for f in fs {
                    let r = self.rat_of(f)?;
                    acc = self.rat_mul(&acc, &r)?;
                }
                acc
            }
            Pow(b, p) if **b == Const(crate::expr::Const::E) && !p.is_number() => {
                // e^(2a - 3b) = (e^a)^2 * (e^b)^-3, so exponentials share kernels
                let terms = match &**p {
                    Add(ts) => ts.clone(),
                    other => vec![other.clone()],
                };
                let mut acc = Rat {
                    num: constant(Number::one()),
                    den: vec![],
                };
                for t in terms {
                    let (c, rest) = t.coeff_and_rest();
                    let r = match c.as_int() {
                        Some(k) if k.abs() <= 64 && !rest.is_number() => {
                            let a = self.atom(pow(Const(crate::expr::Const::E), inner(&rest)));
                            self.rat_pow(&a, k)?
                        }
                        _ => self.atom(pow(Const(crate::expr::Const::E), inner(&t))),
                    };
                    acc = self.rat_mul(&acc, &r)?;
                }
                acc
            }
            Pow(b, p) => match p.num() {
                Some(n) if n.is_integer() && n.to_f64().abs() <= 64.0 && n.is_exact() => {
                    let r = self.rat_of(b)?;
                    self.rat_pow(&r, n.as_int().unwrap())?
                }
                Some(Number::Rat(n, q)) if q > 1 && n.abs() <= 64 => {
                    // b^(n/q) = (b^(1/q))^n
                    let k = pow(inner(b), Expr::Num(Number::rat(1, q).unwrap()));
                    let r = self.atom(k);
                    self.rat_pow(&r, n)?
                }
                _ => self.atom(pow(inner(b), inner(p))),
            },
            Func(f, a) => self.atom(func(*f, inner(a))),
            other => self.atom(other.clone()),
        })
    }

    /// cancel denominator factors that divide the numerator
    fn reduce(&mut self, r0: Rat) -> R<Rat> {
        let mut num = r0.num.clone();
        // split denominator factors further using candidate factors
        let mut split: Vec<(Poly, u32)> = vec![];
        let candidates = self.candidates.clone();
        for (p, k) in &r0.den {
            let mut rest = p.clone();
            let mut found: Vec<(Poly, u32)> = vec![];
            for c in &candidates {
                if *c == rest || rest.len() <= 1 {
                    continue;
                }
                while let Some(q) = self.divide(&rest, c)? {
                    if q.is_empty() {
                        break;
                    }
                    found.push((c.clone(), *k));
                    rest = q;
                    if as_constant(&rest).is_some() {
                        break;
                    }
                }
            }
            if found.is_empty() {
                split.push((p.clone(), *k));
            } else {
                let (c, fs) = match as_constant(&rest) {
                    Some(c) => (c, vec![]),
                    None => self.factor_den(&rest)?,
                };
                let ck = c
                    .recip()
                    .ok_or(GiveUp)?
                    .pow(Number::int(*k as i128))
                    .ok_or(GiveUp)?;
                num = self.mul(&num, &constant(ck))?;
                split.extend(found);
                split.extend(fs.into_iter().map(|(q, e)| (q, e * k)));
            }
        }
        let den = merge(&split, &[]);
        let mut out = vec![];
        for (p, k) in den {
            let mut left = k;
            while left > 0 {
                match self.divide(&num, &p)? {
                    Some(q) => {
                        num = q;
                        left -= 1;
                    }
                    None => break,
                }
            }
            if left > 0 {
                out.push((p, left));
            }
        }
        Ok(Rat { num, den: out })
    }

    fn mono_expr(&self, mo: &Mono) -> Vec<Expr> {
        mo.iter()
            .map(|&(k, e)| pow(self.kernels[k].clone(), Expr::int(e as i128)))
            .collect()
    }

    fn poly_expr(&self, p: &Poly) -> Expr {
        add(p
            .iter()
            .map(|(mo, c)| {
                let mut f = self.mono_expr(mo);
                f.push(Expr::Num(c.clone()));
                mul(f)
            })
            .collect())
    }

    /// a polynomial as an expression, with the sign chosen so that it reads
    /// naturally (r-r_s rather than r_s-r); returns whether it was negated
    fn signed(&self, p: &Poly) -> (bool, Expr) {
        let e = self.poly_expr(p);
        match &e {
            Expr::Add(v)
                if {
                    let negs = v.iter().filter(|t| t.is_negative_term()).count();
                    2 * negs > v.len() || (2 * negs == v.len() && v[0].is_negative_term())
                } =>
            {
                let q: Poly = p.iter().map(|(m, c)| (m.clone(), c.neg())).collect();
                (true, self.poly_expr(&q))
            }
            _ => (false, e),
        }
    }

    fn expr_of(&mut self, r: &Rat) -> Expr {
        if r.num.is_empty() {
            return Expr::zero();
        }
        // numerator: monomial content times a primitive polynomial
        let mut content: Mono = r.num.keys().next().unwrap().clone();
        for mo in r.num.keys() {
            content = content
                .into_iter()
                .filter_map(|(k, e)| mo.iter().find(|x| x.0 == k).map(|x| (k, e.min(x.1))))
                .collect();
        }
        let rest: Poly = r
            .num
            .iter()
            .map(|(m, c)| (mono_div(m, &content).unwrap(), c.clone()))
            .collect();
        let (mut c, mut prim) = normalize(&rest);
        let mut factors = self.mono_expr(&content);
        // pull out factors seen in the input, such as (R + a*cos(u))^2
        for cand in self.candidates.clone() {
            let mut k = 0;
            while prim.len() > 1 && cand != prim {
                match self.divide(&prim, &cand) {
                    Ok(Some(q)) if !q.is_empty() => {
                        prim = q;
                        k += 1;
                    }
                    _ => break,
                }
            }
            if k > 0 {
                let (flip, e) = self.signed(&cand);
                if flip && k % 2 == 1 {
                    c = c.neg();
                }
                factors.push(pow(e, Expr::int(k)));
            }
        }
        let (flip, e) = self.signed(&prim);
        if flip {
            c = c.neg();
            prim = prim.iter().map(|(m, v)| (m.clone(), v.neg())).collect();
        }
        let mut ties = vec![];
        for (p, k) in &r.den {
            let e = self.poly_expr(p);
            let tie = matches!(&e, Expr::Add(v) if 2 * v.iter().filter(|t| t.is_negative_term()).count() == v.len());
            if tie && k % 2 == 1 {
                // r^2-k or k-r^2 look alike: decide below, to fix the overall sign
                ties.push((p.clone(), e, *k));
                continue;
            }
            let (flip, e) = self.signed(p);
            if flip && k % 2 == 1 {
                c = c.neg();
            }
            factors.push(pow(e, Expr::int(-(*k as i128))));
        }
        for (p, e, k) in ties {
            if c.is_negative() {
                c = c.neg();
                let q: Poly = p.iter().map(|(m, v)| (m.clone(), v.neg())).collect();
                factors.push(pow(self.poly_expr(&q), Expr::int(-(k as i128))));
            } else {
                factors.push(pow(e, Expr::int(-(k as i128))));
            }
        }
        let mut main = factors.clone();
        main.push(Expr::Num(c.clone()));
        main.push(e.clone());
        let main = mul(main);
        // -(r-r_s) reads better as r_s-r
        if c.is_negative() && prim.len() > 1 {
            let q: Poly = prim.iter().map(|(m, v)| (m.clone(), v.neg())).collect();
            factors.push(Expr::Num(c.neg()));
            factors.push(self.poly_expr(&q));
            let alt = mul(factors);
            if size(&alt) < size(&main) {
                return alt;
            }
        }
        main
    }
}

fn merge(a: &[(Poly, u32)], b: &[(Poly, u32)]) -> Vec<(Poly, u32)> {
    let mut m: Vec<(Poly, u32)> = vec![];
    for (p, k) in a.iter().chain(b) {
        match m.iter_mut().find(|x| x.0 == *p) {
            Some(x) => x.1 += k,
            None => m.push((p.clone(), *k)),
        }
    }
    m.retain(|x| x.1 > 0);
    m
}

/// arguments of functions and powers are tidied without trigonometric rewrites
fn inner(e: &Expr) -> Expr {
    match e {
        Expr::Num(_) | Expr::Sym(_) | Expr::Const(_) => e.clone(),
        _ => one(e, Trig::None).unwrap_or_else(|| e.clone()),
    }
}

fn one(e: &Expr, trig: Trig) -> Option<Expr> {
    one_with(e, trig, vec![])
}

fn one_with(e: &Expr, trig: Trig, trig_args: Vec<(Expr, Trig)>) -> Option<Expr> {
    let mut cx = Ctx::new(trig);
    cx.trig_args = trig_args;
    let r = cx.rat_of(e).ok()?;
    let r = cx.reduce(r).ok()?;
    Some(cx.expr_of(&r))
}

/// Printed length, with a penalty for decimals and for each sum,
/// so `-sin(x)^2` wins over `cos(x)^2-1`
pub fn size(e: &Expr) -> usize {
    let s = show(e);
    let b = s.as_bytes();
    let mut signs = 0;
    for i in 1..b.len() {
        if (b[i] == b'+' || b[i] == b'-')
            && !matches!(b[i - 1], b'(' | b'e' | b'E' | b'^' | b'*' | b'/')
        {
            signs += 1;
        }
    }
    let decimals = s.matches('.').count();
    // one fraction bar reads better than nested ones: 5/(20-3*b), not 1/(4-3*b/5)
    let bars = s.matches('/').count().saturating_sub(1);
    s.len() + 4 * signs + 4 * decimals + 4 * bars
}

/// Tidy an expression: rational normal form, trying sin^2 -> cos^2 both ways,
/// keeping the smallest result (and the input if nothing is smaller).
pub fn tidy(e: &Expr) -> Expr {
    match e {
        Expr::Matrix(rows) => {
            return Expr::Matrix(rows.iter().map(|r| r.iter().map(tidy).collect()).collect());
        }
        Expr::Num(_)
        | Expr::Sym(_)
        | Expr::Const(_)
        | Expr::Bool(_)
        | Expr::Str(_)
        | Expr::Obj(_) => return e.clone(),
        _ => {}
    }
    let mut best = e.clone();
    let mut best_size = size(e);
    if std::env::var("GALILEO_CHECK").is_ok() {
        for t in [Trig::None, Trig::CosToSin, Trig::SinToCos] {
            if let Some(c) = one(e, t) {
                check_same(e, &c, t as u8);
            }
        }
    }
    // the directions of sin^2 + cos^2 = 1 to try: per argument when there are
    // few (so a^2*cos(theta)^2 + c^2*sin(theta)^2 can keep both), else uniform
    let mut args: Vec<Expr> = vec![];
    fn trig_args(e: &Expr, out: &mut Vec<Expr>) {
        if let Expr::Func(Func::Sin | Func::Cos, a) = e {
            let a = inner(a);
            if !out.contains(&a) {
                out.push(a);
            }
        }
        match e {
            Expr::Add(v) | Expr::Mul(v) | Expr::Apply(_, v) => {
                v.iter().for_each(|x| trig_args(x, out))
            }
            Expr::Pow(a, b) => {
                trig_args(a, out);
                trig_args(b, out)
            }
            Expr::Func(_, a) => trig_args(a, out),
            _ => {}
        }
    }
    trig_args(e, &mut args);
    const DIRS: [Trig; 3] = [Trig::None, Trig::CosToSin, Trig::SinToCos];
    let mut plans: Vec<(Trig, Vec<(Expr, Trig)>)> = DIRS.iter().map(|t| (*t, vec![])).collect();
    if args.len() == 2 {
        for a in DIRS {
            for b in DIRS {
                if a != b {
                    plans.push((Trig::None, vec![(args[0].clone(), a), (args[1].clone(), b)]));
                }
            }
        }
    }
    for (t, over) in plans {
        if let Some(c) = one_with(e, t, over) {
            let s = size(&c);
            if s < best_size || (c.is_zero() && !best.is_zero()) {
                best = c;
                best_size = s;
            }
        }
    }
    best
}

fn check_same(a: &Expr, b: &Expr, mode: u8) {
    let syms = a.symbols();
    let names: Vec<&str> = syms.iter().map(|s| s.as_str()).collect();
    let ca = crate::numeric::compile(a, &names);
    let cb = crate::numeric::compile(b, &names);
    for k in 0..3 {
        let x: Vec<f64> = (0..names.len())
            .map(|i| 0.3 + 0.17 * ((i * 7 + k * 3) % 11) as f64)
            .collect();
        let (va, vb) = (crate::numeric::run(&ca, &x), crate::numeric::run(&cb, &x));
        if va.is_finite() && (va - vb).abs() > 1e-6 * va.abs().max(1.0) {
            eprintln!(
                "TIDY MISMATCH mode {}: {} => {} ({} vs {})",
                mode,
                show(a),
                show(b),
                va,
                vb
            );
            return;
        }
    }
}

/// True when the expression is identically zero (as far as the normal form can tell)
pub fn is_zero(e: &Expr) -> bool {
    e.is_zero()
        || [Trig::None, Trig::SinToCos]
            .iter()
            .any(|t| one(e, *t).is_some_and(|x| x.is_zero()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::Session;
    use crate::parser::parse;

    fn t(src: &str) -> String {
        let mut s = Session::new();
        let v = match s.run(src).pop().unwrap().unwrap() {
            crate::eval::Output::Value(v) => v,
            _ => panic!(),
        };
        let _ = parse;
        show(&tidy(&v))
    }

    #[test]
    fn cancels_polynomial_factors() {
        assert_eq!(t("(r^2-r_s^2)/(r-r_s)"), "r+r_s");
        assert_eq!(t("r/(r-r_s)*(r-r_s)/r^3"), "1/r^2");
        assert_eq!(t("1/(2*r) - 1/(2*(r-r_s))"), "r_s/(2*r*(r_s-r))");
    }

    #[test]
    fn trig() {
        assert_eq!(t("sin(x)^2+cos(x)^2"), "1");
        assert_eq!(t("1-cos(x)^2"), "sin(x)^2");
        assert_eq!(t("sqrt(r)^2/r"), "1");
    }
}
