//! Complex analysis: Laurent series by series arithmetic, residues, poles,
//! numeric contour integrals, domain coloring and conformal-map pictures.
//!
//! A series is a list of exact coefficients c_k of h^(v + k), h = z - a, known
//! up to (not including) h^(v + len). Rational functions, exp, log, powers and
//! the trigonometric and hyperbolic functions are expanded by recurrences, so
//! poles of any order and removable singularities come out directly. An
//! essential singularity (exp(1/z)) is expanded in w = 1/h instead.

use crate::build::*;
use crate::calc::{deriv, simplify};
use crate::cnum::{C, ceval};
use crate::eval::{R, Session};
use crate::expr::{Const, Expr, Func, Obj};
use crate::num::Number;
use crate::parser::Node;
use crate::plot::{Figure, Raster, Series};
use crate::print::show;
use std::rc::Rc;

#[derive(Clone, Debug)]
struct Ser {
    v: i64,
    c: Vec<Expr>,
}

#[derive(Debug)]
enum SErr {
    Essential,
    Branch,
    Unsupported(String),
}

type SR = Result<Ser, SErr>;

/// is the exact coefficient zero?
fn is_zero(e: &Expr) -> bool {
    if e.is_zero() {
        return true;
    }
    if e.symbols().is_empty() {
        if let Some(z) = ceval(e, &[]) {
            return z.abs() < 1e-13;
        }
    }
    false
}

fn tidy(e: Expr) -> Expr {
    let s = simplify(&e);
    if is_zero(&s) { Expr::zero() } else { s }
}

impl Ser {
    fn prec(&self) -> i64 {
        self.v + self.c.len() as i64
    }
    fn get(&self, e: i64) -> Expr {
        if e < self.v || e >= self.prec() {
            Expr::zero()
        } else {
            self.c[(e - self.v) as usize].clone()
        }
    }
    fn konst(e: Expr, prec: i64) -> Ser {
        let mut s = Ser { v: 0, c: vec![] };
        if prec > 0 {
            s.c.push(tidy(e));
            s.c.resize(prec as usize, Expr::zero());
        } else {
            s.v = prec;
        }
        s.norm()
    }
    fn norm(mut self) -> Ser {
        let k = self.c.iter().take_while(|e| is_zero(e)).count();
        self.c.drain(..k);
        self.v += k as i64;
        self
    }
    fn add(&self, o: &Ser) -> Ser {
        let p = self.prec().min(o.prec());
        let lo = self.v.min(o.v).min(p);
        let c = (lo..p).map(|e| tidy(add2(self.get(e), o.get(e)))).collect();
        Ser { v: lo, c }.norm()
    }
    fn scale(&self, k: &Expr) -> Ser {
        Ser { v: self.v, c: self.c.iter().map(|e| tidy(mul2(e.clone(), k.clone()))).collect() }.norm()
    }
    fn mul(&self, o: &Ser) -> Ser {
        let n = self.c.len().min(o.c.len());
        let c = (0..n)
            .map(|k| tidy(add((0..=k).map(|j| mul2(self.c[j].clone(), o.c[k - j].clone())).collect())))
            .collect();
        Ser { v: self.v + o.v, c }.norm()
    }
    fn inv(&self) -> SR {
        let a = self.clone().norm();
        if a.c.is_empty() {
            return Err(SErr::Unsupported("division by a series that is zero to the working order".into()));
        }
        let b0 = tidy(pow(a.c[0].clone(), Expr::int(-1)));
        let mut b: Vec<Expr> = vec![b0.clone()];
        for k in 1..a.c.len() {
            let s = add((1..=k).map(|j| mul2(a.c[j].clone(), b[k - j].clone())).collect());
            b.push(tidy(neg(mul2(b0.clone(), s))));
        }
        Ok(Ser { v: -a.v, c: b })
    }
    fn powi(&self, n: i64) -> SR {
        let base = if n < 0 { self.inv()? } else { self.clone().norm() };
        let mut k = n.unsigned_abs();
        let mut r = Ser::konst(Expr::one(), base.c.len() as i64);
        let mut b = base;
        while k > 0 {
            if k & 1 == 1 {
                r = r.mul(&b);
            }
            k >>= 1;
            if k > 0 {
                b = b.mul(&b);
            }
        }
        Ok(r)
    }
    /// constant term and the rest (which must have positive valuation)
    fn split(&self) -> (Expr, Ser) {
        let c0 = self.get(0);
        let rest = self.add(&Ser::konst(neg(c0.clone()), self.prec()));
        (c0, rest)
    }
    /// sum g_k u^k for a series u with u.v >= 1
    fn compose(u: &Ser, g: &dyn Fn(usize) -> Expr, prec: i64) -> Ser {
        let mut acc = Ser::konst(g(0), prec);
        let mut p = Ser::konst(Expr::one(), prec);
        let mut k = 1;
        while u.c.is_empty() || (k as i64) * u.v < prec {
            if u.c.is_empty() {
                break;
            }
            p = p.mul(u);
            let gk = g(k);
            if !gk.is_zero() {
                acc = acc.add(&p.scale(&gk));
            }
            k += 1;
            if k > 200 {
                break;
            }
        }
        acc
    }
}

fn fact(k: usize) -> Expr {
    Expr::Num(Number::from_bigint((1..=k as u64).fold(num_bigint::BigInt::from(1), |a, b| a * b)))
}

fn recip(k: usize) -> Expr {
    pow(fact(k), Expr::int(-1))
}

fn with_sign(k: usize, e: Expr) -> Expr {
    if k % 2 == 1 { neg(e) } else { e }
}

/// Expand e around the leaf series `z` for variable x
fn series(e: &Expr, x: &str, z: &Ser, prec: i64) -> SR {
    if !e.symbols().iter().any(|s| s == x) {
        return Ok(Ser::konst(e.clone(), prec));
    }
    match e {
        Expr::Sym(s) if s == x => Ok(z.clone()),
        Expr::Add(v) => {
            let mut acc = series(&v[0], x, z, prec)?;
            for t in &v[1..] {
                acc = acc.add(&series(t, x, z, prec)?);
            }
            Ok(acc)
        }
        Expr::Mul(v) => {
            let mut acc = series(&v[0], x, z, prec)?;
            for t in &v[1..] {
                acc = acc.mul(&series(t, x, z, prec)?);
            }
            Ok(acc)
        }
        Expr::Pow(b, ex) => {
            if let Expr::Const(Const::E) = **b {
                return exp_s(&series(ex, x, z, prec)?);
            }
            if let Expr::Num(n) = &**ex {
                let s = series(b, x, z, prec)?;
                if let Some(k) = n.as_int() {
                    return s.powi(k as i64);
                }
                return pow_rat(&s, n);
            }
            // b^e = exp(e log b)
            let l = log_s(&series(b, x, z, prec)?)?;
            exp_s(&series(ex, x, z, prec)?.mul(&l))
        }
        Expr::Func(f, a) => {
            let s = series(a, x, z, prec)?;
            match f {
                Func::Sin | Func::Cos | Func::Sinh | Func::Cosh => trig_s(*f, &s),
                Func::Tan => trig_s(Func::Sin, &s)?.mul(&trig_s(Func::Cos, &s)?.inv()?).pipe(Ok),
                Func::Tanh => trig_s(Func::Sinh, &s)?.mul(&trig_s(Func::Cosh, &s)?.inv()?).pipe(Ok),
                Func::Log => log_s(&s),
                Func::Atan => {
                    // atan(c0 + u) = atan(c0) + integral of u'/(1 + s^2)
                    let (c0, _) = s.split();
                    if s.v < 0 {
                        return Err(SErr::Essential);
                    }
                    let d = s.deriv();
                    let q = d.mul(&Ser::konst(Expr::one(), s.prec()).add(&s.mul(&s)).inv()?);
                    Ok(q.integ().add(&Ser::konst(func(Func::Atan, c0), s.prec())))
                }
                _ => Err(SErr::Unsupported(format!("{} is not expanded", f.name()))),
            }
        }
        _ => Err(SErr::Unsupported(format!("can't expand {}", show(e)))),
    }
}

trait Pipe: Sized {
    fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T {
        f(self)
    }
}
impl Pipe for Ser {}

impl Ser {
    fn deriv(&self) -> Ser {
        let c = self
            .c
            .iter()
            .enumerate()
            .map(|(k, e)| tidy(mul2(Expr::int((self.v + k as i64) as i128), e.clone())))
            .collect();
        Ser { v: self.v - 1, c }.norm()
    }
    /// antiderivative with zero constant; needs no h^-1 term
    fn integ(&self) -> Ser {
        let c = self
            .c
            .iter()
            .enumerate()
            .map(|(k, e)| tidy(div(e.clone(), Expr::int((self.v + k as i64 + 1) as i128))))
            .collect();
        Ser { v: self.v + 1, c }.norm()
    }
}

fn exp_s(s: &Ser) -> SR {
    if s.v < 0 && !s.c.is_empty() {
        return Err(SErr::Essential);
    }
    let (c0, u) = s.split();
    let p = s.prec();
    Ok(Ser::compose(&u, &recip, p).scale(&exp(c0)))
}

fn trig_s(f: Func, s: &Ser) -> SR {
    if s.v < 0 && !s.c.is_empty() {
        return Err(SErr::Essential);
    }
    let (c0, u) = s.split();
    let p = s.prec();
    let hyper = matches!(f, Func::Sinh | Func::Cosh);
    // sin u, cos u (or sinh, cosh) as power series
    let su = Ser::compose(&u, &|k| if k % 2 == 1 { if hyper { recip(k) } else { with_sign(k / 2, recip(k)) } } else { Expr::zero() }, p);
    let cu = Ser::compose(&u, &|k| if k % 2 == 0 { if hyper { recip(k) } else { with_sign(k / 2, recip(k)) } } else { Expr::zero() }, p);
    let (sf, cf) = if hyper { (Func::Sinh, Func::Cosh) } else { (Func::Sin, Func::Cos) };
    let (s0, k0) = (func(sf, c0.clone()), func(cf, c0));
    Ok(match f {
        // sin(c + u) = sin c cos u + cos c sin u; cos(c + u) = cos c cos u - sin c sin u
        Func::Sin | Func::Sinh => cu.scale(&s0).add(&su.scale(&k0)),
        _ => {
            let sign = if hyper { s0.clone() } else { neg(s0.clone()) };
            cu.scale(&k0).add(&su.scale(&sign))
        }
    })
}

fn log_s(s: &Ser) -> SR {
    let a = s.clone().norm();
    if a.c.is_empty() || a.v != 0 {
        return Err(SErr::Branch);
    }
    let c0 = a.c[0].clone();
    let w = a.scale(&pow(c0.clone(), Expr::int(-1))).add(&Ser::konst(Expr::int(-1), a.prec()));
    let l = Ser::compose(&w, &|k| if k == 0 { Expr::zero() } else { with_sign(k + 1, rat(1, k as i128)) }, a.prec());
    Ok(l.add(&Ser::konst(func(Func::Log, c0), a.prec())))
}

fn pow_rat(s: &Ser, r: &Number) -> SR {
    let a = s.clone().norm();
    if a.c.is_empty() {
        return Err(SErr::Unsupported("power of a series that is zero to the working order".into()));
    }
    let shift = Number::int(a.v as i128).mul(r);
    let Some(sh) = shift.as_int() else {
        return Err(SErr::Branch);
    };
    let c0 = a.c[0].clone();
    let w = Ser { v: 0, c: a.c.clone() }.scale(&pow(c0.clone(), Expr::int(-1))).add(&Ser::konst(Expr::int(-1), a.c.len() as i64));
    let re = Expr::Num(r.clone());
    // binomial series (1 + w)^r
    let g = |k: usize| {
        let mut num = Expr::one();
        for j in 0..k {
            num = mul2(num, sub(re.clone(), Expr::int(j as i128)));
        }
        div(num, fact(k))
    };
    let b = Ser::compose(&w, &g, a.c.len() as i64).scale(&pow(c0, re.clone()));
    Ok(Ser { v: b.v + sh as i64, c: b.c })
}

/// The Laurent series of f around x = a (a = None: around infinity, in w = 1/x),
/// known at least up to h^n
fn expand(f: &Expr, x: &str, a: &Expr, n: i64, inverse: bool) -> SR {
    let mut last = Err(SErr::Unsupported("no expansion".into()));
    for pad in [6i64, 12, 24, 48] {
        let p = n + 1 + pad;
        let leaf = if inverse {
            // x = a + 1/w
            Ser { v: -1, c: { let mut c = vec![Expr::one(), a.clone()]; c.resize((p + 1).max(2) as usize, Expr::zero()); c } }
        } else {
            let mut c = vec![a.clone(), Expr::one()];
            c.resize(p.max(2) as usize, Expr::zero());
            Ser { v: 0, c }.norm()
        };
        match series(f, x, &leaf, p) {
            Ok(s) if s.prec() > n => return Ok(s),
            Ok(s) => last = Ok(s),
            Err(e) => return Err(e),
        }
    }
    last
}

pub enum Kind {
    Regular,
    Removable,
    Pole(i64),
    Essential,
}

pub struct Laurent {
    /// (exponent, coefficient), increasing exponents, nonzero coefficients
    pub terms: Vec<(i64, Expr)>,
    pub kind: Kind,
    pub residue: Expr,
}

fn has_neg(s: &Ser) -> bool {
    s.v < 0 && !s.c.is_empty()
}

/// Laurent series of f around x = a: terms with exponents from -m (essential: at most m negative) up to n
pub fn laurent(f: &Expr, x: &str, a: &Expr, n: i64, m: i64) -> Result<Laurent, String> {
    let err = |e: SErr| match e {
        SErr::Branch => format!("{} = {} is a branch point (a log or root is zero there): no Laurent series", x, show(a)),
        SErr::Essential => "essential singularity".to_string(),
        SErr::Unsupported(s) => s,
    };
    match expand(f, x, a, n, false) {
        Ok(s) => {
            let terms: Vec<(i64, Expr)> = (s.v..=n).map(|e| (e, s.get(e))).filter(|(_, c)| !c.is_zero()).collect();
            let kind = if has_neg(&s) {
                Kind::Pole(-s.v)
            } else if crate::calc::subst(f, x, a) .pipe_ok() {
                Kind::Regular
            } else {
                Kind::Removable
            };
            Ok(Laurent { residue: s.get(-1), terms, kind })
        }
        Err(SErr::Essential) => {
            // in w = 1/h: exponent k of w is exponent -k of h
            let s = expand(f, x, a, m.max(1), true).map_err(err)?;
            if s.v < -n.max(0) - 60 {
                return Err("essential singularity with an infinite regular part: no finite expansion shown".into());
            }
            let mut terms: Vec<(i64, Expr)> = (s.v..=m).map(|k| (-k, s.get(k))).filter(|(e, c)| !c.is_zero() && *e <= n).collect();
            terms.sort_by_key(|t| t.0);
            Ok(Laurent { residue: s.get(1), terms, kind: Kind::Essential })
        }
        Err(e) => Err(err(e)),
    }
}

trait OkVal {
    fn pipe_ok(&self) -> bool;
}
impl OkVal for Expr {
    /// a finite value (no division by zero)
    fn pipe_ok(&self) -> bool {
        let s = simplify(self);
        match ceval(&s, &[]) {
            Some(z) => z.is_finite() && !crate::print::show(&s).contains("inf"),
            None => s.symbols().iter().all(|n| n != "inf"),
        }
    }
}

fn sum_terms(l: &Laurent, x: &str, a: &Expr) -> Expr {
    let h = if a.is_zero() { Expr::sym(x) } else { sub(Expr::sym(x), a.clone()) };
    // increasing powers, as a series is read
    let ts: Vec<Expr> = l.terms.iter().map(|(e, c)| mul2(c.clone(), pow(h.clone(), Expr::int(*e as i128)))).collect();
    match ts.len() {
        0 => Expr::zero(),
        1 => ts[0].clone(),
        _ => Expr::Add(ts),
    }
}

/// a complex number from two floats, dropping round-off
fn cnum_expr(z: C) -> Expr {
    // drop round-off: tiny parts, and digits beyond 12 significant ones
    let clean = |v: f64| {
        if v.abs() < 1e-10 * (1.0 + z.abs()) {
            return 0.0;
        }
        let k = 10f64.powi(11 - v.abs().log10().floor() as i32);
        (v * k).round() / k
    };
    let (re, im) = (clean(z.re), clean(z.im));
    if im == 0.0 {
        Expr::float(re)
    } else if re == 0.0 {
        mul2(Expr::float(im), Expr::Const(Const::I))
    } else {
        add2(Expr::float(re), mul2(Expr::float(im), Expr::Const(Const::I)))
    }
}

/// hue from the argument, brightness from the modulus, faint rings at powers of 2
fn color(w: C) -> [u8; 3] {
    if !w.is_finite() {
        return [255, 255, 255];
    }
    let m = w.abs();
    let h = (w.arg() / (2.0 * std::f64::consts::PI)).rem_euclid(1.0);
    let ring = if m > 0.0 { m.log2().rem_euclid(1.0) } else { 0.0 };
    let l = (0.08 + 0.84 * (2.0 / std::f64::consts::PI) * m.atan()) * (0.88 + 0.12 * ring);
    let s = 0.9;
    // HSL to RGB
    let q = if l < 0.5 { l * (1.0 + s) } else { l + s - l * s };
    let p = 2.0 * l - q;
    let hue = |t: f64| {
        let t = t.rem_euclid(1.0);
        let v = if t < 1.0 / 6.0 {
            p + (q - p) * 6.0 * t
        } else if t < 0.5 {
            q
        } else if t < 2.0 / 3.0 {
            p + (q - p) * (2.0 / 3.0 - t) * 6.0
        } else {
            p
        };
        (v.clamp(0.0, 1.0) * 255.0).round() as u8
    };
    [hue(h + 1.0 / 3.0), hue(h), hue(h - 1.0 / 3.0)]
}

/// f as a numeric function of z; non-analytic f (conj, abs, real, imag, arg)
/// is built from z = zre_ + i zim_ with real parts.
pub(crate) struct CFn {
    e: Expr,
    x: String,
    split: bool,
}

impl CFn {
    pub(crate) fn at(&self, z: C) -> Option<C> {
        if self.split {
            ceval(&self.e, &[("zre_", C::new(z.re, 0.0)), ("zim_", C::new(z.im, 0.0))])
        } else {
            ceval(&self.e, &[(self.x.as_str(), z)])
        }
    }
}

fn uses_parts(n: &Node) -> bool {
    match n {
        Node::Call(f, a) => matches!(f.as_str(), "conj" | "abs" | "real" | "imag" | "arg") || a.iter().any(uses_parts),
        Node::Neg(a) | Node::Transpose(a) => uses_parts(a),
        Node::Bin(_, a, b) => uses_parts(a) || uses_parts(b),
        _ => false,
    }
}

impl Session {
    pub(crate) fn cfn(&mut self, f: &Node, x: &str) -> R<CFn> {
        if !uses_parts(f) {
            let e = self.eval_hidden(f, &[x.to_string()])?;
            return Ok(CFn { e, x: x.into(), split: false });
        }
        let z = add2(Expr::Sym("zre_".into()), mul2(Expr::Const(Const::I), Expr::Sym("zim_".into())));
        let old = self.vars.insert(x.to_string(), z);
        let r = self.eval(f);
        match old {
            Some(v) => self.vars.insert(x.to_string(), v),
            None => self.vars.remove(x),
        };
        Ok(CFn { e: r?, x: x.into(), split: true })
    }
}

impl Session {
    pub(crate) fn cx_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "laurent" => self.laurent_cmd(args),
            "residue" => self.residue_cmd(args),
            "poles" => self.poles_cmd(args),
            "cint" => self.cint_cmd(args),
            "cplot" => self.cplot_cmd(args),
            "cmap" => self.cmap_cmd(args),
            _ => return None,
        };
        Some(r)
    }

    fn var_point(&mut self, f: &Node, x: &Node, a: &Node, cmd: &str) -> R<(Expr, String, Expr)> {
        let Node::Ident(x) = x else {
            return Err(format!("{}: the second argument must be the variable, like z", cmd));
        };
        let fx = self.eval_hidden(f, std::slice::from_ref(x))?;
        let a = simplify(&self.eval(a)?);
        if a.symbols().iter().any(|s| s == x) {
            return Err(format!("{}: the point must not contain {}", cmd, x));
        }
        Ok((fx, x.clone(), a))
    }

    fn laurent_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (f, x, a, n) = match args {
            [f, x, a, n] => (f, x, a, n),
            _ => return Err("usage: laurent(f, z, a, n): the Laurent series of f around z = a, up to (z - a)^n".into()),
        };
        let (fx, x, a) = self.var_point(f, x, a, "laurent")?;
        let n = self.eval(n)?.num().and_then(|k| k.as_int()).filter(|k| (-20..=30).contains(k)).ok_or("laurent: n must be a whole number from -20 to 30")? as i64;
        let l = laurent(&fx, &x, &a, n, 6)?;
        let h = if a.is_zero() { x.clone() } else { format!("({} − {})", x, show(&a)) };
        let what = match l.kind {
            Kind::Regular => format!("f is analytic at {} = {}, so this is its Taylor series", x, show(&a)),
            Kind::Removable => format!("{} = {} is a removable singularity: the series has no negative powers", x, show(&a)),
            Kind::Pole(m) => format!(
                "{} = {} is a pole of order {}: the principal part has powers down to {}^−{}",
                x, show(&a), m, h, m
            ),
            Kind::Essential => format!(
                "{} = {} is an essential singularity: infinitely many negative powers, shown down to {}^−{}",
                x, show(&a), h, -l.terms.first().map_or(0, |t| t.0).min(0)
            ),
        };
        self.note(format!("Laurent series around {} = {}: {}. The residue (the coefficient of {}^−1) is {}.", x, show(&a), what, h, show(&l.residue)));
        Ok(sum_terms(&l, &x, &a))
    }

    fn residue_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x, a] = args else {
            return Err("usage: residue(f, z, a): the coefficient of 1/(z - a) in the Laurent series of f around z = a".into());
        };
        let (fx, x, a) = self.var_point(f, x, a, "residue")?;
        let l = laurent(&fx, &x, &a, 0, 1)?;
        let how = match l.kind {
            Kind::Regular | Kind::Removable => "f has no pole there, so the residue is 0".to_string(),
            Kind::Pole(1) => format!("a simple pole: the residue is the limit of ({} − a) f as {} → a", x, x),
            Kind::Pole(m) => format!("a pole of order {}: the residue is the coefficient of 1/({} − a) in the Laurent series, equivalently the derivative of order {} of ({} − a)^{} f at a, divided by {}!", m, x, m - 1, x, m, m - 1),
            Kind::Essential => "an essential singularity: the residue is the coefficient of 1/(z − a) in the Laurent series".to_string(),
        };
        self.note(format!("Residue of f at {} = {}: {}.", x, show(&a), how));
        Ok(l.residue)
    }

    fn poles_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x] = args else {
            return Err("usage: poles(f, z): the poles of a rational function, one row [pole, order, residue] each".into());
        };
        let Node::Ident(x) = x else {
            return Err("poles: the second argument must be the variable".into());
        };
        let fx = self.eval_hidden(f, std::slice::from_ref(x))?;
        let (_, den) = crate::calc::num_den(&crate::calc::together(&fx));
        if !den.symbols().iter().any(|s| s == x) {
            self.note("The function has no denominator in the variable, so no poles.");
            return Ok(Expr::Matrix(vec![]));
        }
        let roots = crate::cas::solve(&den, x).map_err(|_| "poles: could not find where the denominator is zero; use residue(f, z, a) at a known point".to_string())?;
        let mut rows = vec![];
        let mut seen: Vec<Expr> = vec![];
        for r in roots {
            let r = simplify(&r);
            if seen.contains(&r) {
                continue;
            }
            seen.push(r.clone());
            if let Ok(l) = laurent(&fx, x, &r, 0, 1) {
                if let Kind::Pole(m) = l.kind {
                    rows.push(vec![r, Expr::int(m as i128), l.residue]);
                }
            }
        }
        self.note("Poles: one row [pole, order, residue] for each zero of the denominator that the numerator does not cancel.");
        Ok(Expr::Matrix(rows))
    }

    /// numeric integral of f along the path z = g(t), t from t0 to t1
    fn cint_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x, g, t, t0, t1] = args else {
            return Err("usage: cint(f, z, path, t, t0, t1): the integral of f dz along z = path(t), t from t0 to t1, e.g. cint(1/z, z, exp(i*t), t, 0, 2*pi)".into());
        };
        let (Node::Ident(x), Node::Ident(t)) = (x, t) else {
            return Err("cint: give the variable z and the path parameter t as names".into());
        };
        let fx = self.cfn(f, x)?;
        let gt = self.eval_hidden(g, std::slice::from_ref(t))?;
        let dg = simplify(&deriv(&gt, t));
        let (ea, eb) = (self.eval(t0)?, self.eval(t1)?);
        let (a, b) = (self.number(t0, "cint: t0")?, self.number(t1, "cint: t1")?);
        let n = 4096;
        let h = (b - a) / n as f64;
        let mut acc = C::new(0.0, 0.0);
        for k in 0..=n {
            let tv = C::new(a + h * k as f64, 0.0);
            let z = ceval(&gt, &[(t.as_str(), tv)]).ok_or("cint: can't evaluate the path numerically")?;
            let dz = ceval(&dg, &[(t.as_str(), tv)]).ok_or("cint: can't evaluate the path's derivative")?;
            let w = fx.at(z).ok_or("cint: can't evaluate f numerically")?;
            let v = w.mul(dz);
            if !v.is_finite() {
                return Err(format!("cint: f is infinite on the path (at t = {:.6}); move the path off the singularity", tv.re));
            }
            let wgt = if k == 0 || k == n { 1.0 } else if k % 2 == 1 { 4.0 } else { 2.0 };
            acc = acc.add(v.scale(wgt));
        }
        let r = acc.scale(h / 3.0);
        let k = r.im / (2.0 * std::f64::consts::PI);
        let hint = if r.re.abs() < 1e-8 && (k - k.round()).abs() < 1e-8 && k.round() != 0.0 {
            format!(" This is {} × 2πi.", k.round())
        } else {
            String::new()
        };
        self.note(format!("Contour integral of f dz along z = {}, {} from {} to {}: the integral of f(z(t)) z'(t) dt, computed numerically with Simpson's rule on {} steps.{}", show(&gt), t, show(&ea), show(&eb), n, hint));
        Ok(cnum_expr(r))
    }

    fn region(&mut self, args: &[Node], cmd: &str) -> R<(CFn, f64, f64, f64, f64)> {
        let [f, x, x0, x1, y0, y1] = args else {
            return Err(format!("usage: {}(f, z, x0, x1, y0, y1) over the rectangle x0 ≤ Re z ≤ x1, y0 ≤ Im z ≤ y1", cmd));
        };
        let Node::Ident(x) = x else {
            return Err(format!("{}: the second argument must be the variable", cmd));
        };
        let fx = self.cfn(f, x)?;
        let r = (self.number(x0, "x0")?, self.number(x1, "x1")?, self.number(y0, "y0")?, self.number(y1, "y1")?);
        if r.0 >= r.1 || r.2 >= r.3 {
            return Err(format!("{}: need x0 < x1 and y0 < y1", cmd));
        }
        Ok((fx, r.0, r.1, r.2, r.3))
    }

    /// domain coloring: hue = arg f, brightness = |f|
    fn cplot_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (fx, x0, x1, y0, y1) = self.region(args, "cplot")?;
        let w = 280usize;
        let h = ((w as f64) * (y1 - y0) / (x1 - x0)).round().clamp(20.0, 400.0) as usize;
        let mut px = Vec::with_capacity(w * h * 3);
        for j in 0..h {
            let y = y1 - (j as f64 + 0.5) / h as f64 * (y1 - y0);
            for k in 0..w {
                let xv = x0 + (k as f64 + 0.5) / w as f64 * (x1 - x0);
                let v = fx.at(C::new(xv, y)).ok_or("cplot: can't evaluate f numerically")?;
                px.extend_from_slice(&color(v));
            }
        }
        self.note("Domain coloring: each point z is colored by f(z). The hue shows the argument (red for positive real values, then yellow, green, cyan, blue, magenta counterclockwise), the brightness the modulus (black near 0, white near infinity), with faint rings where |f| doubles. A zero is a point where all colors meet in a dark spot, a pole a white one; the colors circle once per order.");
        let mut fig = Figure::new(vec![Series { label: "hue: arg f(z), brightness: |f(z)|".into(), points: vec![], ..Default::default() }], "Re z", "Im z", true);
        fig.image = Some(Raster { w, h, rgb: px, x0, x1, y0, y1 });
        Ok(Expr::Obj(Rc::new(Obj::Figure(fig))))
    }

    /// the images of the grid lines Re z = const and Im z = const under f
    fn cmap_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (fx, x0, x1, y0, y1) = self.region(args, "cmap")?;
        let lines = 11;
        let steps = 240;
        let mut horiz = vec![];
        let mut vert = vec![];
        let at = |re: f64, im: f64| -> (f64, f64) {
            match fx.at(C::new(re, im)) {
                Some(v) if v.is_finite() && v.abs() < 1e6 => (v.re, v.im),
                _ => (f64::NAN, f64::NAN),
            }
        };
        for l in 0..lines {
            let y = y0 + (y1 - y0) * l as f64 / (lines - 1) as f64;
            for s in 0..=steps {
                horiz.push(at(x0 + (x1 - x0) * s as f64 / steps as f64, y));
            }
            horiz.push((f64::NAN, f64::NAN));
            let xv = x0 + (x1 - x0) * l as f64 / (lines - 1) as f64;
            for s in 0..=steps {
                vert.push(at(xv, y0 + (y1 - y0) * s as f64 / steps as f64));
            }
            vert.push((f64::NAN, f64::NAN));
        }
        self.note(format!("The images under f of the 11 horizontal lines Im z = const (blue) and the 11 vertical lines Re z = const (red) of the rectangle [{}, {}] × [{}, {}]. Where f is analytic with f' ≠ 0 the images still cross at right angles: the map is conformal.", x0, x1, y0, y1));
        let s1 = Series { label: "images of Im z = const".into(), points: horiz, thin: true, ..Default::default() };
        let s2 = Series { label: "images of Re z = const".into(), points: vert, thin: true, ..Default::default() };
        Ok(Expr::Obj(Rc::new(Obj::Figure(Figure::new(vec![s1, s2], "Re w", "Im w", true)))))
    }
}

/// The integral over the real line (or over [0, inf) for an even integrand) of
/// R(x) cos(kx) or R(x) sin(kx), R rational with numeric coefficients and
/// deg den > deg num: 2 pi i times the residues of R e^(ikz) in the upper half
/// plane (Jordan's lemma), plus pi i times those on the real axis where the
/// integrand stays finite.
pub fn fourier_integral(fx: &Expr, x: &str, half: bool) -> Option<Expr> {
    use crate::calc::{num_den, together};
    if fx.symbols().iter().any(|s| s != x) {
        return None;
    }
    let fs = match fx {
        Expr::Mul(v) => v.clone(),
        o => vec![o.clone()],
    };
    let mut trig = None;
    let mut rest = vec![];
    for f in fs {
        match &f {
            Expr::Func(g @ (Func::Cos | Func::Sin), a) if a.contains_sym(x) && trig.is_none() => {
                let co = crate::cas::poly_coeffs(a, x)?;
                if co.len() != 2 || !co[0].is_zero() {
                    return None;
                }
                trig = Some((*g, co[1].clone()));
            }
            _ => rest.push(f),
        }
    }
    let (g, k) = match trig {
        Some(t) => t,
        None => return rational_line_integral(fx, x, half),
    };
    let kv = crate::numeric::value(&k);
    if !kv.is_finite() || kv == 0.0 {
        return None;
    }
    let (k, sign) = if kv < 0.0 { (neg(k), if g == Func::Sin { -1 } else { 1 }) } else { (k, 1) };
    let r = together(&mul(rest));
    let (n, d) = num_den(&r);
    let (pn, pd) = (crate::cas::poly_coeffs(&crate::calc::expand(&n), x)?, crate::cas::poly_coeffs(&crate::calc::expand(&d), x)?);
    if pd.len() < 2 || pd.len() <= pn.len() {
        return None;
    }
    if half {
        // only an even integrand halves
        let m = crate::calc::subst(fx, x, &neg(Expr::sym(x)));
        if !crate::cas::is_zero(&simplify(&sub(m, fx.clone()))) {
            return None;
        }
    }
    let roots = crate::cas::solve(&d, x).ok()?;
    let ez = mul2(r.clone(), exp(mul(vec![Expr::Const(Const::I), k.clone(), Expr::sym(x)])));
    let mut acc = vec![];
    for p in roots {
        let pv = ceval(&p, &[])?;
        if pv.im > 1e-12 {
            let l = laurent(&ez, x, &p, 0, 6).ok()?;
            acc.push(mul(vec![Expr::int(2), Expr::Const(Const::Pi), Expr::Const(Const::I), l.residue]));
        } else if pv.im.abs() <= 1e-12 {
            // allowed only where the integrand is finite (sin(x)/x at 0)
            let at = |h: f64| ceval(fx, &[(x, C::new(pv.re + h, 0.0))]).map(|c| c.re);
            match (at(1e-7), at(-1e-7)) {
                (Some(u), Some(v)) if u.is_finite() && v.is_finite() && (u - v).abs() < 1e-3 * u.abs().max(1.0) => {}
                _ => return None,
            }
            let l = laurent(&ez, x, &p, 0, 6).ok()?;
            acc.push(mul(vec![Expr::Const(Const::Pi), Expr::Const(Const::I), l.residue]));
        }
    }
    let total = simplify(&add(acc));
    let (re, im) = crate::extra_cmd::parts(&total).ok()?;
    let mut v = if g == Func::Cos { re } else { im };
    if sign < 0 {
        v = neg(v);
    }
    if half {
        v = div(v, Expr::int(2));
    }
    Some(simplify(&v))
}

/// The integral over the real line of a rational function with numeric
/// coefficients, no real poles and deg den >= deg num + 2: 2 pi i times the
/// residues in the upper half plane. half: [0, inf) of an even function.
fn rational_line_integral(fx: &Expr, x: &str, half: bool) -> Option<Expr> {
    use crate::calc::{num_den, together};
    let r = together(fx);
    let (n, d) = num_den(&r);
    let (pn, pd) = (crate::cas::poly_coeffs(&crate::calc::expand(&n), x)?, crate::cas::poly_coeffs(&crate::calc::expand(&d), x)?);
    if pd.len() < pn.len() + 2 {
        return None;
    }
    if half {
        let m = crate::calc::subst(fx, x, &neg(Expr::sym(x)));
        if !crate::cas::is_zero(&simplify(&sub(m, fx.clone()))) {
            return None;
        }
    }
    let mut acc = vec![];
    for p in crate::cas::solve(&d, x).ok()? {
        let pv = ceval(&p, &[])?;
        if pv.im.abs() <= 1e-12 {
            return None;
        }
        if pv.im > 0.0 {
            let l = laurent(&r, x, &p, 0, 6).ok()?;
            acc.push(mul(vec![Expr::int(2), Expr::Const(Const::Pi), Expr::Const(Const::I), l.residue]));
        }
    }
    let (re, _) = crate::extra_cmd::parts(&simplify(&add(acc))).ok()?;
    let v = if half { div(re, Expr::int(2)) } else { re };
    let v = tidy_radicals(&simplify(&v));
    // sanity: a real, finite number
    ceval(&v, &[]).filter(|c| c.re.is_finite() && c.im.abs() < 1e-9 * c.re.abs().max(1.0))?;
    Some(v)
}

/// The integral over a full period [a, a + 2 pi] of a rational function of
/// cos t and sin t: with z = e^(it), cos t = (z + 1/z)/2, sin t = (z - 1/z)/(2i),
/// dt = dz/(iz), it is 2 pi i times the residues inside the unit circle.
pub fn period_integral(fx: &Expr, t: &str) -> Option<Expr> {
    if fx.symbols().iter().any(|s| s != t) {
        return None;
    }
    let z = "z__";
    let zz = Expr::sym(z);
    fn swap(e: &Expr, t: &str, zz: &Expr) -> Expr {
        match e {
            Expr::Func(Func::Cos, a) if matches!(&**a, Expr::Sym(s) if s == t) => {
                div(add2(zz.clone(), div(Expr::one(), zz.clone())), Expr::int(2))
            }
            Expr::Func(Func::Sin, a) if matches!(&**a, Expr::Sym(s) if s == t) => div(
                sub(zz.clone(), div(Expr::one(), zz.clone())),
                mul2(Expr::int(2), Expr::Const(Const::I)),
            ),
            _ => e.map_children(&mut |c| swap(c, t, zz)),
        }
    }
    let g = swap(fx, t, &zz);
    if g.contains_sym(t) {
        return None;
    }
    let g = crate::calc::together(&crate::calc::rationalize(&div(g, mul2(Expr::Const(Const::I), zz.clone()))));
    let (_, d) = crate::calc::num_den(&g);
    crate::cas::poly_coeffs(&crate::calc::expand(&d), z)?;
    let mut acc = vec![];
    for p in crate::cas::solve(&d, z).ok()? {
        let pv = ceval(&p, &[])?;
        let r = pv.abs();
        if (r - 1.0).abs() < 1e-12 {
            return None;
        }
        if r < 1.0 {
            let l = laurent(&g, z, &p, 0, 6).ok()?;
            acc.push(l.residue);
        }
    }
    let total = simplify(&mul(vec![Expr::int(2), Expr::Const(Const::Pi), Expr::Const(Const::I), add(acc)]));
    let (re, _) = crate::extra_cmd::parts(&total).ok()?;
    let v = tidy_radicals(&simplify(&re));
    ceval(&v, &[]).filter(|c| c.re.is_finite())?;
    Some(v)
}

/// shortest of a few rewritings that clear square roots from denominators
fn tidy_radicals(v: &Expr) -> Expr {
    let mut best = v.clone();
    let t = simplify(&crate::calc::rationalize(v));
    let (n, d) = crate::calc::num_den(&t);
    let e = div(crate::calc::expand(&n), crate::calc::expand(&d));
    let r = crate::cas_cmd::rationalize_root(&e);
    for c in [t.clone(), crate::cas_cmd::rationalize_root(&t), simplify(&crate::cas_cmd::rationalize_root(&t)), r.clone(), simplify(&r), crate::calc::factor(&simplify(&r))] {
        let c = simplify(&crate::calc::expand(&c));
        let ok = matches!((ceval(&c, &[]), ceval(v, &[])), (Some(a), Some(b)) if (a.re - b.re).abs() <= 1e-9 * b.re.abs().max(1.0));
        if ok && c.size() < best.size() {
            best = c;
        }
    }
    best
}
