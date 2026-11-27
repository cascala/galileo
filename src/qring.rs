//! Quotient rings K[x]/(f) as values, K = Q or Z/p: `numfield(x^2 - 2)` (f
//! irreducible over Q, a number field), `quotient(x^2 + 1, x)` and
//! `quotient(x^2 + 1, x, 3)`.
//!
//! Elements print as Galileo that rebuilds them: `numfield(x^2-2, x+1)`,
//! `quotient(x^2+1, x, 3, x+1)` (third argument the characteristic, 0 for Q).

use crate::expr::{Expr, Obj};
use crate::modsolve as ms;
use crate::parser::Op;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, Zero};
use std::rc::Rc;

pub type R<T> = Result<T, String>;
type Q = BigRational;
/// coefficients low to high, no leading zeros
type P = Vec<Q>;

/// largest degree of f
const MAX_DEG: usize = 60;

#[derive(Debug, PartialEq)]
pub struct QRing {
    /// monic modulus, low to high
    pub f: P,
    pub var: String,
    /// characteristic: None for Q
    pub p: Option<BigInt>,
    pub numfield: bool,
    pub field: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QElem {
    pub ring: Rc<QRing>,
    pub v: P,
}

fn q(n: BigInt) -> Q {
    Q::from_integer(n)
}

impl QRing {
    fn norm(&self, c: Q) -> Q {
        match &self.p {
            None => c,
            Some(p) => {
                let d = crate::numtheory::modinv(&c.denom().mod_floor(p), p).unwrap_or_default();
                q((c.numer() * d).mod_floor(p))
            }
        }
    }
    fn trim(&self, v: P) -> P {
        let mut v: P = v.into_iter().map(|c| self.norm(c)).collect();
        while v.last().is_some_and(|c| c.is_zero()) {
            v.pop();
        }
        v
    }
    fn inv_c(&self, c: &Q) -> Q {
        self.norm(c.recip())
    }
    fn rem(&self, a: &P, b: &P) -> P {
        let mut a = self.trim(a.clone());
        let lb = self.inv_c(b.last().unwrap());
        while a.len() >= b.len() {
            let k = a.len() - b.len();
            let c = self.norm(a.last().unwrap() * &lb);
            for (i, bc) in b.iter().enumerate() {
                a[i + k] = self.norm(&a[i + k] - &c * bc);
            }
            a = self.trim(a);
        }
        a
    }
    fn mul(&self, a: &P, b: &P) -> P {
        if a.is_empty() || b.is_empty() {
            return vec![];
        }
        let mut r = vec![Q::zero(); a.len() + b.len() - 1];
        for (i, x) in a.iter().enumerate() {
            for (j, y) in b.iter().enumerate() {
                r[i + j] += x * y;
            }
        }
        self.rem(&r, &self.f)
    }
    pub fn degree(&self) -> usize {
        self.f.len() - 1
    }
    pub fn name(&self) -> String {
        let f = crate::print::show(&poly_expr(&self.f, &self.var));
        if self.numfield {
            format!("numfield({})", f)
        } else {
            match &self.p {
                None => format!("quotient({}, {})", f, self.var),
                Some(p) => format!("quotient({}, {}, {})", f, self.var, p),
            }
        }
    }
    pub fn base(&self) -> String {
        match &self.p {
            None => "Q".into(),
            Some(p) => format!("Z/{}", p),
        }
    }
    pub fn elem(self: &Rc<QRing>, v: P) -> QElem {
        let v = self.rem(&v, &self.f);
        QElem {
            ring: self.clone(),
            v,
        }
    }
    pub fn from_expr(self: &Rc<QRing>, e: &Expr) -> R<QElem> {
        if let Some(x) = as_elem(e) {
            if x.ring == *self {
                return Ok(x.clone());
            }
            return Err(format!(
                "{} is in {}, not {}",
                x.text(),
                x.ring.name(),
                self.name()
            ));
        }
        let bad = || {
            format!(
                "{}: expected a polynomial in {} with rational coefficients, got {}",
                self.name(),
                self.var,
                crate::print::show(e)
            )
        };
        let cs = crate::cas::poly_coeffs(e, &self.var).ok_or_else(bad)?;
        let mut v = vec![];
        for c in &cs {
            let c = crate::calc::simplify(c);
            let r = c
                .num()
                .filter(|n| n.is_exact())
                .and_then(|n| n.big())
                .ok_or_else(bad)?;
            if let Some(p) = &self.p {
                if r.denom().gcd(p) != BigInt::one() {
                    return Err(format!(
                        "{}: {} has no inverse mod {}",
                        self.name(),
                        r.denom(),
                        p
                    ));
                }
            }
            v.push(r);
        }
        Ok(self.elem(v))
    }
    pub fn generator(self: &Rc<QRing>) -> QElem {
        self.elem(vec![Q::zero(), Q::one()])
    }
}

pub fn poly_expr(v: &[Q], x: &str) -> Expr {
    use crate::build::*;
    let terms: Vec<Expr> = v
        .iter()
        .enumerate()
        .rev()
        .filter(|(_, c)| !c.is_zero())
        .map(|(i, c)| {
            let c = Expr::Num(crate::num::Number::from_big(c.clone()));
            match i {
                0 => c,
                1 => mul2(c, Expr::sym(x)),
                _ => mul2(c, pow(Expr::sym(x), Expr::int(i as i128))),
            }
        })
        .collect();
    if terms.is_empty() {
        Expr::zero()
    } else {
        add(terms)
    }
}

/// Build K[x]/(f); numfield asks f to be irreducible over Q
pub fn make(f: &Expr, var: &str, p: Option<BigInt>, numfield: bool) -> R<Rc<QRing>> {
    let what = if numfield { "numfield" } else { "quotient" };
    let cs = crate::cas::poly_coeffs(f, var).ok_or_else(|| {
        format!(
            "{}: {} is not a polynomial in {}",
            what,
            crate::print::show(f),
            var
        )
    })?;
    let mut v: P = vec![];
    for c in &cs {
        let c = crate::calc::simplify(c);
        v.push(
            c.num()
                .filter(|n| n.is_exact())
                .and_then(|n| n.big())
                .ok_or_else(|| format!("{}: the coefficients must be rational numbers", what))?,
        );
    }
    let mut ring = QRing {
        f: vec![],
        var: var.to_string(),
        p,
        numfield,
        field: false,
    };
    let mut v = ring.trim(v);
    if v.len() < 2 {
        return Err(format!(
            "{}: the polynomial must have degree at least 1",
            what
        ));
    }
    if v.len() > MAX_DEG + 1 {
        return Err(format!("{}: degree up to {}", what, MAX_DEG));
    }
    let lc = ring.inv_c(v.last().unwrap());
    v = v.into_iter().map(|c| ring.norm(c * &lc)).collect();
    ring.f = v;
    ring.field = irreducible(&ring);
    if numfield && !ring.field {
        return Err(format!(
            "numfield: {} factors over Q, so the quotient is not a field; use quotient({}, {})",
            crate::print::show(&poly_expr(&ring.f, var)),
            crate::print::show(&poly_expr(&ring.f, var)),
            var
        ));
    }
    Ok(Rc::new(ring))
}

fn irreducible(r: &QRing) -> bool {
    let n = r.degree();
    match &r.p {
        None => {
            let den = r.f.iter().fold(BigInt::one(), |a, c| a.lcm(c.denom()));
            let z: Vec<BigInt> =
                r.f.iter()
                    .map(|c| (c * q(den.clone())).to_integer())
                    .collect();
            match crate::algfactor::factor_zpoly(&z) {
                Some((_, fs)) => fs.len() == 1 && fs[0].1 == 1,
                None => false,
            }
        }
        Some(p) => {
            let f: Vec<BigInt> = r.f.iter().map(|c| c.to_integer()).collect();
            let x = vec![BigInt::zero(), BigInt::one()];
            let one = vec![BigInt::one()];
            let mut xp = x.clone();
            for _ in 0..n / 2 {
                xp = ms::powmod(&xp, p, &f, p);
                if ms::gcd(&ms::sub(&xp, &x, p), &f, p) != one {
                    return false;
                }
            }
            // squarefree: no repeated factor of degree > n/2 is possible, so done
            true
        }
    }
}

impl QElem {
    pub fn text(&self) -> String {
        let r = &self.ring;
        let f = crate::print::show(&poly_expr(&r.f, &r.var));
        let g = crate::print::show(&poly_expr(&self.v, &r.var));
        if r.numfield {
            format!("numfield({}, {})", f, g)
        } else {
            let p = r.p.clone().unwrap_or_default();
            format!("quotient({}, {}, {}, {})", f, r.var, p, g)
        }
    }
    pub fn lift(&self) -> Expr {
        poly_expr(&self.v, &self.ring.var)
    }
    fn same(&self, o: &QElem) -> R<()> {
        if self.ring == o.ring {
            Ok(())
        } else {
            Err(format!(
                "can't combine elements of {} and {}",
                self.ring.name(),
                o.ring.name()
            ))
        }
    }
    pub fn add(&self, o: &QElem) -> QElem {
        let n = self.v.len().max(o.v.len());
        let z = Q::zero();
        self.ring.elem(
            (0..n)
                .map(|i| self.v.get(i).unwrap_or(&z) + o.v.get(i).unwrap_or(&z))
                .collect(),
        )
    }
    pub fn neg(&self) -> QElem {
        self.ring.elem(self.v.iter().map(|c| -c).collect())
    }
    pub fn mul(&self, o: &QElem) -> QElem {
        QElem {
            ring: self.ring.clone(),
            v: self.ring.mul(&self.v, &o.v),
        }
    }
    /// inverse by the extended Euclidean algorithm
    pub fn inv(&self) -> R<QElem> {
        let r = &self.ring;
        let (mut a, mut b) = (r.f.clone(), self.v.clone());
        let (mut sa, mut sb): (P, P) = (vec![], vec![Q::one()]);
        while !b.is_empty() {
            // a = qt*b + rm
            let mut rm = a.clone();
            let mut qt = vec![Q::zero(); a.len().saturating_sub(b.len()) + 1];
            let lb = r.inv_c(b.last().unwrap());
            while rm.len() >= b.len() && !rm.is_empty() {
                let k = rm.len() - b.len();
                let c = r.norm(rm.last().unwrap() * &lb);
                for (i, bc) in b.iter().enumerate() {
                    rm[i + k] = r.norm(&rm[i + k] - &c * bc);
                }
                qt[k] = c;
                rm = r.trim(rm);
            }
            // s = sa - qt*sb
            let mut prod = vec![Q::zero(); qt.len() + sb.len()];
            for (i, x) in qt.iter().enumerate() {
                for (j, y) in sb.iter().enumerate() {
                    prod[i + j] += x * y;
                }
            }
            let n = sa.len().max(prod.len());
            let z = Q::zero();
            let s: P = r.trim(
                (0..n)
                    .map(|i| sa.get(i).unwrap_or(&z) - prod.get(i).unwrap_or(&z))
                    .collect(),
            );
            a = b;
            b = rm;
            sa = sb;
            sb = s;
        }
        // a = gcd, sa*g ≡ a (mod f)
        if a.len() != 1 {
            return Err(format!(
                "{} has no inverse in {}: it shares the factor {} with the modulus",
                self.text(),
                r.name(),
                crate::print::show(&poly_expr(
                    &r.trim(a.iter().map(|c| c * r.inv_c(a.last().unwrap())).collect()),
                    &r.var
                ))
            ));
        }
        let k = r.inv_c(&a[0]);
        Ok(r.elem(sa.into_iter().map(|c| c * &k).collect()))
    }
    pub fn pow(&self, e: &BigInt) -> R<QElem> {
        let mut b = if e.is_negative() {
            self.inv()?
        } else {
            self.clone()
        };
        let e = e.abs();
        let mut acc = self.ring.elem(vec![Q::one()]);
        for i in (0..e.bits()).rev() {
            acc = acc.mul(&acc);
            if e.bit(i) {
                acc = acc.mul(&b);
            }
        }
        b = acc;
        Ok(b)
    }

    /// matrix of multiplication by self on the basis 1, x, ..., x^(n-1); column j = self*x^j
    fn matrix(&self) -> Vec<Vec<Q>> {
        let n = self.ring.degree();
        let mut cols = vec![];
        let mut cur = self.v.clone();
        for _ in 0..n {
            let mut c = cur.clone();
            c.resize(n, Q::zero());
            cols.push(c);
            cur = self.ring.mul(&cur, &vec![Q::zero(), Q::one()]);
        }
        (0..n)
            .map(|i| (0..n).map(|j| cols[j][i].clone()).collect())
            .collect()
    }
    pub fn trace(&self) -> Q {
        let m = self.matrix();
        self.ring.norm((0..m.len()).map(|i| m[i][i].clone()).sum())
    }
    pub fn norm(&self) -> Q {
        let r = &self.ring;
        let mut m = self.matrix();
        let n = m.len();
        let mut det = Q::one();
        for c in 0..n {
            let Some(piv) = (c..n).find(|&i| !m[i][c].is_zero()) else {
                return Q::zero();
            };
            if piv != c {
                m.swap(piv, c);
                det = -det;
            }
            det = r.norm(det * &m[c][c]);
            let ic = r.inv_c(&m[c][c]);
            for i in c + 1..n {
                let k = r.norm(&m[i][c] * &ic);
                if k.is_zero() {
                    continue;
                }
                for j in c..n {
                    let t = &m[c][j] * &k;
                    m[i][j] = r.norm(&m[i][j] - t);
                }
            }
        }
        det
    }
    /// monic polynomial of least degree with g(self) = 0, low to high
    pub fn minpoly(&self) -> P {
        let r = &self.ring;
        let n = r.degree();
        // echelon rows of powers with their combinations
        let mut rows: Vec<(P, P)> = vec![]; // (vector, combination), vector has pivot at its first nonzero
        let mut pw = r.elem(vec![Q::one()]);
        for k in 0..=n {
            let mut v = pw.v.clone();
            v.resize(n, Q::zero());
            let mut comb = vec![Q::zero(); k + 1];
            comb[k] = Q::one();
            for (rv, rc) in &rows {
                let piv = rv.iter().position(|c| !c.is_zero()).unwrap();
                if !v[piv].is_zero() {
                    let t = r.norm(&v[piv] * r.inv_c(&rv[piv]));
                    for i in 0..n {
                        v[i] = r.norm(&v[i] - &t * &rv[i]);
                    }
                    for i in 0..rc.len() {
                        comb[i] = r.norm(&comb[i] - &t * &rc[i]);
                    }
                }
            }
            if v.iter().all(|c| c.is_zero()) {
                return r.trim(comb);
            }
            rows.push((v, comb));
            pw = pw.mul(self);
        }
        unreachable!("degree n powers are always dependent")
    }
}

pub fn as_elem(e: &Expr) -> Option<&QElem> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::QElem(x) => Some(x),
            _ => None,
        },
        _ => None,
    }
}

pub fn as_ring(e: &Expr) -> Option<Rc<QRing>> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::QRing(r) => Some(r.clone()),
            Obj::QElem(x) => Some(x.ring.clone()),
            _ => None,
        },
        _ => None,
    }
}

pub fn value(x: QElem) -> Expr {
    Expr::Obj(Rc::new(Obj::QElem(x)))
}

pub fn ring_value(r: Rc<QRing>) -> Expr {
    Expr::Obj(Rc::new(Obj::QRing(r)))
}

pub fn qnum(c: Q) -> Expr {
    Expr::Num(crate::num::Number::from_big(c))
}

/// Arithmetic when a side is a quotient-ring element
pub fn arith(op: Op, a: &Expr, b: &Expr) -> R<Expr> {
    let ring = as_elem(a).or(as_elem(b)).unwrap().ring.clone();
    if op == Op::Pow {
        let x = ring.from_expr(a)?;
        let e = b
            .num()
            .filter(|n| n.is_integer())
            .and_then(|n| n.as_bigint())
            .ok_or_else(|| {
                format!(
                    "an exponent must be a whole number, got {}",
                    crate::print::show(b)
                )
            })?;
        return Ok(value(x.pow(&e)?));
    }
    let lift = |e: &Expr| -> R<QElem> {
        match as_elem(e) {
            Some(x) => {
                x.same(&QElem {
                    ring: ring.clone(),
                    v: vec![],
                })?;
                Ok(x.clone())
            }
            None if e.num().is_some_and(|n| n.is_exact()) => ring.from_expr(e),
            None => Err(format!(
                "can't combine an element of {} with {}",
                ring.name(),
                crate::print::show(e)
            )),
        }
    };
    let (x, y) = (lift(a)?, lift(b)?);
    Ok(value(match op {
        Op::Add => x.add(&y),
        Op::Sub => x.add(&y.neg()),
        Op::Mul => x.mul(&y),
        Op::Div => x.mul(&y.inv()?),
        _ => return Err("not an arithmetic operation".into()),
    }))
}

pub fn equal(a: &Expr, b: &Expr) -> R<bool> {
    let ring = as_elem(a).or(as_elem(b)).unwrap().ring.clone();
    let side = |e: &Expr| match as_elem(e) {
        Some(x) => Ok(Some(x.clone())),
        None if e.num().is_some_and(|n| n.is_exact()) => ring.from_expr(e).map(Some),
        None => Ok(None),
    };
    Ok(match (side(a)?, side(b)?) {
        (Some(x), Some(y)) => x == y,
        _ => false,
    })
}
