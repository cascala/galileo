//! Finite rings and fields as values: `zmod(n)` (the integers mod n), `gf(p)`
//! and `gf(p^k)` (the field with p^k elements, built as Z/p[a] modulo a
//! primitive polynomial of degree k, so `a` itself generates the unit group).
//!
//! Elements print as Galileo that rebuilds them: `zmod(12, 5)`, `gf(7, 3)`,
//! `gf(9, a + 1)`.

use crate::expr::{Expr, Obj};
use crate::modsolve as ms;
use crate::numtheory as nt;
use crate::parser::Op;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;
use std::rc::Rc;

pub type R<T> = Result<T, String>;

/// largest field degree k for gf(p^k)
const MAX_K: u32 = 64;
/// elements(F) lists rings up to this size
pub const MAX_LIST: u64 = 4096;
/// baby-step giant-step tables up to this size
const MAX_BSGS: u64 = 4_000_000;

#[derive(Debug, PartialEq)]
pub struct Ring {
    /// n for zmod(n), p for gf(p^k)
    pub n: BigInt,
    pub k: u32,
    /// monic modulus polynomial, low to high (gf(p^k) with k > 1 only)
    pub m: Vec<BigInt>,
    pub gf: bool,
    /// size of the unit group with its factorization
    units: BigInt,
    units_f: Vec<(BigInt, u32)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Elem {
    pub ring: Rc<Ring>,
    /// coefficients low to high, reduced, no leading zeros (zero is empty)
    pub v: Vec<BigInt>,
}

impl Ring {
    pub fn size(&self) -> BigInt {
        num_traits::pow(self.n.clone(), self.k as usize)
    }
    pub fn is_field(&self) -> bool {
        self.gf || nt::is_prime(&self.n)
    }
    pub fn name(&self) -> String {
        if self.gf {
            format!("gf({})", self.size())
        } else {
            format!("zmod({})", self.n)
        }
    }
    pub fn units(&self) -> &BigInt {
        &self.units
    }
    pub fn elem(self: &Rc<Ring>, v: Vec<BigInt>) -> Elem {
        let v = if self.k == 1 {
            ms::trim(vec![
                v.first().cloned().unwrap_or_default().mod_floor(&self.n),
            ])
        } else {
            ms::rem(&ms::reduce(&v, &self.n), &self.m, &self.n)
        };
        Elem {
            ring: self.clone(),
            v,
        }
    }
    pub(crate) fn int(self: &Rc<Ring>, c: BigInt) -> Elem {
        self.elem(vec![c])
    }
    /// the generator symbol `a` (gf(p^k), k > 1)
    pub fn a(self: &Rc<Ring>) -> Elem {
        self.elem(vec![BigInt::zero(), BigInt::one()])
    }

    /// Element from a number, a polynomial in `a`, or an element of this ring
    pub fn from_expr(self: &Rc<Ring>, e: &Expr) -> R<Elem> {
        if let Some(x) = as_elem(e) {
            return if x.ring == *self {
                Ok(x.clone())
            } else {
                Err(format!(
                    "{} is in {}, not {}",
                    x.text(),
                    x.ring.name(),
                    self.name()
                ))
            };
        }
        if let Some(r) = e.num().and_then(|n| n.big()) {
            let d = self.int(r.denom().clone());
            return self.int(r.numer().clone()).div(&d);
        }
        if self.k > 1 {
            if let Some(cs) = crate::cas::poly_coeffs(e, "a") {
                let mut acc = self.int(BigInt::zero());
                let mut pw = self.int(BigInt::one());
                let a = self.a();
                for c in &cs {
                    let c = self.from_expr(&crate::calc::simplify(c))?;
                    acc = acc.add(&c.mul(&pw));
                    pw = pw.mul(&a);
                }
                return Ok(acc);
            }
        }
        let what = if self.k > 1 {
            "a number or a polynomial in a"
        } else {
            "a number"
        };
        Err(format!(
            "{}: expected {}, got {}",
            self.name(),
            what,
            crate::print::show(e)
        ))
    }

    /// The elements in order (small rings only)
    pub fn elements(self: &Rc<Ring>) -> R<Vec<Elem>> {
        let q = self.size();
        if q > BigInt::from(MAX_LIST) {
            return Err(format!(
                "elements: {} has {} elements; listing stops at {}",
                self.name(),
                q,
                MAX_LIST
            ));
        }
        let q = q.to_u64().unwrap();
        Ok((0..q)
            .map(|t| self.elem(digits(t, &self.n, self.k)))
            .collect())
    }

    /// A generator of the unit group, if it is cyclic
    pub fn generator(self: &Rc<Ring>) -> R<Elem> {
        if self.k > 1 {
            return Ok(self.a());
        }
        if !self.cyclic() {
            return Err(format!(
                "generator: the units of {} are not cyclic (only n = 2, 4, p^k and 2p^k have one)",
                self.name()
            ));
        }
        let mut t = BigInt::one();
        for _ in 0..1_000_000 {
            let x = self.int(t.clone());
            if x.is_unit() && x.order()? == self.units {
                return Ok(x);
            }
            t += 1;
        }
        Err("generator: none found in the first million candidates".into())
    }

    fn cyclic(&self) -> bool {
        if self.is_field() {
            return true;
        }
        let mut n = self.n.clone();
        if n == BigInt::from(4) {
            return true;
        }
        if n.is_even() {
            n /= 2;
            if n.is_even() {
                return false;
            }
        }
        if n.is_one() {
            return true;
        }
        nt::factorize(&n).map(|f| f.len() == 1).unwrap_or(false)
    }
}

fn digits(mut t: u64, p: &BigInt, k: u32) -> Vec<BigInt> {
    let p = p.to_u64().unwrap();
    (0..k)
        .map(|_| {
            let d = t % p;
            t /= p;
            BigInt::from(d)
        })
        .collect()
}

pub fn as_elem(e: &Expr) -> Option<&Elem> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Elem(x) => Some(x),
            _ => None,
        },
        _ => None,
    }
}

pub fn as_ring(e: &Expr) -> Option<Rc<Ring>> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Ring(r) => Some(r.clone()),
            _ => None,
        },
        _ => None,
    }
}

pub fn value(x: Elem) -> Expr {
    Expr::Obj(Rc::new(Obj::Elem(x)))
}

/// F(x) for a number, an element, a matrix or a list of them
pub fn embed(r: &Rc<Ring>, x: &Expr) -> R<Expr> {
    match x {
        Expr::Matrix(m) => Ok(Expr::Matrix(
            m.iter()
                .map(|row| row.iter().map(|e| embed(r, e)).collect::<R<Vec<_>>>())
                .collect::<R<Vec<_>>>()?,
        )),
        Expr::Obj(o) if matches!(&**o, Obj::List(_)) => {
            let Obj::List(v) = &**o else { unreachable!() };
            Ok(Expr::Obj(Rc::new(Obj::List(
                v.iter().map(|e| embed(r, e)).collect::<R<Vec<_>>>()?,
            ))))
        }
        _ => r.from_expr(x).map(value),
    }
}

pub fn ring_value(r: Rc<Ring>) -> Expr {
    Expr::Obj(Rc::new(Obj::Ring(r)))
}

fn factor(n: &BigInt) -> R<Vec<(BigInt, u32)>> {
    if n.is_one() {
        Ok(vec![])
    } else {
        nt::factorize(n)
    }
}

/// zmod(n)
pub fn zmod(n: &BigInt) -> R<Rc<Ring>> {
    if *n < BigInt::from(2) {
        return Err("zmod: n must be at least 2".into());
    }
    let f = factor(n)?;
    let mut phi = BigInt::one();
    for (p, e) in &f {
        phi *= (p - 1u32) * num_traits::pow(p.clone(), *e as usize - 1);
    }
    let units_f = factor(&phi)?;
    Ok(Rc::new(Ring {
        n: n.clone(),
        k: 1,
        m: vec![],
        gf: false,
        units: phi,
        units_f,
    }))
}

/// gf(q) for a prime power q
pub fn gf(q: &BigInt) -> R<Rc<Ring>> {
    let f = if *q < BigInt::from(2) {
        vec![]
    } else {
        factor(q)?
    };
    let (p, k) = match f.as_slice() {
        [(p, k)] => (p.clone(), *k),
        _ => {
            return Err(format!(
                "gf: {} is not a prime power; finite fields have p^k elements",
                q
            ));
        }
    };
    if k > MAX_K {
        return Err(format!("gf: degree k up to {}", MAX_K));
    }
    let units = q - 1u32;
    let units_f = factor(&units)?;
    let m = if k == 1 {
        vec![]
    } else {
        primitive_poly(&p, k, &units, &units_f)?
    };
    Ok(Rc::new(Ring {
        n: p,
        k,
        m,
        gf: true,
        units,
        units_f,
    }))
}

/// The first monic primitive polynomial of degree k over Z/p, counting the
/// lower coefficients as base-p digits (constant term first)
fn primitive_poly(p: &BigInt, k: u32, units: &BigInt, uf: &[(BigInt, u32)]) -> R<Vec<BigInt>> {
    let x = vec![BigInt::zero(), BigInt::one()];
    let one = vec![BigInt::one()];
    let kf = factor(&BigInt::from(k))?;
    let mut lower: Vec<BigInt> = vec![BigInt::zero(); k as usize];
    for _ in 0..10_000_000u64 {
        // next candidate
        for d in lower.iter_mut() {
            *d += 1;
            if *d < *p {
                break;
            }
            *d = BigInt::zero();
        }
        if lower[0].is_zero() {
            continue; // divisible by x
        }
        let mut m = lower.clone();
        m.push(BigInt::one());
        // Rabin: x^(p^k) = x mod m, and gcd(x^(p^(k/r)) - x, m) = 1
        let frob = |j: u32| ms::powmod(&x, &num_traits::pow(p.clone(), j as usize), &m, p);
        if frob(k) != ms::rem(&x, &m, p) {
            continue;
        }
        if kf.iter().any(|(r, _)| {
            let j = k / r.to_u32().unwrap();
            ms::gcd(&ms::sub(&frob(j), &x, p), &m, p) != one
        }) {
            continue;
        }
        // primitive: x^(units/r) != 1 for every prime r
        if uf
            .iter()
            .all(|(r, _)| ms::powmod(&x, &(units / r), &m, p) != one)
        {
            return Ok(m);
        }
    }
    Err("gf: no primitive polynomial found".into())
}

impl Elem {
    fn same(&self, o: &Elem) -> R<()> {
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
    pub fn is_zero(&self) -> bool {
        self.v.is_empty()
    }
    fn n(&self) -> &BigInt {
        &self.ring.n
    }
    fn c0(&self) -> BigInt {
        self.v.first().cloned().unwrap_or_default()
    }
    pub fn add(&self, o: &Elem) -> Elem {
        let len = self.v.len().max(o.v.len());
        let v = (0..len)
            .map(|i| {
                self.v.get(i).cloned().unwrap_or_default() + o.v.get(i).cloned().unwrap_or_default()
            })
            .collect();
        self.ring.elem(v)
    }
    pub fn neg(&self) -> Elem {
        self.ring.elem(self.v.iter().map(|c| -c).collect())
    }
    pub fn mul(&self, o: &Elem) -> Elem {
        if self.ring.k == 1 {
            self.ring.int(self.c0() * o.c0())
        } else {
            Elem {
                ring: self.ring.clone(),
                v: ms::mulmod(&self.v, &o.v, &self.ring.m, self.n()),
            }
        }
    }
    pub fn is_unit(&self) -> bool {
        if self.ring.k > 1 {
            !self.is_zero()
        } else {
            self.c0().gcd(self.n()).is_one()
        }
    }
    pub fn inv(&self) -> R<Elem> {
        if !self.is_unit() {
            let why = if self.is_zero() {
                String::new()
            } else {
                format!(
                    " (it shares the factor {} with {})",
                    self.c0().gcd(self.n()),
                    self.n()
                )
            };
            return Err(format!(
                "{} has no inverse in {}{}",
                self.text(),
                self.ring.name(),
                why
            ));
        }
        if self.ring.k == 1 {
            Ok(self.ring.int(nt::modinv(&self.c0(), self.n()).unwrap()))
        } else {
            Ok(self.pow_nat(&(self.ring.units.clone() - 1u32)))
        }
    }
    pub fn div(&self, o: &Elem) -> R<Elem> {
        Ok(self.mul(&o.inv()?))
    }
    fn pow_nat(&self, e: &BigInt) -> Elem {
        if self.ring.k == 1 {
            self.ring.int(self.c0().modpow(e, self.n()))
        } else {
            Elem {
                ring: self.ring.clone(),
                v: ms::powmod(&self.v, e, &self.ring.m, self.n()),
            }
        }
    }
    pub fn pow(&self, e: &BigInt) -> R<Elem> {
        if e.is_negative() {
            Ok(self.inv()?.pow_nat(&-e))
        } else {
            Ok(self.pow_nat(e))
        }
    }
    pub fn is_one(&self) -> bool {
        self.v.len() == 1 && self.v[0].is_one()
    }

    /// Multiplicative order
    pub fn order(&self) -> R<BigInt> {
        if !self.is_unit() {
            return Err(format!(
                "order: {} is not a unit of {}, so it has no multiplicative order",
                self.text(),
                self.ring.name()
            ));
        }
        let mut ord = self.ring.units.clone();
        for (r, _) in &self.ring.units_f {
            while (&ord % r).is_zero() && self.pow_nat(&(&ord / r)).is_one() {
                ord /= r;
            }
        }
        Ok(ord)
    }

    /// The integer (k = 1) or polynomial in a that represents this element
    pub fn lift(&self) -> Expr {
        if self.ring.k == 1 {
            return Expr::Num(crate::num::Number::from_bigint(self.c0()));
        }
        poly_expr(&self.v, "a")
    }

    pub fn text(&self) -> String {
        let body = if self.ring.k == 1 {
            self.c0().to_string()
        } else {
            crate::print::show(&poly_expr(&self.v, "a"))
        };
        let size = if self.ring.gf {
            self.ring.size()
        } else {
            self.ring.n.clone()
        };
        format!(
            "{}({}, {})",
            if self.ring.gf { "gf" } else { "zmod" },
            size,
            body
        )
    }
}

pub fn poly_expr(v: &[BigInt], x: &str) -> Expr {
    use crate::build::*;
    let terms: Vec<Expr> = v
        .iter()
        .enumerate()
        .rev()
        .filter(|(_, c)| !c.is_zero())
        .map(|(i, c)| {
            let c = Expr::Num(crate::num::Number::from_bigint(c.clone()));
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

/// x such that g^x = h (Pohlig–Hellman, baby-step giant-step in each prime part)
pub fn dlog(h: &Elem, g: &Elem) -> R<BigInt> {
    h.same(g)?;
    let n = g.order()?;
    if !h.is_unit() {
        return Err(format!("dlog: {} is not a unit", h.text()));
    }
    let mut residues = vec![];
    for (r, e) in factor(&n)? {
        let re = num_traits::pow(r.clone(), e as usize);
        // g0 has order r; solve for the base-r digits of x mod r^e
        let g0 = g.pow_nat(&(&n / &r));
        let mut x = BigInt::zero();
        let mut rk = BigInt::one();
        for _ in 0..e {
            let t = h.mul(&g.pow_nat(&x).inv()?).pow_nat(&(&n / (&rk * &r)));
            let d = bsgs(&t, &g0, &r)?;
            x += &d * &rk;
            rk *= &r;
        }
        residues.push((x, re));
    }
    let mut x = BigInt::zero();
    let mut m = BigInt::one();
    for (a, mm) in residues {
        // x ≡ a mod mm
        let t = ((a - &x) * nt::modinv(&m, &mm).unwrap()).mod_floor(&mm);
        x += &m * t;
        m *= mm;
    }
    if g.pow_nat(&x) != *h {
        return Err(format!("dlog: {} is not a power of {}", h.text(), g.text()));
    }
    Ok(x)
}

/// d in [0, r) with g^d = t, where g has order r
fn bsgs(t: &Elem, g: &Elem, r: &BigInt) -> R<BigInt> {
    let m = r.sqrt() + 1u32;
    if m > BigInt::from(MAX_BSGS) {
        return Err(format!(
            "dlog: the group has a prime factor {} that is too large for baby-step giant-step",
            r
        ));
    }
    let m = m.to_u64().unwrap();
    let mut table: HashMap<Vec<BigInt>, u64> = HashMap::new();
    let mut cur = g.ring.int(BigInt::one());
    for j in 0..m {
        table.entry(cur.v.clone()).or_insert(j);
        cur = cur.mul(g);
    }
    let step = g.pow_nat(&BigInt::from(m)).inv()?;
    let mut y = t.clone();
    for i in 0..=m {
        if let Some(j) = table.get(&y.v) {
            return Ok((BigInt::from(i) * m + j) % r);
        }
        y = y.mul(&step);
    }
    Err(format!("dlog: {} is not a power of {}", t.text(), g.text()))
}

/// Arithmetic when at least one side is a ring element
pub fn arith(op: Op, a: &Expr, b: &Expr) -> R<Expr> {
    let ring = as_elem(a).or(as_elem(b)).unwrap().ring.clone();
    if op == Op::Pow {
        let x = ring.from_expr(a)?;
        if as_elem(b).is_some() {
            return Err("an exponent must be a whole number, not a ring element".into());
        }
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
    let lift = |e: &Expr| -> R<Elem> {
        match as_elem(e) {
            Some(x) => {
                x.same(&Elem {
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
        Op::Div => x.div(&y)?,
        _ => return Err("not an arithmetic operation".into()),
    }))
}

/// a == b when a side is a ring element
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
