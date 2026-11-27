//! Searching for factoring procedures. A procedure is a tiny program in one
//! of two shapes:
//!
//! * `iter(f, h, x0)`: x starts at x0 and becomes f(x, y, i) at step i; y is
//!   x saved at the last power-of-two step (Brent's cycle finding); the
//!   product of h(x, y, i) over a batch of steps is checked with gcd(., N).
//!   Pollard rho is `iter(x^2 + 1, x - y, 2)`, Pollard p - 1 is
//!   `iter(pow(x, i), x - 1, 2)`.
//! * `rel(s, fb)`: at step i, s = s(x, i) (x is the previous s) and
//!   r = s^2 mod N (least absolute residue). When r is a square t^2,
//!   gcd(s - t, N) is tried; with a factor base of fb primes, every r that
//!   factors over it is kept, and once there are more relations than primes,
//!   linear algebra mod 2 combines them into a square (Dixon, quadratic
//!   sieve). Fermat is `rel(rt + i, 0)`, Hart's method `rel(rti, 0)`.
//!
//! Everything is computed modulo N. Leaves: x, y, i, rt = ceil(sqrt N),
//! rti = ceil(sqrt(i N)), B = floor(N^(1/3)), d0, d1 (the last two digits of
//! N in base B) and small integers; operations + - *, a^k, pow(a, i) = a^i,
//! luc(a) = the Lucas sequence V_i(a) (Williams p + 1), inv(a) (a modular
//! inverse: when it does not exist, gcd(a, N) is a factor).
//!
//! The cost of a run is counted in work units (one multiplication mod N
//! each), the same on every machine, so programs are compared fairly.

use crate::factorlab::{self as lab, Rng};
use crate::labgroups::{self as lg, World};
use crate::parser::{Node, Op, Stmt};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::cell::RefCell;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum E {
    X,
    Y,
    I,
    Rt,
    Rti,
    B,
    D0,
    D1,
    C(i64),
    Add(Box<E>, Box<E>),
    Sub(Box<E>, Box<E>),
    Mul(Box<E>, Box<E>),
    Pow(Box<E>, u32),
    PowI(Box<E>),
    Luc(Box<E>),
    Inv(Box<E>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prog {
    Iter {
        f: E,
        h: E,
        x0: E,
    },
    Rel {
        s: E,
        fb: usize,
    },
    /// grp(world, e, r): powers in a group mod N, see labgroups
    Grp {
        w: World,
        e: E,
        r: u64,
    },
}

// ---------- printing and parsing ----------

fn show_e(e: &E, top: bool) -> String {
    let wrap = |s: String| if top { s } else { format!("({})", s) };
    match e {
        E::X => "x".into(),
        E::Y => "y".into(),
        E::I => "i".into(),
        E::Rt => "rt".into(),
        E::Rti => "rti".into(),
        E::B => "B".into(),
        E::D0 => "d0".into(),
        E::D1 => "d1".into(),
        E::C(c) if *c < 0 => wrap(c.to_string()),
        E::C(c) => c.to_string(),
        E::Add(a, b) => wrap(format!("{} + {}", show_e(a, false), show_e(b, false))),
        E::Sub(a, b) => wrap(format!("{} - {}", show_e(a, false), show_e(b, false))),
        E::Mul(a, b) => wrap(format!("{}*{}", show_e(a, false), show_e(b, false))),
        E::Pow(a, k) if matches!(**a, E::Pow(..)) => format!("({})^{}", show_e(a, true), k),
        E::Pow(a, k) => format!("{}^{}", show_e(a, false), k),
        E::PowI(a) => format!("pow({}, i)", show_e(a, true)),
        E::Luc(a) => format!("luc({})", show_e(a, true)),
        E::Inv(a) => format!("inv({})", show_e(a, true)),
    }
}

impl std::fmt::Display for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", show_e(self, true))
    }
}

impl std::fmt::Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prog::Iter { f: g, h, x0 } => write!(f, "iter({}, {}, {})", g, h, x0),
            Prog::Rel { s, fb } => write!(f, "rel({}, {})", s, fb),
            Prog::Grp { w, e, r } => write!(f, "grp({}, {}, {})", w, e, r),
        }
    }
}

fn to_e(n: &Node) -> Result<E, String> {
    let b = |n: &Node| to_e(n).map(Box::new);
    Ok(match n {
        Node::Ident(s) => match s.as_str() {
            "x" => E::X,
            "y" => E::Y,
            "i" => E::I,
            "rt" => E::Rt,
            "rti" => E::Rti,
            "B" => E::B,
            "d0" => E::D0,
            "d1" => E::D1,
            o => {
                return Err(format!(
                    "unknown name {} in a lab program (use x, y, i, rt, rti, B, d0, d1)",
                    o
                ));
            }
        },
        Node::Num(s) => E::C(
            s.parse::<i64>()
                .map_err(|_| format!("{}: only whole numbers in lab programs", s))?,
        ),
        Node::Neg(a) => match to_e(a)? {
            E::C(c) => E::C(-c),
            o => E::Sub(Box::new(E::C(0)), Box::new(o)),
        },
        Node::Bin(Op::Add, x, y) => E::Add(b(x)?, b(y)?),
        Node::Bin(Op::Sub, x, y) => E::Sub(b(x)?, b(y)?),
        Node::Bin(Op::Mul, x, y) => E::Mul(b(x)?, b(y)?),
        Node::Bin(Op::Pow, x, y) => match &**y {
            Node::Ident(s) if s == "i" => E::PowI(b(x)?),
            Node::Num(k) => {
                let k: u32 = k
                    .parse()
                    .map_err(|_| "exponents are whole numbers 1 to 64, or i".to_string())?;
                if !(1..=64).contains(&k) {
                    return Err("exponents are whole numbers 1 to 64, or i".into());
                }
                E::Pow(b(x)?, k)
            }
            _ => return Err("exponents are whole numbers 1 to 64, or i".into()),
        },
        Node::Call(f, args) => match (f.as_str(), &args[..]) {
            ("pow", [a, e]) if matches!(e, Node::Ident(s) if s == "i") => E::PowI(b(a)?),
            ("luc", [a]) => E::Luc(b(a)?),
            ("inv", [a]) => E::Inv(b(a)?),
            _ => {
                return Err(format!(
                    "{}(...) is not a lab operation (use pow(a, i), luc(a), inv(a))",
                    f
                ));
            }
        },
        _ => {
            return Err(
                "lab programs use + - * ^, pow, luc, inv and the names x y i rt rti B d0 d1".into(),
            );
        }
    })
}

/// Parses `iter(f, h, x0)` or `rel(s, fb)`.
pub fn parse_prog(text: &str) -> Result<Prog, String> {
    let st = crate::parser::parse(text)?;
    let [Stmt::Expr(Node::Call(name, args))] = &st[..] else {
        return Err("a lab program looks like iter(f, h, x0) or rel(s, fb)".into());
    };
    match (name.as_str(), &args[..]) {
        ("iter", [f, h]) => Ok(Prog::Iter {
            f: to_e(f)?,
            h: to_e(h)?,
            x0: E::C(2),
        }),
        ("iter", [f, h, x0]) => Ok(Prog::Iter {
            f: to_e(f)?,
            h: to_e(h)?,
            x0: to_e(x0)?,
        }),
        ("rel", [s]) => Ok(Prog::Rel { s: to_e(s)?, fb: 0 }),
        ("rel", [s, Node::Num(k)]) => {
            let fb: usize = k.parse().map_err(|_| "rel(s, fb): fb is a whole number")?;
            if fb > 4096 {
                return Err("rel(s, fb): at most 4096 primes".into());
            }
            Ok(Prog::Rel { s: to_e(s)?, fb })
        }
        ("grp", [w, e]) => Ok(Prog::Grp {
            w: world_of(w)?,
            e: to_e(e)?,
            r: 0,
        }),
        ("grp", [w, e, Node::Num(k)]) => {
            let r: u64 = k
                .parse()
                .map_err(|_| "grp(world, e, r): r is a whole number")?;
            Ok(Prog::Grp {
                w: world_of(w)?,
                e: to_e(e)?,
                r,
            })
        }
        _ => Err("a lab program looks like iter(f, h, x0), rel(s, fb) or grp(world, e, r)".into()),
    }
}

fn world_of(n: &Node) -> Result<World, String> {
    let num = |n: &Node| -> Result<i64, String> {
        match n {
            Node::Num(s) => s
                .parse()
                .map_err(|_| "world parameters are whole numbers".to_string()),
            Node::Neg(a) => match &**a {
                Node::Num(s) => s
                    .parse::<i64>()
                    .map(|v| -v)
                    .map_err(|_| "world parameters are whole numbers".to_string()),
                _ => Err("world parameters are whole numbers".into()),
            },
            _ => Err("world parameters are whole numbers".into()),
        }
    };
    match n {
        Node::Ident(s) if s == "mul" => Ok(World::Mul),
        Node::Ident(s) if s == "mat" => Ok(World::Mat),
        Node::Call(f, a) if a.len() == 1 && f == "quad" => Ok(World::Quad(num(&a[0])?)),
        Node::Call(f, a) if a.len() == 1 && f == "cubic" => Ok(World::Cubic(num(&a[0])?)),
        Node::Call(f, a) if a.len() == 1 && f == "ec" => Ok(World::Ec(num(&a[0])?)),
        _ => Err("worlds: mul, quad(d), cubic(c), mat, ec(a)".into()),
    }
}

/// a whole-number value of e at step i (x and the other names count as 1), between 1 and 2^24
pub fn int_eval(e: &E, i: u64) -> u64 {
    fn go(e: &E, i: i128) -> i128 {
        let c = |v: i128| v.clamp(-(1 << 40), 1 << 40);
        match e {
            E::I => i,
            E::C(k) => *k as i128,
            E::Add(a, b) => c(go(a, i) + go(b, i)),
            E::Sub(a, b) => c(go(a, i) - go(b, i)),
            E::Mul(a, b) => c(go(a, i).saturating_mul(go(b, i))),
            E::Pow(a, k) => {
                let v = go(a, i);
                let mut r: i128 = 1;
                for _ in 0..*k {
                    r = c(r.saturating_mul(v));
                }
                r
            }
            E::PowI(a) | E::Luc(a) | E::Inv(a) => go(a, i),
            _ => 1,
        }
    }
    go(e, i as i128).clamp(1, 1 << 24) as u64
}

pub fn is_prog_text(t: &str) -> bool {
    let t = t.trim_start();
    t.starts_with("iter(") || t.starts_with("rel(") || t.starts_with("grp(")
}

// ---------- arithmetic modulo N ----------

trait Ring {
    type V: Clone + PartialEq;
    fn n(&self) -> &BigInt;
    fn of(&self, b: &BigInt) -> Self::V;
    fn big(&self, v: &Self::V) -> BigInt;
    fn add(&self, a: &Self::V, b: &Self::V) -> Self::V;
    fn sub(&self, a: &Self::V, b: &Self::V) -> Self::V;
    fn mul(&self, a: &Self::V, b: &Self::V) -> Self::V;
    fn small(&self, c: i64) -> Self::V;
    fn is_zero(&self, a: &Self::V) -> bool;
    fn gcd(&self, a: &Self::V) -> BigInt;
    /// least absolute residue as a BigInt
    fn signed(&self, a: &Self::V) -> BigInt;
}

struct R64 {
    n: u64,
    nb: BigInt,
}

impl Ring for R64 {
    type V = u64;
    fn n(&self) -> &BigInt {
        &self.nb
    }
    fn of(&self, b: &BigInt) -> u64 {
        b.mod_floor(&self.nb).to_u64().unwrap()
    }
    fn big(&self, v: &u64) -> BigInt {
        BigInt::from(*v)
    }
    fn add(&self, a: &u64, b: &u64) -> u64 {
        let s = *a as u128 + *b as u128;
        (if s >= self.n as u128 {
            s - self.n as u128
        } else {
            s
        }) as u64
    }
    fn sub(&self, a: &u64, b: &u64) -> u64 {
        if a >= b { a - b } else { a + (self.n - b) }
    }
    fn mul(&self, a: &u64, b: &u64) -> u64 {
        ((*a as u128 * *b as u128) % self.n as u128) as u64
    }
    fn small(&self, c: i64) -> u64 {
        c.rem_euclid(self.n as i64) as u64
    }
    fn is_zero(&self, a: &u64) -> bool {
        *a == 0
    }
    fn gcd(&self, a: &u64) -> BigInt {
        BigInt::from(a.gcd(&self.n))
    }
    fn signed(&self, a: &u64) -> BigInt {
        if *a > self.n / 2 {
            BigInt::from(*a) - &self.nb
        } else {
            BigInt::from(*a)
        }
    }
}

struct RBig {
    n: BigInt,
}

impl Ring for RBig {
    type V = BigInt;
    fn n(&self) -> &BigInt {
        &self.n
    }
    fn of(&self, b: &BigInt) -> BigInt {
        b.mod_floor(&self.n)
    }
    fn big(&self, v: &BigInt) -> BigInt {
        v.clone()
    }
    fn add(&self, a: &BigInt, b: &BigInt) -> BigInt {
        let s = a + b;
        if s >= self.n { s - &self.n } else { s }
    }
    fn sub(&self, a: &BigInt, b: &BigInt) -> BigInt {
        let s = a - b;
        if s.is_negative() { s + &self.n } else { s }
    }
    fn mul(&self, a: &BigInt, b: &BigInt) -> BigInt {
        (a * b) % &self.n
    }
    fn small(&self, c: i64) -> BigInt {
        BigInt::from(c).mod_floor(&self.n)
    }
    fn is_zero(&self, a: &BigInt) -> bool {
        a.is_zero()
    }
    fn gcd(&self, a: &BigInt) -> BigInt {
        a.gcd(&self.n)
    }
    fn signed(&self, a: &BigInt) -> BigInt {
        if a > &(&self.n / 2u32) {
            a - &self.n
        } else {
            a.clone()
        }
    }
}

/// Values that depend only on N
struct Consts<V> {
    rt: V,
    b: V,
    d0: V,
    d1: V,
}

/// a factor was found while evaluating
struct Found(BigInt);

struct Ctx<'a, Rg: Ring> {
    r: &'a Rg,
    c: &'a Consts<Rg::V>,
    x: Rg::V,
    y: Rg::V,
    i: u64,
    rti: Option<Rg::V>,
}

impl<Rg: Ring> Ctx<'_, Rg> {
    fn pow(&mut self, a: &Rg::V, mut e: u64) -> Rg::V {
        // square-and-multiply; the first multiplication by 1 is free
        let mut base = a.clone();
        let mut acc: Option<Rg::V> = None;
        let mut cost = 0.0;
        while e > 0 {
            if e & 1 == 1 {
                acc = Some(match acc {
                    None => base.clone(),
                    Some(v) => {
                        cost += 1.0;
                        self.r.mul(&v, &base)
                    }
                });
            }
            e >>= 1;
            if e > 0 {
                base = self.r.mul(&base, &base);
                cost += 1.0;
            }
        }
        lab::spend(cost);
        acc.unwrap_or_else(|| self.r.small(1))
    }

    /// Lucas V_e(a): V_0 = 2, V_1 = a, V_{2k} = V_k^2 - 2, V_{2k+1} = V_k V_{k+1} - a
    fn lucas(&mut self, a: &Rg::V, e: u64) -> Rg::V {
        let two = self.r.small(2);
        if e == 0 {
            return two;
        }
        let (mut v0, mut v1) = (a.clone(), self.r.sub(&self.r.mul(a, a), &two));
        let bits = 64 - e.leading_zeros();
        for k in (0..bits - 1).rev() {
            if (e >> k) & 1 == 1 {
                v0 = self.r.sub(&self.r.mul(&v0, &v1), a);
                v1 = self.r.sub(&self.r.mul(&v1, &v1), &two);
            } else {
                v1 = self.r.sub(&self.r.mul(&v0, &v1), a);
                v0 = self.r.sub(&self.r.mul(&v0, &v0), &two);
            }
        }
        lab::spend(2.0 * bits as f64);
        v0
    }

    fn eval(&mut self, e: &E) -> Result<Rg::V, Found> {
        Ok(match e {
            E::X => self.x.clone(),
            E::Y => self.y.clone(),
            E::I => self.r.small(self.i as i64),
            E::Rt => self.c.rt.clone(),
            E::B => self.c.b.clone(),
            E::D0 => self.c.d0.clone(),
            E::D1 => self.c.d1.clone(),
            E::C(c) => self.r.small(*c),
            E::Rti => {
                if self.rti.is_none() {
                    let v = lab::ceil_sqrt(&(self.r.n() * BigInt::from(self.i.max(1))));
                    lab::spend(3.0);
                    self.rti = Some(self.r.of(&v));
                }
                self.rti.clone().unwrap()
            }
            E::Add(a, b) => {
                let (a, b) = (self.eval(a)?, self.eval(b)?);
                self.r.add(&a, &b)
            }
            E::Sub(a, b) => {
                let (a, b) = (self.eval(a)?, self.eval(b)?);
                self.r.sub(&a, &b)
            }
            E::Mul(a, b) => {
                let (a, b) = (self.eval(a)?, self.eval(b)?);
                lab::spend(1.0);
                self.r.mul(&a, &b)
            }
            E::Pow(a, k) => {
                let a = self.eval(a)?;
                self.pow(&a, *k as u64)
            }
            E::PowI(a) => {
                let a = self.eval(a)?;
                let i = self.i.max(1);
                self.pow(&a, i)
            }
            E::Luc(a) => {
                let a = self.eval(a)?;
                let i = self.i.max(1);
                self.lucas(&a, i)
            }
            E::Inv(a) => {
                let a = self.eval(a)?;
                lab::spend(10.0);
                if self.r.is_zero(&a) {
                    return Ok(a);
                }
                let g = self.r.gcd(&a);
                if !g.is_one() {
                    return Err(Found(g));
                }
                let inv = crate::numtheory::modinv(&self.r.big(&a), self.r.n()).unwrap();
                self.r.of(&inv)
            }
        })
    }
}

fn nontrivial(g: &BigInt, n: &BigInt) -> bool {
    g > &BigInt::one() && g < n
}

fn consts<Rg: Ring>(r: &Rg) -> Consts<Rg::V> {
    let n = r.n();
    let b = n.cbrt();
    let d0 = n.mod_floor(&b);
    let d1 = (n / &b).mod_floor(&b);
    Consts {
        rt: r.of(&lab::ceil_sqrt(n)),
        b: r.of(&b),
        d0: r.of(&d0),
        d1: r.of(&d1),
    }
}

fn run_iter<Rg: Ring>(r: &Rg, f: &E, h: &E, x0: &E) -> Option<BigInt> {
    let n = r.n().clone();
    let c = consts(r);
    let zero = r.small(0);
    let mut cx = Ctx {
        r,
        c: &c,
        x: zero.clone(),
        y: zero.clone(),
        i: 0,
        rti: None,
    };
    let x = match cx.eval(x0) {
        Ok(v) => v,
        Err(Found(g)) => return nontrivial(&g, &n).then_some(g),
    };
    cx.x = x.clone();
    cx.y = x;
    const BATCH: u64 = 32;
    loop {
        let saved = (cx.x.clone(), cx.y.clone(), cx.i);
        let mut acc;
        // one batch; on gcd = N, replay it step by step
        for replay in [false, true] {
            if replay {
                (cx.x, cx.y, cx.i) = saved.clone();
            }
            acc = r.small(1);
            for _ in 0..BATCH {
                cx.i += 1;
                cx.rti = None;
                let step = (|| -> Result<Rg::V, Found> {
                    let x = cx.eval(f)?;
                    cx.x = x;
                    cx.eval(h)
                })();
                let hv = match step {
                    Ok(v) => v,
                    Err(Found(g)) => {
                        if nontrivial(&g, &n) {
                            return Some(g);
                        }
                        r.small(1)
                    }
                };
                if cx.i.is_power_of_two() {
                    cx.y = cx.x.clone();
                }
                if replay {
                    let g = r.gcd(&hv);
                    lab::spend(4.0);
                    if nontrivial(&g, &n) {
                        return Some(g);
                    }
                    if g == n {
                        return None; // h is 0 mod N here: this program is stuck
                    }
                } else {
                    acc = r.mul(&acc, &hv);
                    lab::spend(1.0);
                }
                if lab::out_of_budget() {
                    return None;
                }
            }
            if replay {
                break;
            }
            let g = r.gcd(&acc);
            lab::spend(4.0);
            if nontrivial(&g, &n) {
                return Some(g);
            }
            if g.is_one() {
                break;
            }
        }
        if lab::out_of_budget() {
            return None;
        }
    }
}

fn run_rel<Rg: Ring>(r: &Rg, s: &E, fbn: usize) -> Option<BigInt> {
    let n = r.n().clone();
    let c = consts(r);
    // factor base: -1 and primes p with (N/p) = 1
    let mut fb: Vec<u64> = vec![];
    if fbn > 0 {
        fb.push(2);
        for p in lab::small_primes(200_000).into_iter().skip(1) {
            if fb.len() >= fbn {
                break;
            }
            let m = (&n % p).to_u64().unwrap();
            if m == 0 {
                return nontrivial(&BigInt::from(p), &n).then(|| BigInt::from(p));
            }
            if lab::legendre_u64(m, p) == 1 {
                fb.push(p);
            }
            lab::spend(1.0);
        }
    }
    let cols = fb.len() + 1;
    let mut rels: Vec<(BigInt, Vec<u32>)> = vec![];
    let mut seen: HashSet<BigInt> = HashSet::new();
    let mut wanted = cols + 5;
    let rt0 = c.rt.clone();
    let mut cx = Ctx {
        r,
        c: &c,
        x: rt0.clone(),
        y: rt0,
        i: 0,
        rti: None,
    };
    loop {
        cx.i += 1;
        cx.rti = None;
        let sv = match cx.eval(s) {
            Ok(v) => v,
            Err(Found(g)) => {
                if nontrivial(&g, &n) {
                    return Some(g);
                }
                if lab::spend(1.0) {
                    return None;
                }
                continue;
            }
        };
        cx.x = sv.clone();
        let sq = r.mul(&sv, &sv);
        lab::spend(2.0);
        let res = r.signed(&sq);
        if res.is_zero() {
            let g = r.gcd(&sv);
            if nontrivial(&g, &n) {
                return Some(g);
            }
        } else if let Some(t) = lab::is_square(&res) {
            let g = (r.big(&sv) - t).gcd(&n);
            if nontrivial(&g, &n) {
                return Some(g);
            }
        } else if !fb.is_empty() {
            let mut m = res.abs();
            let mut exps = vec![0u32; cols];
            if res.is_negative() {
                exps[0] = 1;
            }
            if let Some(mut w) = m.to_u64() {
                for (k, &p) in fb.iter().enumerate() {
                    while w % p == 0 {
                        w /= p;
                        exps[k + 1] += 1;
                    }
                    if w == 1 {
                        break;
                    }
                }
                m = BigInt::from(w);
            } else {
                for (k, &p) in fb.iter().enumerate() {
                    while (&m % p).is_zero() {
                        m /= p;
                        exps[k + 1] += 1;
                    }
                }
            }
            lab::spend(fb.len() as f64 * 0.25);
            let key = r.big(&sv);
            if m.is_one() && seen.insert(key.clone()) {
                rels.push((key, exps));
                if rels.len() >= wanted {
                    if let Some(g) = combine(&n, &fb, &rels) {
                        return Some(g);
                    }
                    wanted = rels.len() + 5;
                }
            }
        }
        if lab::out_of_budget() {
            return None;
        }
    }
}

/// linear algebra mod 2 on the exponent vectors; tries each dependency
fn combine(n: &BigInt, fb: &[u64], rels: &[(BigInt, Vec<u32>)]) -> Option<BigInt> {
    let cols = fb.len() + 1;
    let r = rels.len();
    let (cw, hw) = (cols.div_ceil(64), r.div_ceil(64));
    let mut rows: Vec<(Vec<u64>, Vec<u64>)> = (0..r)
        .map(|i| {
            let mut v = vec![0u64; cw];
            for (c, &e) in rels[i].1.iter().enumerate() {
                if e & 1 == 1 {
                    v[c / 64] |= 1 << (c % 64);
                }
            }
            let mut h = vec![0u64; hw];
            h[i / 64] |= 1 << (i % 64);
            (v, h)
        })
        .collect();
    lab::spend((r * r * (cw + hw)) as f64 / 16.0);
    let mut pr = 0;
    for c in 0..cols {
        let Some(k) = (pr..r).find(|&i| rows[i].0[c / 64] >> (c % 64) & 1 == 1) else {
            continue;
        };
        rows.swap(pr, k);
        let (pv, ph) = rows[pr].clone();
        for i in 0..r {
            if i != pr && rows[i].0[c / 64] >> (c % 64) & 1 == 1 {
                for w in 0..cw {
                    rows[i].0[w] ^= pv[w];
                }
                for w in 0..hw {
                    rows[i].1[w] ^= ph[w];
                }
            }
        }
        pr += 1;
    }
    for (v, h) in rows.iter().skip(pr) {
        if v.iter().any(|&w| w != 0) {
            continue;
        }
        let mut x = BigInt::one();
        let mut tot = vec![0u32; cols];
        for i in 0..r {
            if h[i / 64] >> (i % 64) & 1 == 1 {
                x = x * &rels[i].0 % n;
                for (t, e) in tot.iter_mut().zip(&rels[i].1) {
                    *t += e;
                }
            }
        }
        let mut y = BigInt::one();
        for (k, &p) in fb.iter().enumerate() {
            if tot[k + 1] > 0 {
                y = y * BigInt::from(p).modpow(&BigInt::from(tot[k + 1] / 2), n) % n;
            }
        }
        let g = (&x - &y).gcd(n);
        if nontrivial(&g, n) {
            return Some(g);
        }
        lab::spend(cols as f64);
    }
    None
}

/// Runs a program on n within the current budget.
pub fn run_prog(p: &Prog, n: &BigInt) -> Option<BigInt> {
    if n.is_even() {
        return Some(BigInt::from(2));
    }
    if let Prog::Grp { w, e, r } = p {
        return lg::run(*w, &|i| int_eval(e, i), *r, n);
    }
    match n.to_u64().filter(|&v| v < 1 << 63) {
        Some(v) => {
            let r = R64 {
                n: v,
                nb: n.clone(),
            };
            match p {
                Prog::Iter { f, h, x0 } => run_iter(&r, f, h, x0),
                Prog::Rel { s, fb } => run_rel(&r, s, *fb),
                Prog::Grp { .. } => unreachable!(),
            }
        }
        None => {
            let r = RBig { n: n.clone() };
            match p {
                Prog::Iter { f, h, x0 } => run_iter(&r, f, h, x0),
                Prog::Rel { s, fb } => run_rel(&r, s, *fb),
                Prog::Grp { .. } => unreachable!(),
            }
        }
    }
}

// ---------- random programs ----------

const FBS: [usize; 8] = [0, 8, 16, 32, 64, 128, 256, 512];
const CONSTS: [i64; 8] = [-1, 1, 2, 3, 5, 7, 11, -2];

fn leaf(rng: &mut Rng, iter: bool) -> E {
    let w = rng.below(if iter { 12 } else { 10 });
    match w {
        0..=2 => E::X,
        3 => E::I,
        4 => E::Rt,
        5 => E::Rti,
        6 => [E::B, E::D0, E::D1][rng.below(3) as usize].clone(),
        7..=9 => E::C(CONSTS[rng.below(8) as usize]),
        _ => E::Y,
    }
}

fn rand_e(rng: &mut Rng, depth: u32, iter: bool) -> E {
    if depth == 0 || rng.below(10) < 3 {
        return leaf(rng, iter);
    }
    let b = |rng: &mut Rng| Box::new(rand_e(rng, depth - 1, iter));
    match rng.below(14) {
        0..=2 => E::Add(b(rng), b(rng)),
        3..=4 => E::Sub(b(rng), b(rng)),
        5..=7 => E::Mul(b(rng), b(rng)),
        8..=10 => E::Pow(b(rng), [2, 2, 3, 4][rng.below(4) as usize]),
        11 => E::PowI(b(rng)),
        12 => E::Luc(b(rng)),
        _ => E::Inv(b(rng)),
    }
}

pub const RESTARTS: [u64; 6] = [0, 0, 20, 50, 100, 300];

fn rand_exponent(rng: &mut Rng) -> E {
    let b = Box::new;
    match rng.below(6) {
        0 | 1 => E::I,
        2 => E::Pow(b(E::I), 2),
        3 => E::Mul(b(E::C([2, 3, 6, 12][rng.below(4) as usize])), b(E::I)),
        4 => E::Add(b(E::I), b(E::C([1, 2, -1][rng.below(3) as usize]))),
        _ => E::C([2, 3, 6, 30, 210][rng.below(5) as usize]),
    }
}

pub fn random_prog(rng: &mut Rng) -> Prog {
    if rng.below(4) == 0 {
        return Prog::Grp {
            w: lg::random_world(rng),
            e: rand_exponent(rng),
            r: RESTARTS[rng.below(6) as usize],
        };
    }
    if rng.below(3) < 2 {
        let h = match rng.below(4) {
            0 => E::Sub(Box::new(E::X), Box::new(E::Y)),
            1 => E::Sub(Box::new(E::X), Box::new(E::C(1))),
            _ => rand_e(rng, 2, true),
        };
        Prog::Iter {
            f: rand_e(rng, 3, true),
            h,
            x0: E::C([2, 3, 5][rng.below(3) as usize]),
        }
    } else {
        Prog::Rel {
            s: rand_e(rng, 3, false),
            fb: FBS[rng.below(8) as usize],
        }
    }
}

/// Folds constants and drops + 0, * 1, (a^j)^k -> a^(jk): equal programs print equally.
pub fn simplify(e: &E) -> E {
    use E::*;
    let bx = Box::new;
    match e {
        Add(a, b) => match (simplify(a), simplify(b)) {
            (C(x), C(y)) => C(x.saturating_add(y)),
            (C(0), o) | (o, C(0)) => o,
            (o, C(c)) => Add(bx(C(c)), bx(o)),
            (a, b) => Add(bx(a), bx(b)),
        },
        Sub(a, b) => match (simplify(a), simplify(b)) {
            (C(x), C(y)) => C(x.saturating_sub(y)),
            (o, C(0)) => o,
            (a, b) if a == b => C(0),
            (a, b) => Sub(bx(a), bx(b)),
        },
        Mul(a, b) => match (simplify(a), simplify(b)) {
            (C(x), C(y)) => C(x.saturating_mul(y)),
            (C(0), _) | (_, C(0)) => C(0),
            (C(1), o) | (o, C(1)) => o,
            (o, C(c)) => Mul(bx(C(c)), bx(o)),
            (a, b) => Mul(bx(a), bx(b)),
        },
        Pow(a, k) => match simplify(a) {
            C(c) if c.unsigned_abs() < 1000 && *k <= 4 => C(c.saturating_pow(*k)),
            Pow(b, j) if j * k <= 64 => Pow(b, j * k),
            o if *k == 1 => o,
            o => Pow(bx(o), *k),
        },
        PowI(a) => PowI(bx(simplify(a))),
        Luc(a) => Luc(bx(simplify(a))),
        Inv(a) => Inv(bx(simplify(a))),
        o => o.clone(),
    }
}

pub fn simplify_prog(p: &Prog) -> Prog {
    match p {
        Prog::Iter { f, h, x0 } => Prog::Iter {
            f: simplify(f),
            h: simplify(h),
            x0: simplify(x0),
        },
        Prog::Rel { s, fb } => Prog::Rel {
            s: simplify(s),
            fb: *fb,
        },
        Prog::Grp { w, e, r } => Prog::Grp {
            w: *w,
            e: simplify(e),
            r: *r,
        },
    }
}

/// What a program computes on a fixed modulus in its first steps: programs
/// with the same fingerprint behave the same, so only one is scored.
fn fingerprint(p: &Prog) -> u64 {
    if let Prog::Grp { w, e, r } = p {
        let mut h: u64 = 7;
        for b in format!("{}/{}", w, r).bytes() {
            h = (h ^ b as u64).wrapping_mul(0x100_0000_01B3);
        }
        for i in 1..=12 {
            h = (h ^ int_eval(e, i)).wrapping_mul(0x100_0000_01B3);
        }
        return h;
    }
    let n = BigInt::from(1_000_000_016_000_000_063u64); // 1000000007 * 1000000009
    let r = R64 {
        n: 1_000_000_016_000_000_063,
        nb: n,
    };
    let c = consts(&r);
    let mut cx = Ctx {
        r: &r,
        c: &c,
        x: 2,
        y: 2,
        i: 0,
        rti: None,
    };
    let mut h: u64 = match p {
        Prog::Iter { .. } => 1,
        Prog::Rel { fb, .. } => 1000 + *fb as u64,
        Prog::Grp { .. } => unreachable!(),
    };
    let saved = lab::spent();
    if let Prog::Iter { x0, .. } = p {
        cx.x = cx.eval(x0).unwrap_or(0);
        cx.y = cx.x;
    } else {
        cx.x = c.rt;
    }
    for _ in 0..12 {
        cx.i += 1;
        cx.rti = None;
        let v = match p {
            Prog::Iter { f, h, .. } => cx.eval(f).and_then(|x| {
                cx.x = x;
                cx.eval(h)
            }),
            Prog::Rel { s, .. } => cx.eval(s).inspect(|&v| cx.x = v),
            Prog::Grp { .. } => unreachable!(),
        }
        .unwrap_or(0);
        if cx.i.is_power_of_two() {
            cx.y = cx.x;
        }
        h = (h ^ v).wrapping_mul(0x100_0000_01B3);
    }
    let _ = saved;
    h
}

fn size(e: &E) -> usize {
    match e {
        E::Add(a, b) | E::Sub(a, b) | E::Mul(a, b) => 1 + size(a) + size(b),
        E::Pow(a, _) | E::PowI(a) | E::Luc(a) | E::Inv(a) => 1 + size(a),
        _ => 1,
    }
}

/// the k-th node (preorder) of e, mutable
fn node_mut(e: &mut E, k: &mut usize) -> Option<*mut E> {
    if *k == 0 {
        return Some(e as *mut E);
    }
    *k -= 1;
    match e {
        E::Add(a, b) | E::Sub(a, b) | E::Mul(a, b) => node_mut(a, k).or_else(|| node_mut(b, k)),
        E::Pow(a, _) | E::PowI(a) | E::Luc(a) | E::Inv(a) => node_mut(a, k),
        _ => None,
    }
}

fn replace_random(e: &mut E, rng: &mut Rng, with: impl FnOnce(&mut Rng, &E) -> E) {
    let mut k = rng.below(size(e) as u64) as usize;
    if let Some(ptr) = node_mut(e, &mut k) {
        // SAFETY: ptr points into e, which we hold mutably and do not touch otherwise here
        let target = unsafe { &mut *ptr };
        *target = with(rng, target);
    }
}

fn subtree(e: &E, rng: &mut Rng) -> E {
    let mut c = e.clone();
    let mut k = rng.below(size(&c) as u64) as usize;
    match node_mut(&mut c, &mut k) {
        // SAFETY: as above
        Some(p) => unsafe { (*p).clone() },
        None => c,
    }
}

fn mutate_e(e: &mut E, rng: &mut Rng, iter: bool) {
    replace_random(e, rng, |rng, old| match rng.below(4) {
        0 => rand_e(rng, 2, iter),
        1 => match old {
            E::C(c) => E::C(c + [-1, 1, 2, -2][rng.below(4) as usize]),
            E::Pow(a, k) => E::Pow(a.clone(), (*k + 1).min(8).max(2)),
            _ => leaf(rng, iter),
        },
        2 => E::Add(Box::new(old.clone()), Box::new(leaf(rng, iter))),
        _ => E::Pow(Box::new(old.clone()), 2),
    });
    if size(e) > 25 {
        *e = rand_e(rng, 2, iter);
    }
}

pub fn mutate(p: &Prog, other: &Prog, rng: &mut Rng) -> Prog {
    let mut p = p.clone();
    let donor = match other {
        Prog::Iter { f, .. } => f.clone(),
        Prog::Rel { s, .. } => s.clone(),
        Prog::Grp { e, .. } => e.clone(),
    };
    let cross = rng.below(4) == 0;
    match &mut p {
        Prog::Iter { f, h, x0 } => match rng.below(10) {
            0 => {
                p = Prog::Rel {
                    s: f.clone(),
                    fb: FBS[rng.below(8) as usize],
                }
            }
            1..=2 => mutate_e(h, rng, true),
            3 => *x0 = E::C(CONSTS[rng.below(8) as usize].abs() + 1),
            _ if cross => {
                let d = subtree(&donor, rng);
                replace_random(f, rng, |_, _| d);
            }
            _ => mutate_e(f, rng, true),
        },
        Prog::Rel { s, fb } => match rng.below(10) {
            0 => {
                p = Prog::Iter {
                    f: s.clone(),
                    h: E::Sub(Box::new(E::X), Box::new(E::Y)),
                    x0: E::C(2),
                }
            }
            1..=2 => *fb = FBS[rng.below(8) as usize],
            _ if cross => {
                let d = subtree(&donor, rng);
                replace_random(s, rng, |_, _| d);
            }
            _ => mutate_e(s, rng, false),
        },
        Prog::Grp { w, e, r } => match rng.below(10) {
            0..=1 => *w = lg::random_world(rng),
            2 => {
                *w = match *w {
                    World::Quad(d) => World::Quad(d + [-1, 1][rng.below(2) as usize]),
                    World::Cubic(c) => World::Cubic(c + [-1, 1][rng.below(2) as usize]),
                    World::Ec(a) => World::Ec(a + [-1, 1][rng.below(2) as usize]),
                    o => o,
                }
            }
            3..=4 => *r = RESTARTS[rng.below(6) as usize],
            5 => *e = rand_exponent(rng),
            _ => {
                mutate_e(e, rng, false);
                if size(e) > 7 {
                    *e = rand_exponent(rng);
                }
            }
        },
    }
    p
}

// ---------- scoring ----------

/// Training sizes (digits) and instances per size
pub const TRAIN: [u32; 4] = [12, 14, 16, 18];
pub const PER_SIZE: u64 = 3;

/// Work cap per instance: 4 times what Pollard rho typically needs, plus a
/// fixed 50000 so methods with a start-up cost (ECM-like group programs,
/// factor bases) are not ruled out on the smallest numbers
pub fn cap(digits: u32) -> f64 {
    12.0 * 10f64.powf(digits as f64 / 4.0) + 50_000.0
}

#[derive(Clone, Debug)]
pub struct Score {
    /// sum of log2(work) over the training set; a miss counts log2(cap) + 3
    pub score: f64,
    pub solved: usize,
    pub total: usize,
    /// (digits, work) of the successful runs
    pub runs: Vec<(u32, f64)>,
    /// log2(work) per case, misses as log2(cap) + 3
    pub logs: Vec<f64>,
}

impl Score {
    /// exponent a in work ~ N^a, fitted on the successful runs
    pub fn exponent(&self) -> Option<f64> {
        let pts: Vec<(f64, f64)> = self.runs.iter().map(|&(d, w)| (d as f64, w)).collect();
        if pts.iter().map(|p| p.0 as u32).collect::<HashSet<_>>().len() < 2 {
            return None;
        }
        lab::fit(0, &pts).map(|f| f.1)
    }
}

pub struct TrainSet {
    pub cases: Vec<(u32, BigInt)>,
}

/// Held-out sizes for validation: never seen during the search
pub const TEST: [u32; 4] = [14, 16, 18, 20];
pub const TEST_PER_SIZE: u64 = 6;

impl TrainSet {
    pub fn new(seed: u64) -> TrainSet {
        Self::with(seed, &TRAIN, PER_SIZE)
    }
    /// the held-out set (different numbers, more of them, one size larger)
    pub fn test() -> TrainSet {
        Self::with(1_000_001, &TEST, TEST_PER_SIZE)
    }
    pub fn with(seed: u64, sizes: &[u32], per: u64) -> TrainSet {
        let mut cases = vec![];
        for &d in sizes {
            for k in 0..per {
                cases.push((
                    d,
                    lab::semiprime(d, seed.wrapping_mul(7919).wrapping_add(k)),
                ));
            }
        }
        TrainSet {
            cases: cases.into_iter().map(|(d, (n, _, _))| (d, n)).collect(),
        }
    }
}

pub fn score(p: &Prog, t: &TrainSet) -> Score {
    let mut s = Score {
        score: 0.0,
        solved: 0,
        total: t.cases.len(),
        runs: vec![],
        logs: vec![],
    };
    let mut give_up = false;
    let mut first_misses = 0;
    for (d, n) in &t.cases {
        let c = cap(*d);
        if give_up {
            s.score += c.log2() + 3.0;
            s.logs.push(c.log2() + 3.0);
            continue;
        }
        lab::start_budget(c, f64::INFINITY);
        let f = run_prog(p, n);
        let w = lab::end_budget();
        let ok = f.is_some_and(|f| nontrivial(&f, n) && (n % &f).is_zero());
        if ok {
            s.solved += 1;
            s.score += w.max(1.0).log2();
            s.logs.push(w.max(1.0).log2());
            s.runs.push((*d, w.max(1.0)));
        } else {
            s.score += c.log2() + 3.0;
            s.logs.push(c.log2() + 3.0);
            // every number of the smallest size missed: not worth trying the bigger ones
            if *d == t.cases[0].0 {
                first_misses += 1;
                if first_misses == t.cases.iter().filter(|c| c.0 == *d).count() {
                    give_up = true;
                }
            }
        }
    }
    s
}

/// Well-known procedures written in the program language, for reference
pub const CLASSICS: &[(&str, &str)] = &[
    ("Pollard rho", "iter(x^2 + 1, x - y, 2)"),
    ("Pollard p - 1", "iter(pow(x, i), x - 1, 2)"),
    ("Williams p + 1", "iter(luc(x), x - 2, 3)"),
    ("Fermat", "rel(rt + i, 0)"),
    ("Hart one-line", "rel(rti, 0)"),
    ("Dixon near sqrt(N)", "rel(rt + i, 64)"),
    ("Dixon near sqrt(iN)", "rel(rti, 64)"),
    ("p - 1 as a group program", "grp(mul, i, 0)"),
    ("p + 1 side (a + b sqrt(-1))", "grp(quad(-1), i, 0)"),
    ("cyclotomic Phi_3 (Bach-Shallit)", "grp(cubic(2), i, 0)"),
    ("2 x 2 matrices", "grp(mat, i, 0)"),
    ("ECM, 100 steps per curve", "grp(ec(3), i, 100)"),
];

// ---------- the leaderboard ----------

#[derive(Clone)]
pub struct Entry {
    pub prog: Prog,
    pub score: Score,
    pub found_by: String,
    /// the score on the held-out set, when it was checked
    pub test: Option<Score>,
    /// held-out comparison with rho: (times less work, p-value)
    pub held: Option<(f64, f64)>,
    /// the training sizes, e.g. "12-18"
    pub sizes: String,
}

thread_local! {
    static BOARD: RefCell<Vec<Entry>> = const { RefCell::new(vec![]) };
}

pub fn board() -> Vec<Entry> {
    BOARD.with(|b| b.borrow().clone())
}

pub fn clear_board() {
    BOARD.with(|b| b.borrow_mut().clear());
}

fn post(e: Entry) {
    BOARD.with(|b| {
        let mut b = b.borrow_mut();
        let text = e.prog.to_string();
        if let Some(old) = b
            .iter_mut()
            .find(|o| o.prog.to_string() == text && o.sizes == e.sizes)
        {
            if e.score.score < old.score.score || (e.test.is_some() && old.test.is_none()) {
                *old = e;
            }
        } else {
            b.push(e);
        }
        // checked programs first, by how much less work than rho they need on new numbers
        b.sort_by(|a, c| match (a.held, c.held) {
            (Some(x), Some(y)) => y.0.partial_cmp(&x.0).unwrap(),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => a.score.score.partial_cmp(&c.score.score).unwrap(),
        });
        b.truncate(40);
    });
}

pub struct SearchResult {
    pub evaluated: usize,
    pub distinct: usize,
    pub best: Vec<Entry>,
    pub classics: Vec<(String, Entry)>,
    pub units: f64,
    pub seconds: Option<f64>,
    /// Pollard rho on the held-out set, for comparison
    pub rho_test: Score,
    pub train_sizes: Vec<u32>,
    pub test_sizes: Vec<u32>,
    pub train_count: usize,
    pub test_count: usize,
    /// whether the classics were put into the first population
    pub seeded: bool,
}

/// geometric-mean work ratio of b over a on the same set: how many times
/// less work a needs (misses count as cap * 8)
pub fn speedup(a: &Score, b: &Score) -> f64 {
    2f64.powf((b.score - a.score) / a.total.max(1) as f64)
}

/// paired comparison of a and b on the same cases: (t statistic, two-sided
/// p-value) for "a needs a different amount of work than b"
pub fn paired(a: &Score, b: &Score) -> (f64, f64) {
    let d: Vec<f64> = a.logs.iter().zip(&b.logs).map(|(x, y)| y - x).collect();
    let n = d.len() as f64;
    if n < 3.0 {
        return (0.0, 1.0);
    }
    let m = d.iter().sum::<f64>() / n;
    let v = d.iter().map(|x| (x - m).powi(2)).sum::<f64>() / (n - 1.0);
    if v <= 0.0 {
        return (0.0, 1.0);
    }
    let t = m / (v / n).sqrt();
    (t, crate::lableak::t_pvalue(t, n - 1.0))
}

/// Evolutionary search (`mode` "evolve"), random sampling ("random") or
/// brute-force enumeration of small programs ("exhaustive"), for about
/// `seconds` (in the browser: an equivalent amount of work).
pub fn search(mode: &str, seconds: f64, seed: u64) -> SearchResult {
    search_with(mode, seconds, seed, &TRAIN)
}

/// The same with chosen training sizes (digits). Every candidate first has to
/// solve three 12-digit numbers, so hopeless programs cost little even when
/// the training numbers are big; the held-out sizes are the training sizes + 2.
pub fn search_with(mode: &str, seconds: f64, seed: u64, sizes: &[u32]) -> SearchResult {
    let default = sizes == TRAIN;
    let t = if default {
        TrainSet::new(1)
    } else {
        let mut t = TrainSet::with(1, &[12], PER_SIZE);
        t.cases.extend(
            TrainSet::with(1, sizes, PER_SIZE)
                .cases
                .into_iter()
                .filter(|c| c.0 != 12),
        );
        t
    };
    let test_sizes: Vec<u32> = if default {
        TEST.to_vec()
    } else {
        sizes.iter().map(|d| d + 2).collect()
    };
    let train_sizes: Vec<u32> = t
        .cases
        .iter()
        .map(|c| c.0)
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    let sizes_label = format!("{}-{}", train_sizes[0], train_sizes.last().unwrap());
    let seeded = !default && sizes.iter().copied().max().unwrap_or(0) > 20;
    let unit_budget = seconds * 3.0e6;
    let mut units = 0.0;
    let mut rng = Rng::new(seed);
    // hashes of the program texts tried (a hash, not the text, so long runs stay small)
    let mut seen: HashSet<u64> = HashSet::new();
    let mut evaluated = 0;
    let mut scored: Vec<Entry> = vec![];
    let mut prints: HashSet<u64> = HashSet::new();
    let mut eval = |p: Prog,
                    by: &str,
                    scored: &mut Vec<Entry>,
                    seen: &mut HashSet<u64>,
                    units: &mut f64|
     -> bool {
        let p = simplify_prog(&p);
        let text = p.to_string();
        let key = {
            use std::hash::{Hash, Hasher};
            let mut h = std::collections::hash_map::DefaultHasher::new();
            text.hash(&mut h);
            h.finish()
        };
        if !seen.insert(key) {
            return false;
        }
        if !prints.insert(fingerprint(&p)) {
            *units += 50.0;
            return false;
        }
        let s = score(&p, &t);
        *units += s.runs.iter().map(|r| r.1).sum::<f64>() + (s.total - s.solved) as f64 * 1000.0;
        evaluated += 1;
        let e = Entry {
            prog: p,
            score: s,
            found_by: by.to_string(),
            test: None,
            held: None,
            sizes: sizes_label.clone(),
        };
        if e.score.solved > 0 {
            post(e.clone());
        }
        if scored.len() > 4000 {
            // keep only the best, so long runs do not fill the memory
            scored.sort_by(|a, b| a.score.score.partial_cmp(&b.score.score).unwrap());
            scored.truncate(500);
        }
        scored.push(e);
        true
    };
    let classics: Vec<(String, Entry)> = CLASSICS
        .iter()
        .map(|(name, text)| {
            let p = parse_prog(text).unwrap();
            (
                name.to_string(),
                Entry {
                    score: score(&p, &t),
                    prog: p,
                    found_by: "classic".into(),
                    test: None,
                    held: None,
                    sizes: sizes_label.clone(),
                },
            )
        })
        .collect();
    // the budget is for the search itself: scoring the classics above is set-up
    // (several seconds in an unoptimized build, which used to eat short budgets whole)
    let clock = lab::Clock::start();
    let done = |units: f64| -> bool {
        match clock.seconds() {
            Some(s) => s > seconds,
            None => units > unit_budget,
        }
    };
    match mode {
        "exhaustive" => {
            let small_iter: Vec<Vec<E>> = (0..=5).map(|k| trees(k, true)).collect();
            let small_rel: Vec<Vec<E>> = (0..=5).map(|k| trees(k, false)).collect();
            'outer: for sz in 1..=9 {
                let mut go = true;
                each_tree(sz, &small_iter, &mut |f| {
                    for h in [
                        E::Sub(Box::new(E::X), Box::new(E::Y)),
                        E::Sub(Box::new(E::X), Box::new(E::C(1))),
                    ] {
                        eval(
                            Prog::Iter {
                                f: f.clone(),
                                h,
                                x0: E::C(2),
                            },
                            "exhaustive",
                            &mut scored,
                            &mut seen,
                            &mut units,
                        );
                        if done(units) {
                            go = false;
                            return false;
                        }
                    }
                    true
                });
                if !go {
                    break 'outer;
                }
                each_tree(sz, &small_rel, &mut |s| {
                    for fb in [0, 64, 256] {
                        eval(
                            Prog::Rel { s: s.clone(), fb },
                            "exhaustive",
                            &mut scored,
                            &mut seen,
                            &mut units,
                        );
                        if done(units) {
                            go = false;
                            return false;
                        }
                    }
                    true
                });
                if !go {
                    break 'outer;
                }
                if sz <= 4 {
                    // group programs: every world, every exponent made of i and constants
                    for e in trees(sz, false).into_iter().filter(|e| int_only(e)) {
                        for w in lg::all_worlds() {
                            for r in [0, 30, 100] {
                                eval(
                                    Prog::Grp { w, e: e.clone(), r },
                                    "exhaustive",
                                    &mut scored,
                                    &mut seen,
                                    &mut units,
                                );
                                if done(units) {
                                    break 'outer;
                                }
                            }
                        }
                    }
                }
            }
        }
        "random" => {
            while !done(units) {
                eval(
                    random_prog(&mut rng),
                    "random",
                    &mut scored,
                    &mut seen,
                    &mut units,
                );
            }
        }
        _ => {
            // evolution: start from random programs (no classics, so the search
            // has to find things on its own)
            let pop_size = 40;
            let mut pop: Vec<Entry> = vec![];
            if seeded {
                // on big numbers random programs almost never solve anything, so start from the classics
                for (_, c) in &classics {
                    if eval(c.prog.clone(), "evolve", &mut scored, &mut seen, &mut units) {
                        pop.push(scored.last().unwrap().clone());
                    }
                }
            }
            while pop.len() < pop_size * 2 && !done(units) {
                if eval(
                    random_prog(&mut rng),
                    "evolve",
                    &mut scored,
                    &mut seen,
                    &mut units,
                ) {
                    pop.push(scored.last().unwrap().clone());
                }
            }
            let mut stall = 0;
            while !done(units) {
                pop.sort_by(|a, b| a.score.score.partial_cmp(&b.score.score).unwrap());
                pop.truncate(pop_size);
                let best_before = pop[0].score.score;
                let mut children = vec![];
                for _ in 0..pop_size {
                    let pick = |rng: &mut Rng| {
                        let a = rng.below(pop.len() as u64) as usize;
                        let b = rng.below(pop.len() as u64) as usize;
                        a.min(b)
                    };
                    let (a, b) = (pick(&mut rng), pick(&mut rng));
                    let child = if rng.below(10) == 0 {
                        random_prog(&mut rng)
                    } else {
                        mutate(&pop[a].prog, &pop[b].prog, &mut rng)
                    };
                    if eval(child, "evolve", &mut scored, &mut seen, &mut units) {
                        children.push(scored.last().unwrap().clone());
                    }
                    if done(units) {
                        break;
                    }
                }
                pop.extend(children);
                pop.sort_by(|a, b| a.score.score.partial_cmp(&b.score.score).unwrap());
                if pop[0].score.score < best_before - 1e-9 {
                    stall = 0;
                } else {
                    stall += 1;
                    if stall > 30 {
                        // restart most of the population, keep the best few
                        pop.truncate(5);
                        stall = 0;
                    }
                }
            }
        }
    }
    scored.sort_by(|a, b| a.score.score.partial_cmp(&b.score.score).unwrap());
    let distinct = seen.len();
    scored.truncate(10);
    let seconds = clock.seconds();
    // check the best ones on numbers the search never saw
    let test = if default {
        TrainSet::test()
    } else {
        TrainSet::with(1_000_001, &test_sizes, TEST_PER_SIZE)
    };
    let rho_test = score(&parse_prog(CLASSICS[0].1).unwrap(), &test);
    for e in scored.iter_mut().take(5) {
        if e.score.solved > 0 {
            let ts = score(&e.prog, &test);
            e.held = Some((speedup(&ts, &rho_test), paired(&ts, &rho_test).1));
            e.test = Some(ts);
            post(e.clone());
        }
    }
    let (train_count, test_count) = (t.cases.len(), test.cases.len());
    SearchResult {
        evaluated,
        distinct,
        best: scored,
        classics,
        units,
        seconds,
        rho_test,
        train_sizes,
        test_sizes,
        train_count,
        test_count,
        seeded,
    }
}

/// whether e uses only i, constants, + - * and ^k
fn int_only(e: &E) -> bool {
    match e {
        E::I | E::C(_) => true,
        E::Add(a, b) | E::Sub(a, b) | E::Mul(a, b) => int_only(a) && int_only(b),
        E::Pow(a, _) => int_only(a),
        _ => false,
    }
}

/// calls f on every expression with exactly `size` nodes, using the stored
/// lists for small sizes and building bigger ones on the fly; stops when f
/// returns false (and then returns false)
pub fn each_tree(size: usize, small: &[Vec<E>], f: &mut dyn FnMut(E) -> bool) -> bool {
    if size < small.len() {
        for e in &small[size] {
            if !f(e.clone()) {
                return false;
            }
        }
        return true;
    }
    let ok = each_tree(size - 1, small, &mut |a| {
        for u in [
            E::Pow(Box::new(a.clone()), 2),
            E::PowI(Box::new(a.clone())),
            E::Luc(Box::new(a.clone())),
            E::Inv(Box::new(a.clone())),
        ] {
            if !f(u) {
                return false;
            }
        }
        true
    });
    if !ok {
        return false;
    }
    for l in 1..size - 1 {
        let r = size - 1 - l;
        let ok = each_tree(l, small, &mut |a| {
            each_tree(r, small, &mut |b| {
                for e in [
                    E::Add(Box::new(a.clone()), Box::new(b.clone())),
                    E::Sub(Box::new(a.clone()), Box::new(b.clone())),
                    E::Mul(Box::new(a.clone()), Box::new(b.clone())),
                ] {
                    if !f(e) {
                        return false;
                    }
                }
                true
            })
        });
        if !ok {
            return false;
        }
    }
    true
}

/// every expression with exactly `size` nodes over a small alphabet
pub fn trees(size: usize, iter: bool) -> Vec<E> {
    let mut leaves = vec![E::X, E::I, E::Rt, E::Rti, E::C(1), E::C(2), E::C(-1)];
    if iter {
        leaves.push(E::Y);
    }
    let mut by: Vec<Vec<E>> = vec![vec![], leaves];
    for s in 2..=size {
        let mut v = vec![];
        for a in &by[s - 1] {
            v.push(E::Pow(Box::new(a.clone()), 2));
            v.push(E::PowI(Box::new(a.clone())));
            v.push(E::Luc(Box::new(a.clone())));
            v.push(E::Inv(Box::new(a.clone())));
        }
        for l in 1..s - 1 {
            let r = s - 1 - l;
            for a in &by[l] {
                for b in &by[r] {
                    v.push(E::Add(Box::new(a.clone()), Box::new(b.clone())));
                    v.push(E::Sub(Box::new(a.clone()), Box::new(b.clone())));
                    v.push(E::Mul(Box::new(a.clone()), Box::new(b.clone())));
                }
            }
        }
        by.push(v);
        if by.iter().map(|v| v.len()).sum::<usize>() > 2_000_000 {
            break;
        }
    }
    by.get(size).cloned().unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classics_parse_print_and_factor() {
        for (_, text) in CLASSICS {
            let p = parse_prog(text).unwrap();
            assert_eq!(&p.to_string(), text);
        }
        let (n, p, q) = lab::semiprime(16, 3);
        for text in ["iter(x^2 + 1, x - y, 2)", "rel(rti, 0)", "rel(rti, 64)"] {
            lab::start_budget(1e8, 10.0);
            let f = run_prog(&parse_prog(text).unwrap(), &n)
                .unwrap_or_else(|| panic!("{} failed", text));
            lab::end_budget();
            assert!(f == p || f == q);
        }
        // big ring
        let (n, p, q) = lab::semiprime(22, 3);
        lab::start_budget(1e8, 10.0);
        let f = run_prog(&parse_prog("iter(x^2 + 1, x - y, 2)").unwrap(), &n).unwrap();
        lab::end_budget();
        assert!(f == p || f == q);
    }

    #[test]
    fn group_programs_parse_and_run() {
        for t in [
            "grp(mul, i, 0)",
            "grp(quad(-1), i^2, 50)",
            "grp(ec(3), i, 100)",
            "grp(mat, 2*i, 0)",
            "grp(cubic(2), i, 0)",
        ] {
            assert_eq!(parse_prog(t).unwrap().to_string(), t);
        }
        let (n, p, q) = lab::semiprime(18, 2);
        lab::start_budget(1e9, 20.0);
        let f = run_prog(&parse_prog("grp(ec(3), i, 100)").unwrap(), &n).unwrap();
        lab::end_budget();
        assert!(f == p || f == q);
    }

    #[test]
    fn rho_scores_well() {
        let t = TrainSet::new(1);
        let s = score(&parse_prog("iter(x^2 + 1, x - y, 2)").unwrap(), &t);
        assert_eq!(s.solved, s.total);
        let a = s.exponent().unwrap();
        assert!(a > 0.15 && a < 0.35, "rho exponent {}", a);
        let bad = score(&parse_prog("iter(x + 1, x - 1, 2)").unwrap(), &t);
        assert_eq!(bad.solved, 0);
    }

    #[test]
    fn short_search_runs() {
        let r = search("evolve", 2.0, 1);
        assert!(r.evaluated > 10);
        let r = search("exhaustive", 2.0, 1);
        assert!(r.evaluated > 10);
    }
}
