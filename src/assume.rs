//! Assumptions about symbols: assume(a > 0), assume(n, integer).
//! They let limits, integrals and simplification decide signs and
//! integer multiples of pi: exp(-a t) -> 0 at infinity when a > 0,
//! sqrt(a^2) = a, sin(n pi) = 0 and cos(n pi) = (-1)^n for an integer n.

use crate::expr::{Const, Expr, Func};
use std::cell::RefCell;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Facts {
    /// Some(1) positive, Some(-1) negative
    pub sign: Option<i32>,
    pub integer: bool,
}

thread_local! {
    static FACTS: RefCell<BTreeMap<String, Facts>> = const { RefCell::new(BTreeMap::new()) };
}

pub fn clear() {
    FACTS.with(|f| f.borrow_mut().clear());
}

pub fn forget(name: &str) {
    FACTS.with(|f| {
        f.borrow_mut().remove(name);
    });
}

pub fn set_sign(name: &str, s: i32) {
    FACTS.with(|f| f.borrow_mut().entry(name.to_string()).or_default().sign = Some(s));
}

pub fn set_integer(name: &str) {
    FACTS.with(|f| f.borrow_mut().entry(name.to_string()).or_default().integer = true);
}

pub fn facts(name: &str) -> Facts {
    FACTS.with(|f| f.borrow().get(name).copied().unwrap_or_default())
}

pub fn any() -> bool {
    FACTS.with(|f| !f.borrow().is_empty())
}

/// all assumptions, readable: ["a > 0", "n integer"]
pub fn list() -> Vec<String> {
    FACTS.with(|f| {
        f.borrow()
            .iter()
            .flat_map(|(n, x)| {
                let mut v = vec![];
                match x.sign {
                    Some(1) => v.push(format!("{} > 0", n)),
                    Some(_) => v.push(format!("{} < 0", n)),
                    None => {}
                }
                if x.integer {
                    v.push(format!("{} integer", n));
                }
                v
            })
            .collect()
    })
}

/// The sign of e when the assumptions decide it: 1, -1, or None
pub fn sign(e: &Expr) -> Option<i32> {
    match e {
        Expr::Num(n) => {
            if n.is_zero() {
                Some(0)
            } else if crate::build::gauss(e).is_some_and(|g| !g.1.is_zero()) {
                None
            } else if n.is_negative() {
                Some(-1)
            } else {
                Some(1)
            }
        }
        Expr::Sym(s) if s.starts_with('`') => Some(1),
        Expr::Sym(s) => facts(s).sign,
        Expr::Const(Const::Pi) | Expr::Const(Const::E) => Some(1),
        Expr::Mul(v) => {
            let mut s = 1;
            for f in v {
                s *= sign(f)?;
            }
            Some(s)
        }
        Expr::Add(v) => {
            let signs: Option<Vec<i32>> = v.iter().map(sign).collect();
            let signs = signs?;
            if signs.iter().all(|&s| s >= 0) && signs.iter().any(|&s| s > 0) {
                Some(1)
            } else if signs.iter().all(|&s| s <= 0) && signs.iter().any(|&s| s < 0) {
                Some(-1)
            } else {
                None
            }
        }
        Expr::Pow(b, x) => {
            if **b == Expr::Const(Const::E) {
                return Some(1);
            }
            let sb = sign(b)?;
            if sb > 0 {
                return Some(1);
            }
            let k = x.num()?.as_int()?;
            if sb == 0 {
                return if k > 0 { Some(0) } else { None };
            }
            Some(if k % 2 == 0 { 1 } else { -1 })
        }
        Expr::Func(Func::Abs, x) => {
            if sign(x).is_some_and(|s| s != 0) {
                Some(1)
            } else {
                None
            }
        }
        Expr::Func(Func::Cosh, _) => Some(1),
        _ => None,
    }
}

/// e is an integer under the assumptions (integer numbers, integer symbols,
/// and sums and products of them)
pub fn is_integer(e: &Expr) -> bool {
    match e {
        Expr::Num(n) => n.is_integer(),
        Expr::Sym(s) => facts(s).integer,
        Expr::Add(v) | Expr::Mul(v) => v.iter().all(is_integer),
        Expr::Pow(b, x) => is_integer(b) && x.num().is_some_and(|k| k.is_integer() && !k.is_negative()),
        _ => false,
    }
}

/// x = k*pi with k an integer expression containing an integer symbol: Some(k)
pub fn integer_pi_multiple(x: &Expr) -> Option<Expr> {
    if !any() {
        return None;
    }
    let Expr::Mul(v) = x else { return None };
    if !v.contains(&Expr::Const(Const::Pi)) {
        return None;
    }
    let rest: Vec<Expr> = v.iter().filter(|f| **f != Expr::Const(Const::Pi)).cloned().collect();
    let k = crate::build::mul(rest);
    if k.symbols().is_empty() || !is_integer(&k) {
        return None;
    }
    Some(k)
}

/// (-1)^k for an integer expression k: 1 when k is even, (-1)^m when k = (odd)*m
pub fn minus_one_power(k: &Expr) -> Expr {
    use crate::build::*;
    // split off an integer coefficient
    let (c, rest) = match k {
        Expr::Mul(v) if v.first().is_some_and(|f| f.num().is_some_and(|n| n.is_integer())) => {
            (v[0].num().unwrap().as_int().unwrap_or(1), mul(v[1..].to_vec()))
        }
        _ => (1, k.clone()),
    };
    if c % 2 == 0 {
        Expr::one()
    } else {
        pow(Expr::int(-1), rest)
    }
}

/// sample positive values for the assumed-positive symbols in e (and negative
/// ones for the negative symbols); None when e has other free symbols
pub fn samples(e: &Expr, avoid: &str) -> Option<Vec<Vec<(String, Expr)>>> {
    let syms: Vec<String> = e.symbols().into_iter().filter(|s| s != avoid).collect();
    if syms.is_empty() {
        return None;
    }
    let mut signs = vec![];
    for s in &syms {
        signs.push(facts(s).sign?);
    }
    let vals = [
        [Expr::Num(crate::num::Number::rat(7, 10).unwrap()), Expr::int(2), Expr::Num(crate::num::Number::rat(37, 10).unwrap())],
        [Expr::Num(crate::num::Number::rat(13, 10).unwrap()), Expr::Num(crate::num::Number::rat(1, 3).unwrap()), Expr::int(5)],
    ];
    Some(
        (0..3)
            .map(|k| {
                syms.iter()
                    .zip(&signs)
                    .enumerate()
                    .map(|(i, (s, &sg))| {
                        let v = vals[i % 2][k].clone();
                        (s.clone(), if sg < 0 { crate::build::neg(v) } else { v })
                    })
                    .collect()
            })
            .collect(),
    )
}
