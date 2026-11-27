//! Quaternions a + b i + c j + d k as values, written quat(a, b, c, d); the
//! components may be symbolic. Unit quaternions rotate 3-space (q v q^-1),
//! which is the double cover SU(2) -> SO(3): q and -q give the same rotation.

use crate::build::*;
use crate::calc::simplify;
use crate::expr::{Expr, Func, Obj};
use crate::parser::Op;
use std::rc::Rc;

pub type R<T> = Result<T, String>;

#[derive(Clone, Debug, PartialEq)]
pub struct Quat(pub [Expr; 4]);

impl Quat {
    pub fn text(&self) -> String {
        format!(
            "quat({})",
            self.0
                .iter()
                .map(crate::print::show)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
    fn tidy(v: [Expr; 4]) -> Quat {
        Quat(v.map(|e| simplify(&e)))
    }
    pub fn scalar(s: Expr) -> Quat {
        Quat([s, Expr::zero(), Expr::zero(), Expr::zero()])
    }
    pub fn add(&self, o: &Quat) -> Quat {
        let [a, b, c, d] = self.0.clone();
        let [e, f, g, h] = o.0.clone();
        Quat::tidy([add2(a, e), add2(b, f), add2(c, g), add2(d, h)])
    }
    pub fn neg(&self) -> Quat {
        Quat::tidy(self.0.clone().map(neg))
    }
    /// Hamilton product
    pub fn mul(&self, o: &Quat) -> Quat {
        let [a1, b1, c1, d1] = self.0.clone();
        let [a2, b2, c2, d2] = o.0.clone();
        let m = |x: &Expr, y: &Expr| mul2(x.clone(), y.clone());
        Quat::tidy([
            add(vec![
                m(&a1, &a2),
                neg(m(&b1, &b2)),
                neg(m(&c1, &c2)),
                neg(m(&d1, &d2)),
            ]),
            add(vec![
                m(&a1, &b2),
                m(&b1, &a2),
                m(&c1, &d2),
                neg(m(&d1, &c2)),
            ]),
            add(vec![
                m(&a1, &c2),
                neg(m(&b1, &d2)),
                m(&c1, &a2),
                m(&d1, &b2),
            ]),
            add(vec![
                m(&a1, &d2),
                m(&b1, &c2),
                neg(m(&c1, &b2)),
                m(&d1, &a2),
            ]),
        ])
    }
    pub fn conj(&self) -> Quat {
        let [a, b, c, d] = self.0.clone();
        Quat::tidy([a, neg(b), neg(c), neg(d)])
    }
    /// a^2 + b^2 + c^2 + d^2
    pub fn norm2(&self) -> Expr {
        simplify(&add(self
            .0
            .iter()
            .map(|x| pow(x.clone(), Expr::int(2)))
            .collect()))
    }
    pub fn norm(&self) -> Expr {
        simplify(&sqrt(self.norm2()))
    }
    pub fn inv(&self) -> R<Quat> {
        let n = self.norm2();
        if n.is_zero() {
            return Err("quat(0, 0, 0, 0) has no inverse".into());
        }
        Ok(Quat::tidy(self.conj().0.map(|x| div(x, n.clone()))))
    }
    pub fn pow(&self, e: i128) -> R<Quat> {
        let base = if e < 0 { self.inv()? } else { self.clone() };
        let mut acc = Quat::scalar(Expr::one());
        for _ in 0..e.unsigned_abs() {
            acc = acc.mul(&base);
        }
        Ok(acc)
    }
    pub fn is_zero(&self) -> bool {
        self.0.iter().all(|x| x.is_zero())
    }

    /// q v q^-1 for v = [x, y, z]
    pub fn rotate(&self, v: &[Expr]) -> R<Vec<Expr>> {
        let p = Quat([Expr::zero(), v[0].clone(), v[1].clone(), v[2].clone()]);
        let r = self.mul(&p).mul(&self.inv()?);
        Ok(r.0[1..].to_vec())
    }
    /// the 3x3 rotation matrix of q (for a unit q; scaled by |q|^2 otherwise divided out)
    pub fn rotation_matrix(&self) -> R<Vec<Vec<Expr>>> {
        let cols: Vec<Vec<Expr>> = (0..3)
            .map(|k| {
                let mut e = vec![Expr::zero(); 3];
                e[k] = Expr::one();
                self.rotate(&e)
            })
            .collect::<R<_>>()?;
        Ok((0..3)
            .map(|i| (0..3).map(|j| cols[j][i].clone()).collect())
            .collect())
    }
    /// the matrix [[a + b i, c + d i], [-c + d i, a - b i]] in SU(2) for a unit q
    pub fn su2(&self) -> Vec<Vec<Expr>> {
        let [a, b, c, d] = self.0.clone();
        let i = Expr::Const(crate::expr::Const::I);
        let s = |e: Expr| simplify(&e);
        vec![
            vec![
                s(add2(a.clone(), mul2(b.clone(), i.clone()))),
                s(add2(c.clone(), mul2(d.clone(), i.clone()))),
            ],
            vec![s(add2(neg(c), mul2(d, i.clone()))), s(sub(a, mul2(b, i)))],
        ]
    }
}

/// the unit quaternion of a rotation by angle t about the axis [x, y, z]
pub fn from_axis(t: &Expr, axis: &[Expr]) -> R<Quat> {
    let len = simplify(&sqrt(add(axis
        .iter()
        .map(|x| pow(x.clone(), Expr::int(2)))
        .collect())));
    if len.is_zero() {
        return Err("quat: the rotation axis must not be zero".into());
    }
    let half = div(t.clone(), Expr::int(2));
    let s = func(Func::Sin, half.clone());
    Ok(Quat::tidy([
        func(Func::Cos, half),
        div(mul2(s.clone(), axis[0].clone()), len.clone()),
        div(mul2(s.clone(), axis[1].clone()), len.clone()),
        div(mul2(s, axis[2].clone()), len),
    ]))
}

pub fn as_quat(e: &Expr) -> Option<&Quat> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Quat(q) => Some(q),
            _ => None,
        },
        _ => None,
    }
}

pub fn value(q: Quat) -> Expr {
    Expr::Obj(Rc::new(Obj::Quat(q)))
}

fn lift(e: &Expr) -> R<Quat> {
    match as_quat(e) {
        Some(q) => Ok(q.clone()),
        None if e.is_scalar() => Ok(Quat::scalar(e.clone())),
        None => Err(format!(
            "can't combine a quaternion with {}",
            crate::print::show(e)
        )),
    }
}

pub fn arith(op: Op, a: &Expr, b: &Expr) -> R<Expr> {
    if op == Op::Pow {
        let q = lift(a)?;
        let e = b
            .num()
            .filter(|n| n.is_integer())
            .and_then(|n| n.as_int())
            .filter(|k| k.abs() <= 1000)
            .ok_or("a quaternion power must be a whole number up to 1000")?;
        return Ok(value(q.pow(e)?));
    }
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
    let (x, y) = (lift(a)?, lift(b)?);
    Ok(x.add(&y.neg()).is_zero())
}
