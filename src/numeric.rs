//! Fast numeric evaluation of expressions, for sampling plots and integrating ODEs.
//!
//! An expression is compiled once into a small tree over variable slots and then
//! evaluated many times. Anything that has no real value (an unbound symbol, `i`,
//! a matrix) evaluates to NaN.

use crate::expr::{Const, Expr, Func};

#[derive(Clone, Debug)]
pub enum Code {
    K(f64),
    Var(usize),
    Add(Vec<Code>),
    Mul(Vec<Code>),
    PowI(Box<Code>, i32),
    Pow(Box<Code>, Box<Code>),
    F(Func, Box<Code>),
    Atan2(Box<Code>, Box<Code>),
    /// a special function (gamma, erf, besselj, ...)
    Sp(&'static str, Vec<Code>),
}

/// Compile `e` as a function of the named variables (in that order)
pub fn compile(e: &Expr, names: &[&str]) -> Code {
    use Code::*;
    match e {
        Expr::Num(n) => K(n.to_f64()),
        Expr::Const(Const::Pi) => K(std::f64::consts::PI),
        Expr::Const(Const::E) => K(std::f64::consts::E),
        Expr::Sym(s) => match names.iter().position(|n| n == s) {
            Some(i) => Var(i),
            None => K(f64::NAN),
        },
        Expr::Add(v) => fold(Add(v.iter().map(|x| compile(x, names)).collect())),
        Expr::Mul(v) => fold(Mul(v.iter().map(|x| compile(x, names)).collect())),
        Expr::Pow(b, p) => {
            let cb = compile(b, names);
            match p.num() {
                Some(n) if n.is_integer() && n.to_f64().abs() < 1e6 => {
                    fold(PowI(Box::new(cb), n.to_f64() as i32))
                }
                _ => fold(Pow(Box::new(cb), Box::new(compile(p, names)))),
            }
        }
        Expr::Func(f, a) => fold(F(*f, Box::new(compile(a, names)))),
        Expr::Apply(f, v) if f == "atan2" && v.len() == 2 => fold(Atan2(
            Box::new(compile(&v[0], names)),
            Box::new(compile(&v[1], names)),
        )),
        Expr::Apply(f, v) if crate::special::is_special(f, v.len()) => {
            let name = crate::special::NAMES.iter().find(|p| p.0 == f).unwrap().0;
            fold(Sp(name, v.iter().map(|x| compile(x, names)).collect()))
        }
        _ => K(f64::NAN),
    }
}

/// constant folding
fn fold(c: Code) -> Code {
    let constant = match &c {
        Code::Add(v) | Code::Mul(v) | Code::Sp(_, v) => v.iter().all(|x| matches!(x, Code::K(_))),
        Code::PowI(b, _) | Code::F(_, b) => matches!(**b, Code::K(_)),
        Code::Pow(a, b) | Code::Atan2(a, b) => {
            matches!(**a, Code::K(_)) && matches!(**b, Code::K(_))
        }
        _ => false,
    };
    if constant { Code::K(run(&c, &[])) } else { c }
}

pub fn run(c: &Code, x: &[f64]) -> f64 {
    match c {
        Code::K(v) => *v,
        Code::Var(i) => x[*i],
        Code::Add(v) => v.iter().map(|t| run(t, x)).sum(),
        Code::Mul(v) => v.iter().map(|t| run(t, x)).product(),
        Code::PowI(b, n) => run(b, x).powi(*n),
        Code::Pow(b, p) => run(b, x).powf(run(p, x)),
        Code::F(f, a) => f.apply(run(a, x)),
        Code::Atan2(y, xx) => run(y, x).atan2(run(xx, x)),
        Code::Sp(f, v) => {
            let a: Vec<f64> = v.iter().map(|t| run(t, x)).collect();
            crate::special::value(f, &a)
        }
    }
}

/// Value of an expression without free variables (NaN if it has none)
pub fn value(e: &Expr) -> f64 {
    run(&compile(e, &[]), &[])
}

/// Free symbols of `e` other than `bound`, in order of appearance
pub fn free(e: &Expr, bound: &[&str]) -> Vec<String> {
    e.symbols()
        .into_iter()
        .filter(|s| !bound.contains(&s.as_str()))
        .collect()
}
