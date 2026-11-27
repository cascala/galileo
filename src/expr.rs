//! The expression tree.
//!
//! Expressions are always kept in canonical form: the smart constructors in
//! [`crate::build`] flatten nested sums and products, fold numbers, collect
//! like terms and powers, and sort operands. Subtraction is `a + (-1)*b` and
//! division is `a * b^-1`; the printer turns those back into `-` and `/`.

use crate::num::Number;
use std::cmp::Ordering;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Const {
    Pi,
    E,
    I,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Func {
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Log,
    Abs,
    Sinh,
    Cosh,
    Tanh,
    Asinh,
    Acosh,
    Atanh,
}

impl Func {
    pub fn name(self) -> &'static str {
        match self {
            Func::Sin => "sin",
            Func::Cos => "cos",
            Func::Tan => "tan",
            Func::Asin => "asin",
            Func::Acos => "acos",
            Func::Atan => "atan",
            Func::Log => "log",
            Func::Abs => "abs",
            Func::Sinh => "sinh",
            Func::Cosh => "cosh",
            Func::Tanh => "tanh",
            Func::Asinh => "asinh",
            Func::Acosh => "acosh",
            Func::Atanh => "atanh",
        }
    }
    /// numeric value
    pub fn apply(self, v: f64) -> f64 {
        match self {
            Func::Sin => v.sin(),
            Func::Cos => v.cos(),
            Func::Tan => v.tan(),
            Func::Asin => v.asin(),
            Func::Acos => v.acos(),
            Func::Atan => v.atan(),
            Func::Log => v.ln(),
            Func::Abs => v.abs(),
            Func::Sinh => v.sinh(),
            Func::Cosh => v.cosh(),
            Func::Tanh => v.tanh(),
            Func::Asinh => v.asinh(),
            Func::Acosh => v.acosh(),
            Func::Atanh => v.atanh(),
        }
    }
    pub fn from_name(s: &str) -> Option<Func> {
        Some(match s {
            "sin" => Func::Sin,
            "cos" => Func::Cos,
            "tan" => Func::Tan,
            "asin" => Func::Asin,
            "acos" => Func::Acos,
            "atan" => Func::Atan,
            "log" => Func::Log,
            "abs" => Func::Abs,
            "sinh" => Func::Sinh,
            "cosh" => Func::Cosh,
            "tanh" => Func::Tanh,
            "asinh" => Func::Asinh,
            "acosh" => Func::Acosh,
            "atanh" => Func::Atanh,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Num(Number),
    Sym(String),
    Const(Const),
    Add(Vec<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Func(Func, Box<Expr>),
    /// an undefined function such as a(t), or a derivative of one, a'(t)
    Apply(String, Vec<Expr>),
    Matrix(Vec<Vec<Expr>>),
    Bool(bool),
    Str(String),
    /// figures, metrics and other displayable results
    Obj(Rc<Obj>),
}

/// Results that are not algebraic expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Figure(crate::plot::Figure),
    Figure3D(crate::plot::Figure3D),
    Metric(crate::geometry::Metric),
    Components(crate::geometry::Components),
    Equations(crate::geometry::Equations),
    Report(crate::discover::Report),
    Group(crate::fgroup::Group),
    /// {a, b, c}
    List(Vec<Expr>),
    /// {name: value, ...}
    Record(Vec<(String, Expr)>),
    Ring(std::rc::Rc<crate::ffield::Ring>),
    Elem(crate::ffield::Elem),
    QRing(std::rc::Rc<crate::qring::QRing>),
    QElem(crate::qring::QElem),
    Quat(crate::quat::Quat),
    Graph(crate::graph::Graph),
}

impl Obj {
    pub fn kind(&self) -> &'static str {
        match self {
            Obj::Figure(_) => "Figure",
            Obj::Figure3D(_) => "Figure3D",
            Obj::Metric(_) => "Metric",
            Obj::Components(_) => "Components",
            Obj::Equations(_) => "Equations",
            Obj::Report(r) => r.kind,
            Obj::Group(_) => "Group",
            Obj::List(_) => "List",
            Obj::Record(_) => "Record",
            Obj::Ring(_) => "Ring",
            Obj::Elem(_) => "RingElement",
            Obj::QRing(_) => "Ring",
            Obj::QElem(_) => "RingElement",
            Obj::Quat(_) => "Quaternion",
            Obj::Graph(_) => "Graph",
        }
    }
    /// Plain text rendering
    pub fn text(&self) -> String {
        match self {
            Obj::Figure(f) => f.ascii(),
            Obj::Figure3D(f) => f.text(),
            Obj::Metric(m) => m.text(),
            Obj::Components(c) => c.text(),
            Obj::Equations(e) => e.text(),
            Obj::Group(g) => g.name.clone(),
            Obj::List(v) => crate::lang::list_text(v),
            Obj::Record(v) => crate::lang::record_text(v),
            Obj::Ring(r) => r.name(),
            Obj::Elem(x) => x.text(),
            Obj::QRing(r) => r.name(),
            Obj::QElem(x) => x.text(),
            Obj::Quat(q) => q.text(),
            Obj::Graph(g) => g.name.clone(),
            Obj::Report(r) => r
                .shown_value()
                .map(|v| crate::print::show(&v))
                .unwrap_or_default(),
        }
    }
}

use Expr::*;

impl Expr {
    pub fn int(n: i128) -> Expr {
        Num(Number::int(n))
    }
    pub fn float(f: f64) -> Expr {
        Num(Number::Float(f))
    }
    pub fn sym(s: &str) -> Expr {
        Sym(s.to_string())
    }
    pub fn zero() -> Expr {
        Expr::int(0)
    }
    pub fn one() -> Expr {
        Expr::int(1)
    }

    pub fn num(&self) -> Option<Number> {
        if let Num(n) = self {
            Some(n.clone())
        } else {
            None
        }
    }
    pub fn is_zero(&self) -> bool {
        matches!(self, Num(n) if n.is_zero())
    }
    pub fn is_one(&self) -> bool {
        matches!(self, Num(n) if n.is_one())
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Num(_))
    }
    pub fn is_matrix(&self) -> bool {
        matches!(self, Matrix(_))
    }
    pub fn is_scalar(&self) -> bool {
        !matches!(self, Matrix(_) | Bool(_) | Str(_) | Obj(_))
    }

    /// Split a term into its numeric coefficient and the rest: `3*x*y` -> (3, x*y)
    pub fn coeff_and_rest(&self) -> (Number, Expr) {
        match self {
            Num(n) => (n.clone(), Expr::one()),
            Mul(f) => match &f[0] {
                Num(n) => {
                    let rest: Vec<Expr> = f[1..].to_vec();
                    let rest = if rest.len() == 1 {
                        rest.into_iter().next().unwrap()
                    } else {
                        Mul(rest)
                    };
                    (n.clone(), rest)
                }
                _ => (Number::one(), self.clone()),
            },
            _ => (Number::one(), self.clone()),
        }
    }

    /// Split a factor into base and exponent: `x^2` -> (x, 2), `y` -> (y, 1)
    pub fn base_exp(&self) -> (Expr, Expr) {
        match self {
            Pow(b, e) => ((**b).clone(), (**e).clone()),
            _ => (self.clone(), Expr::one()),
        }
    }

    /// A term that prints with a leading minus sign
    pub fn is_negative_term(&self) -> bool {
        match self {
            Num(n) => n.is_negative(),
            Mul(f) => matches!(&f[0], Num(n) if n.is_negative()),
            _ => false,
        }
    }

    pub fn contains_sym(&self, name: &str) -> bool {
        match self {
            Sym(s) => s == name,
            Add(v) | Mul(v) => v.iter().any(|e| e.contains_sym(name)),
            Pow(a, b) => a.contains_sym(name) || b.contains_sym(name),
            Func(_, a) => a.contains_sym(name),
            Apply(_, v) => v.iter().any(|e| e.contains_sym(name)),
            Matrix(rows) => rows.iter().flatten().any(|e| e.contains_sym(name)),
            _ => false,
        }
    }

    /// Free symbols, sorted and unique
    pub fn symbols(&self) -> Vec<String> {
        fn go(e: &Expr, out: &mut Vec<String>) {
            match e {
                Sym(s) => out.push(s.clone()),
                Add(v) | Mul(v) => v.iter().for_each(|x| go(x, out)),
                Pow(a, b) => {
                    go(a, out);
                    go(b, out)
                }
                Func(_, a) => go(a, out),
                Apply(_, v) => v.iter().for_each(|x| go(x, out)),
                Matrix(rows) => rows.iter().flatten().for_each(|x| go(x, out)),
                _ => {}
            }
        }
        let mut v = vec![];
        go(self, &mut v);
        v.sort();
        v.dedup();
        v
    }

    /// Number of nodes; used to pick the simplest of several equivalent forms
    pub fn size(&self) -> usize {
        match self {
            Add(v) | Mul(v) => 1 + v.iter().map(|e| e.size()).sum::<usize>(),
            Pow(a, b) => 1 + a.size() + b.size(),
            Func(_, a) => 1 + a.size(),
            Apply(_, v) => 1 + v.iter().map(|e| e.size()).sum::<usize>(),
            Matrix(rows) => rows.iter().flatten().map(|e| e.size()).sum(),
            _ => 1,
        }
    }

    /// Apply `f` to every direct child and rebuild in canonical form
    pub fn map_children(&self, f: &mut dyn FnMut(&Expr) -> Expr) -> Expr {
        use crate::build::*;
        match self {
            Add(v) => add(v.iter().map(&mut *f).collect()),
            Mul(v) => mul(v.iter().map(&mut *f).collect()),
            Pow(a, b) => pow(f(a), f(b)),
            Func(g, a) => func(*g, f(a)),
            Apply(g, v) => crate::special::apply(g, v.iter().map(&mut *f).collect()),
            Matrix(rows) => Matrix(
                rows.iter()
                    .map(|r| r.iter().map(&mut *f).collect())
                    .collect(),
            ),
            _ => self.clone(),
        }
    }
}

fn rank(e: &Expr) -> u8 {
    match e {
        Num(_) => 0,
        Const(_) => 1,
        Sym(_) => 2,
        Func(..) | Apply(..) => 3,
        Mul(_) => 4,
        Add(_) => 5,
        Pow(..) => 6, // compared by base before rank is consulted
        Matrix(_) => 7,
        Bool(_) => 8,
        Str(_) => 9,
        Obj(_) => 10,
    }
}

/// Total structural order used to sort the factors of a product.
/// Powers sort next to their base: x < x^2 < y.
pub fn cmp(a: &Expr, b: &Expr) -> Ordering {
    if let (Pow(..), _) | (_, Pow(..)) = (a, b) {
        let (ab, ae) = a.base_exp();
        let (bb, be) = b.base_exp();
        return cmp(&ab, &bb).then_with(|| cmp(&ae, &be));
    }
    let (ra, rb) = (rank(a), rank(b));
    if ra != rb {
        return ra.cmp(&rb);
    }
    match (a, b) {
        (Num(x), Num(y)) => x.cmp_value(y),
        (Const(x), Const(y)) => x.cmp(y),
        (Sym(x), Sym(y)) => x.cmp(y),
        (Func(f, x), Func(g, y)) => cmp(x, y).then(f.cmp(g)),
        (Apply(f, x), Apply(g, y)) => f.cmp(g).then_with(|| cmp_lists(x, y)),
        (Func(..), Apply(..)) => Ordering::Less,
        (Apply(..), Func(..)) => Ordering::Greater,
        (Add(x), Add(y)) | (Mul(x), Mul(y)) => cmp_lists(x, y),
        (Matrix(x), Matrix(y)) => x.len().cmp(&y.len()),
        (Bool(x), Bool(y)) => x.cmp(y),
        (Str(x), Str(y)) => x.cmp(y),
        _ => Ordering::Equal,
    }
}

fn cmp_lists(x: &[Expr], y: &[Expr]) -> Ordering {
    for (a, b) in x.iter().zip(y) {
        let c = cmp(a, b);
        if c != Ordering::Equal {
            return c;
        }
    }
    x.len().cmp(&y.len())
}

/// Order of terms in a sum: by variables, higher powers first, numbers last.
/// `x^2 + x*y + x + y^2 + y + 1`
pub fn term_cmp(a: &Expr, b: &Expr) -> Ordering {
    let ka = monomial(a);
    let kb = monomial(b);
    for (x, y) in ka.iter().zip(&kb) {
        let c = cmp(&x.0, &y.0);
        if c != Ordering::Equal {
            return c;
        }
        let c = cmp(&y.1, &x.1); // higher exponent first
        if c != Ordering::Equal {
            return c;
        }
    }
    kb.len().cmp(&ka.len()).then_with(|| cmp(a, b))
}

fn monomial(e: &Expr) -> Vec<(Expr, Expr)> {
    let (_, rest) = e.coeff_and_rest();
    match rest {
        Num(_) => vec![],
        Mul(f) => f.iter().map(|x| x.base_exp()).collect(),
        other => vec![other.base_exp()],
    }
}
