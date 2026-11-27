//! Printing expressions in Galileo's input syntax (output can be pasted back).

use crate::expr::{Const, Expr};
use crate::num::Number;
use Expr::*;

const P_ADD: u8 = 1;
const P_MUL: u8 = 2;
const P_POW: u8 = 3;
const P_ATOM: u8 = 4;

pub fn show(e: &Expr) -> String {
    match e {
        Matrix(rows) => match crate::units::matrix_units(rows) {
            Some((m, u)) => format!("{} {}", show_matrix(&m), crate::units::text(&u)),
            None => show_matrix(rows),
        },
        Str(s) => s.clone(),
        Obj(o) => o.text(),
        _ => fmt(e).0,
    }
}

/// A result as shown to the user: like [`show`], but a string keeps its
/// quotes so the output can be read back as Galileo
pub fn show_value(e: &Expr) -> String {
    match e {
        Str(_) => fmt(e).0,
        _ => show(e),
    }
}

/// A matrix as Galileo input: [1, 2, 3] on one line, and one row per line
/// with aligned columns when there are several rows:
/// [2, 3;
///  3, 2;
///  5, 1]
fn show_matrix(rows: &[Vec<Expr>]) -> String {
    // inside [ ] a space separates entries, so quantities go in parentheses
    let cell = |x: &Expr| {
        if crate::units::has_units(x) { format!("({})", fmt(x).0) } else { fmt(x).0 }
    };
    let cells: Vec<Vec<String>> = rows
        .iter()
        .map(|r| r.iter().map(cell).collect())
        .collect();
    if cells.len() <= 1 {
        return format!(
            "[{}]",
            cells.first().map(|r| r.join(", ")).unwrap_or_default()
        );
    }
    let ncol = cells.iter().map(|r| r.len()).max().unwrap_or(0);
    let width: Vec<usize> = (0..ncol)
        .map(|j| {
            cells
                .iter()
                .filter_map(|r| r.get(j))
                .map(|c| c.chars().count())
                .max()
                .unwrap_or(0)
        })
        .collect();
    let lines: Vec<String> = rows
        .iter()
        .zip(&cells)
        .map(|(r, c)| {
            let n = c.len();
            c.iter()
                .enumerate()
                .map(|(j, t)| {
                    let pad = " ".repeat(width[j] - t.chars().count());
                    // numbers line up on the right, text on the left; no
                    // trailing spaces after the last column
                    if matches!(r[j], Str(_)) {
                        if j + 1 < n {
                            format!("{}{}", t, pad)
                        } else {
                            t.clone()
                        }
                    } else {
                        format!("{}{}", pad, t)
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        })
        .collect();
    format!("[{}]", lines.join(";\n "))
}

fn wrap(s: (String, u8), min: u8) -> String {
    if s.1 < min { format!("({})", s.0) } else { s.0 }
}

fn num_prec(n: &Number) -> u8 {
    if n.is_negative() {
        P_ADD
    } else if !n.is_integer() && n.is_exact() {
        P_MUL
    } else {
        P_ATOM
    }
}

/// (text, precedence of the outermost operator)
fn fmt(e: &Expr) -> (String, u8) {
    if let Some((c, u)) = crate::units::printable(e) {
        return fmt_quantity(&c, &crate::units::text(&u));
    }
    // a sum with the same units in every term: (2*t+3) m
    if let Add(_) = e {
        if crate::units::has_units(e) {
            if let Some((c, u)) = crate::units::split(e) {
                if !u.is_empty() && !crate::units::has_units(&c) {
                    return fmt_quantity(&c, &crate::units::text(&u));
                }
            }
        }
    }
    match e {
        Num(n) => (n.to_string(), num_prec(n)),
        Sym(s) => (s.clone(), P_ATOM),
        Const(c) => (
            match c {
                Const::Pi => "pi",
                Const::E => "e",
                Const::I => "i",
            }
            .into(),
            P_ATOM,
        ),
        Bool(b) => (b.to_string(), P_ATOM),
        Str(s) => (format!("\"{}\"", s), P_ATOM),
        Obj(o)
            if matches!(
                &**o,
                crate::expr::Obj::List(_)
                    | crate::expr::Obj::Record(_)
                    | crate::expr::Obj::Group(_)
                    | crate::expr::Obj::Ring(_)
                    | crate::expr::Obj::Elem(_) | crate::expr::Obj::QRing(_) | crate::expr::Obj::QElem(_) | crate::expr::Obj::Quat(_)
                    | crate::expr::Obj::Graph(_)
            ) =>
        {
            (o.text(), P_ATOM)
        }
        Obj(o) => (format!("<{}>", o.kind()), P_ATOM),
        Matrix(rows) if crate::units::matrix_units(rows).is_some() => {
            let (m, u) = crate::units::matrix_units(rows).unwrap();
            (format!("{} {}", fmt(&Matrix(m)).0, crate::units::text(&u)), P_MUL)
        }
        Matrix(rows) => (
            format!(
                "[{}]",
                rows.iter()
                    .map(|r| r.iter().map(|x| if crate::units::has_units(x) { format!("({})", fmt(x).0) } else { fmt(x).0 }).collect::<Vec<_>>().join(", "))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
            P_ATOM,
        ),
        Func(f, a) => (format!("{}({})", f.name(), fmt(a).0), P_ATOM),
        Apply(f, v) if f == "==" && v.len() == 2 => {
            (format!("{} == {}", fmt(&v[0]).0, fmt(&v[1]).0), 0)
        }
        Apply(f, v) if f.ends_with('\'') && v.len() == 1 => {
            // y'(t) is written d(y, t), y''(t) d(y, t, 2)
            let base = f.trim_end_matches('\'');
            let k = f.len() - base.len();
            let order = if k > 1 { format!(", {}", k) } else { String::new() };
            (format!("d({}, {}{})", base, fmt(&v[0]).0, order), P_ATOM)
        }
        Apply(f, v) if partial(f, v).is_some() => (partial(f, v).unwrap(), P_ATOM),
        Apply(f, v) => (
            format!(
                "{}({})",
                f,
                v.iter().map(|x| fmt(x).0).collect::<Vec<_>>().join(", ")
            ),
            P_ATOM,
        ),
        Add(terms) => {
            // lead with a positive term when there is one: b-a rather than -a+b
            let mut terms = terms.clone();
            // complex numbers read real part first: 3-2*i, not -2*i+3
            let gaussian = crate::build::gauss(e).is_some();
            if gaussian {
                terms.rotate_right(1);
            } else {
                // polynomials in one variable keep descending powers: -x^3/6 + x
                let poly = terms.len() >= 3 && crate::extra_cmd::univariate_poly(e);
                if let Some(k) = terms
                    .iter()
                    .position(|t| !t.is_negative_term())
                    .filter(|_| !poly)
                {
                    let t = terms.remove(k);
                    terms.insert(0, t);
                }
                // a + b*i: the imaginary part last
                let (re, im): (Vec<Expr>, Vec<Expr>) =
                    terms.into_iter().partition(|t| !crate::extra_cmd::has_i(t));
                terms = re.into_iter().chain(im).collect();
            }
            let mut s = String::new();
            for (i, t) in terms.iter().enumerate() {
                if t.is_negative_term() {
                    s.push('-');
                    s.push_str(&wrap(fmt(&crate::build::neg(t.clone())), P_MUL));
                } else {
                    if i > 0 {
                        s.push('+');
                    }
                    s.push_str(&wrap(fmt(t), P_MUL));
                }
            }
            (s, P_ADD)
        }
        Mul(_) => fmt_mul(e),
        Pow(b, x) => {
            if let Num(n) = &**x {
                if n.is_negative() {
                    return fmt_mul(e);
                }
                if *n == Number::rat(1, 2).unwrap() {
                    return (format!("sqrt({})", fmt(b).0), P_ATOM);
                }
            }
            if **b == Const(Const::E) {
                return (format!("exp({})", fmt(x).0), P_ATOM);
            }
            (
                format!("{}^{}", wrap(fmt(b), P_ATOM), wrap(fmt(x), P_ATOM)),
                P_POW,
            )
        }
    }
}

/// A quantity: 3 m/s, -2.5 kg, (3/2) m, (2*t) m/s^2 (read back the same way)
fn fmt_quantity(c: &Expr, units: &str) -> (String, u8) {
    let (cs, cp) = fmt(c);
    match c {
        Num(n) if n.is_negative() && (n.is_integer() || !n.is_exact()) => {
            (format!("{} {}", cs, units), P_ADD)
        }
        Num(n) if n.is_integer() || !n.is_exact() => (format!("{} {}", cs, units), P_MUL),
        _ if cp >= P_ATOM && cs.ends_with(')') => (format!("{} {}", cs, units), P_MUL),
        _ => (format!("({}) {}", cs, units), P_MUL),
    }
}

/// Products, written as `sign coefficient*numerator/denominator`
fn fmt_mul(e: &Expr) -> (String, u8) {
    let factors: Vec<Expr> = match e {
        Mul(f) => f.clone(),
        other => vec![other.clone()],
    };
    let mut coeff = Number::one();
    let mut numer: Vec<Expr> = vec![];
    let mut denom: Vec<Expr> = vec![];
    for f in factors {
        match &f {
            Num(n) => coeff = coeff.mul(n),
            Pow(b, x) if x.num().is_some_and(|n| n.is_negative()) => denom.push(crate::build::pow(
                (**b).clone(),
                Num(x.num().unwrap().neg()),
            )),
            _ => numer.push(f),
        }
    }
    let negative = coeff.is_negative();
    let coeff = coeff.abs();
    let mut ns: Vec<String> = vec![];
    let mut ds: Vec<String> = vec![];
    match coeff {
        Number::Rat(n, d) => {
            if n != 1 || numer.is_empty() {
                ns.push(n.to_string());
            }
            if d != 1 {
                ds.push(d.to_string());
            }
        }
        Number::Big(ref r) => {
            if r.is_integer() {
                ns.push(r.numer().to_string());
            } else {
                ns.push(r.numer().to_string());
                ds.push(r.denom().to_string());
            }
        }
        Number::Float(_) | Number::Dec(..) | Number::Xd(..) => {
            if !coeff.is_one() || numer.is_empty() {
                ns.push(coeff.to_string())
            }
        }
    }
    ns.extend(numer.iter().map(|f| wrap(fmt(f), P_POW)));
    ds.extend(denom.iter().map(|f| wrap(fmt(f), P_POW)));
    let mut s = ns.join("*");
    if !ds.is_empty() {
        let d = if ds.len() > 1 {
            format!("({})", ds.join("*"))
        } else {
            ds.pop().unwrap()
        };
        s = format!("{}/{}", s, d);
    }
    if negative {
        s = format!("-{}", s);
        (s, P_ADD)
    } else {
        (s, P_MUL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build::*;
    fn x() -> Expr {
        Expr::sym("x")
    }
    #[test]
    fn prints_nicely() {
        let e = sub(
            mul(vec![Expr::int(3), pow(x(), Expr::int(2))]),
            mul(vec![Expr::int(2), x()]),
        );
        assert_eq!(show(&e), "3*x^2-2*x");
        assert_eq!(show(&div(x(), Expr::int(3))), "x/3");
        assert_eq!(
            show(&div(Expr::one(), mul(vec![x(), Expr::sym("y")]))),
            "1/(x*y)"
        );
        assert_eq!(show(&neg(add(vec![x(), Expr::one()]))), "-(x+1)");
        assert_eq!(
            show(&pow(add(vec![x(), Expr::one()]), Expr::int(2))),
            "(x+1)^2"
        );
        assert_eq!(show(&sqrt(x())), "sqrt(x)");
        assert_eq!(show(&add(vec![x(), Expr::int(-9)])), "x-9");
    }
}

/// f,12(x, y) (a partial derivative by position) written d(d(f(x, y), x), y),
/// when the arguments are distinct names
fn partial(f: &str, v: &[Expr]) -> Option<String> {
    let (base, idx) = f.rsplit_once(',')?;
    if base.is_empty() || idx.is_empty() || !idx.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let names: Vec<&str> = v.iter().map(|a| if let Sym(s) = a { Some(s.as_str()) } else { None }).collect::<Option<_>>()?;
    if (1..names.len()).any(|j| names[..j].contains(&names[j])) {
        return None;
    }
    let mut out = format!("{}({})", base, names.join(", "));
    for c in idx.chars() {
        let j = c.to_digit(10)? as usize;
        let x = names.get(j.checked_sub(1)?)?;
        out = format!("d({}, {})", out, x);
    }
    Some(out)
}
