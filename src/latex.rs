//! LaTeX rendering of expressions, for typeset output in notebooks.

use crate::build::{neg, pow};
use crate::expr::{Const, Expr, Func};
use crate::num::Number;
use Expr::*;

const GREEK: [&str; 34] = [
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
    "lambda", "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi",
    "omega", "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi", "Psi",
    "Omega",
];

fn base_name(b: &str) -> String {
    if GREEK.contains(&b) {
        format!("\\{}", b)
    } else if b.chars().count() > 1 {
        format!("\\mathrm{{{}}}", b)
    } else {
        b.to_string()
    }
}

/// A variable name: greek letters, subscripts (r_s), and primes as dots (x' and x'')
pub fn name(n: &str) -> String {
    let primes = n.len() - n.trim_end_matches('\'').len();
    let n = n.trim_end_matches('\'');
    let (base, sub) = match n.find('_') {
        Some(i) => (&n[..i], &n[i + 1..]),
        None => (n, ""),
    };
    let mut s = base_name(base);
    if !sub.is_empty() {
        let sb = if GREEK.contains(&sub) {
            format!("\\{}", sub)
        } else {
            sub.to_string()
        };
        s = format!("{}_{{{}}}", s, sb);
    }
    match primes {
        0 => s,
        1 => format!("\\dot{{{}}}", s),
        2 => format!("\\ddot{{{}}}", s),
        k => format!("{}{}", s, "'".repeat(k)),
    }
}

/// a'(t) with primes, f_{,1}(r, theta) for partial derivatives
fn apply_name(f: &str) -> String {
    match f.split_once(',') {
        Some((b, idx)) => format!("{}_{{,{}}}", name(b), idx),
        None => {
            let primes = f.len() - f.trim_end_matches('\'').len();
            format!("{}{}", name(f.trim_end_matches('\'')), "'".repeat(primes))
        }
    }
}

pub fn number(n: Number) -> String {
    match n {
        Number::Dec(..) => n.to_string(),
        Number::Xd(..) | Number::XdBig(_) => n.to_string(),
        Number::Big(ref r) if r.is_integer() => r.numer().to_string(),
        Number::Big(ref r) => {
            let a = r.numer();
            if a.sign() == num_bigint::Sign::Minus {
                format!("-\\frac{{{}}}{{{}}}", -a, r.denom())
            } else {
                format!("\\frac{{{}}}{{{}}}", a, r.denom())
            }
        }
        Number::Rat(a, 1) => a.to_string(),
        Number::Rat(a, b) => {
            if a < 0 {
                format!("-\\frac{{{}}}{{{}}}", -a, b)
            } else {
                format!("\\frac{{{}}}{{{}}}", a, b)
            }
        }
        Number::Float(f) => {
            if f == f.round() && f.abs() < 1e15 {
                format!("{}", f as i64)
            } else {
                let s = format!("{:.6e}", f);
                let (m, x) = s.split_once('e').unwrap();
                let m = m.trim_end_matches('0').trim_end_matches('.');
                let x: i32 = x.parse().unwrap_or(0);
                if (-4..6).contains(&x) {
                    let v = format!("{:.6}", f);
                    let mut v = v.trim_end_matches('0').to_string();
                    if v.ends_with('.') {
                        v.pop();
                    }
                    // keep about 6 significant digits
                    v
                } else {
                    format!("{}\\times 10^{{{}}}", m, x)
                }
            }
        }
    }
}

const P_ADD: u8 = 1;
const P_MUL: u8 = 2;
const P_POW: u8 = 3;
const P_ATOM: u8 = 4;

fn wrap(s: (String, u8), min: u8) -> String {
    if s.1 < min {
        format!("\\left({}\\right)", s.0)
    } else {
        s.0
    }
}

pub fn latex(e: &Expr) -> String {
    match e {
        Matrix(rows) => format!(
            "\\begin{{pmatrix}} {} \\end{{pmatrix}}",
            rows.iter()
                .map(|r| r.iter().map(latex).collect::<Vec<_>>().join(" & "))
                .collect::<Vec<_>>()
                .join(" \\\\ ")
        ),
        _ => fmt(e).0,
    }
}

fn func_name(f: Func) -> &'static str {
    match f {
        Func::Sin => "\\sin",
        Func::Cos => "\\cos",
        Func::Tan => "\\tan",
        Func::Asin => "\\arcsin",
        Func::Acos => "\\arccos",
        Func::Atan => "\\arctan",
        Func::Log => "\\ln",
        Func::Abs => "",
        Func::Sinh => "\\sinh",
        Func::Cosh => "\\cosh",
        Func::Tanh => "\\tanh",
        Func::Asinh => "\\operatorname{arsinh}",
        Func::Acosh => "\\operatorname{arcosh}",
        Func::Atanh => "\\operatorname{artanh}",
    }
}

fn fmt(e: &Expr) -> (String, u8) {
    if let Some((c, u)) = crate::units::printable(e).or_else(|| {
        if matches!(e, Add(_)) && crate::units::has_units(e) {
            crate::units::split(e).filter(|(c, u)| !u.is_empty() && !crate::units::has_units(c))
        } else {
            None
        }
    }) {
        let (cs, cp) = fmt(&c);
        let cs = if matches!(c, Add(_)) { format!("\\left({}\\right)", cs) } else { cs };
        return (format!("{}\\,{}", cs, crate::units::latex(&u)), cp.min(P_MUL));
    }
    match e {
        Num(n) => (
            number(n.clone()),
            if n.is_negative() {
                P_ADD
            } else if n.is_integer() {
                P_ATOM
            } else {
                P_MUL
            },
        ),
        Sym(s) => (name(s), P_ATOM),
        Const(Const::Pi) => ("\\pi".into(), P_ATOM),
        Const(Const::E) => ("e".into(), P_ATOM),
        Const(Const::I) => ("i".into(), P_ATOM),
        Bool(b) => (format!("\\mathrm{{{}}}", b), P_ATOM),
        Str(s) => (format!("\\text{{{}}}", s), P_ATOM),
        Obj(o) => (format!("\\text{{{}}}", o.text().replace(['{', '}'], "")), P_ATOM),
        Matrix(_) => (latex(e), P_ATOM),
        Func(Func::Abs, a) => (format!("\\left|{}\\right|", fmt(a).0), P_ATOM),
        Func(f, a) => (
            format!("{}\\left({}\\right)", func_name(*f), fmt(a).0),
            P_ATOM,
        ),
        Apply(f, v) if f == "==" && v.len() == 2 => {
            (format!("{} = {}", fmt(&v[0]).0, fmt(&v[1]).0), 0)
        }
        Apply(f, v) => {
            let args = v.iter().map(|x| fmt(x).0).collect::<Vec<_>>().join(", ");
            if let Some(s) = special_latex(f, v) {
                return (s, P_ATOM);
            }
            let f = if f == "atan2" {
                "\\operatorname{atan2}".to_string()
            } else {
                apply_name(f)
            };
            (format!("{}\\left({}\\right)", f, args), P_ATOM)
        }
        Add(terms) => {
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
                    s.push_str(" - ");
                    s.push_str(&wrap(fmt(&neg(t.clone())), P_MUL));
                } else {
                    if i > 0 {
                        s.push_str(" + ");
                    }
                    s.push_str(&wrap(fmt(t), P_MUL));
                }
            }
            (s.trim_start().to_string(), P_ADD)
        }
        Held(op, v) => {
            use crate::expr::Hop;
            let bin = |sym: &str, l: u8, r: u8, p: u8| {
                (format!("{}{}{}", wrap(fmt(&v[0]), l), sym, wrap(fmt(&v[1]), r)), p)
            };
            match op {
                Hop::Add => bin(" + ", P_ADD, P_MUL, P_ADD),
                Hop::Sub => bin(" - ", P_ADD, P_MUL, P_ADD),
                Hop::Mul | Hop::EMul => bin(" \\cdot ", P_MUL, P_POW, P_MUL),
                Hop::Div | Hop::EDiv => (format!("\\frac{{{}}}{{{}}}", fmt(&v[0]).0, fmt(&v[1]).0), P_ATOM),
                Hop::Pow | Hop::EPow => (format!("{}^{{{}}}", wrap(fmt(&v[0]), P_ATOM), fmt(&v[1]).0), P_POW),
                Hop::Neg => (format!("-{}", wrap(fmt(&v[0]), P_POW)), P_ADD),
                Hop::Fn(f) if f == "sqrt" => (format!("\\sqrt{{{}}}", fmt(&v[0]).0), P_ATOM),
                Hop::Fn(f) => (
                    format!("\\{}\\left({}\\right)", f, v.iter().map(|x| fmt(x).0).collect::<Vec<_>>().join(", ")),
                    P_ATOM,
                ),
            }
        }
        Mul(_) => fmt_mul(e),
        Pow(b, x) => {
            if let Num(n) = (**x).clone() {
                if n.is_negative() {
                    return fmt_mul(e);
                }
                if n == Number::rat(1, 2).unwrap() {
                    return (format!("\\sqrt{{{}}}", fmt(b).0), P_ATOM);
                }
                if let Number::Rat(1, q) = n {
                    return (format!("\\sqrt[{}]{{{}}}", q, fmt(b).0), P_ATOM);
                }
            }
            if **b == Const(Const::E) {
                return (format!("e^{{{}}}", fmt(x).0), P_POW);
            }
            // sin^2(x) rather than (sin(x))^2
            if let Func(f, a) = &**b {
                if *f != Func::Abs {
                    return (
                        format!(
                            "{}^{{{}}}\\left({}\\right)",
                            func_name(*f),
                            fmt(x).0,
                            fmt(a).0
                        ),
                        P_ATOM,
                    );
                }
            }
            (format!("{}^{{{}}}", wrap(fmt(b), P_ATOM), fmt(x).0), P_POW)
        }
    }
}

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
            Pow(b, x) if x.num().is_some_and(|n| n.is_negative()) => {
                denom.push(pow((**b).clone(), Num(x.num().unwrap().neg())))
            }
            _ => numer.push(f),
        }
    }
    let negative = coeff.is_negative();
    let coeff = coeff.abs();
    let (cn, cd) = match coeff {
        Number::Rat(n, d) => (
            n.to_string(),
            if d == 1 { None } else { Some(d.to_string()) },
        ),
        Number::Big(ref r) if r.is_integer() => (r.numer().to_string(), None),
        Number::Big(ref r) => (r.numer().to_string(), Some(r.denom().to_string())),
        Number::Float(_) | Number::Dec(..) | Number::Xd(..) | Number::XdBig(_) => (number(coeff), None),
    };
    let join = |fs: &[Expr], lead: Option<String>| -> String {
        let mut parts: Vec<String> = lead.into_iter().collect();
        let n = fs.len();
        for f in fs {
            // a lone sum needs no parentheses in a numerator or denominator
            parts.push(if n == 1 && parts.is_empty() {
                fmt(f).0
            } else {
                wrap(fmt(f), P_POW)
            });
        }
        parts.join(" \\, ")
    };
    let lead = if cn == "1" && !numer.is_empty() {
        None
    } else {
        Some(cn)
    };
    let mut s = if denom.is_empty() && cd.is_none() {
        let mut parts: Vec<String> = lead.into_iter().collect();
        parts.extend(numer.iter().map(|f| wrap(fmt(f), P_POW)));
        parts.join(" \\, ")
    } else {
        let top = if numer.is_empty() {
            lead.unwrap_or("1".into())
        } else {
            join(&numer, lead)
        };
        let bottom = join(&denom, cd);
        format!("\\frac{{{}}}{{{}}}", top, bottom)
    };
    if negative {
        s = format!("-{}", s);
        (s, P_ADD)
    } else {
        (s, P_MUL)
    }
}

/// \Gamma(x), \operatorname{erf}(x), J_n(x), \psi^{(n)}(x), ...
fn special_latex(f: &str, v: &[Expr]) -> Option<String> {
    if !crate::special::is_special(f, v.len()) {
        return None;
    }
    let a = |k: usize| fmt(&v[k]).0;
    Some(match f {
        "factorial" => match &v[0] {
            Expr::Sym(_) => format!("{}!", a(0)),
            Expr::Num(n) if n.is_integer() && !n.is_negative() => format!("{}!", a(0)),
            _ => format!("\\left({}\\right)!", a(0)),
        },
        "gamma" => format!("\\Gamma\\left({}\\right)", a(0)),
        "digamma" => format!("\\psi\\left({}\\right)", a(0)),
        "polygamma" => format!("\\psi^{{({})}}\\left({}\\right)", a(0), a(1)),
        "zeta" => format!("\\zeta\\left({}\\right)", a(0)),
        "polylog" => format!("\\operatorname{{Li}}_{{{}}}\\left({}\\right)", a(0), a(1)),
        "heaviside" => format!("H\\left({}\\right)", a(0)),
        "dirac" => format!("\\delta\\left({}\\right)", a(0)),
        "sign" => format!("\\operatorname{{sgn}}\\left({}\\right)", a(0)),
        "erf" | "erfc" | "erfi" => format!("\\operatorname{{{}}}\\left({}\\right)", f, a(0)),
        "besselj" => format!("J_{{{}}}\\left({}\\right)", a(0), a(1)),
        "bessely" => format!("Y_{{{}}}\\left({}\\right)", a(0), a(1)),
        "besseli" => format!("I_{{{}}}\\left({}\\right)", a(0), a(1)),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{Output, Session};
    fn tex(src: &str) -> String {
        match Session::tidy().run(src).pop().unwrap().unwrap() {
            Output::Value(v) => latex(&v),
            _ => panic!(),
        }
    }
    #[test]
    fn renders() {
        assert_eq!(tex("x^2/2"), "\\frac{x^{2}}{2}");
        assert_eq!(tex("-r_s/(2*r^2)"), "-\\frac{r_{s}}{2 \\, r^{2}}");
        assert_eq!(tex("sin(theta)^2"), "\\sin^{2}\\left(\\theta\\right)");
        assert_eq!(tex("1/(r-r_s)"), "\\frac{1}{r - r_{s}}");
        assert_eq!(name("phi''"), "\\ddot{\\phi}");
    }
}
