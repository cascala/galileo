//! Commands for groups as values ([`crate::fgroup`]): constructors, subgroups,
//! cosets, quotients, conjugacy classes, Sylow subgroups.

use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::ffield as ff;
use crate::fgroup::{self as fg, Group, Perm};
use crate::parser::Node;
use std::rc::Rc;

fn gval(g: Group) -> Expr {
    Expr::Obj(Rc::new(Obj::Group(g)))
}

fn group_of(e: &Expr, f: &str) -> R<Group> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Group(g) => Ok(g.clone()),
            _ => Err(format!(
                "{}: expected a group such as sym(4), got {}",
                f,
                o.kind()
            )),
        },
        _ => Err(format!(
            "{}: expected a group such as sym(4) or group([2, 1, 3], [2, 3, 1])",
            f
        )),
    }
}

fn perm_in(e: &Expr, n: usize, f: &str) -> R<Perm> {
    let p = crate::extra_cmd::perm_of(e, f)?;
    if p.len() > n {
        return Err(format!(
            "{}: {} moves more than {} points",
            f,
            crate::print::show(e),
            n
        ));
    }
    let mut seen = vec![false; p.len()];
    for &k in &p {
        if k == 0 || k > p.len() || seen[k - 1] {
            return Err(format!(
                "{}: {} is not a permutation",
                f,
                crate::print::show(e)
            ));
        }
        seen[k - 1] = true;
    }
    Ok((0..n)
        .map(|i| {
            if i < p.len() {
                (p[i] - 1) as u16
            } else {
                i as u16
            }
        })
        .collect())
}

fn perm_expr(p: &Perm) -> Vec<Expr> {
    p.iter().map(|&k| Expr::int(k as i128 + 1)).collect()
}

fn rows(ps: &[Perm]) -> Expr {
    Expr::Matrix(ps.iter().map(perm_expr).collect())
}

fn small(e: &Expr, f: &str, lo: i128, hi: i128) -> R<usize> {
    let n = crate::extra_cmd::int_of(e, f)?;
    if n < lo || n > hi {
        return Err(format!("{}: n from {} to {}", f, lo, hi));
    }
    Ok(n as usize)
}

impl Session {
    /// Some(result) if f is a group command and the arguments fit it
    pub(crate) fn algebra_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        const NAMES: &[&str] = &[
            "sym",
            "alt",
            "cyclic",
            "dihedral",
            "units",
            "group",
            "order",
            "elements",
            "gens",
            "isabelian",
            "center",
            "subgroup",
            "cosets",
            "isnormal",
            "quotient",
            "classes",
            "derived",
            "issolvable",
            "issimple",
            "sylow",
            "orders",
            "dprod",
            "cayley",
            "kernel",
            "image",
            "ishom",
            "isomorphic",
            "abelianinv",
            "cayleygraph",
            "zmod",
            "gf",
            "generator",
            "dlog",
            "characteristic",
            "modulus",
            "lift",
            "isfield",
            "groebner",
            "polyrem",
            "inideal",
            "numfield",
            "minpoly",
            "galois",
            "quat",
            "conj",
            "rotate",
            "rotmatrix",
            "su2",
            "parts",
            "su",
            "so",
            "sl",
            "bracket",
            "coords",
            "adjoint",
            "killing",
        ];
        if !NAMES.contains(&f) {
            return None;
        }
        let hide = |i: usize| (f == "gf" || f == "zmod") && i == 1;
        let v = match args
            .iter()
            .enumerate()
            .map(|(i, a)| {
                if hide(i) {
                    self.eval_hidden(a, &["a".to_string()])
                } else {
                    self.eval(a)
                }
            })
            .collect::<R<Vec<Expr>>>()
        {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        let is_group = |e: &Expr| matches!(e, Expr::Obj(o) if matches!(&**o, Obj::Group(_)));
        // cayley of a list of permutations is the older command
        if f == "cayley" && !v.first().is_some_and(is_group) {
            return None;
        }
        let qringish = |e: &Expr| crate::qring::as_ring(e).is_some();
        if f == "numfield"
            || (f == "quotient" && !v.first().is_some_and(is_group))
            || v.first().is_some_and(qringish)
        {
            return Some(self.qring_cmd(f, &v));
        }
        if f == "minpoly" {
            return Some(Err(
                "minpoly(x) takes an element of a number field, such as numfield(x^2 - 2, x + 1)"
                    .into(),
            ));
        }
        if matches!(f, "quat" | "rotate" | "rotmatrix" | "su2" | "parts")
            || (f == "conj" && v.first().and_then(crate::quat::as_quat).is_some())
        {
            return Some(self.quat_cmd(f, &v));
        }
        if f == "conj" {
            return None;
        }
        if matches!(
            f,
            "su" | "so" | "sl" | "bracket" | "coords" | "adjoint" | "killing"
        ) {
            return Some(self.lie_cmd(f, &v));
        }
        if f == "galois" {
            return Some(self.galois_cmd(&v));
        }
        if matches!(f, "groebner" | "polyrem" | "inideal") {
            return Some(self.groebner_cmd(f, &v));
        }
        let ringish = |e: &Expr| ff::as_ring(e).is_some() || ff::as_elem(e).is_some();
        if matches!(
            f,
            "zmod"
                | "gf"
                | "generator"
                | "dlog"
                | "characteristic"
                | "modulus"
                | "lift"
                | "isfield"
        ) || v.first().is_some_and(ringish)
        {
            return Some(self.ring_cmd(f, &v));
        }
        Some(self.algebra(f, &v))
    }

    fn groebner_cmd(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        use crate::groebner::{Order, Ring};
        let usage = || {
            format!(
                "usage: groebner([f1, f2, ...], [x, y, ...]), polyrem(f, [f1, ...], [x, ...]), inideal(f, [f1, ...], [x, ...]); add \"grevlex\" or \"grlex\" for another order and a prime p to work mod p (see help(\"{}\"))",
                f
            )
        };
        let items = |e: &Expr| -> Option<Vec<Expr>> {
            match e {
                Expr::Matrix(m) if m.len() == 1 => Some(m[0].clone()),
                Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => {
                    Some(m.iter().map(|r| r[0].clone()).collect())
                }
                Expr::Obj(o) => match &**o {
                    Obj::List(l) => Some(l.clone()),
                    _ => None,
                },
                e => Some(vec![e.clone()]),
            }
        };
        let (target, rest) = if f == "groebner" {
            (None, v)
        } else {
            match v.split_first() {
                Some((t, r)) => (Some(t.clone()), r),
                None => return Err(usage()),
            }
        };
        let [fs, xs, opts @ ..] = rest else {
            return Err(usage());
        };
        let fs = items(fs).ok_or_else(usage)?;
        let vars: Vec<String> = items(xs)
            .ok_or_else(usage)?
            .iter()
            .map(|x| match x {
                Expr::Sym(s) => Ok(s.clone()),
                _ => Err(format!(
                    "{}: the variables must be names, got {}",
                    f,
                    crate::print::show(x)
                )),
            })
            .collect::<R<_>>()?;
        let mut ring = Ring {
            p: None,
            order: Order::Lex,
        };
        for o in opts {
            match o {
                Expr::Str(s) => {
                    ring.order = Order::parse(s).ok_or_else(|| {
                        format!("{}: orders are \"lex\", \"grlex\" and \"grevlex\"", f)
                    })?
                }
                e => {
                    let p = e
                        .num()
                        .filter(|n| n.is_integer())
                        .and_then(|n| n.as_bigint())
                        .filter(crate::numtheory::is_prime)
                        .ok_or_else(usage)?;
                    ring.p = Some(p);
                }
            }
        }
        let to_poly = |e: &Expr| -> R<crate::groebner::Poly> {
            let ts = crate::factor_cmd::terms_of(e, &vars).ok_or_else(|| {
                format!(
                    "{}: {} is not a polynomial in {} with rational coefficients",
                    f,
                    crate::print::show(e),
                    vars.join(", ")
                )
            })?;
            ring.poly(
                ts.into_iter()
                    .map(|(m, c)| (m.into_iter().map(|k| k as u32).collect(), c))
                    .collect(),
            )
        };
        let polys = fs.iter().map(to_poly).collect::<R<Vec<_>>>()?;
        let g = ring.groebner(polys)?;
        let show_poly = |p: &crate::groebner::Poly| -> Expr {
            use crate::build::*;
            add(p
                .iter()
                .map(|(m, c)| {
                    let mut fs = vec![Expr::Num(crate::num::Number::from_big(c.clone()))];
                    for (x, &k) in vars.iter().zip(m) {
                        if k > 0 {
                            fs.push(pow(Expr::sym(x), Expr::int(k as i128)));
                        }
                    }
                    mul(fs)
                })
                .collect())
        };
        let field = match &ring.p {
            Some(p) => format!("Z/{}", p),
            None => "Q".into(),
        };
        match target {
            None => {
                let lex_last = ring.order == Order::Lex && vars.len() > 1;
                let dim0 = crate::groebner::zero_dimensional(&g, vars.len());
                let order = match ring.order {
                    Order::Lex => "lex",
                    Order::Grlex => "grlex",
                    Order::Grevlex => "grevlex",
                };
                let mut note = format!(
                    "A reduced Gröbner basis over {} ({} order, {}): it generates the same ideal, and a polynomial lies in the ideal exactly when polyrem gives 0.",
                    field,
                    order,
                    vars.join(" > ")
                );
                if g.len() == 1 && g[0].len() == 1 && g[0][0].0.iter().all(|&e| e == 0) {
                    note = format!(
                        "The basis is [1]: 1 is in the ideal, so the equations have no common solution, even over {}.",
                        if ring.p.is_some() {
                            "the algebraic closure"
                        } else {
                            "the complex numbers"
                        }
                    );
                } else if lex_last && dim0 {
                    note.push_str(&format!(
                        " In lex order the last polynomials involve only the last variables, so the system can be solved from {} upward; finitely many solutions.",
                        vars[vars.len() - 1]
                    ));
                }
                self.note(&note);
                Ok(Expr::Matrix(vec![g.iter().map(show_poly).collect()]))
            }
            Some(t) => {
                let r = ring.reduce(&to_poly(&t)?, &g);
                if f == "inideal" {
                    Ok(Expr::Bool(r.is_empty()))
                } else if r.is_empty() {
                    Ok(Expr::zero())
                } else {
                    Ok(show_poly(&r))
                }
            }
        }
    }

    fn qring_cmd(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        use crate::qring as qr;
        let var_of = |e: &Expr| -> R<String> {
            match e {
                Expr::Sym(s) => Ok(s.clone()),
                _ => Err(format!(
                    "{}: expected a variable name, got {}",
                    f,
                    crate::print::show(e)
                )),
            }
        };
        let elem = |e: &Expr| -> R<qr::QElem> {
            qr::as_elem(e)
                .cloned()
                .ok_or_else(|| format!("{}: expected an element such as numfield(x^2 - 2, x)", f))
        };
        let made = |s: &mut Self, r: Rc<qr::QRing>, x: Option<&Expr>| -> R<Expr> {
            match x {
                Some(x) => Ok(qr::value(r.from_expr(x)?)),
                None => {
                    let kind = if r.field {
                        "a field"
                    } else {
                        "not a field (the modulus factors)"
                    };
                    s.note(&format!(
                        "{}[{}]/({}): {} has degree {} over {} and is {}; its elements are polynomials in {} of degree below {}, and {} = 0 in it.",
                        r.base(),
                        r.var,
                        crate::print::show(&qr::poly_expr(&r.f, &r.var)),
                        r.name(),
                        r.degree(),
                        r.base(),
                        kind,
                        r.var,
                        r.degree(),
                        crate::print::show(&qr::poly_expr(&r.f, &r.var)),
                    ));
                    Ok(qr::ring_value(r))
                }
            }
        };
        let prime = |e: &Expr| -> R<Option<num_bigint::BigInt>> {
            let n = e
                .num()
                .filter(|n| n.is_integer())
                .and_then(|n| n.as_bigint())
                .ok_or_else(|| format!("{}: the characteristic must be 0 or a prime", f))?;
            if n.is_zero() {
                Ok(None)
            } else if crate::numtheory::is_prime(&n) {
                Ok(Some(n))
            } else {
                Err(format!(
                    "{}: the characteristic must be 0 or a prime, got {}",
                    f, n
                ))
            }
        };
        use num_traits::Zero;
        match (f, v) {
            ("numfield", [p, rest @ ..]) if rest.len() <= 1 => {
                let syms = p.symbols();
                if syms.len() != 1 {
                    return Err(
                        "numfield: give a polynomial in one variable, such as numfield(x^2 - 2)"
                            .into(),
                    );
                }
                let r = qr::make(p, &syms[0], None, true)?;
                made(self, r, rest.first())
            }
            ("quotient", [p, x, rest @ ..]) if rest.len() <= 2 => {
                let ch = match rest.first() {
                    Some(c) => prime(c)?,
                    None => None,
                };
                let r = qr::make(p, &var_of(x)?, ch, false)?;
                made(self, r, rest.get(1))
            }
            ("minpoly", [a]) => {
                let a = elem(a)?;
                let m = a.minpoly();
                self.note(&format!(
                    "The monic polynomial of least degree over {} that vanishes at {}; it has degree {}, at most {}, the degree of the ring.",
                    a.ring.base(),
                    a.text(),
                    m.len() - 1,
                    a.ring.degree()
                ));
                Ok(qr::poly_expr(&m, &a.ring.var))
            }
            ("lift", [a]) => Ok(elem(a)?.lift()),
            ("modulus", [r]) => {
                let r = qr::as_ring(r).unwrap();
                Ok(qr::poly_expr(&r.f, &r.var))
            }
            ("isfield", [r]) => Ok(Expr::Bool(qr::as_ring(r).unwrap().field)),
            ("characteristic", [r]) => Ok(Expr::Num(crate::num::Number::from_bigint(
                qr::as_ring(r)
                    .unwrap()
                    .p
                    .clone()
                    .unwrap_or_else(num_bigint::BigInt::zero),
            ))),
            ("order", [r]) => {
                let r = qr::as_ring(r).unwrap();
                match &r.p {
                    Some(p) => {
                        let q = num_traits::pow(p.clone(), r.degree());
                        self.note(&format!(
                            "{} has {}^{} = {} elements.",
                            r.name(),
                            p,
                            r.degree(),
                            q
                        ));
                        Ok(Expr::Num(crate::num::Number::from_bigint(q)))
                    }
                    None => Err(format!("order: {} is infinite", r.name())),
                }
            }
            _ => Err(format!(
                "{}: wrong arguments; numfield(f), quotient(f, x), quotient(f, x, p) build rings, and K(g) builds elements",
                f
            )),
        }
    }

    fn quat_cmd(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        use crate::quat::{self as qt, Quat};
        let q = |e: &Expr| -> R<Quat> {
            qt::as_quat(e)
                .cloned()
                .ok_or_else(|| format!("{}: expected a quaternion such as quat(1, 2, 3, 4)", f))
        };
        let vec3 = |e: &Expr| -> R<Vec<Expr>> {
            let items: Vec<Expr> = match e {
                Expr::Matrix(m) if m.len() == 1 => m[0].clone(),
                Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => {
                    m.iter().map(|r| r[0].clone()).collect()
                }
                Expr::Obj(o) => match &**o {
                    Obj::List(l) => l.clone(),
                    _ => vec![],
                },
                _ => vec![],
            };
            if items.len() == 3 {
                Ok(items)
            } else {
                Err(format!("{}: expected a vector [x, y, z]", f))
            }
        };
        let scalar = |e: &Expr| -> R<Expr> {
            if e.is_scalar() {
                Ok(e.clone())
            } else {
                Err(format!(
                    "quat: components must be numbers or expressions, got {}",
                    crate::print::show(e)
                ))
            }
        };
        match (f, v) {
            ("quat", [a, b, c, d]) => Ok(qt::value(Quat([
                scalar(a)?,
                scalar(b)?,
                scalar(c)?,
                scalar(d)?,
            ]))),
            ("quat", [t, axis]) => {
                let r = qt::from_axis(&scalar(t)?, &vec3(axis)?)?;
                self.note("The unit quaternion cos(t/2) + sin(t/2) (x i + y j + z k)/|(x, y, z)|: rotate(q, v) turns v by the angle t about the axis.");
                Ok(qt::value(r))
            }
            ("quat", [a]) => Ok(qt::value(Quat::scalar(scalar(a)?))),
            ("conj", [a]) => {
                self.note("The conjugate a - b i - c j - d k; q conj(q) = |q|^2, so q^-1 = conj(q)/|q|^2.");
                Ok(qt::value(q(a)?.conj()))
            }
            ("parts", [a]) => Ok(Expr::Matrix(vec![q(a)?.0.to_vec()])),
            ("rotate", [a, x]) => {
                let r = q(a)?.rotate(&vec3(x)?)?;
                Ok(Expr::Matrix(vec![r]))
            }
            ("rotmatrix", [a]) => {
                let a = q(a)?;
                self.note("The matrix of v -> q v q^-1, a rotation (in SO(3)); q and -q give the same matrix, so unit quaternions (SU(2)) cover SO(3) twice.");
                Ok(Expr::Matrix(a.rotation_matrix()?))
            }
            ("su2", [a]) => {
                self.note("q = a + b i + c j + d k as the complex matrix [a + b i, c + d i; -c + d i, a - b i]; for |q| = 1 it lies in SU(2), and quaternion products become matrix products.");
                Ok(Expr::Matrix(q(a)?.su2()))
            }
            _ => Err(format!(
                "{}: usage quat(a, b, c, d), quat(angle, [x, y, z]), conj(q), norm(q), parts(q), rotate(q, [x, y, z]), rotmatrix(q), su2(q)",
                f
            )),
        }
    }

    fn lie_cmd(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        use crate::lie;
        let mat = |e: &Expr| -> R<crate::linalg::Mat> {
            match e {
                Expr::Matrix(m) if !m.is_empty() && m.len() == m[0].len() => Ok(m.clone()),
                _ => Err(format!(
                    "{}: expected a square matrix, got {}",
                    f,
                    crate::print::show(e)
                )),
            }
        };
        let basis = |e: &Expr| -> R<Vec<crate::linalg::Mat>> {
            match e {
                Expr::Obj(o) => match &**o {
                    Obj::List(l) => l.iter().map(mat).collect(),
                    _ => Err(format!("{}: expected a list of matrices such as su(2)", f)),
                },
                _ => Err(format!("{}: expected a list of matrices such as su(2)", f)),
            }
        };
        let list = |ms: Vec<crate::linalg::Mat>| {
            crate::lang::list(ms.into_iter().map(Expr::Matrix).collect())
        };
        match (f, v) {
            ("su" | "so" | "sl", [n]) => {
                let n = small(n, f, 2, lie::MAX_N as i128)?;
                let (b, what, dim) = match f {
                    "su" => (lie::su(n), "traceless anti-Hermitian", n * n - 1),
                    "so" => (lie::so(n), "antisymmetric", n * (n - 1) / 2),
                    _ => (lie::sl(n), "traceless", n * n - 1),
                };
                let extra = match (f, n) {
                    ("su", 2) => {
                        " Here e_k = -i sigma_k / 2, so [e1, e2] = e3 and cyclically, exactly like so(3): the two algebras are isomorphic."
                    }
                    ("so", 3) => {
                        " Here Lx, Ly, Lz generate rotations about the axes, with [Lx, Ly] = Lz and cyclically."
                    }
                    ("sl", 2) => " Here h, e, f with [h, e] = 2e, [h, f] = -2f, [e, f] = h.",
                    _ => "",
                };
                self.note(&format!(
                    "A basis of {}({}), the {} {}x{} matrices: dimension {}. bracket(A, B) = A B - B A stays inside.{}",
                    f, n, what, n, n, dim, extra
                ));
                Ok(list(b))
            }
            ("bracket", [a, b]) => {
                if let (Some(p), Some(q)) = (crate::quat::as_quat(a), crate::quat::as_quat(b)) {
                    return Ok(crate::quat::value(p.mul(q).add(&q.mul(p).neg())));
                }
                Ok(Expr::Matrix(lie::bracket(&mat(a)?, &mat(b)?)?))
            }
            ("coords", [x, b]) => Ok(Expr::Matrix(vec![lie::coords(&mat(x)?, &basis(b)?)?])),
            ("adjoint", [x, b]) => {
                self.note("The matrix of ad(X) = [X, .] in the basis: column j holds the coordinates of [X, e_j].");
                Ok(Expr::Matrix(lie::adjoint(&mat(x)?, &basis(b)?)?))
            }
            ("killing", [b]) => {
                let k = lie::killing(&basis(b)?)?;
                self.note("The Killing form B(x, y) = trace(ad x ad y) on the basis. By Cartan's criterion it is nondegenerate (det != 0) exactly when the algebra is semisimple; for a compact algebra like su(n) or so(n) it is negative definite.");
                Ok(Expr::Matrix(k))
            }
            _ => Err(format!(
                "{}: usage su(n), so(n), sl(n), bracket(A, B), coords(X, basis), adjoint(X, basis), killing(basis)",
                f
            )),
        }
    }

    fn galois_cmd(&mut self, v: &[Expr]) -> R<Expr> {
        use num_bigint::BigInt;
        let (p, x) = match v {
            [p] => {
                let s = p.symbols();
                if s.len() != 1 {
                    return Err(
                        "galois: give a polynomial in one variable, such as galois(x^5 - x - 1)"
                            .into(),
                    );
                }
                (p, s[0].clone())
            }
            [p, Expr::Sym(x)] => (p, x.clone()),
            _ => return Err("usage: galois(f) or galois(f, x)".into()),
        };
        let cs = crate::cas::poly_coeffs(p, &x).ok_or_else(|| {
            format!(
                "galois: {} is not a polynomial in {}",
                crate::print::show(p),
                x
            )
        })?;
        let mut qs = vec![];
        for c in &cs {
            let c = crate::calc::simplify(c);
            qs.push(
                c.num()
                    .filter(|n| n.is_exact())
                    .and_then(|n| n.big())
                    .ok_or("galois: the coefficients must be rational numbers")?,
            );
        }
        while qs.last().is_some_and(|c| num_traits::Zero::is_zero(c)) {
            qs.pop();
        }
        if qs.len() < 2 {
            return Err("galois: the polynomial must have degree at least 1".into());
        }
        let den = qs.iter().fold(BigInt::from(1), |a, c| {
            num_integer::Integer::lcm(&a, c.denom())
        });
        let z: Vec<BigInt> = qs
            .iter()
            .map(|c| (c * num_rational::BigRational::from_integer(den.clone())).to_integer())
            .collect();
        if z.len() > 6 {
            return Err("galois: degree up to 5".into());
        }
        let irreducible = z.len() == 2
            || crate::algfactor::factor_zpoly(&z)
                .is_some_and(|(_, fs)| fs.len() == 1 && fs[0].1 == 1);
        if !irreducible {
            return Err(format!(
                "galois: {} factors over Q; give one irreducible factor (factor({}) shows them)",
                crate::print::show(p),
                crate::print::show(p)
            ));
        }
        let g = crate::galois::galois(&z)?;
        let n = z.len() - 1;
        let solv = g.group.is_solvable()?;
        let radicals = if n <= 4 {
            "Every group here is solvable, so the roots are expressible by radicals: solve gives them (Cardano for degree 3, Ferrari for 4).".to_string()
        } else if solv {
            "The group is solvable, so the roots are expressible by radicals (though solve does not build quintic radicals).".to_string()
        } else {
            format!(
                "The group is not solvable ({} contains the simple group A5), so no formula with + - * / and roots gives the roots of this polynomial (Abel–Ruffini, Galois).",
                g.group.name
            )
        };
        self.note(&format!(
            "The Galois group of {} over Q, acting on its {} roots, has order {}. {} {}",
            crate::print::show(p),
            n,
            g.group.order(),
            g.method,
            radicals
        ));
        Ok(gval(g.group))
    }

    fn ring_cmd(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        use num_bigint::BigInt;
        let big = |e: &Expr| -> R<BigInt> {
            e.num()
                .filter(|n| n.is_integer())
                .and_then(|n| n.as_bigint())
                .ok_or_else(|| {
                    format!(
                        "{}: expected a whole number, got {}",
                        f,
                        crate::print::show(e)
                    )
                })
        };
        let ring = |e: &Expr| -> R<Rc<ff::Ring>> {
            ff::as_ring(e)
                .or_else(|| ff::as_elem(e).map(|x| x.ring.clone()))
                .ok_or_else(|| format!("{}: expected a ring such as gf(9) or zmod(12)", f))
        };
        let elem = |e: &Expr| -> R<ff::Elem> {
            ff::as_elem(e)
                .cloned()
                .ok_or_else(|| format!("{}: expected an element such as gf(7, 3)", f))
        };
        let num = |n: BigInt| Expr::Num(crate::num::Number::from_bigint(n));
        match (f, v) {
            ("zmod" | "gf", [n, rest @ ..]) if rest.len() <= 1 => {
                let n = big(n)?;
                let r = if f == "gf" {
                    ff::gf(&n)?
                } else {
                    ff::zmod(&n)?
                };
                match rest {
                    [x] => Ok(ff::value(r.from_expr(x)?)),
                    _ => {
                        let field = if r.is_field() {
                            "a field"
                        } else {
                            "not a field"
                        };
                        self.note(&format!(
                            "{} has {} elements and is {}{}.",
                            r.name(),
                            r.size(),
                            field,
                            if r.k > 1 {
                                format!(
                                    "; its elements are polynomials in `a` of degree below {}, with {} = 0",
                                    r.k,
                                    crate::print::show(&ff::poly_expr(&r.m, "a"))
                                )
                            } else {
                                String::new()
                            }
                        ));
                        Ok(ff::ring_value(r))
                    }
                }
            }
            ("order", [x]) => match ff::as_elem(x) {
                Some(x) => {
                    let o = x.order()?;
                    self.note(&format!(
                        "{}^{} = 1 and no smaller positive power is 1; the order divides {}, the number of units of {}.",
                        x.text(),
                        o,
                        x.ring.units(),
                        x.ring.name()
                    ));
                    Ok(num(o))
                }
                None => {
                    let r = ring(x)?;
                    self.note(&format!("{} has {} elements.", r.name(), r.size()));
                    Ok(num(r.size()))
                }
            },
            ("elements", [r]) => {
                let es = ring(r)?.elements()?;
                Ok(Expr::Matrix(vec![es.into_iter().map(ff::value).collect()]))
            }
            ("generator", [r]) => {
                let r = ring(r)?;
                let g = r.generator()?;
                self.note(&format!(
                    "Every unit of {} is a power of {}: it has order {}, the number of units.",
                    r.name(),
                    g.text(),
                    r.units()
                ));
                Ok(ff::value(g))
            }
            ("dlog", [h, g]) => {
                let (h, g) = (elem(h)?, elem(g)?);
                let x = ff::dlog(&h, &g)?;
                self.note(&format!(
                    "{}^{} = {} (Pohlig–Hellman: one small log per prime factor of the order of {}, then the Chinese remainder theorem).",
                    g.text(),
                    x,
                    h.text(),
                    g.text()
                ));
                Ok(num(x))
            }
            ("characteristic", [r]) => Ok(num(ring(r)?.n.clone())),
            ("modulus", [r]) => {
                let r = ring(r)?;
                Ok(if r.k > 1 {
                    ff::poly_expr(&r.m, "a")
                } else {
                    num(r.n.clone())
                })
            }
            ("lift", [x]) => Ok(elem(x)?.lift()),
            ("isfield", [r]) => Ok(Expr::Bool(ring(r)?.is_field())),
            _ => Err(format!(
                "{}: wrong arguments for a ring command; see help(\"{}\")",
                f, f
            )),
        }
    }

    fn algebra(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        let one = |f: &str| -> R<Group> {
            match v {
                [g] => group_of(g, f),
                _ => Err(format!("usage: {}(G) for a group G", f)),
            }
        };
        match f {
            "sym" | "alt" | "cyclic" | "dihedral" | "units" => {
                let [n] = v else {
                    return Err(format!("usage: {}(n)", f));
                };
                let n = small(n, f, 1, 1000)?;
                let g = match f {
                    "sym" => fg::sym(n),
                    "alt" => fg::alt(n),
                    "cyclic" => fg::cyclic(n),
                    "dihedral" => fg::dihedral(n),
                    _ => fg::units(n),
                }?;
                self.note(format!("`{}`: a group of order {}.", g.name, g.order()));
                Ok(gval(g))
            }
            "group" => {
                let g =
                    match v {
                        [Expr::Str(s)] if s.trim_start().starts_with('<') => fg::presentation(s)?,
                        [Expr::Str(s)] => fg::named(s)?,
                        // the rows of generate(...)
                        [Expr::Matrix(m)] if m.len() > 1 => {
                            let n = m.iter().map(|r| r.len()).max().unwrap_or(1);
                            let ps = m
                                .iter()
                                .map(|r| perm_in(&Expr::Matrix(vec![r.clone()]), n, "group"))
                                .collect::<R<Vec<_>>>()?;
                            Group::from_gens(n, ps)?
                        }
                        [] => return Err(
                            "usage: group(p1, p2, ...) generated by permutations, or group(\"Q8\")"
                                .into(),
                        ),
                        ps => {
                            let n = ps
                                .iter()
                                .map(|p| crate::extra_cmd::perm_of(p, "group").map(|q| q.len()))
                                .collect::<R<Vec<_>>>()?
                                .into_iter()
                                .max()
                                .unwrap();
                            let ps = ps
                                .iter()
                                .map(|p| perm_in(p, n, "group"))
                                .collect::<R<Vec<_>>>()?;
                            Group::from_gens(n, ps)?
                        }
                    };
                self.note(format!("A group of order {}.", g.order()));
                Ok(gval(g))
            }
            "order" => Ok(Expr::int(one(f)?.order() as i128)),
            "elements" => {
                let g = one(f)?;
                if g.order() > 5040 {
                    return Err(format!(
                        "elements: {} has {} elements, too many to show",
                        g.name,
                        g.order()
                    ));
                }
                Ok(rows(g.elements()))
            }
            "gens" => Ok(rows(&one(f)?.gens)),
            "isabelian" => Ok(Expr::Bool(one(f)?.is_abelian())),
            "issolvable" => {
                let g = one(f)?;
                let s = g.derived_series()?;
                let ok = s.last().unwrap().order() == 1;
                self.note(format!(
                    "Derived series G ⊇ G' ⊇ G'' ⊇ …, orders {}: {}.",
                    s.iter()
                        .map(|h| h.order().to_string())
                        .collect::<Vec<_>>()
                        .join(" ⊇ "),
                    if ok {
                        "it reaches the trivial group, so G is solvable"
                    } else {
                        "it stops before the trivial group, so G is not solvable"
                    }
                ));
                Ok(Expr::Bool(ok))
            }
            "issimple" => Ok(Expr::Bool(one(f)?.is_simple()?)),
            "center" => {
                let g = one(f)?;
                let z = g.center()?;
                self.note(format!(
                    "The center (elements that commute with everything) has order {}.",
                    z.order()
                ));
                Ok(gval(z))
            }
            "derived" => {
                let g = one(f)?;
                let d = g.derived()?;
                self.note(format!(
                    "The commutator subgroup [G, G], generated by all a⁻¹b⁻¹ab, has order {} (index {}); G/[G, G] is the largest abelian quotient.",
                    d.order(),
                    g.order() / d.order()
                ));
                Ok(gval(d))
            }
            "subgroup" => {
                let [g, ps @ ..] = v else {
                    return Err("usage: subgroup(G, p1, p2, ...)".into());
                };
                let g = group_of(g, f)?;
                let ps = ps
                    .iter()
                    .map(|p| perm_in(p, g.degree, f))
                    .collect::<R<Vec<_>>>()?;
                if let Some(p) = ps.iter().find(|p| !g.contains(p)) {
                    return Err(format!(
                        "subgroup: {} is not in {}",
                        fg::show_perm(p),
                        g.name
                    ));
                }
                let h = g.sub(ps)?;
                self.note(format!(
                    "A subgroup of order {}; by Lagrange's theorem it divides {} = |G| (index {}).",
                    h.order(),
                    g.order(),
                    g.order() / h.order()
                ));
                Ok(gval(h))
            }
            "cosets" | "isnormal" | "quotient" => {
                let [a, b] = v else {
                    return Err(format!("usage: {}(G, H) for a subgroup H of G", f));
                };
                let (mut g, mut h) = (group_of(a, f)?, group_of(b, f)?);
                if f == "isnormal" && g.is_subgroup_of(&h) && !h.is_subgroup_of(&g) {
                    std::mem::swap(&mut g, &mut h);
                }
                if !h.is_subgroup_of(&g) {
                    return Err(format!("{}: {} is not a subgroup of {}", f, h.name, g.name));
                }
                match f {
                    "cosets" => {
                        let c = g.cosets(&h);
                        self.note(format!(
                            "{} left cosets gH of {} elements each: {} = {} · {} (Lagrange). One representative per row.",
                            c.len(),
                            h.order(),
                            g.order(),
                            c.len(),
                            h.order()
                        ));
                        Ok(rows(&c.iter().map(|c| c[0].clone()).collect::<Vec<_>>()))
                    }
                    "isnormal" => Ok(Expr::Bool(g.is_normal(&h))),
                    _ => {
                        if !g.is_normal(&h) {
                            return Err(format!(
                                "quotient: {} is not normal in {} (g H g⁻¹ ≠ H for some g), so the cosets do not form a group",
                                h.name, g.name
                            ));
                        }
                        let q = g.quotient(&h)?;
                        self.note(format!(
                            "G/N has order {} / {} = {}; its elements are the cosets of N, acting here as permutations of the cosets.",
                            g.order(),
                            h.order(),
                            q.order()
                        ));
                        Ok(gval(q))
                    }
                }
            }
            "classes" => {
                let g = one(f)?;
                let cls = g.classes();
                let sizes: Vec<String> = cls.iter().map(|c| c.len().to_string()).collect();
                self.note(format!(
                    "{} conjugacy classes (one representative per row, in cycle notation {}). Class equation: {} = {}.",
                    cls.len(),
                    cls.iter().map(|c| fg::cycles(&c[0])).collect::<Vec<_>>().join(", "),
                    g.order(),
                    sizes.join(" + ")
                ));
                Ok(rows(&cls.iter().map(|c| c[0].clone()).collect::<Vec<_>>()))
            }
            "orders" => {
                let g = one(f)?;
                Ok(Expr::Matrix(
                    g.order_counts()
                        .into_iter()
                        .map(|(o, c)| vec![Expr::int(o as i128), Expr::int(c as i128)])
                        .collect(),
                ))
            }
            "sylow" => {
                let [a, p] = v else {
                    return Err("usage: sylow(G, p) for a prime p".into());
                };
                let g = group_of(a, f)?;
                let p = small(p, f, 2, 1_000_000)?;
                if !crate::numtheory::is_prime(&num_bigint::BigInt::from(p)) {
                    return Err(format!("sylow: {} is not prime", p));
                }
                let s = g.sylow(p)?;
                let m = g.order() / s.order();
                let np: Vec<String> = (1..=m)
                    .filter(|d| m % d == 0 && d % p == 1 % p)
                    .map(|d| d.to_string())
                    .collect();
                self.note(format!(
                    "A Sylow {}-subgroup, of order {} (the largest power of {} dividing {}). Sylow's theorems: they are all conjugate, and their number divides {} and is 1 mod {}, so it is one of {}.",
                    p, s.order(), p, g.order(), m, p, np.join(", ")
                ));
                Ok(gval(s))
            }
            "dprod" => {
                let [a, b] = v else {
                    return Err("usage: dprod(G, H), the direct product".into());
                };
                Ok(gval(group_of(a, f)?.product(&group_of(b, f)?)?))
            }
            "cayley" => {
                let g = one(f)?;
                if g.order() > 200 {
                    return Err(format!(
                        "cayley: {} has {} elements, too many for a table",
                        g.name,
                        g.order()
                    ));
                }
                self.note("Entry (i, j) is the number of the element e_i·e_j, numbering as in elements(G).");
                Ok(Expr::Matrix(
                    g.table()
                        .into_iter()
                        .map(|r| r.into_iter().map(|k| Expr::int(k as i128)).collect())
                        .collect(),
                ))
            }
            "kernel" | "image" | "ishom" => {
                let [a, b, imgs @ ..] = v else {
                    return Err(format!(
                        "usage: {}(G, H, f1, f2, ...) where f_i is the image in H of the i-th generator of G (see gens(G))",
                        f
                    ));
                };
                let (g, h) = (group_of(a, f)?, group_of(b, f)?);
                let imgs = imgs
                    .iter()
                    .map(|p| perm_in(p, h.degree, f))
                    .collect::<R<Vec<_>>>()?;
                if let Some(p) = imgs.iter().find(|p| !h.contains(p)) {
                    return Err(format!("{}: {} is not in {}", f, fg::show_perm(p), h.name));
                }
                match f {
                    "ishom" => Ok(Expr::Bool(fg::hom_table(&g, &h, &imgs).is_ok())),
                    "kernel" => {
                        let k = fg::kernel(&g, &h, &imgs)?;
                        let im = fg::image(&g, &h, &imgs)?;
                        self.note(format!(
                            "The kernel (what goes to the identity) has order {}, the image order {}: |G| = {} = {} · {} (first isomorphism theorem, G/ker ≅ image).",
                            k.order(), im.order(), g.order(), k.order(), im.order()
                        ));
                        Ok(gval(k))
                    }
                    _ => Ok(gval(fg::image(&g, &h, &imgs)?)),
                }
            }
            "isomorphic" => {
                let [a, b] = v else {
                    return Err("usage: isomorphic(G, H)".into());
                };
                let (g, h) = (group_of(a, f)?, group_of(b, f)?);
                if g.order() > 2000 || h.order() > 2000 {
                    return Err("isomorphic: groups up to order 2000".into());
                }
                match fg::isomorphism(&g, &h) {
                    Some((gs, imgs)) => {
                        self.note(format!(
                            "An isomorphism, sending generators (in cycle notation) to: {}.",
                            gs.iter()
                                .zip(&imgs)
                                .map(|(x, y)| format!("{} ↦ {}", fg::cycles(x), fg::cycles(y)))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                        Ok(Expr::Bool(true))
                    }
                    None => {
                        self.note("No isomorphism: the orders, element orders or class sizes differ, or no assignment of generators works.");
                        Ok(Expr::Bool(false))
                    }
                }
            }
            "abelianinv" => {
                let g = one(f)?;
                let d = g.derived()?;
                let q = if d.order() == 1 {
                    g.clone()
                } else {
                    g.quotient(&d)?
                };
                let inv = fg::abelian_invariants(&q);
                let text = if inv.is_empty() {
                    "the trivial group".to_string()
                } else {
                    inv.iter()
                        .map(|d| format!("Z/{}", d))
                        .collect::<Vec<_>>()
                        .join(" × ")
                };
                self.note(format!(
                    "{} ≅ {}, with each number dividing the next (invariant factors){}.",
                    if d.order() == 1 { "G" } else { "G/[G, G]" },
                    text,
                    if d.order() == 1 {
                        ""
                    } else {
                        "; G is not abelian, so this is its largest abelian quotient"
                    }
                ));
                Ok(Expr::Matrix(vec![
                    inv.into_iter().map(|d| Expr::int(d as i128)).collect(),
                ]))
            }
            "cayleygraph" => {
                let g = one(f)?;
                if g.order() > 400 {
                    return Err(format!(
                        "cayleygraph: {} has {} elements, too many to draw",
                        g.name,
                        g.order()
                    ));
                }
                Ok(cayley_graph(&g))
            }
            _ => unreachable!(),
        }
    }
}

/// vertices on a circle, one colored set of arrows per generator
fn cayley_graph(g: &Group) -> Expr {
    use crate::plot::{Figure, Series};
    let n = g.order();
    // lay out the cosets x, s x, s^2 x, ... of the first generator s as polygons:
    // concentric for up to 3 of them, otherwise around a circle
    let els = g.elements();
    let idx = |p: &Perm| els.iter().position(|e| e == p).unwrap();
    let mut pos = vec![(0.0f64, 0.0f64); n];
    let mut placed = vec![false; n];
    let s0 = g.gens.first().cloned().unwrap_or_else(|| g.identity());
    let m = fg::order_of(&s0);
    let ncos = n / m;
    // breadth-first, so neighbouring cosets come out next to each other
    let mut order = vec![g.identity()];
    let mut seen = vec![false; n];
    seen[idx(&g.identity())] = true;
    let mut k = 0;
    while k < order.len() {
        for s in &g.gens {
            let y = fg::mul(s, &order[k]);
            let j = idx(&y);
            if !seen[j] {
                seen[j] = true;
                order.push(y);
            }
        }
        k += 1;
    }
    let tau = std::f64::consts::TAU;
    let mut c = 0;
    for x in &order {
        if placed[idx(x)] {
            continue;
        }
        let mut y = x.clone();
        for k in 0..m {
            let j = idx(&y);
            placed[j] = true;
            let t = tau * k as f64 / m as f64 + tau / 4.0;
            pos[j] = if ncos <= 3 {
                let r = 1.0 - 0.4 * c as f64;
                (r * t.cos(), r * t.sin())
            } else {
                let ct = tau * c as f64 / ncos as f64;
                let r = (0.8 * std::f64::consts::PI / ncos as f64).min(0.35);
                (ct.cos() + r * t.cos(), ct.sin() + r * t.sin())
            };
            y = fg::mul(&s0, &y);
        }
        c += 1;
    }
    let at = |i: usize| pos[i];
    let mut series = vec![];
    for s in &g.gens {
        let mut pts = vec![];
        for (i, x) in g.elements().iter().enumerate() {
            let y = fg::mul(s, x);
            let j = idx(&y);
            if i == j {
                continue;
            }
            let (a, b) = (at(i), at(j));
            pts.push(a);
            pts.push(b);
            // an arrowhead
            let (dx, dy) = (b.0 - a.0, b.1 - a.1);
            let l = (dx * dx + dy * dy).sqrt().max(1e-9);
            let (ux, uy) = (dx / l * 0.06, dy / l * 0.06);
            pts.push((b.0 - ux - uy * 0.6, b.1 - uy + ux * 0.6));
            pts.push(b);
            pts.push((b.0 - ux + uy * 0.6, b.1 - uy - ux * 0.6));
            pts.push((f64::NAN, f64::NAN));
        }
        series.push(Series {
            label: format!("× {}", fg::cycles(s)),
            points: pts,
            thin: true,
            ..Default::default()
        });
    }
    series.push(Series {
        label: "elements".into(),
        points: (0..n).map(at).collect(),
        markers: true,
        color: Some("#333333".into()),
        ..Default::default()
    });
    let mut fig = Figure::new(series, "", "", true);
    fig.title = format!("Cayley graph of {}", g.name);
    Expr::Obj(Rc::new(Obj::Figure(fig)))
}
