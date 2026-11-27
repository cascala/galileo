//! Physical units: 3 m/s, 9.81 m/s^2, 2 kg*m/s^2.
//!
//! A unit is a symbol with an internal name (`m), so the algebra works on
//! quantities as on any product: 3 m * 2 m = 6 m^2, (6 m)/(2 s) = 3 m/s.
//! Sums check dimensions (3 m + 2 s is an error) and convert to the units of
//! the first term (1 km + 300 m = 13/10 km); convert(q, km/h) changes units.

use crate::build;
use crate::expr::{Const, Expr};
use crate::num::Number;

type R<T> = Result<T, String>;

/// dimensions: length, mass, time, current, temperature, amount
type Dims = [i32; 6];

const L: Dims = [1, 0, 0, 0, 0, 0];
const M: Dims = [0, 1, 0, 0, 0, 0];
const T: Dims = [0, 0, 1, 0, 0, 0];
const I: Dims = [0, 0, 0, 1, 0, 0];
const K: Dims = [0, 0, 0, 0, 1, 0];
const N: Dims = [0, 0, 0, 0, 0, 1];
const NONE: Dims = [0; 6];

const DIM_NAMES: [&str; 6] = ["length", "mass", "time", "current", "temperature", "amount"];

#[derive(Clone, Copy)]
enum Scale {
    /// an exact ratio to the SI unit
    Rat(i128, i128),
    /// pi/180 (degrees)
    Degree,
}

/// (name, scale to SI, dimensions as [L, M, T, I, K, N], takes SI prefixes)
const UNITS: &[(&str, Scale, Dims, bool)] = &[
    ("m", Scale::Rat(1, 1), L, true),
    ("g", Scale::Rat(1, 1000), M, true),
    ("s", Scale::Rat(1, 1), T, true),
    ("A", Scale::Rat(1, 1), I, true),
    ("K", Scale::Rat(1, 1), K, true),
    ("mol", Scale::Rat(1, 1), N, true),
    ("N", Scale::Rat(1, 1), [1, 1, -2, 0, 0, 0], true),
    ("J", Scale::Rat(1, 1), [2, 1, -2, 0, 0, 0], true),
    ("W", Scale::Rat(1, 1), [2, 1, -3, 0, 0, 0], true),
    ("Pa", Scale::Rat(1, 1), [-1, 1, -2, 0, 0, 0], true),
    ("Hz", Scale::Rat(1, 1), [0, 0, -1, 0, 0, 0], true),
    ("C", Scale::Rat(1, 1), [0, 0, 1, 1, 0, 0], true),
    ("V", Scale::Rat(1, 1), [2, 1, -3, -1, 0, 0], true),
    ("ohm", Scale::Rat(1, 1), [2, 1, -3, -2, 0, 0], true),
    ("F", Scale::Rat(1, 1), [-2, -1, 4, 2, 0, 0], true),
    ("H", Scale::Rat(1, 1), [2, 1, -2, -2, 0, 0], true),
    ("T", Scale::Rat(1, 1), [0, 1, -2, -1, 0, 0], true),
    ("Wb", Scale::Rat(1, 1), [2, 1, -2, -1, 0, 0], true),
    ("eV", Scale::Rat(1602176634, 10_000_000_000_000_000_000_000_000_000), [2, 1, -2, 0, 0, 0], true),
    ("L", Scale::Rat(1, 1000), [3, 0, 0, 0, 0, 0], true),
    ("Wh", Scale::Rat(3600, 1), [2, 1, -2, 0, 0, 0], true),
    ("cal", Scale::Rat(4184, 1000), [2, 1, -2, 0, 0, 0], true),
    ("bar", Scale::Rat(100000, 1), [-1, 1, -2, 0, 0, 0], true),
    ("min", Scale::Rat(60, 1), T, false),
    ("h", Scale::Rat(3600, 1), T, false),
    ("day", Scale::Rat(86400, 1), T, false),
    ("yr", Scale::Rat(31557600, 1), T, false),
    ("atm", Scale::Rat(101325, 1), [-1, 1, -2, 0, 0, 0], false),
    ("lb", Scale::Rat(45359237, 100_000_000), M, false),
    ("ft", Scale::Rat(3048, 10000), L, false),
    ("inch", Scale::Rat(254, 10000), L, false),
    ("mi", Scale::Rat(1609344, 1000), L, false),
    ("mph", Scale::Rat(1609344, 3600000), [1, 0, -1, 0, 0, 0], false),
    ("AU", Scale::Rat(149597870700, 1), L, false),
    ("ly", Scale::Rat(9460730472580800, 1), L, false),
    ("rad", Scale::Rat(1, 1), NONE, false),
    ("deg", Scale::Degree, NONE, false),
];

const PREFIXES: &[(&str, i128, i128)] = &[
    ("G", 1_000_000_000, 1),
    ("M", 1_000_000, 1),
    ("k", 1000, 1),
    ("c", 1, 100),
    ("m", 1, 1000),
    ("u", 1, 1_000_000),
    ("n", 1, 1_000_000_000),
    ("p", 1, 1_000_000_000_000),
];

/// (scale, dimensions) of a unit name, with an SI prefix (km, ms, MeV, uF)
fn lookup(name: &str) -> Option<(Scale, Dims)> {
    if let Some(&(_, s, d, _)) = UNITS.iter().find(|u| u.0 == name) {
        return Some((s, d));
    }
    for &(p, pn, pd) in PREFIXES {
        if let Some(rest) = name.strip_prefix(p) {
            // centi only for metres (cm), as usual
            if p == "c" && rest != "m" {
                continue;
            }
            if let Some(&(_, Scale::Rat(n, d), dims, true)) = UNITS.iter().find(|u| u.0 == rest) {
                let (n, d) = (n.checked_mul(pn)?, d.checked_mul(pd)?);
                return Some((Scale::Rat(n, d), dims));
            }
        }
    }
    None
}

pub fn is_unit(name: &str) -> bool {
    lookup(name).is_some()
}

/// the symbol of a unit
pub fn sym(name: &str) -> Expr {
    Expr::Sym(format!("`{}", name))
}

/// the unit name of a unit symbol
pub fn unit_name(s: &str) -> Option<&str> {
    s.strip_prefix('`')
}

fn scale_expr(s: Scale) -> Expr {
    match s {
        Scale::Rat(n, d) => Expr::Num(Number::rat(n, d).unwrap()),
        Scale::Degree => build::div(Expr::Const(Const::Pi), Expr::int(180)),
    }
}

/// units as (name, exponent), in order
pub type Units = Vec<(String, Number)>;

/// Parses unit text: kg*m/s^2, J/(kg*K), 1/s
pub fn parse(text: &str) -> R<Units> {
    let c: Vec<char> = text.chars().filter(|c| !c.is_whitespace()).collect();
    let mut i = 0;
    let u = product(&c, &mut i)?;
    if i < c.len() {
        return Err(format!("can't read the units {}", text));
    }
    Ok(merge(u))
}

fn product(c: &[char], i: &mut usize) -> R<Units> {
    let mut out = factor(c, i)?;
    while *i < c.len() && (c[*i] == '*' || c[*i] == '/') {
        let div = c[*i] == '/';
        *i += 1;
        let f = factor(c, i)?;
        for (n, e) in f {
            out.push((n, if div { e.neg() } else { e }));
        }
    }
    Ok(out)
}

fn factor(c: &[char], i: &mut usize) -> R<Units> {
    let mut u = if *i < c.len() && c[*i] == '(' {
        *i += 1;
        let u = product(c, i)?;
        if *i >= c.len() || c[*i] != ')' {
            return Err("missing ) in units".into());
        }
        *i += 1;
        u
    } else if *i < c.len() && c[*i] == '1' {
        *i += 1;
        vec![]
    } else {
        let start = *i;
        while *i < c.len() && c[*i].is_alphabetic() {
            *i += 1;
        }
        let name: String = c[start..*i].iter().collect();
        if !is_unit(&name) {
            return Err(if name.is_empty() {
                "expected a unit".into()
            } else {
                format!("{} is not a unit", name)
            });
        }
        vec![(name, Number::one())]
    };
    if *i < c.len() && c[*i] == '^' {
        *i += 1;
        let start = *i;
        if *i < c.len() && c[*i] == '-' {
            *i += 1;
        }
        let paren = *i < c.len() && c[*i] == '(';
        if paren {
            *i += 1;
        }
        while *i < c.len() && (c[*i].is_ascii_digit() || c[*i] == '-' || (paren && c[*i] == '/')) {
            *i += 1;
        }
        let mut t: String = c[start..*i].iter().collect();
        if paren {
            if *i >= c.len() || c[*i] != ')' {
                return Err("missing ) in units".into());
            }
            *i += 1;
            t = t.replace('(', "");
        }
        let k = match t.split_once('/') {
            Some((a, b)) => Number::rat(a.parse().map_err(|_| "bad exponent")?, b.parse().map_err(|_| "bad exponent")?),
            None => t.parse::<i128>().ok().map(|k| Number::rat(k, 1).unwrap()),
        }
        .ok_or("bad exponent in units")?;
        for x in u.iter_mut() {
            x.1 = x.1.mul(&k);
        }
    }
    Ok(u)
}

/// adds up the exponents of repeated units, drops zero exponents
fn merge(u: Units) -> Units {
    let mut out: Units = vec![];
    for (n, e) in u {
        match out.iter_mut().find(|x| x.0 == n) {
            Some(x) => x.1 = x.1.add(&e),
            None => out.push((n, e)),
        }
    }
    out.retain(|x| !x.1.is_zero());
    out
}

/// the product of the unit symbols
pub fn expr_of(u: &Units) -> Expr {
    build::mul(
        u.iter()
            .map(|(n, e)| build::pow(sym(n), Expr::Num(e.clone())))
            .collect(),
    )
}

fn is_unit_factor(f: &Expr) -> Option<(String, Number)> {
    match f {
        Expr::Sym(s) => unit_name(s).map(|n| (n.to_string(), Number::one())),
        Expr::Pow(b, x) => match (&**b, &**x) {
            (Expr::Sym(s), Expr::Num(k)) => unit_name(s).map(|n| (n.to_string(), k.clone())),
            _ => None,
        },
        _ => None,
    }
}

/// a quantity split into (coefficient, units); None for a sum whose terms
/// have different units
pub fn split(e: &Expr) -> Option<(Expr, Units)> {
    match e {
        Expr::Mul(fs) => {
            let mut rest = vec![];
            let mut u = vec![];
            for f in fs {
                match is_unit_factor(f) {
                    Some(x) => u.push(x),
                    None => rest.push(f.clone()),
                }
            }
            Some((build::mul(rest), u))
        }
        Expr::Add(ts) => {
            let parts: Option<Vec<(Expr, Units)>> = ts.iter().map(split).collect();
            let parts = parts?;
            let u = parts[0].1.clone();
            if parts.iter().any(|p| !same(&p.1, &u)) {
                return None;
            }
            Some((build::add(parts.into_iter().map(|p| p.0).collect()), u))
        }
        _ => match is_unit_factor(e) {
            Some(x) => Some((Expr::one(), vec![x])),
            None => Some((e.clone(), vec![])),
        },
    }
}

fn same(a: &Units, b: &Units) -> bool {
    a.len() == b.len() && a.iter().all(|x| b.contains(x))
}

pub fn has_units(e: &Expr) -> bool {
    match e {
        Expr::Sym(s) => s.starts_with('`'),
        Expr::Add(v) | Expr::Mul(v) | Expr::Apply(_, v) => v.iter().any(has_units),
        Expr::Pow(a, b) => has_units(a) || has_units(b),
        Expr::Func(_, a) => has_units(a),
        Expr::Matrix(m) => m.iter().flatten().any(has_units),
        _ => false,
    }
}

fn dims(u: &Units) -> R<Dims> {
    let mut d = NONE;
    for (n, e) in u {
        let (_, ud) = lookup(n).ok_or_else(|| format!("{} is not a unit", n))?;
        let k = e.as_int().ok_or("units with fractional powers can't be compared")? as i32;
        for j in 0..6 {
            d[j] += k * ud[j];
        }
    }
    Ok(d)
}

fn scale(u: &Units) -> Expr {
    build::mul(
        u.iter()
            .map(|(n, e)| build::pow(scale_expr(lookup(n).unwrap().0), Expr::Num(e.clone())))
            .collect(),
    )
}

/// the dimension written out: "length/time^2"
fn dim_text(d: &Dims) -> String {
    let part = |pos: bool| {
        let v: Vec<String> = (0..6)
            .filter(|&j| if pos { d[j] > 0 } else { d[j] < 0 })
            .map(|j| {
                let k = d[j].abs();
                if k == 1 { DIM_NAMES[j].to_string() } else { format!("{}^{}", DIM_NAMES[j], k) }
            })
            .collect();
        v
    };
    let (n, dn) = (part(true), part(false));
    let num = if n.is_empty() { "1".to_string() } else { n.join("*") };
    match dn.len() {
        0 if n.is_empty() => "dimensionless".into(),
        0 => num,
        1 => format!("{}/{}", num, dn[0]),
        _ => format!("{}/({})", num, dn.join("*")),
    }
}

/// a sum or difference of quantities: None when there are no units to check
pub fn add(a: &Expr, b: &Expr, subtract: bool) -> R<Option<Expr>> {
    if !has_units(a) && !has_units(b) {
        return Ok(None);
    }
    let (Some((ca, ua)), Some((cb, ub))) = (split(a), split(b)) else { return Ok(None) };
    if same(&ua, &ub) {
        return Ok(None);
    }
    let pure_number = |c: &Expr| c.is_number() || c.symbols().is_empty();
    if ua.is_empty() || ub.is_empty() {
        let (plain, q) = if ua.is_empty() { (&ca, b) } else { (&cb, a) };
        if plain.is_zero() {
            let r = if ua.is_empty() && subtract { build::neg(q.clone()) } else { q.clone() };
            return Ok(Some(r));
        }
        if pure_number(plain) {
            return Err(format!(
                "can't add {} and {}: one has units and the other doesn't",
                crate::print::show(a),
                crate::print::show(b)
            ));
        }
        return Ok(None);
    }
    let (da, db) = (dims(&ua)?, dims(&ub)?);
    // 2 kg*a - 10 N with an unknown a: leave the sum to solve
    if da != db && !(ca.symbols().is_empty() && cb.symbols().is_empty()) {
        return Ok(None);
    }
    if da != db {
        return Err(format!(
            "can't add {} and {}: {} and {} are different dimensions",
            crate::print::show(a),
            crate::print::show(b),
            dim_text(&da),
            dim_text(&db)
        ));
    }
    let cb = build::mul(vec![cb, scale(&ub), build::pow(scale(&ua), Expr::int(-1))]);
    let c = if subtract { build::sub(ca, cb) } else { build::add2(ca, cb) };
    Ok(Some(build::mul2(decimal(crate::calc::simplify(&c)), expr_of(&ua))))
}

/// a converted value: an exact fraction such as 250/9 becomes a decimal
/// (27.78), integers stay exact
fn decimal(c: Expr) -> Expr {
    match &c {
        Expr::Num(n) if n.is_exact() && !n.is_integer() => {
            // 15 significant digits: 1 eV = 1.602176634e-19 J, without float noise
            let f = n.to_f64();
            Expr::float(format!("{:.14e}", f).parse().unwrap_or(f))
        }
        _ => c,
    }
}

/// after a product, quotient or power: units that cancel to a pure number
/// (km/m, J/(N*m)) become that number; angles (rad, deg) stay
pub fn tidy(e: Expr) -> Expr {
    if !has_units(&e) {
        return e;
    }
    let Some((c, u)) = split(&e) else { return e };
    if u.is_empty() || u.iter().all(|(n, _)| n == "rad" || n == "deg") {
        return e;
    }
    match dims(&u) {
        Ok(d) if d == NONE => crate::calc::simplify(&build::mul2(c, scale(&u))),
        _ => merge_alike(c, u).unwrap_or(e),
    }
}

/// units of the same kind in one product become one unit: cm^2*kg/m^3 is
/// kg/m times a number. The SI unit (scale 1) is kept when there is one.
fn merge_alike(c: Expr, u: Units) -> Option<Expr> {
    let info: Vec<(Scale, Dims)> = u.iter().map(|(n, _)| lookup(n)).collect::<Option<_>>()?;
    let mut target: Vec<usize> = (0..u.len()).collect();
    let mut changed = false;
    for i in 0..u.len() {
        if info[i].1 == NONE {
            continue;
        }
        let same: Vec<usize> = (0..u.len()).filter(|&j| info[j].1 == info[i].1).collect();
        if same.len() < 2 {
            continue;
        }
        let keep = same
            .iter()
            .copied()
            .find(|&j| matches!(info[j].0, Scale::Rat(1, 1)))
            .unwrap_or(same[0]);
        if target[i] != keep {
            target[i] = keep;
            changed = true;
        }
    }
    if !changed {
        return None;
    }
    let mut factor = vec![c];
    let mut out: Units = vec![];
    for (i, (_, e)) in u.iter().enumerate() {
        let k = target[i];
        if k != i {
            let ratio = build::div(scale_expr(info[i].0), scale_expr(info[k].0));
            factor.push(build::pow(ratio, Expr::Num(e.clone())));
        }
        out.push((u[k].0.clone(), e.clone()));
    }
    let out = merge(out);
    let k = crate::calc::simplify(&build::mul(factor));
    // a conversion factor was applied: a fraction becomes a decimal, as in convert
    Some(build::mul2(decimal(k), expr_of(&out)))
}

/// a quantity to a power: (3 m)^2 = 9 m^2, sqrt(4 m^2) = 2 m
pub fn pow(a: &Expr, k: &Expr) -> Option<Expr> {
    if !has_units(a) {
        return None;
    }
    let kn = k.num()?;
    let (c, u) = split(a)?;
    // a fractional power goes through SI base units: sqrt(100 N/(kg*m)) = 10 s^-1
    let (c, u) = if !kn.is_integer() && u.iter().any(|(_, e)| !e.mul(&kn).is_integer()) {
        split(&convert(a, None).ok()?)?
    } else {
        (c, u)
    };
    let u: Units = u.into_iter().map(|(n, e)| (n, e.mul(&kn))).collect();
    Some(build::mul2(crate::calc::simplify(&build::pow(c, k.clone())), expr_of(&merge(u))))
}

/// the argument of sin, exp, log, ...: a plain number; angles in deg or rad
/// become radians
pub fn plain_arg(f: &str, a: &Expr) -> R<Expr> {
    if let Expr::Matrix(m) = a {
        let mut rows = vec![];
        for r in m {
            let mut row = vec![];
            for x in r {
                row.push(plain_arg(f, x)?);
            }
            rows.push(row);
        }
        return Ok(Expr::Matrix(rows));
    }
    if !has_units(a) {
        return Ok(a.clone());
    }
    let (c, u) = split(a).ok_or_else(|| format!("{} has mixed units", crate::print::show(a)))?;
    let d = dims(&u)?;
    if d != NONE {
        return Err(format!(
            "{}({}): the argument has units ({}); {} needs a number or an angle",
            f,
            crate::print::show(a),
            dim_text(&d),
            f
        ));
    }
    Ok(crate::calc::simplify(&build::mul2(c, scale(&u))))
}

/// convert(q, km/h), or convert(q) to SI base units
pub fn convert(q: &Expr, target: Option<&Expr>) -> R<Expr> {
    if let Expr::Matrix(m) = q {
        let mut rows = vec![];
        for r in m {
            let mut row = vec![];
            for x in r {
                row.push(convert(x, target)?);
            }
            rows.push(row);
        }
        return Ok(Expr::Matrix(rows));
    }
    let (c, u) = split(q).ok_or_else(|| format!("{} has mixed units", crate::print::show(q)))?;
    let d = dims(&u)?;
    let tu: Units = match target {
        None => {
            let base = ["m", "kg", "s", "A", "K", "mol"];
            (0..6)
                .filter(|&j| d[j] != 0)
                .map(|j| (base[j].to_string(), Number::rat(d[j] as i128, 1).unwrap()))
                .collect()
        }
        Some(Expr::Str(s)) => parse(s)?,
        Some(t) => {
            let (tc, tu) = split(t).ok_or("convert: the target should be units, such as km/h")?;
            if !tc.is_one() || tu.is_empty() {
                return Err("convert: the target should be units, such as km/h".into());
            }
            tu
        }
    };
    let td = dims(&tu)?;
    if td != d {
        return Err(format!(
            "can't convert {} to {}: {} is not {}",
            crate::print::show(q),
            crate::print::show(&expr_of(&tu)),
            dim_text(&d),
            dim_text(&td)
        ));
    }
    let k = build::mul(vec![c, scale(&u), build::pow(scale(&tu), Expr::int(-1))]);
    Ok(build::mul2(decimal(crate::calc::simplify(&k)), expr_of(&tu)))
}

/// dimension(q): "length/time"
pub fn dimension(q: &Expr) -> R<String> {
    let (_, u) = split(q).ok_or_else(|| format!("{} has mixed units", crate::print::show(q)))?;
    Ok(dim_text(&dims(&u)?))
}

/// (name, value, units, description)
pub const CONSTANTS: &[(&str, &str, &str, &str)] = &[
    ("c", "299792458", "m/s", "speed of light in vacuum"),
    ("G", "6.67430e-11", "m^3/(kg*s^2)", "Newton's gravitational constant"),
    ("g", "9.80665", "m/s^2", "standard acceleration of gravity"),
    ("h", "6.62607015e-34", "J*s", "Planck's constant"),
    ("hbar", "1.054571817e-34", "J*s", "reduced Planck constant h/(2 pi)"),
    ("k", "1.380649e-23", "J/K", "Boltzmann's constant"),
    ("kB", "1.380649e-23", "J/K", "Boltzmann's constant (as k)"),
    ("NA", "6.02214076e23", "1/mol", "Avogadro's number"),
    ("R", "8.314462618", "J/(mol*K)", "gas constant"),
    ("e", "1.602176634e-19", "C", "elementary charge"),
    ("me", "9.1093837139e-31", "kg", "electron mass"),
    ("mp", "1.67262192595e-27", "kg", "proton mass"),
    ("eps0", "8.8541878188e-12", "F/m", "vacuum permittivity"),
    ("mu0", "1.25663706127e-6", "N/A^2", "vacuum permeability"),
    ("sigma", "5.670374419e-8", "W/(m^2*K^4)", "Stefan-Boltzmann constant"),
    ("Mearth", "5.9722e24", "kg", "mass of the Earth"),
    ("Rearth", "6.371e6", "m", "mean radius of the Earth"),
    ("Msun", "1.98847e30", "kg", "mass of the Sun"),
    ("Rsun", "6.957e8", "m", "radius of the Sun"),
    ("Mmoon", "7.342e22", "kg", "mass of the Moon"),
];

/// const.c, const.G, ...
pub fn constant(name: &str) -> Option<Expr> {
    let &(_, v, u, _) = CONSTANTS.iter().find(|c| c.0 == name)?;
    let n = Expr::Num(Number::parse(v)?);
    Some(build::mul2(n, expr_of(&parse(u).ok()?)))
}

/// the text of units for printing: kg*m/s^2, J/(kg*K), 1/s
pub fn text(u: &Units) -> String {
    let pow_text = |n: &str, k: &Number| {
        if k.is_one() {
            n.to_string()
        } else if k.is_integer() {
            format!("{}^{}", n, k)
        } else {
            format!("{}^({})", n, k)
        }
    };
    let num: Vec<String> = u.iter().filter(|x| !x.1.is_negative()).map(|(n, k)| pow_text(n, k)).collect();
    let den: Vec<String> = u.iter().filter(|x| x.1.is_negative()).map(|(n, k)| pow_text(n, &k.neg())).collect();
    if num.is_empty() {
        // s^-1, not 1/s, so that 5 s^-1 reads back as a quantity
        return u.iter().map(|(n, k)| pow_text(n, k)).collect::<Vec<_>>().join("*");
    }
    let n = num.join("*");
    match den.len() {
        0 => n,
        1 => format!("{}/{}", n, den[0]),
        _ => format!("{}/({})", n, den.join("*")),
    }
}

/// units for LaTeX: \mathrm{kg\,m/s^{2}}
pub fn latex(u: &Units) -> String {
    let name = |n: &str| -> String {
        let n = if n == "ohm" {
            "\\Omega".to_string()
        } else if let Some(r) = n.strip_suffix("ohm") {
            format!("{}\\Omega", r.replace('u', "\\mu "))
        } else if n.starts_with('u') && n.len() > 1 && lookup(n).is_some() && !UNITS.iter().any(|x| x.0 == n) {
            format!("\\mu {}", &n[1..])
        } else {
            n.to_string()
        };
        n
    };
    let pow_text = |n: &str, k: &Number| {
        if k.is_one() { name(n) } else { format!("{}^{{{}}}", name(n), k) }
    };
    let num: Vec<String> = u.iter().filter(|x| !x.1.is_negative()).map(|(n, k)| pow_text(n, k)).collect();
    let den: Vec<String> = u.iter().filter(|x| x.1.is_negative()).map(|(n, k)| pow_text(n, &k.neg())).collect();
    let n = if num.is_empty() { "1".to_string() } else { num.join("\\,") };
    let t = match den.len() {
        0 => n,
        1 => format!("{}/{}", n, den[0]),
        _ => format!("{}/({})", n, den.join("\\,")),
    };
    format!("\\mathrm{{{}}}", t)
}

/// for printing: a product split into (coefficient, units) when it has units
pub fn printable(e: &Expr) -> Option<(Expr, Units)> {
    if !matches!(e, Expr::Mul(_) | Expr::Sym(_) | Expr::Pow(..)) || !has_units(e) {
        return None;
    }
    let (c, u) = split(e)?;
    if u.is_empty() || has_units(&c) {
        return None;
    }
    Some((c, u))
}

/// a matrix whose entries all have the same units: (the plain matrix, units)
pub fn matrix_units(rows: &[Vec<Expr>]) -> Option<(Vec<Vec<Expr>>, Units)> {
    if !rows.iter().flatten().any(has_units) {
        return None;
    }
    let mut u0: Option<Units> = None;
    let mut out = vec![];
    for r in rows {
        let mut row = vec![];
        for x in r {
            let (c, u) = split(x)?;
            if has_units(&c) {
                return None;
            }
            if c.is_zero() && u.is_empty() {
                row.push(c);
                continue;
            }
            match &u0 {
                None => u0 = Some(u),
                Some(v) if same(v, &u) => {}
                _ => return None,
            }
            row.push(c);
        }
        out.push(row);
    }
    Some((out, u0?))
}

/// the numbers of a quantity (or of each entry of a matrix), without units
pub fn strip(e: &Expr) -> Expr {
    match e {
        Expr::Matrix(m) => Expr::Matrix(m.iter().map(|r| r.iter().map(strip).collect()).collect()),
        _ => match split(e) {
            Some((c, _)) => c,
            None => e.clone(),
        },
    }
}
