//! Special functions: gamma, digamma, polygamma, erf, erfc, erfi, zeta and
//! the Bessel functions J, Y, I of whole-number order.
//!
//! They live in the expression tree as `Expr::Apply(name, args)`, like atan2.
//! [`apply`] builds them and knows the exact values (gamma(5) = 24,
//! gamma(1/2) = sqrt(pi), zeta(2) = pi^2/6, erf(0) = 0); [`value`] evaluates
//! them numerically; [`deriv`] differentiates them. The classical orthogonal
//! polynomials (Legendre, Hermite, Laguerre, Chebyshev) are plain polynomials
//! and come from [`orthopoly`].

use crate::build::*;
use crate::expr::{Const, Expr};
use crate::num::Number;
use std::f64::consts::PI;

fn int(n: i128) -> Expr {
    Expr::int(n)
}

/// (name, number of arguments)
pub const NAMES: &[(&str, usize)] = &[
    ("gamma", 1),
    ("digamma", 1),
    ("polygamma", 2),
    ("erf", 1),
    ("erfc", 1),
    ("erfi", 1),
    ("zeta", 1),
    ("polylog", 2),
    ("besselj", 2),
    ("bessely", 2),
    ("besseli", 2),
    ("heaviside", 1),
    ("dirac", 1),
    ("sign", 1),
];

pub fn is_special(f: &str, n: usize) -> bool {
    NAMES.iter().any(|(g, k)| *g == f && *k == n)
}

fn app(f: &str, args: Vec<Expr>) -> Expr {
    Expr::Apply(f.to_string(), args)
}

fn int_of(e: &Expr) -> Option<i128> {
    e.num().filter(|n| n.is_exact() && n.is_integer()).and_then(|n| n.as_int())
}

fn float_of(e: &Expr) -> Option<f64> {
    e.num().filter(|n| !n.is_exact()).map(|n| n.to_f64())
}

/// Problems that make a call meaningless (poles, non-integer orders), checked
/// when the user types the call
pub fn check(f: &str, args: &[Expr]) -> Result<(), String> {
    let pole = |what: &str| Err(format!("{} has a pole at {}", f, what));
    match f {
        "gamma" | "digamma" | "polygamma" => {
            let x = args.last().unwrap();
            if let Some(n) = int_of(x) {
                if n <= 0 {
                    return pole(&n.to_string());
                }
            }
            if f == "polygamma" {
                match int_of(&args[0]) {
                    Some(n) if (0..=60).contains(&n) => {}
                    _ => return Err("polygamma(n, x): n must be a whole number from 0 to 60".into()),
                }
            }
            Ok(())
        }
        "zeta" if args[0].num().is_some_and(|n| n.is_one()) => pole("1"),
        "besselj" | "bessely" | "besseli" => {
            if int_of(&args[0]).is_none_or(|n| n.abs() > 1000) {
                return Err(format!(
                    "{}(n, x): the order n must be a whole number (up to 1000)",
                    f
                ));
            }
            if f == "bessely" && args[1].is_zero() {
                return pole("x = 0");
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Build f(args), using exact values and symmetries where they exist
pub fn apply(f: &str, args: Vec<Expr>) -> Expr {
    if !is_special(f, args.len()) {
        return app(f, args);
    }
    // numeric arguments with a float among them: evaluate
    if args.iter().all(|a| a.num().is_some()) && args.iter().any(|a| float_of(a).is_some()) {
        let v: Vec<f64> = args.iter().map(|a| a.num().unwrap().to_f64()).collect();
        let r = value(f, &v);
        if r.is_finite() {
            return Expr::float(r);
        }
    }
    match f {
        "gamma" => gamma_exact(&args[0]).unwrap_or_else(|| app(f, args)),
        "polygamma" if args[0].is_zero() => apply("digamma", vec![args[1].clone()]),
        "erf" | "erfi" => {
            let x = &args[0];
            if x.is_zero() {
                Expr::zero()
            } else if x.is_negative_term() {
                neg(apply(f, vec![neg(x.clone())]))
            } else {
                app(f, args)
            }
        }
        "erfc" if args[0].is_zero() => Expr::one(),
        "heaviside" | "sign" | "dirac" => {
            let x = &args[0];
            let s = crate::assume::sign(x).or_else(|| {
                // numbers like sqrt(2) - 1 and pi - 3
                if x.symbols().is_empty() {
                    let v = crate::numeric::value(x);
                    v.is_finite().then_some(if v > 0.0 { 1 } else if v < 0.0 { -1 } else { 0 })
                } else {
                    None
                }
            });
            match (f, s) {
                ("heaviside", Some(1)) => Expr::one(),
                ("heaviside", Some(0)) => crate::build::rat(1, 2),
                ("heaviside", Some(_)) => Expr::zero(),
                ("sign", Some(k)) => Expr::int(k as i128),
                ("dirac", Some(k)) if k != 0 => Expr::zero(),
                _ => app(f, args),
            }
        }
        "zeta" => zeta_exact(&args[0]).unwrap_or_else(|| app(f, args)),
        "polylog" => polylog_exact(&args[0], &args[1]).unwrap_or_else(|| app(f, args)),
        "besselj" | "bessely" | "besseli" => {
            let (n, x) = (&args[0], &args[1]);
            if let Some(k) = int_of(n) {
                if k < 0 {
                    // J_{-n} = (-1)^n J_n, same for Y; I_{-n} = I_n
                    let pos = apply(f, vec![Expr::int(-k), x.clone()]);
                    return if f == "besseli" || k % 2 == 0 { pos } else { neg(pos) };
                }
                if x.is_zero() && f != "bessely" {
                    return if k == 0 { Expr::one() } else { Expr::zero() };
                }
                if x.is_negative_term() && f != "bessely" {
                    // J_n(-x) = (-1)^n J_n(x), same for I
                    let pos = apply(f, vec![n.clone(), neg(x.clone())]);
                    return if k % 2 == 0 { pos } else { neg(pos) };
                }
            }
            app(f, args)
        }
        _ => app(f, args),
    }
}

fn gamma_exact(x: &Expr) -> Option<Expr> {
    let n = x.num().filter(|n| n.is_exact())?;
    if let Some(k) = n.as_int().filter(|_| n.is_integer()) {
        if (1..=1000).contains(&k) {
            return crate::cas::factorial(k - 1).ok();
        }
        return None;
    }
    // half-integers: gamma(k + 1/2) = (2k)! sqrt(pi) / (4^k k!)
    if n.denom() == Some(2) {
        let k2 = n.numer()?; // 2k + 1
        let k = (k2 - 1) / 2;
        let sp = sqrt(Expr::Const(Const::Pi));
        if (0..=500).contains(&k) {
            let mut c = Number::one();
            // (2k)! / (4^k k!) = prod_{j=1..k} (2j - 1)/2
            for j in 1..=k {
                c = c.mul(Number::rat(2 * j - 1, 2).unwrap());
            }
            return Some(mul2(Expr::Num(c), sp));
        }
        if (-500..0).contains(&k) {
            // gamma(1/2 - m) = (-2)^m / (1*3*...*(2m-1)) sqrt(pi)
            let m = -k;
            let mut c = Number::one();
            for j in 1..=m {
                c = c.mul(Number::rat(-2, 2 * j - 1).unwrap());
            }
            return Some(mul2(Expr::Num(c), sp));
        }
    }
    None
}

/// Bernoulli numbers B_0 .. B_m (B_1 = -1/2)
pub fn bernoulli(m: usize) -> Vec<Number> {
    let mut b: Vec<Number> = vec![Number::one()];
    for n in 1..=m {
        // sum_{k=0}^{n} C(n+1, k) B_k = 0
        let mut s = Number::zero();
        let mut c = Number::one(); // C(n+1, 0)
        for (k, bk) in b.iter().enumerate() {
            s = s.add(c.mul(bk));
            c = c
                .mul(Number::int((n + 1 - k) as i128))
                .div(Number::int(k as i128 + 1))
                .unwrap();
        }
        // c is now C(n+1, n)
        b.push(s.neg().div(&c).unwrap());
    }
    b
}

fn zeta_exact(s: &Expr) -> Option<Expr> {
    let k = int_of(s)?;
    if k == 0 {
        return Some(rat(-1, 2));
    }
    if k < 0 {
        if k % 2 == 0 {
            return Some(Expr::zero());
        }
        let n = (-k) as usize;
        if n > 200 {
            return None;
        }
        // zeta(-n) = -B_{n+1} / (n + 1)
        let b = bernoulli(n + 1);
        return Some(Expr::Num(b[n + 1].neg().div(Number::int(n as i128 + 1)).unwrap()));
    }
    if k % 2 == 0 && k <= 200 {
        // zeta(2m) = (-1)^(m+1) B_{2m} (2 pi)^(2m) / (2 (2m)!)
        let n = k as usize;
        let b = bernoulli(n);
        let mut f = Number::one();
        for j in 2..=n {
            f = f.mul(Number::int(j as i128));
        }
        let two_n = Number::int(2).pow(Number::int(k)).unwrap();
        let mut c = b[n].mul(two_n).div(f.mul(Number::int(2))).unwrap();
        if (k / 2) % 2 == 0 {
            c = c.neg();
        }
        return Some(mul2(Expr::Num(c), pow(Expr::Const(Const::Pi), Expr::int(k))));
    }
    None
}

/// Li_s(z): exact for s = 1 and whole s <= 0 (rational functions of z), at
/// z = 0, 1, -1 and Li_2(1/2)
fn polylog_exact(s: &Expr, z: &Expr) -> Option<Expr> {
    if z.is_zero() {
        return Some(Expr::zero());
    }
    let k = int_of(s)?;
    if k == 1 {
        return Some(neg(func(crate::expr::Func::Log, sub(Expr::one(), z.clone()))));
    }
    if k <= 0 {
        if k < -40 {
            return None;
        }
        // Li_0 = z/(1 - z), Li_(n-1) = z d/dz Li_n
        let w = "z__";
        let mut l = div(Expr::sym(w), sub(Expr::one(), Expr::sym(w)));
        for _ in 0..(-k) {
            l = crate::calc::together(&mul2(Expr::sym(w), crate::calc::deriv(&l, w)));
        }
        let (n, d) = crate::calc::num_den(&l);
        let n = crate::cas::factor_poly(&crate::calc::expand(&n)).unwrap_or(n);
        let l = div(n, d);
        return Some(crate::calc::simplify(&crate::calc::subst(&l, w, z)));
    }
    if z.is_one() && k > 1 {
        return Some(apply("zeta", vec![s.clone()]));
    }
    if z.num().is_some_and(|n| n.is_exact() && n.to_f64() == -1.0) && k > 1 {
        // Li_s(-1) = -(1 - 2^(1-s)) zeta(s)
        let c = sub(Expr::one(), pow(int(2), int(1 - k)));
        return Some(crate::calc::simplify(&neg(mul2(c, apply("zeta", vec![s.clone()])))));
    }
    if k == 2 && z.num().is_some_and(|n| n.is_exact() && n.to_f64() == 0.5) {
        let pi2 = pow(Expr::Const(Const::Pi), int(2));
        return Some(sub(div(pi2, int(12)), div(pow(func(crate::expr::Func::Log, int(2)), int(2)), int(2))));
    }
    None
}

/// Li_s(z) for complex z and whole s >= 1 (s = 1 and 2 everywhere, s >= 3
/// for |z| <= 1); other real s only for |z| <= 1/2. (re, im), NaN if unknown.
pub fn polylog_c(s: f64, z: (f64, f64)) -> (f64, f64) {
    use crate::cnum::C;
    let z = C::new(z.0, z.1);
    let nan = (f64::NAN, f64::NAN);
    let r = z.abs();
    let si = (s.fract() == 0.0).then_some(s as i64);
    if r == 0.0 {
        return (0.0, 0.0);
    }
    if si == Some(1) {
        let v = C::new(1.0, 0.0).sub(z).ln().scale(-1.0);
        return (v.re, v.im);
    }
    if si.is_some_and(|k| k <= 0) {
        // z d/dz on the series is not needed: sum the rational function
        let k = -si.unwrap();
        let mut l: Vec<f64> = vec![0.0, 1.0]; // numerator coefficients of Li_0 = z/(1-z): z
        // Li_-n = A_n(z)/(1-z)^(n+1) with Eulerian polynomials A_n
        for n in 1..=k {
            let mut nl = vec![0.0; l.len() + 1];
            for (j, c) in l.iter().enumerate() {
                // z d/dz [c z^j (1-z)^-(n)] = c (j z^j (1-z) + n z^(j+1)) / (1-z)^(n+1)
                nl[j] += c * j as f64;
                nl[j + 1] += c * (n as f64 - j as f64);
            }
            l = nl;
        }
        let mut num = C::new(0.0, 0.0);
        let mut zp = C::new(1.0, 0.0);
        for c in &l {
            num = num.add(zp.scale(*c));
            zp = zp.mul(z);
        }
        let v = num.div(C::new(1.0, 0.0).sub(z).powi(k + 1));
        return (v.re, v.im);
    }
    if r <= 0.5 {
        let mut acc = C::new(0.0, 0.0);
        let mut zp = z;
        for k in 1..400 {
            let t = zp.scale(1.0 / (k as f64).powf(s));
            acc = acc.add(t);
            if t.abs() < 1e-18 * acc.abs().max(1e-300) {
                break;
            }
            zp = zp.mul(z);
        }
        return (acc.re, acc.im);
    }
    let Some(n) = si else { return nan };
    if r > 1.0 + 1e-12 {
        if n == 2 {
            // Li_2(z) = -pi^2/6 - log(-z)^2/2 - Li_2(1/z)
            let w = C::new(1.0, 0.0).div(z);
            let (a, b) = polylog_c(2.0, (w.re, w.im));
            let mut l = z.scale(-1.0).ln();
            if z.im == 0.0 {
                // on the cut z > 1: the value of -integral of log(1 - t)/t with the principal log
                l.im = PI;
            }
            let v = C::new(-PI * PI / 6.0 - a, -b).sub(l.mul(l).scale(0.5));
            return (v.re, v.im);
        }
        return nan;
    }
    // |log z| < 2 pi: Li_n(e^mu) = mu^(n-1)/(n-1)! (H_(n-1) - log(-mu)) + sum_(k != n-1) zeta(n-k) mu^k/k!
    let mu = z.ln();
    if mu.abs() < 1e-15 {
        return (zeta(n as f64), 0.0);
    }
    let zeta_at = |m: i64| -> f64 {
        if m == 1 {
            f64::NAN
        } else if m == 0 {
            -0.5
        } else if m > 1 {
            zeta(m as f64)
        } else if (-m) % 2 == 0 {
            0.0
        } else {
            // zeta(1 - 2j) = (-1)^j 2 (2j-1)! zeta(2j) / (2 pi)^(2j)
            let j = (1 - m) / 2;
            let mut v = 2.0 * zeta(2.0 * j as f64);
            for t in 1..2 * j {
                v *= t as f64 / (2.0 * PI);
            }
            v /= 2.0 * PI;
            if j % 2 == 1 { -v } else { v }
        }
    };
    let mut acc = C::new(0.0, 0.0);
    let mut mk = C::new(1.0, 0.0); // mu^k / k!
    for k in 0..200i64 {
        if k == n - 1 {
            let h: f64 = (1..n).map(|j| 1.0 / j as f64).sum();
            let t = mk.mul(C::new(h, 0.0).sub(mu.scale(-1.0).ln()));
            acc = acc.add(t);
        } else {
            let zk = zeta_at(n - k);
            let t = mk.scale(zk);
            acc = acc.add(t);
            if k > n + 2 && zk != 0.0 && t.abs() < 1e-18 {
                break;
            }
        }
        mk = mk.mul(mu).scale(1.0 / (k + 1) as f64);
    }
    (acc.re, acc.im)
}

// ---------- derivatives ----------

/// d/dx of f(args), with the chain rule; None when there is no rule (zeta)
pub fn deriv(f: &str, args: &[Expr], x: &str) -> Option<Expr> {
    use crate::calc::deriv as d;
    let two_over_sqrt_pi = || div(int(2), sqrt(Expr::Const(Const::Pi)));
    Some(match f {
        "gamma" => mul(vec![
            apply("gamma", args.to_vec()),
            apply("digamma", args.to_vec()),
            d(&args[0], x),
        ]),
        "digamma" => mul2(
            apply("polygamma", vec![Expr::one(), args[0].clone()]),
            d(&args[0], x),
        ),
        "polygamma" => mul2(
            apply("polygamma", vec![add2(args[0].clone(), Expr::one()), args[1].clone()]),
            d(&args[1], x),
        ),
        "heaviside" => mul2(apply("dirac", args.to_vec()), d(&args[0], x)),
        "sign" => mul(vec![int(2), apply("dirac", args.to_vec()), d(&args[0], x)]),
        "dirac" => return None,
        "polylog" => {
            if args[0].contains_sym(x) {
                return None;
            }
            let lower = apply("polylog", vec![sub(args[0].clone(), Expr::one()), args[1].clone()]);
            mul2(div(lower, args[1].clone()), d(&args[1], x))
        }
        "erf" | "erfc" | "erfi" => {
            let u2 = pow(args[0].clone(), int(2));
            let g = if f == "erfi" { exp(u2) } else { exp(neg(u2)) };
            let r = mul(vec![two_over_sqrt_pi(), g, d(&args[0], x)]);
            if f == "erfc" { neg(r) } else { r }
        }
        "besselj" | "bessely" | "besseli" => {
            let (n, u) = (&args[0], &args[1]);
            if n.contains_sym(x) {
                return None;
            }
            let lo = apply(f, vec![sub(n.clone(), Expr::one()), u.clone()]);
            let hi = apply(f, vec![add2(n.clone(), Expr::one()), u.clone()]);
            let s = if f == "besseli" { add2(lo, hi) } else { sub(lo, hi) };
            mul2(div(s, int(2)), d(u, x))
        }
        _ => return None,
    })
}

// ---------- limits at infinity ----------

/// Limit of f(..., u) as u goes to +inf (sign 1) or -inf (sign -1):
/// Some(Some(value)), Some(None) for +inf, or None when unknown
pub fn at_infinity(f: &str, sign: i32) -> Option<Option<Expr>> {
    Some(match (f, sign) {
        ("erf", 1) => Some(Expr::one()),
        ("erf", _) => Some(Expr::int(-1)),
        ("erfc", 1) => Some(Expr::zero()),
        ("erfc", _) => Some(Expr::int(2)),
        ("zeta", 1) => Some(Expr::one()),
        ("heaviside", 1) => Some(Expr::one()),
        ("heaviside", _) => Some(Expr::zero()),
        ("sign", k) => Some(Expr::int(k as i128)),
        ("dirac", _) => Some(Expr::zero()),
        ("besselj" | "bessely", 1) => Some(Expr::zero()),
        ("gamma" | "digamma" | "erfi" | "besseli", 1) => None,
        _ => return None,
    })
}

// ---------- numerics ----------

/// Numeric value; NaN where undefined
pub fn value(f: &str, a: &[f64]) -> f64 {
    match f {
        "gamma" => gamma(a[0]),
        "digamma" => digamma(a[0]),
        "polygamma" => {
            if a[0] == 0.0 {
                digamma(a[1])
            } else if a[0] >= 1.0 && a[0].fract() == 0.0 {
                polygamma(a[0] as u32, a[1])
            } else {
                f64::NAN
            }
        }
        "heaviside" => if a[0] > 0.0 { 1.0 } else if a[0] < 0.0 { 0.0 } else { 0.5 },
        "sign" => if a[0] > 0.0 { 1.0 } else if a[0] < 0.0 { -1.0 } else { 0.0 },
        "dirac" => if a[0] != 0.0 { 0.0 } else { f64::NAN },
        "erf" => erf(a[0]),
        "erfc" => erfc(a[0]),
        "erfi" => erfi(a[0]),
        "zeta" => zeta(a[0]),
        "polylog" => {
            let (re, im) = polylog_c(a[0], (a[1], 0.0));
            if im.abs() <= 1e-14 * re.abs().max(1.0) { re } else { f64::NAN }
        }
        "besselj" | "bessely" | "besseli" => {
            if a[0].fract() != 0.0 || a[0].abs() > 1000.0 {
                return f64::NAN;
            }
            let n = a[0] as i32;
            match f {
                "besselj" => bessel_j(n, a[1]),
                "bessely" => bessel_y(n, a[1]),
                _ => bessel_i(n, a[1]),
            }
        }
        _ => f64::NAN,
    }
}

#[allow(clippy::excessive_precision)]
pub fn gamma(x: f64) -> f64 {
    if x <= 0.0 && x.fract() == 0.0 {
        return f64::NAN;
    }
    if x < 0.5 {
        return PI / ((PI * x).sin() * gamma(1.0 - x));
    }
    if x.fract() == 0.0 && x <= 171.0 {
        return (1..x as u64).fold(1.0, |acc, k| acc * k as f64);
    }
    const G: f64 = 7.0;
    const C: [f64; 9] = [
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7,
    ];
    let x = x - 1.0;
    let mut a = C[0];
    let t = x + G + 0.5;
    for (i, c) in C.iter().enumerate().skip(1) {
        a += c / (x + i as f64);
    }
    // t^(x+0.5) e^-t, split to delay overflow
    let p = t.powf((x + 0.5) / 2.0);
    (2.0 * PI).sqrt() * p * (p * (-t).exp()) * a
}

pub fn digamma(x: f64) -> f64 {
    if !x.is_finite() || (x <= 0.0 && x.fract() == 0.0) {
        return f64::NAN;
    }
    let (mut x, mut r) = (x, 0.0);
    if x < -1e5 {
        // reflection: psi(1 - x) - psi(x) = pi cot(pi x)
        return digamma(1.0 - x) - PI / (PI * x).tan();
    }
    while x < 10.0 {
        r -= 1.0 / x;
        x += 1.0;
    }
    let x2 = 1.0 / (x * x);
    // asymptotic series with Bernoulli numbers
    let s = x2
        * (1.0 / 12.0
            - x2 * (1.0 / 120.0
                - x2 * (1.0 / 252.0
                    - x2 * (1.0 / 240.0 - x2 * (1.0 / 132.0 - x2 * (691.0 / 32760.0 - x2 / 12.0))))));
    r + x.ln() - 0.5 / x - s
}

pub fn polygamma(n: u32, x: f64) -> f64 {
    if !x.is_finite() || (x <= 0.0 && x.fract() == 0.0) || n > 60 {
        return f64::NAN;
    }
    let fact = |k: u32| (1..=k).fold(1.0f64, |a, j| a * j as f64);
    let sign = if n % 2 == 1 { 1.0 } else { -1.0 }; // (-1)^(n+1)
    let (mut x, mut r) = (x, 0.0);
    let nf = fact(n);
    while x < 20.0 + n as f64 {
        r += sign * nf / x.powi(n as i32 + 1);
        x += 1.0;
    }
    // (-1)^(n+1) [ (n-1)!/x^n + n!/(2 x^(n+1)) + sum B_2k (2k+n-1)! / ((2k)! x^(2k+n)) ]
    let b2k = [1.0 / 6.0, -1.0 / 30.0, 1.0 / 42.0, -1.0 / 30.0, 5.0 / 66.0, -691.0 / 2730.0, 7.0 / 6.0];
    let mut s = fact(n - 1) / x.powi(n as i32) + nf / (2.0 * x.powi(n as i32 + 1));
    for (k, b) in b2k.iter().enumerate() {
        let k = k as u32 + 1;
        s += b * fact(2 * k + n - 1) / (fact(2 * k) * x.powi((2 * k + n) as i32));
    }
    r + sign * s
}

pub fn erf(x: f64) -> f64 {
    if x.is_nan() {
        return f64::NAN;
    }
    if x.abs() >= 3.0 {
        return x.signum() * (1.0 - erfc(x.abs()));
    }
    // 2/sqrt(pi) e^(-x^2) sum 2^n x^(2n+1) / (1*3*...*(2n+1)): positive terms
    let (mut t, mut s) = (x, x);
    let x2 = x * x;
    for n in 1..200 {
        t *= 2.0 * x2 / (2 * n + 1) as f64;
        s += t;
        if t.abs() < 1e-17 * s.abs() {
            break;
        }
    }
    2.0 / PI.sqrt() * (-x2).exp() * s
}

pub fn erfc(x: f64) -> f64 {
    if x.is_nan() {
        return f64::NAN;
    }
    if x < 2.0 {
        return 1.0 - erf(x);
    }
    if x > 27.0 {
        return 0.0;
    }
    // continued fraction x + (1/2)/(x + 1/(x + (3/2)/(x + ...)))
    let mut t = x;
    for k in (1..=300).rev() {
        t = x + (k as f64 / 2.0) / t;
    }
    (-x * x).exp() / (PI.sqrt() * t)
}

pub fn erfi(x: f64) -> f64 {
    // 2/sqrt(pi) sum x^(2n+1) / (n! (2n+1))
    let x2 = x * x;
    let (mut p, mut s) = (x, x);
    for n in 1..500 {
        p *= x2 / n as f64;
        let t = p / (2 * n + 1) as f64;
        s += t;
        if t.abs() < 1e-17 * s.abs() {
            break;
        }
    }
    2.0 / PI.sqrt() * s
}

pub fn zeta(s: f64) -> f64 {
    if s == 1.0 || s.is_nan() {
        return f64::NAN;
    }
    if s < 0.5 {
        if s.fract() == 0.0 && (s as i64) % 2 == 0 {
            return 0.0; // trivial zeros
        }
        // functional equation
        return 2f64.powf(s) * PI.powf(s - 1.0) * (PI * s / 2.0).sin() * gamma(1.0 - s) * zeta(1.0 - s);
    }
    if s > 60.0 {
        return 1.0 + 2f64.powf(-s) + 3f64.powf(-s);
    }
    // Borwein's algorithm on the alternating series
    let n = 50usize;
    let mut d = vec![0.0f64; n + 1];
    let mut term = 1.0f64;
    let mut acc = 1.0f64;
    d[0] = 1.0;
    for i in 1..=n {
        let (nf, i_f) = (n as f64, i as f64);
        term *= (nf + i_f - 1.0) * 4.0 * (nf - i_f + 1.0) / ((2.0 * i_f) * (2.0 * i_f - 1.0));
        acc += term;
        d[i] = acc;
    }
    let mut sum = 0.0;
    for k in 0..n {
        let sgn = if k % 2 == 0 { 1.0 } else { -1.0 };
        sum += sgn * (d[k] - d[n]) / ((k + 1) as f64).powf(s);
    }
    -sum / (d[n] * (1.0 - 2f64.powf(1.0 - s)))
}

/// J_n(x) by its power series when that has no cancellation, else by the
/// trapezoid rule on Bessel's integral (exponentially accurate on a full period)
pub fn bessel_j(n: i32, x: f64) -> f64 {
    if !x.is_finite() {
        return if x.is_nan() { f64::NAN } else { 0.0 };
    }
    if n < 0 {
        let v = bessel_j(-n, x);
        return if n % 2 == 0 { v } else { -v };
    }
    let h = x / 2.0;
    if h * h < (n + 1) as f64 || x.abs() < 1.0 {
        let mut t = h.powi(n) / (1..=n).fold(1.0f64, |a, k| a * k as f64);
        let mut s = t;
        for k in 1..300 {
            t *= -h * h / (k as f64 * (k + n) as f64);
            s += t;
            if t.abs() < 1e-17 * s.abs().max(1e-300) {
                break;
            }
        }
        return s;
    }
    let m = 2 * (x.abs().ceil() as usize + n as usize) + 64;
    let mut s = 0.0;
    for j in 0..m {
        let t = 2.0 * PI * j as f64 / m as f64;
        s += (n as f64 * t - x * t.sin()).cos();
    }
    s / m as f64
}

/// Y_n(x), x > 0, from its integral representation
pub fn bessel_y(n: i32, x: f64) -> f64 {
    if x.is_nan() || x <= 0.0 {
        return f64::NAN;
    }
    if x.is_infinite() {
        return 0.0;
    }
    if n < 0 {
        let v = bessel_y(-n, x);
        return if n % 2 == 0 { v } else { -v };
    }
    let nf = n as f64;
    let a = crate::cas::quad(&|t: f64| (x * t.sin() - nf * t).sin(), 0.0, PI).unwrap_or(f64::NAN);
    // upper limit where e^(nt - x sinh t) is negligible
    let mut top = 1.0;
    while nf * top - x * top.sinh() > -50.0 && top < 50.0 {
        top += 0.5;
    }
    let sg = if n % 2 == 0 { 1.0 } else { -1.0 };
    let b = crate::cas::quad(
        &|t: f64| (nf * t - x * t.sinh()).exp() + sg * (-nf * t - x * t.sinh()).exp(),
        0.0,
        top,
    )
    .unwrap_or(f64::NAN);
    (a - b) / PI
}

/// I_n(x) by its power series (all terms positive)
pub fn bessel_i(n: i32, x: f64) -> f64 {
    let n = n.abs();
    if !x.is_finite() {
        return f64::NAN;
    }
    if x < 0.0 {
        let v = bessel_i(n, -x);
        return if n % 2 == 0 { v } else { -v };
    }
    if x > 700.0 {
        return f64::INFINITY;
    }
    let h = x / 2.0;
    // work in logs for the first term so large n and x don't overflow early
    let lf: f64 = (1..=n).map(|k| (k as f64).ln()).sum();
    let mut t = if x == 0.0 {
        if n == 0 { 1.0 } else { 0.0 }
    } else {
        (n as f64 * h.ln() - lf).exp()
    };
    let mut s = t;
    for k in 1..2000 {
        t *= h * h / (k as f64 * (k + n) as f64);
        s += t;
        if t < 1e-17 * s && k as f64 > h {
            break;
        }
    }
    s
}

// ---------- orthogonal polynomials ----------

/// legendre, hermite (physicists'), laguerre, chebyshev (first kind, T) and
/// chebyshevu (second kind) of degree n in x, from their three-term recurrences
pub fn orthopoly(kind: &str, n: usize, x: &Expr) -> Option<Expr> {
    let xv = x.clone();
    let (p0, p1) = match kind {
        "legendre" | "chebyshev" => (Expr::one(), xv.clone()),
        "hermite" | "chebyshevu" => (Expr::one(), mul2(int(2), xv.clone())),
        "laguerre" => (Expr::one(), sub(Expr::one(), xv.clone())),
        _ => return None,
    };
    if n == 0 {
        return Some(p0);
    }
    let (mut a, mut b) = (p0, p1);
    for k in 1..n {
        let kk = k as i128;
        let next = match kind {
            // (k+1) P_{k+1} = (2k+1) x P_k - k P_{k-1}
            "legendre" => div(
                sub(mul(vec![int(2 * kk + 1), xv.clone(), b.clone()]), mul2(int(kk), a.clone())),
                int(kk + 1),
            ),
            // H_{k+1} = 2x H_k - 2k H_{k-1}
            "hermite" => sub(mul(vec![int(2), xv.clone(), b.clone()]), mul2(int(2 * kk), a.clone())),
            // (k+1) L_{k+1} = (2k+1-x) L_k - k L_{k-1}
            "laguerre" => div(
                sub(
                    mul2(sub(int(2 * kk + 1), xv.clone()), b.clone()),
                    mul2(int(kk), a.clone()),
                ),
                int(kk + 1),
            ),
            // T_{k+1} = 2x T_k - T_{k-1}, same for U
            _ => sub(mul(vec![int(2), xv.clone(), b.clone()]), a.clone()),
        };
        a = b;
        b = crate::calc::expand(&next);
    }
    Some(crate::calc::expand(&b))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn close(a: f64, b: f64, tol: f64) -> bool {
        (a - b).abs() <= tol * b.abs().max(1e-300) || (a - b).abs() < 1e-300
    }

    #[test]
    #[allow(clippy::approx_constant)] // digamma(1) = -EULER_GAMMA, written out as a literal
    fn values() {
        assert!(close(gamma(0.5), PI.sqrt(), 1e-14));
        assert!(close(gamma(4.5), 11.631_728_396_567_45, 1e-13));
        assert!(close(gamma(-1.5), 2.363_271_801_207_355, 1e-13));
        assert!(close(digamma(1.0), -0.5772156649015329, 1e-14));
        assert!(close(polygamma(1, 1.0), PI * PI / 6.0, 1e-13));
        assert!(close(erf(0.5), 0.5204998778130465, 1e-14));
        assert!(close(erf(2.5), 0.999_593_047_982_555, 1e-15));
        assert!(close(erfc(3.0), 2.209049699858544e-5, 1e-12));
        assert!(close(erfc(5.0), 1.537_459_794_428_035e-12, 1e-11));
        assert!(close(erfi(1.0), 1.650_425_758_797_543, 1e-14));
        assert!(close(zeta(3.0), 1.2020569031595942, 1e-14));
        assert!(close(zeta(0.5), -1.4603545088095868, 1e-13));
        assert!(close(zeta(-1.0), -1.0 / 12.0, 1e-13));
        assert!(bessel_j(0, 2.404825557695773).abs() < 1e-14);
        assert!(close(bessel_j(1, 1.0), 0.44005058574493355, 1e-14));
        assert!(close(bessel_j(0, 30.0), -0.0863679835810403, 1e-12));
        assert!(close(bessel_j(5, 1.0), 2.497_577_302_112_34e-4, 1e-13));
        assert!(close(bessel_y(0, 1.0), 0.08825696421567697, 1e-10));
        assert!(close(bessel_y(2, 3.0), -0.16040039348492377, 1e-10));
        assert!(close(bessel_i(1, 2.0), 1.590_636_854_637_329, 1e-14));
    }

    #[test]
    fn bernoulli_numbers() {
        let b = bernoulli(12);
        assert_eq!(b[1], Number::rat(-1, 2).unwrap());
        assert_eq!(b[2], Number::rat(1, 6).unwrap());
        assert_eq!(b[12], Number::rat(-691, 2730).unwrap());
    }
}
