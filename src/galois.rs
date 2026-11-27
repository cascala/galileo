//! Galois groups of irreducible polynomials over Q of degree up to 5.
//!
//! Degree 3: the discriminant decides A3 or S3. Degree 4: the resolvent cubic
//! and the discriminant (Kappe–Warren separates C4 from D4). Degree 5: the
//! discriminant and the cycle types of Frobenius (how f factors mod p) at many
//! primes; by Chebotarev every cycle type of the group shows up with a fixed
//! positive density, so a type that never appears marks a smaller group.

use crate::fgroup::{self as fg, Group, Perm};
use crate::modsolve as ms;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, Zero};

pub type R<T> = Result<T, String>;
type Q = BigRational;

/// primes used for Frobenius cycle types
const PRIMES: usize = 300;

pub struct Galois {
    pub group: Group,
    pub disc: BigInt,
    pub disc_square: bool,
    /// resolvent cubic (degree 4), low to high
    pub resolvent: Option<Vec<BigInt>>,
    /// cycle types seen, with counts (degree 5)
    pub cycles: Vec<(Vec<usize>, usize)>,
    pub method: String,
}

fn det(mut m: Vec<Vec<Q>>) -> Q {
    let n = m.len();
    let mut d = Q::one();
    for c in 0..n {
        let Some(p) = (c..n).find(|&i| !m[i][c].is_zero()) else {
            return Q::zero();
        };
        if p != c {
            m.swap(p, c);
            d = -d;
        }
        d *= &m[c][c];
        for i in c + 1..n {
            let k = &m[i][c] / &m[c][c];
            for j in c..n {
                let t = &m[c][j] * &k;
                m[i][j] -= t;
            }
        }
    }
    d
}

/// discriminant of a polynomial with integer coefficients (low to high)
pub fn discriminant(f: &[BigInt]) -> BigInt {
    let n = f.len() - 1;
    let df: Vec<BigInt> = (1..=n).map(|i| &f[i] * BigInt::from(i)).collect();
    // Sylvester matrix of f (degree n) and f' (degree n-1): size 2n-1
    let size = 2 * n - 1;
    let mut m = vec![vec![Q::zero(); size]; size];
    for r in 0..n - 1 {
        for (i, c) in f.iter().rev().enumerate() {
            m[r][r + i] = Q::from_integer(c.clone());
        }
    }
    for r in 0..n {
        for (i, c) in df.iter().rev().enumerate() {
            m[n - 1 + r][r + i] = Q::from_integer(c.clone());
        }
    }
    let res = det(m).to_integer();
    let sign = if (n * (n - 1) / 2) % 2 == 1 { -1 } else { 1 };
    sign * res / &f[n]
}

fn is_square(n: &BigInt) -> bool {
    !n.is_negative() && {
        let r = crate::numtheory::isqrt(n);
        &r * &r == *n
    }
}

/// is the rational a/b a square (with b > 0)
fn is_square_q(q: &Q) -> bool {
    is_square(&(q.numer() * q.denom()))
}

/// degrees of the irreducible factors of f mod p (f squarefree mod p)
fn cycle_type(f: &[BigInt], p: &BigInt) -> Vec<usize> {
    let mut g = ms::reduce(f, p);
    let x = vec![BigInt::zero(), BigInt::one()];
    let mut xp = x.clone();
    let mut out = vec![];
    let mut d = 0;
    while g.len() > 1 {
        d += 1;
        if 2 * d > g.len() - 1 {
            out.push(g.len() - 1);
            break;
        }
        xp = ms::powmod(&xp, p, &g, p);
        let h = ms::gcd(&ms::sub(&xp, &x, p), &g, p);
        let k = (h.len() - 1) / d;
        for _ in 0..k {
            out.push(d);
        }
        if k > 0 {
            g = ms::monic(ms::quo(&g, &h, p), p);
            xp = ms::rem(&xp, &g, p);
        }
    }
    out.sort();
    out
}

fn perm(v: &[u16]) -> Perm {
    v.iter().map(|k| k - 1).collect()
}

/// the Galois group of an irreducible integer polynomial (low to high)
pub fn galois(f: &[BigInt]) -> R<Galois> {
    let n = f.len() - 1;
    let disc = discriminant(f);
    let sq = is_square(&disc);
    let mut out = Galois {
        group: fg::cyclic(1)?,
        disc: disc.clone(),
        disc_square: sq,
        resolvent: None,
        cycles: vec![],
        method: String::new(),
    };
    match n {
        1 => {
            out.group = Group::new("group(\"trivial\")", 1, vec![vec![0]])?;
            out.method = "A linear polynomial has its root in Q.".into();
        }
        2 => {
            out.group = fg::sym(2)?;
            out.method = "The two roots are swapped by conjugation.".into();
        }
        3 => {
            out.group = if sq { fg::alt(3)? } else { fg::sym(3)? };
            out.method = format!(
                "The discriminant {} is {}a square, so the group {} inside A3.",
                disc,
                if sq { "" } else { "not " },
                if sq { "lies" } else { "does not lie" }
            );
        }
        4 => {
            // monic form: x^4 + a x^3 + b x^2 + c x + d
            let lc = Q::from_integer(f[4].clone());
            let c = |i: usize| Q::from_integer(f[i].clone()) / &lc;
            let (a, b, cc, d) = (c(3), c(2), c(1), c(0));
            let four = Q::from_integer(4.into());
            // resolvent cubic with roots r1 r2 + r3 r4, ...
            let r: Vec<Q> = vec![
                -(&a * &a * &d - &four * &b * &d + &cc * &cc),
                &a * &cc - &four * &d,
                -b.clone(),
                Q::one(),
            ];
            let den = r.iter().fold(BigInt::one(), |acc, q| acc.lcm(q.denom()));
            let rz: Vec<BigInt> = r
                .iter()
                .map(|q| (q * Q::from_integer(den.clone())).to_integer())
                .collect();
            let roots = rational_roots(&r);
            out.resolvent = Some(rz);
            let rs = format!(
                "the resolvent cubic (roots r1 r2 + r3 r4, r1 r3 + r2 r4, r1 r4 + r2 r3) has {} rational root{}",
                roots.len(),
                if roots.len() == 1 { "" } else { "s" }
            );
            let ds = format!(
                "the discriminant {} is {}a square",
                disc,
                if sq { "" } else { "not " }
            );
            match roots.len() {
                0 => {
                    out.group = if sq { fg::alt(4)? } else { fg::sym(4)? };
                    out.method = format!("{}, and {}.", cap(&rs), ds);
                }
                1 => {
                    let t = &roots[0];
                    let dq = Q::from_integer(disc.clone());
                    let splits = |delta: Q| {
                        delta.is_zero() || is_square_q(&delta) || is_square_q(&(delta * &dq))
                    };
                    let c4 = splits(t * t - &four * &d) && splits(&a * &a - &four * (&b - t));
                    out.group = if c4 { fg::cyclic(4)? } else { fg::dihedral(4)? };
                    out.method = format!(
                        "{}, {}; {} and {} {} split over Q(sqrt({})) (Kappe–Warren), so the group is {}.",
                        cap(&rs),
                        t,
                        sh(&[d.clone(), -t.clone(), Q::one()]),
                        sh(&[&b - t, a.clone(), Q::one()]),
                        if c4 { "both" } else { "do not both" },
                        disc,
                        if c4 {
                            "cyclic of order 4"
                        } else {
                            "dihedral of order 8"
                        }
                    );
                }
                _ => {
                    out.group = Group::new(
                        "group(\"V4\")",
                        4,
                        vec![perm(&[2, 1, 4, 3]), perm(&[3, 4, 1, 2])],
                    )?;
                    out.method = format!("{}, so the group is the Klein four-group.", cap(&rs));
                }
            }
        }
        5 => {
            let mut counts: std::collections::BTreeMap<Vec<usize>, usize> = Default::default();
            let mut used = 0;
            let lead = &f[5];
            for p in crate::numtheory::small::primes_upto(20000) {
                let pb = BigInt::from(p);
                if (lead % &pb).is_zero() || (&disc % &pb).is_zero() {
                    continue;
                }
                *counts.entry(cycle_type(f, &pb)).or_default() += 1;
                used += 1;
                if used == PRIMES {
                    break;
                }
            }
            let has = |t: &[usize]| counts.contains_key(t);
            let three = has(&[1, 1, 3]) || has(&[2, 3]);
            let four = has(&[1, 4]);
            let two2 = has(&[1, 2, 2]);
            out.group = if three {
                if sq { fg::alt(5)? } else { fg::sym(5)? }
            } else if four || !sq {
                Group::from_gens(5, vec![perm(&[2, 3, 4, 5, 1]), perm(&[1, 3, 5, 2, 4])])?
            } else if two2 {
                fg::dihedral(5)?
            } else {
                fg::cyclic(5)?
            };
            out.method = format!(
                "The discriminant {} is {}a square. How f factors mod p gives the cycle type of a Frobenius element; at {} primes the types were {}. By Chebotarev each cycle type of the group appears for a fixed share of the primes (at least 1 in 6 here), so the types that never appeared mark which group it is.",
                disc,
                if sq { "" } else { "not " },
                used,
                counts
                    .iter()
                    .map(|(t, k)| format!(
                        "({}) {} times",
                        t.iter()
                            .map(|d| d.to_string())
                            .collect::<Vec<_>>()
                            .join(" "),
                        k
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            out.cycles = counts.into_iter().collect();
        }
        _ => return Err("galois: degree up to 5".into()),
    }
    Ok(out)
}

fn cap(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// distinct rational roots of a polynomial with rational coefficients (low to high)
fn rational_roots(r: &[Q]) -> Vec<Q> {
    let den = r.iter().fold(BigInt::one(), |acc, q| acc.lcm(q.denom()));
    let z: Vec<BigInt> = r
        .iter()
        .map(|q| (q * Q::from_integer(den.clone())).to_integer())
        .collect();
    let mut out = vec![];
    if let Some((_, fs)) = crate::algfactor::factor_zpoly(&z) {
        for (g, _) in fs {
            if g.len() == 2 {
                out.push(Q::new(-g[0].clone(), g[1].clone()));
            }
        }
    }
    if z[0].is_zero() && !out.iter().any(|q: &Q| q.is_zero()) {
        out.push(Q::zero());
    }
    out.sort();
    out.dedup();
    out
}

fn sh(v: &[Q]) -> String {
    crate::print::show(&crate::qring::poly_expr(v, "x"))
}
