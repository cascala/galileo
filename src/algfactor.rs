//! Algebraic factoring.
//!
//! * Polynomials over the integers: square-free part, then Zassenhaus
//!   (factor mod a small prime by Cantor–Zassenhaus, lift with Hensel's
//!   lemma, recombine the lifted factors). x^n ± 1 goes straight to its
//!   cyclotomic factors.
//! * Cyclotomic polynomials Φ_n(x).
//! * Aurifeuillean factors: Φ_n(s x^2) = ±P(x)P(-x) for squarefree s and the
//!   right n, with P computed exactly in Z[ω], ω a primitive 2n-th root of 1.
//! * Whole numbers a^n ± b^n are cut into their algebraic pieces (values of
//!   cyclotomic polynomials, split again by Aurifeuillean factors) before the
//!   numeric methods of [`crate::numtheory`] work on each piece.

use crate::numtheory as nt;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;

/// Coefficients from x^0 up; no trailing zeros, so the zero polynomial is empty.
pub type ZPoly = Vec<BigInt>;

// ---------- integer polynomials ----------

pub fn trim(mut p: ZPoly) -> ZPoly {
    while p.last().is_some_and(|c| c.is_zero()) {
        p.pop();
    }
    p
}

fn deg(p: &[BigInt]) -> usize {
    p.len().saturating_sub(1)
}

pub fn pmul(a: &[BigInt], b: &[BigInt]) -> ZPoly {
    if a.is_empty() || b.is_empty() {
        return vec![];
    }
    let mut r = vec![BigInt::zero(); a.len() + b.len() - 1];
    for (i, x) in a.iter().enumerate() {
        if x.is_zero() {
            continue;
        }
        for (j, y) in b.iter().enumerate() {
            if !y.is_zero() {
                r[i + j] += x * y;
            }
        }
    }
    trim(r)
}

fn psub(a: &[BigInt], b: &[BigInt]) -> ZPoly {
    let mut r = vec![BigInt::zero(); a.len().max(b.len())];
    for (i, x) in a.iter().enumerate() {
        r[i] += x;
    }
    for (i, x) in b.iter().enumerate() {
        r[i] -= x;
    }
    trim(r)
}

/// a / b when b divides a exactly over the integers
pub fn pdivexact(a: &[BigInt], b: &[BigInt]) -> Option<ZPoly> {
    if b.is_empty() {
        return None;
    }
    if a.len() < b.len() {
        return if a.is_empty() { Some(vec![]) } else { None };
    }
    let mut r = a.to_vec();
    let lb = b.last().unwrap();
    let mut q = vec![BigInt::zero(); a.len() - b.len() + 1];
    for i in (0..q.len()).rev() {
        let top = &r[i + b.len() - 1];
        if top.is_zero() {
            continue;
        }
        let (c, rem) = top.div_rem(lb);
        if !rem.is_zero() {
            return None;
        }
        for (j, y) in b.iter().enumerate() {
            r[i + j] -= &c * y;
        }
        q[i] = c;
    }
    if r.iter().any(|c| !c.is_zero()) {
        return None;
    }
    Some(trim(q))
}

/// gcd of the coefficients, with the sign of the leading coefficient
fn content(p: &[BigInt]) -> BigInt {
    let mut g = BigInt::zero();
    for c in p {
        g = g.gcd(c);
    }
    if p.last().is_some_and(|c| c.is_negative()) {
        -g
    } else {
        g
    }
}

/// p divided by its content: leading coefficient positive
pub fn primitive(p: &[BigInt]) -> ZPoly {
    let c = content(p);
    if c.is_zero() {
        return vec![];
    }
    p.iter().map(|x| x / &c).collect()
}

fn pderiv(p: &[BigInt]) -> ZPoly {
    trim(
        p.iter()
            .enumerate()
            .skip(1)
            .map(|(k, c)| c * BigInt::from(k))
            .collect(),
    )
}

pub fn peval(p: &[BigInt], x: &BigInt) -> BigInt {
    p.iter().rev().fold(BigInt::zero(), |acc, c| acc * x + c)
}

/// p(-x)
fn pneg_x(p: &[BigInt]) -> ZPoly {
    p.iter()
        .enumerate()
        .map(|(k, c)| if k % 2 == 1 { -c } else { c.clone() })
        .collect()
}

/// p(x^k)
fn pcompose_pow(p: &[BigInt], k: usize) -> ZPoly {
    if p.is_empty() {
        return vec![];
    }
    let mut r = vec![BigInt::zero(); deg(p) * k + 1];
    for (i, c) in p.iter().enumerate() {
        r[i * k] = c.clone();
    }
    r
}

/// gcd over the integers (primitive remainder sequence), primitive result
fn pgcd(a: &[BigInt], b: &[BigInt]) -> ZPoly {
    let (mut a, mut b) = (primitive(a), primitive(b));
    if a.len() < b.len() {
        std::mem::swap(&mut a, &mut b);
    }
    while !b.is_empty() {
        // pseudo-remainder of a by b
        let mut r = a.clone();
        let lb = b.last().unwrap().clone();
        while r.len() >= b.len() && !r.is_empty() {
            let lr = r.last().unwrap().clone();
            let shift = r.len() - b.len();
            for c in r.iter_mut() {
                *c *= &lb;
            }
            for (j, y) in b.iter().enumerate() {
                r[shift + j] -= &lr * y;
            }
            r = trim(r);
        }
        a = b;
        b = primitive(&r);
    }
    a
}

// ---------- cyclotomic polynomials ----------

fn small_factor(n: u64) -> Vec<(u64, u32)> {
    nt::small::factorize(n as u128)
        .unwrap_or_default()
        .into_iter()
        .map(|(p, e)| (p as u64, e))
        .collect()
}

pub fn divisors_u64(n: u64) -> Vec<u64> {
    let mut ds = vec![1u64];
    for (p, e) in small_factor(n) {
        let k = ds.len();
        let mut pk = 1;
        for _ in 0..e {
            pk *= p;
            for i in 0..k {
                ds.push(ds[i] * pk);
            }
        }
    }
    ds.sort();
    ds
}

pub fn totient_u64(n: u64) -> u64 {
    small_factor(n)
        .iter()
        .fold(n, |acc, (p, _)| acc / p * (p - 1))
}

fn mobius(n: u64) -> i32 {
    let f = small_factor(n);
    if f.iter().any(|(_, e)| *e > 1) {
        0
    } else if f.len() % 2 == 0 {
        1
    } else {
        -1
    }
}

/// Φ_n(x)
pub fn cyclotomic(n: u64) -> ZPoly {
    if n == 1 {
        return vec![BigInt::from(-1), BigInt::one()];
    }
    let f = small_factor(n);
    // Φ_{p1 p2 ...}: Φ_{mp}(x) = Φ_m(x^p) / Φ_m(x)
    let mut phi: ZPoly = vec![BigInt::from(-1), BigInt::one()];
    let mut rad = 1u64;
    for (p, _) in &f {
        let up = pcompose_pow(&phi, *p as usize);
        phi = pdivexact(&up, &phi).expect("cyclotomic division");
        rad *= p;
    }
    pcompose_pow(&phi, (n / rad) as usize)
}

/// Φ_d(a, b) = b^φ(d) Φ_d(a/b) as a number, from Φ_d = ∏_{e|d} (x^e - 1)^μ(d/e)
pub fn cyclotomic_value(d: u64, a: &BigInt, b: &BigInt) -> BigInt {
    let (mut num, mut den) = (BigInt::one(), BigInt::one());
    for e in divisors_u64(d) {
        let m = mobius(d / e);
        if m == 0 {
            continue;
        }
        let v = num_traits::pow(a.clone(), e as usize) - num_traits::pow(b.clone(), e as usize);
        if m == 1 {
            num *= v;
        } else {
            den *= v;
        }
    }
    num / den
}

// ---------- polynomials mod a small prime ----------

type MPoly = Vec<u64>;

fn mtrim(mut p: MPoly) -> MPoly {
    while p.last() == Some(&0) {
        p.pop();
    }
    p
}

fn mpow(mut a: u64, mut e: u64, p: u64) -> u64 {
    let mut r = 1;
    a %= p;
    while e > 0 {
        if e & 1 == 1 {
            r = r * a % p;
        }
        a = a * a % p;
        e >>= 1;
    }
    r
}

fn minv(a: u64, p: u64) -> u64 {
    mpow(a, p - 2, p)
}

fn mreduce(f: &[BigInt], p: u64) -> MPoly {
    let pb = BigInt::from(p);
    mtrim(
        f.iter()
            .map(|c| c.mod_floor(&pb).to_u64().unwrap())
            .collect(),
    )
}

fn mmul(a: &[u64], b: &[u64], p: u64) -> MPoly {
    if a.is_empty() || b.is_empty() {
        return vec![];
    }
    let mut r = vec![0u64; a.len() + b.len() - 1];
    for (i, &x) in a.iter().enumerate() {
        if x == 0 {
            continue;
        }
        for (j, &y) in b.iter().enumerate() {
            r[i + j] = (r[i + j] + x * y) % p;
        }
    }
    mtrim(r)
}

fn msub(a: &[u64], b: &[u64], p: u64) -> MPoly {
    let mut r = vec![0u64; a.len().max(b.len())];
    for (i, &x) in a.iter().enumerate() {
        r[i] = x;
    }
    for (i, &x) in b.iter().enumerate() {
        r[i] = (r[i] + p - x) % p;
    }
    mtrim(r)
}

fn madd(a: &[u64], b: &[u64], p: u64) -> MPoly {
    let mut r = vec![0u64; a.len().max(b.len())];
    for (i, &x) in a.iter().enumerate() {
        r[i] = x;
    }
    for (i, &x) in b.iter().enumerate() {
        r[i] = (r[i] + x) % p;
    }
    mtrim(r)
}

fn mdivrem(a: &[u64], b: &[u64], p: u64) -> (MPoly, MPoly) {
    let mut r = a.to_vec();
    if r.len() < b.len() {
        return (vec![], r);
    }
    let inv = minv(*b.last().unwrap(), p);
    let mut q = vec![0u64; a.len() - b.len() + 1];
    for i in (0..q.len()).rev() {
        let c = r[i + b.len() - 1] * inv % p;
        if c == 0 {
            continue;
        }
        q[i] = c;
        for (j, &y) in b.iter().enumerate() {
            r[i + j] = (r[i + j] + p - c * y % p) % p;
        }
    }
    (mtrim(q), mtrim(r))
}

fn mmonic(a: &[u64], p: u64) -> MPoly {
    match a.last() {
        None => vec![],
        Some(&l) => {
            let inv = minv(l, p);
            a.iter().map(|c| c * inv % p).collect()
        }
    }
}

fn mgcd(a: &[u64], b: &[u64], p: u64) -> MPoly {
    let (mut a, mut b) = (a.to_vec(), b.to_vec());
    while !b.is_empty() {
        let r = mdivrem(&a, &b, p).1;
        a = b;
        b = r;
    }
    mmonic(&a, p)
}

/// s, t with s a + t b = 1 (a, b coprime)
fn mextgcd(a: &[u64], b: &[u64], p: u64) -> (MPoly, MPoly) {
    let (mut r0, mut r1) = (a.to_vec(), b.to_vec());
    let (mut s0, mut s1): (MPoly, MPoly) = (vec![1], vec![]);
    let (mut t0, mut t1): (MPoly, MPoly) = (vec![], vec![1]);
    while !r1.is_empty() {
        let (q, r) = mdivrem(&r0, &r1, p);
        r0 = std::mem::replace(&mut r1, r);
        let s = msub(&s0, &mmul(&q, &s1, p), p);
        s0 = std::mem::replace(&mut s1, s);
        let t = msub(&t0, &mmul(&q, &t1, p), p);
        t0 = std::mem::replace(&mut t1, t);
    }
    // r0 is a nonzero constant
    let inv = minv(r0[0], p);
    (
        s0.iter().map(|c| c * inv % p).collect(),
        t0.iter().map(|c| c * inv % p).collect(),
    )
}

fn mpowmod(base: &[u64], e: &BigInt, f: &[u64], p: u64) -> MPoly {
    let mut r: MPoly = vec![1];
    let b = mdivrem(base, f, p).1;
    for i in (0..e.bits()).rev() {
        r = mdivrem(&mmul(&r, &r, p), f, p).1;
        if e.bit(i) {
            r = mdivrem(&mmul(&r, &b, p), f, p).1;
        }
    }
    r
}

/// distinct-degree factorization of a monic square-free f: (product of the
/// irreducible factors of degree d, d)
fn ddf(f: &[u64], p: u64) -> Vec<(MPoly, usize)> {
    let mut out = vec![];
    let mut f = f.to_vec();
    let x: MPoly = vec![0, 1];
    let mut h = x.clone();
    let pb = BigInt::from(p);
    let mut d = 0;
    while 2 * (d + 1) <= deg(&bigs(&f)) {
        d += 1;
        h = mpowmod(&h, &pb, &f, p);
        let g = mgcd(&f, &msub(&h, &x, p), p);
        if g.len() > 1 {
            f = mdivrem(&f, &g, p).0;
            h = mdivrem(&h, &f, p).1;
            out.push((g, d));
        }
    }
    if f.len() > 1 {
        let df = f.len() - 1;
        out.push((f, df));
    }
    out
}

fn bigs(f: &[u64]) -> ZPoly {
    f.iter().map(|&c| BigInt::from(c)).collect()
}

/// equal-degree factorization (Cantor–Zassenhaus, odd p): monic irreducible factors of degree d
fn edf(f: &[u64], d: usize, p: u64, seed: &mut u64) -> Vec<MPoly> {
    let n = f.len() - 1;
    if n == d {
        return vec![f.to_vec()];
    }
    let e = if p == 2 {
        BigInt::zero()
    } else {
        (num_traits::pow(BigInt::from(p), d) - 1u32) / 2u32
    };
    loop {
        let a: MPoly = mtrim(
            (0..n)
                .map(|_| {
                    *seed ^= *seed << 13;
                    *seed ^= *seed >> 7;
                    *seed ^= *seed << 17;
                    *seed % p
                })
                .collect(),
        );
        if a.len() < 2 {
            continue;
        }
        let b = if p == 2 {
            // trace map a + a^2 + a^4 + ... + a^(2^(d-1))
            let mut t = a.clone();
            let mut sq = a.clone();
            for _ in 1..d {
                sq = mdivrem(&mmul(&sq, &sq, p), f, p).1;
                t = madd(&t, &sq, p);
            }
            t
        } else {
            msub(&mpowmod(&a, &e, f, p), &[1], p)
        };
        let g = mgcd(f, &b, p);
        if g.len() > 1 && g.len() < f.len() {
            let h = mdivrem(f, &g, p).0;
            let mut out = edf(&g, d, p, seed);
            out.extend(edf(&mmonic(&h, p), d, p, seed));
            return out;
        }
    }
}

// ---------- Zassenhaus ----------

fn modsym(c: &BigInt, m: &BigInt) -> BigInt {
    let r = c.mod_floor(m);
    if &r * 2 > *m { r - m } else { r }
}

fn pmod(f: &[BigInt], m: &BigInt) -> ZPoly {
    trim(f.iter().map(|c| c.mod_floor(m)).collect())
}

/// lift f ≡ g h (mod p), g monic, to f ≡ G H (mod p^k)
fn hensel(f: &[BigInt], g: &[u64], h: &[u64], p: u64, k: u32) -> (ZPoly, ZPoly) {
    let (s, t) = mextgcd(g, h, p);
    let pb = BigInt::from(p);
    let (mut gg, mut hh) = (bigs(g), bigs(h));
    let mut pj = pb.clone();
    for _ in 1..k {
        let next = &pj * &pb;
        let diff = pmod(&psub(f, &pmul(&gg, &hh)), &next);
        let e: MPoly = mtrim(
            diff.iter()
                .map(|c| (c / &pj).mod_floor(&pb).to_u64().unwrap())
                .collect(),
        );
        if !e.is_empty() {
            let (q, r) = mdivrem(&mmul(&t, &e, p), g, p);
            let dh = madd(&mmul(&s, &e, p), &mmul(&q, h, p), p);
            let add = |a: &mut ZPoly, d: &MPoly| {
                if a.len() < d.len() {
                    a.resize(d.len(), BigInt::zero());
                }
                for (i, &c) in d.iter().enumerate() {
                    a[i] += &pj * c;
                }
            };
            add(&mut gg, &r);
            add(&mut hh, &dh);
        }
        pj = next;
        gg = pmod(&gg, &pj);
        hh = pmod(&hh, &pj);
    }
    (gg, hh)
}

fn primes_from(start: u64) -> impl Iterator<Item = u64> {
    (start..).filter(|&n| nt::small::is_prime(n as u128))
}

/// irreducible factors of a primitive, square-free f of degree >= 2 with f(0) != 0
fn zassenhaus(f: &[BigInt]) -> ZPoly2 {
    let lc = f.last().unwrap().clone();
    let df = pderiv(f);
    // choose a prime: f stays square-free and keeps its degree; fewest factors
    let mut best: Option<(u64, Vec<(MPoly, usize)>, usize)> = None;
    let mut tried = 0;
    for p in primes_from(3) {
        if p > 50_000 || tried >= 6 {
            break;
        }
        if (&lc % p).is_zero() {
            continue;
        }
        let fp = mreduce(f, p);
        let dfp = mreduce(&df, p);
        if mgcd(&fp, &dfp, p).len() != 1 {
            continue;
        }
        tried += 1;
        let parts = ddf(&mmonic(&fp, p), p);
        let count: usize = parts.iter().map(|(g, d)| (g.len() - 1) / d).sum();
        if count == 1 {
            return vec![f.to_vec()];
        }
        if best.as_ref().is_none_or(|b| count < b.2) {
            best = Some((p, parts, count));
        }
    }
    let Some((p, parts, _)) = best else {
        return vec![f.to_vec()];
    };
    let mut seed = 0x9E3779B97F4A7C15u64;
    let mut modf: Vec<MPoly> = vec![];
    for (g, d) in parts {
        modf.extend(edf(&g, d, p, &mut seed));
    }
    // coefficient bound for factors (Mignotte), times lc for the recombination
    let norm2: BigInt = f.iter().map(|c| c * c).sum::<BigInt>().sqrt() + 1u32;
    let bound = (BigInt::one() << (deg(f) + 1)) * norm2 * lc.abs() * 2u32;
    let pb = BigInt::from(p);
    let mut k = 1u32;
    let mut m = pb.clone();
    while m <= bound {
        m *= &pb;
        k += 1;
    }
    // lift factor by factor
    let mut lifted: Vec<ZPoly> = vec![];
    let mut rest: ZPoly = pmod(f, &m);
    for i in 0..modf.len() - 1 {
        let g = &modf[i];
        let restp = mreduce(&rest, p);
        let h = mdivrem(&restp, g, p).0;
        let (gg, hh) = hensel(&rest, g, &h, p, k);
        lifted.push(gg);
        rest = hh;
    }
    let inv = lc.modinv(&m).unwrap();
    lifted.push(pmod(&rest.iter().map(|c| c * &inv).collect::<ZPoly>(), &m));
    // recombine
    let mut out = vec![];
    let mut f = f.to_vec();
    let mut idx: Vec<usize> = (0..lifted.len()).collect();
    let mut s = 1;
    let mut work = 0usize;
    while 2 * s <= idx.len() {
        let mut found = false;
        let mut comb: Vec<usize> = (0..s).collect();
        'subsets: loop {
            work += 1;
            if work > 300_000 {
                break 'subsets;
            }
            let lcf = f.last().unwrap().clone();
            // constant term test first
            let c0 = comb.iter().fold(lcf.clone(), |acc, &j| {
                (acc * &lifted[idx[j]][0]).mod_floor(&m)
            });
            let c0 = modsym(&c0, &m);
            if !c0.is_zero() && (&lcf * &f[0]) % &c0 == BigInt::zero() {
                let mut g: ZPoly = vec![lcf.clone()];
                for &j in &comb {
                    g = pmod(&pmul(&g, &lifted[idx[j]]), &m);
                }
                let g: ZPoly = trim(g.iter().map(|c| modsym(c, &m)).collect());
                let g = primitive(&g);
                if let Some(q) = pdivexact(&f, &g) {
                    out.push(g);
                    f = q;
                    let keep: Vec<usize> = (0..idx.len())
                        .filter(|j| !comb.contains(j))
                        .map(|j| idx[j])
                        .collect();
                    idx = keep;
                    found = true;
                    break 'subsets;
                }
            }
            // next combination
            let n = idx.len();
            let mut i = s;
            loop {
                if i == 0 {
                    break 'subsets;
                }
                i -= 1;
                if comb[i] < n - s + i {
                    comb[i] += 1;
                    for j in i + 1..s {
                        comb[j] = comb[j - 1] + 1;
                    }
                    break;
                }
            }
        }
        if work > 300_000 {
            break;
        }
        if !found {
            s += 1;
        }
    }
    out.push(primitive(&f));
    out
}

type ZPoly2 = Vec<ZPoly>;

/// x^n + e for e = ±1, as cyclotomic polynomials
fn binomial_cyclotomic(n: usize, plus: bool) -> Vec<ZPoly> {
    let n = n as u64;
    if plus {
        divisors_u64(2 * n)
            .into_iter()
            .filter(|d| n % d != 0)
            .map(cyclotomic)
            .collect()
    } else {
        divisors_u64(n).into_iter().map(cyclotomic).collect()
    }
}

/// irreducible factorization of a square-free primitive f of degree >= 1
fn factor_sqfree(f: &[BigInt]) -> Vec<ZPoly> {
    let n = deg(f);
    if n <= 1 {
        return vec![f.to_vec()];
    }
    // x^n ± 1
    if f[1..n].iter().all(|c| c.is_zero()) && f[n].is_one() && f[0].abs().is_one() {
        return binomial_cyclotomic(n, f[0].is_one());
    }
    // f(x) = g(x^k) with g = x^m ± 1 handled above; otherwise Zassenhaus
    zassenhaus(f)
}

/// Factor an integer polynomial: (constant, [(irreducible primitive factor, multiplicity)]).
/// None when it is too large to try.
pub fn factor_zpoly(f: &[BigInt]) -> Option<(BigInt, Vec<(ZPoly, u32)>)> {
    let f = trim(f.to_vec());
    if f.is_empty() {
        return Some((BigInt::zero(), vec![]));
    }
    let c = content(&f);
    let mut f: ZPoly = f.iter().map(|x| x / &c).collect();
    let mut out: Vec<(ZPoly, u32)> = vec![];
    // powers of x
    let z = f.iter().take_while(|x| x.is_zero()).count();
    if z > 0 {
        out.push((vec![BigInt::zero(), BigInt::one()], z as u32));
        f.drain(0..z);
    }
    if deg(&f) == 0 {
        return Some((c, out));
    }
    let n = deg(&f);
    let binom = f[1..n].iter().all(|x| x.is_zero()) && f[n].is_one() && f[0].abs().is_one();
    if n > 400 && !(binom && n <= 5000) {
        return None;
    }
    // square-free part
    let g = pgcd(&f, &pderiv(&f));
    let sq = if deg(&g) == 0 {
        f.clone()
    } else {
        pdivexact(&f, &g)?
    };
    let sq = primitive(&sq);
    let mut irr = factor_sqfree(&sq);
    irr.sort_by(|a, b| a.len().cmp(&b.len()).then_with(|| a.cmp(b)));
    for q in irr {
        let mut m = 0;
        while let Some(r) = pdivexact(&f, &q) {
            f = r;
            m += 1;
        }
        out.push((q, m.max(1)));
    }
    // what is left is a unit
    let unit = f.first().cloned().unwrap_or_else(BigInt::one);
    Some((c * unit, out))
}

// ---------- Aurifeuillean factors ----------

/// Kronecker symbol (a / n) for n >= 1
fn kronecker(a: i64, n: u64) -> i32 {
    let mut n = n;
    let a = a;
    let mut r = 1;
    while n % 2 == 0 {
        n /= 2;
        if a % 2 == 0 {
            return 0;
        }
        let am = a.rem_euclid(8);
        if am == 3 || am == 5 {
            r = -r;
        }
    }
    // Jacobi (a / n), n odd
    let mut a = a.rem_euclid(n as i64) as u64;
    while a != 0 {
        while a % 2 == 0 {
            a /= 2;
            if n % 8 == 3 || n % 8 == 5 {
                r = -r;
            }
        }
        std::mem::swap(&mut a, &mut n);
        if a % 4 == 3 && n % 4 == 3 {
            r = -r;
        }
        a %= n;
    }
    if n == 1 { r } else { 0 }
}

pub fn squarefree(s: u64) -> bool {
    small_factor(s).iter().all(|(_, e)| *e == 1)
}

/// whether Φ_n(s x^2) has an Aurifeuillean factorization (s squarefree > 1)
pub fn aurifeuille_applies(s: u64, n: u64) -> bool {
    s > 1
        && if s % 4 == 1 {
            n % s == 0 && (n / s) % 2 == 1
        } else {
            n % (2 * s) == 0 && (n / (2 * s)) % 2 == 1
        }
}

/// negacyclic ring Z[t]/(t^n + 1): c * t^a * v
fn nc_shift(v: &[BigInt], a: usize, neg: bool, n: usize) -> Vec<BigInt> {
    let mut r = vec![BigInt::zero(); n];
    for (i, c) in v.iter().enumerate() {
        if c.is_zero() {
            continue;
        }
        let j = i + a;
        let (j, flip) = (j % n, (j / n) % 2 == 1);
        if flip ^ neg {
            r[j] -= c;
        } else {
            r[j] += c;
        }
    }
    r
}

fn nc_mul(a: &[BigInt], b: &[BigInt], n: usize) -> Vec<BigInt> {
    let mut r = vec![BigInt::zero(); n];
    for (i, x) in a.iter().enumerate() {
        if x.is_zero() {
            continue;
        }
        for (j, y) in b.iter().enumerate() {
            if y.is_zero() {
                continue;
            }
            let k = i + j;
            if k < n {
                r[k] += x * y;
            } else {
                r[k - n] -= x * y;
            }
        }
    }
    r
}

/// reduce a ring element modulo Φ_{2n}(t): the canonical form in Z[ω]
fn reduce_mod(v: &[BigInt], m: &[BigInt]) -> ZPoly {
    let mut r = trim(v.to_vec());
    let dm = m.len();
    while r.len() >= dm {
        let top = r.last().unwrap().clone();
        let shift = r.len() - dm;
        for (j, y) in m.iter().enumerate() {
            r[shift + j] -= &top * y;
        }
        r = trim(r);
    }
    r
}

thread_local! {
    static AURI: std::cell::RefCell<HashMap<(u64, u64), Option<ZPoly>>> = std::cell::RefCell::new(HashMap::new());
}

/// P(x) with Φ_n(s x^2) = ± P(x) P(-x), or None
pub fn aurifeuille_poly(s: u64, n: u64) -> Option<ZPoly> {
    if !squarefree(s) || !aurifeuille_applies(s, n) {
        return None;
    }
    let phi = totient_u64(n);
    if (phi as u128) * (phi as u128) * (n as u128) > 60_000_000 {
        return None;
    }
    if let Some(r) = AURI.with(|c| c.borrow().get(&(s, n)).cloned()) {
        return r;
    }
    let r = aurifeuille_compute(s, n);
    AURI.with(|c| c.borrow_mut().insert((s, n), r.clone()));
    r
}

fn aurifeuille_compute(s: u64, n: u64) -> Option<ZPoly> {
    let nn = n as usize;
    let d = if s % 4 == 1 { s } else { 4 * s };
    // signs ε_a so that the roots ε_a ω^a / √s form a Galois orbit
    let mut h: Vec<Vec<BigInt>> = vec![{
        let mut one = vec![BigInt::zero(); nn];
        one[0] = BigInt::one();
        one
    }];
    for a in 1..n {
        if a.gcd(&n) != 1 {
            continue;
        }
        let c = if a.gcd(&(2 * n)) == 1 { a } else { a + n };
        let chi = kronecker(d as i64, c);
        let eps = if c >= n { -chi } else { chi };
        // h *= (y - eps t^a)
        let mut next: Vec<Vec<BigInt>> = vec![vec![BigInt::zero(); nn]; h.len() + 1];
        for (k, hk) in h.iter().enumerate() {
            for (i, x) in hk.iter().enumerate() {
                next[k + 1][i] += x;
            }
            let sh = nc_shift(hk, a as usize, eps > 0, nn);
            for (i, x) in sh.into_iter().enumerate() {
                next[k][i] += x;
            }
        }
        h = next;
    }
    let m = cyclotomic(2 * n);
    // Gauss sum g = Σ χ(a) ζ_d^a = √d, ζ_d = t^(2n/d)
    let step = (2 * n / d) as usize;
    let mut g = vec![BigInt::zero(); nn];
    for a in 1..d {
        let chi = kronecker(d as i64, a);
        if chi != 0 {
            let mut unit = vec![BigInt::zero(); nn];
            unit[0] = BigInt::from(chi);
            for (i, x) in nc_shift(&unit, a as usize * step, false, nn)
                .into_iter()
                .enumerate()
            {
                g[i] += x;
            }
        }
    }
    // H(y) = Σ c_k y^k; P(x) ∝ H(√s x) = Σ c_k s^{k/2} x^k
    use num_rational::BigRational;
    let phi = h.len() - 1;
    let sb = BigInt::from(s);
    let mut coeffs: Vec<BigRational> = vec![];
    for (k, ck) in h.iter().enumerate() {
        let ck = reduce_mod(ck, &m);
        let rational = ck.len() <= 1;
        // value = r (rational) or u √s
        let (val, root) = if rational {
            (
                BigRational::from_integer(ck.first().cloned().unwrap_or_default()),
                false,
            )
        } else {
            let cg = reduce_mod(&nc_mul(&pad(&ck, nn), &g, nn), &m);
            if cg.len() > 1 {
                return None;
            }
            // c g = q, c = q/√d = q √d / d; √d = √s (d = s) or 2√s
            let q = cg.first().cloned().unwrap_or_default();
            let u = if d == s {
                BigRational::new(q, BigInt::from(d))
            } else {
                BigRational::new(q * 2, BigInt::from(d))
            };
            (u, true)
        };
        // times s^{k/2}: the √s parts must cancel
        let half = num_traits::pow(sb.clone(), k / 2);
        let odd = k % 2 == 1;
        if root != odd {
            if !val.is_zero() {
                return None;
            }
            coeffs.push(BigRational::zero());
            continue;
        }
        let v = if root {
            val * BigRational::from_integer(half * &sb)
        } else {
            val * BigRational::from_integer(half)
        };
        coeffs.push(v);
    }
    let _ = phi;
    let den = coeffs
        .iter()
        .fold(BigInt::one(), |acc, c| acc.lcm(c.denom()));
    let p: ZPoly = trim(
        coeffs
            .iter()
            .map(|c| (c * BigRational::from_integer(den.clone())).to_integer())
            .collect(),
    );
    let p = primitive(&p);
    // check Φ_n(s x^2) ∝ P(x) P(-x)
    let target = cyclotomic(n);
    let mut q: ZPoly = vec![BigInt::zero(); 2 * deg(&target) + 1];
    for (i, c) in target.iter().enumerate() {
        q[2 * i] = c * num_traits::pow(sb.clone(), i);
    }
    let r = pmul(&p, &pneg_x(&p));
    let (lq, lr) = (q.last()?.clone(), r.last()?.clone());
    if r.len() != q.len() || r.iter().zip(&q).any(|(a, b)| a * &lq != b * &lr) {
        return None;
    }
    Some(p)
}

fn pad(v: &[BigInt], n: usize) -> Vec<BigInt> {
    let mut r = v.to_vec();
    r.resize(n, BigInt::zero());
    r
}

// ---------- whole numbers a^n ± b^n ----------

#[derive(Clone, Debug)]
pub struct Piece {
    /// plain text and LaTeX names, e.g. Phi_8(10) and \Phi_{8}(10)
    pub text: String,
    pub latex: String,
    pub value: BigInt,
    /// every prime factor p of the value not dividing d has p = 1 (mod d); 0 if unknown
    pub d: u64,
}

/// the largest k with x a perfect k-th power, and its k-th root
fn perfect_power(x: &BigInt) -> (BigInt, u32) {
    if x <= &BigInt::one() {
        return (x.clone(), 1);
    }
    let bits = x.bits() as u32;
    for k in (2..=bits).rev() {
        let r = x.nth_root(k);
        if &num_traits::pow(r.clone(), k as usize) == x {
            return (r, k);
        }
    }
    (x.clone(), 1)
}

/// squarefree part s and y with t = s y^2, when t can be factored quickly
fn squarefree_split(t: &BigInt) -> Option<(u64, BigInt)> {
    if t.bits() > 120 {
        return None;
    }
    let f = nt::factorize(t).ok()?;
    let mut s = BigInt::one();
    let mut y = BigInt::one();
    for (p, e) in f {
        if e % 2 == 1 {
            s *= &p;
        }
        y *= num_traits::pow(p, (e / 2) as usize);
    }
    Some((s.to_u64()?, y))
}

fn name_ab(a: &BigInt, b: &BigInt) -> (String, String) {
    if b.is_one() {
        (a.to_string(), a.to_string())
    } else {
        (format!("{}, {}", a, b), format!("{}, {}", a, b))
    }
}

/// The algebraic pieces of a^n + b^n (plus) or a^n - b^n, a, b >= 1, whose
/// product is the number. None when there is nothing to split.
pub fn algebraic_pieces(a: &BigInt, b: &BigInt, n: u64, plus: bool) -> Option<Vec<Piece>> {
    let (mut a, mut b) = (a.clone(), b.clone());
    if a.is_zero() || b.is_zero() || a.is_negative() || b.is_negative() || n == 0 {
        return None;
    }
    if !plus && a < b {
        std::mem::swap(&mut a, &mut b);
    }
    if !plus && a == b {
        return None;
    }
    let mut pieces = vec![];
    let g = a.gcd(&b);
    if !g.is_one() {
        a /= &g;
        b /= &g;
        pieces.push(Piece {
            text: format!("{}^{}", g, n),
            latex: format!("{}^{{{}}}", g, n),
            value: num_traits::pow(g.clone(), n as usize),
            d: 0,
        });
    }
    if a == b {
        // a^n + a^n = 2 a^n
        pieces.push(Piece {
            text: "2".into(),
            latex: "2".into(),
            value: BigInt::from(2),
            d: 0,
        });
        return if pieces.len() > 1 { Some(pieces) } else { None };
    }
    // a = α^k, b = β^k: use the largest common power
    let (ra, ka) = perfect_power(&a);
    let (rb, kb) = perfect_power(&b);
    let k = if b.is_one() {
        ka
    } else if a.is_one() {
        kb
    } else {
        ka.gcd(&kb)
    };
    let mut n = n;
    if k > 1 {
        a = if a.is_one() {
            a
        } else {
            num_traits::pow(ra, (ka / k) as usize)
        };
        b = if b.is_one() {
            b
        } else {
            num_traits::pow(rb, (kb / k) as usize)
        };
        n *= k as u64;
    }
    let ds: Vec<u64> = if plus {
        divisors_u64(2 * n)
            .into_iter()
            .filter(|d| n % d != 0)
            .collect()
    } else {
        divisors_u64(n)
    };
    let ab_split = squarefree_split(&(&a * &b));
    let (nt_, nl) = name_ab(&a, &b);
    for d in ds {
        let v = cyclotomic_value(d, &a, &b);
        if v.is_one() {
            continue;
        }
        let text = format!("Phi_{}({})", d, nt_);
        let latex = format!("\\Phi_{{{}}}({})", d, nl);
        // Aurifeuillean split
        if let Some((s, y)) = &ab_split {
            if aurifeuille_applies(*s, d) {
                if let Some(p) = aurifeuille_poly(*s, d) {
                    let phi = deg(&p);
                    let l: BigInt = p
                        .iter()
                        .enumerate()
                        .map(|(i, c)| {
                            c * num_traits::pow(y.clone(), i) * num_traits::pow(b.clone(), phi - i)
                        })
                        .sum();
                    let gl = l.abs().gcd(&v);
                    if !gl.is_one() && gl != v {
                        let other = &v / &gl;
                        let (lo, hi) = if gl < other { (gl, other) } else { (other, gl) };
                        pieces.push(Piece {
                            text: format!("L_{}({})", d, nt_),
                            latex: format!("L_{{{}}}({})", d, nl),
                            value: lo,
                            d,
                        });
                        pieces.push(Piece {
                            text: format!("M_{}({})", d, nt_),
                            latex: format!("M_{{{}}}({})", d, nl),
                            value: hi,
                            d,
                        });
                        continue;
                    }
                }
            }
        }
        pieces.push(Piece {
            text,
            latex,
            value: v,
            d,
        });
    }
    if pieces.len() <= 1 {
        return None;
    }
    Some(pieces)
}

/// Recognize n = b^k ± 1 with a small base b (k >= 2): (b, k, plus)
pub fn detect_binomial(n: &BigInt) -> Option<(BigInt, u64, bool)> {
    if n.bits() < 8 {
        return None;
    }
    for base in 2u32..=1000 {
        let bb = BigInt::from(base);
        let lg = (base as f64).log2();
        let k0 = (n.bits() as f64 / lg).round() as i64;
        for plus in [true, false] {
            let m = if plus { n - 1u32 } else { n + 1u32 };
            for k in (k0 - 1).max(2)..=k0 + 1 {
                if num_traits::pow(bb.clone(), k as usize) == m {
                    // the smallest base: skip perfect powers of a smaller base
                    return Some((bb, k as u64, plus));
                }
            }
        }
    }
    None
}

/// Factor n as far as the budget allows: (primes with multiplicity, composite leftovers)
pub fn factor_partial(n: &BigInt) -> (Vec<BigInt>, Vec<BigInt>) {
    factor_partial_known(n, 0)
}

/// the same, knowing that the large prime factors are 1 (mod d) (d = 0: nothing known).
/// Then p - 1 gets d (or 2d) into its exponent for free.
pub fn factor_partial_known(n: &BigInt, d: u64) -> (Vec<BigInt>, Vec<BigInt>) {
    let mut m = n.abs();
    let mut primes = vec![];
    let mut comps = vec![];
    if m <= BigInt::one() {
        return (primes, comps);
    }
    for p in nt::small::primes_upto(10_000) {
        while (&m % p).is_zero() {
            primes.push(BigInt::from(p));
            m /= p;
        }
    }
    let mut todo = vec![m];
    while let Some(m) = todo.pop() {
        if m.is_one() {
            continue;
        }
        if m.bits() <= 120 {
            match nt::factorize(&m) {
                Ok(f) => {
                    for (p, e) in f {
                        primes.extend(std::iter::repeat_n(p, e as usize));
                    }
                }
                Err(_) => comps.push(m),
            }
        } else if nt::is_prime(&m) {
            primes.push(m);
        } else if let Some(d) = split_known(&m, d) {
            todo.push(&m / &d);
            todo.push(d);
        } else {
            comps.push(m);
        }
    }
    primes.sort();
    comps.sort();
    (primes, comps)
}

/// one factor of a composite m whose prime factors are 1 (mod d)
fn split_known(m: &BigInt, d: u64) -> Option<BigInt> {
    if d <= 2 {
        return nt::pollard_pm1(m)
            .or_else(|| nt::pollard_rho(m))
            .or_else(|| ecm_effort(m));
    }
    // p odd and p = 1 (mod d) give p = 1 (mod 2d) when d is odd. Only p - 1
    // uses it: it gets d into its exponent for free, which matters when d has a
    // prime factor above p - 1's bound. A rho walk x -> x^d + c would need about
    // sqrt(d) times fewer steps but log2(d) times more work per step, which
    // measured no faster for the d of the Cunningham numbers (Brent and Pollard's
    // x^1024 + 1 for 2^256 + 1 is the case where it pays).
    let k = if d % 2 == 1 { 2 * d } else { d };
    nt::pollard_pm1_with(m, k)
        .or_else(|| nt::pollard_rho(m))
        .or_else(|| ecm_effort(m))
}

/// Lenstra's ECM after rho has failed: 4*effort curves, with B1 growing with
/// effort (2000, then 11000 from effort 5, 50000 from effort 50). Rho finds
/// factors of up to about 15 digits; ECM reaches 20 and more.
fn ecm_effort(m: &BigInt) -> Option<BigInt> {
    let e = nt::effort();
    let b1 = if e >= 50 {
        50_000
    } else if e >= 5 {
        11_000
    } else {
        2_000
    };
    crate::groups::ecm(m, 4 * e.min(10_000), b1).map(|(p, _)| p)
}

// ---------- polynomials in several variables ----------

/// a term: exponents of each variable, coefficient
pub type MTerm = (Vec<u32>, BigInt);
type MPolyZ = std::collections::BTreeMap<Vec<u32>, BigInt>;

fn mp_from(terms: &[MTerm]) -> MPolyZ {
    let mut m = MPolyZ::new();
    for (e, c) in terms {
        *m.entry(e.clone()).or_insert_with(BigInt::zero) += c;
    }
    m.retain(|_, c| !c.is_zero());
    m
}

fn mp_mul(a: &MPolyZ, b: &MPolyZ) -> MPolyZ {
    let mut r = MPolyZ::new();
    for (ea, ca) in a {
        for (eb, cb) in b {
            let e: Vec<u32> = ea.iter().zip(eb).map(|(x, y)| x + y).collect();
            *r.entry(e).or_insert_with(BigInt::zero) += ca * cb;
        }
    }
    r.retain(|_, c| !c.is_zero());
    r
}

/// Kronecker substitution: variable i becomes t^(w_i)
fn kron_encode(f: &MPolyZ, w: &[usize]) -> ZPoly {
    let deg = f
        .keys()
        .map(|e| e.iter().zip(w).map(|(x, y)| *x as usize * y).sum::<usize>())
        .max()
        .unwrap_or(0);
    let mut p = vec![BigInt::zero(); deg + 1];
    for (e, c) in f {
        let k: usize = e.iter().zip(w).map(|(x, y)| *x as usize * y).sum();
        p[k] += c;
    }
    trim(p)
}

fn kron_decode(p: &[BigInt], dims: &[usize]) -> MPolyZ {
    let mut m = MPolyZ::new();
    for (k, c) in p.iter().enumerate() {
        if c.is_zero() {
            continue;
        }
        let mut k = k;
        let e: Vec<u32> = dims
            .iter()
            .map(|d| {
                let r = k % d;
                k /= d;
                r as u32
            })
            .collect();
        m.insert(e, c.clone());
    }
    m
}

/// content and sign: the leading term (largest exponent vector) becomes positive
fn mp_normalize(f: &MPolyZ) -> (BigInt, MPolyZ) {
    let mut g = BigInt::zero();
    for c in f.values() {
        g = g.gcd(c);
    }
    if f.iter().next_back().is_some_and(|(_, c)| c.is_negative()) {
        g = -g;
    }
    (
        g.clone(),
        f.iter().map(|(e, c)| (e.clone(), c / &g)).collect(),
    )
}

/// Factor a polynomial with integer coefficients in nv variables:
/// (constant, [(irreducible factor, multiplicity)]). The variables are packed
/// into one by Kronecker substitution (a factor's exponents never exceed the
/// polynomial's), the univariate image is factored, and products of its
/// factors are unpacked and kept when they divide exactly.
pub fn factor_multi(terms: &[MTerm], nv: usize) -> Option<(BigInt, Vec<(Vec<MTerm>, u32)>)> {
    let f = mp_from(terms);
    if f.is_empty() {
        return None;
    }
    let dims: Vec<usize> = (0..nv)
        .map(|i| f.keys().map(|e| e[i] as usize).max().unwrap_or(0) + 1)
        .collect();
    let mut w = vec![1usize; nv];
    for i in 1..nv {
        w[i] = w[i - 1].checked_mul(dims[i - 1])?;
    }
    if w[nv - 1].checked_mul(dims[nv - 1])? > 401 {
        return None;
    }
    let (_, fz) = factor_zpoly(&kron_encode(&f, &w))?;
    let mut pieces: Vec<ZPoly> = vec![];
    for (p, m) in fz {
        for _ in 0..m {
            pieces.push(p.clone());
        }
    }
    let mut cur = f.clone();
    let mut found: Vec<MPolyZ> = vec![];
    let mut s = 1;
    let mut work = 0usize;
    'outer: while 2 * s <= pieces.len() {
        let n = pieces.len();
        let mut comb: Vec<usize> = (0..s).collect();
        loop {
            work += 1;
            if work > 200_000 {
                break 'outer;
            }
            let mut h: ZPoly = vec![BigInt::one()];
            for &j in &comb {
                h = pmul(&h, &pieces[j]);
            }
            let hm = kron_decode(&h, &dims);
            let cz = kron_encode(&cur, &w);
            if let Some(q) = pdivexact(&cz, &h) {
                let qm = kron_decode(&q, &dims);
                if mp_mul(&hm, &qm) == cur {
                    found.push(hm);
                    cur = qm;
                    let keep: Vec<ZPoly> = (0..n)
                        .filter(|j| !comb.contains(j))
                        .map(|j| pieces[j].clone())
                        .collect();
                    pieces = keep;
                    continue 'outer;
                }
            }
            let mut i = s;
            loop {
                if i == 0 {
                    s += 1;
                    continue 'outer;
                }
                i -= 1;
                if comb[i] < n - s + i {
                    comb[i] += 1;
                    for j in i + 1..s {
                        comb[j] = comb[j - 1] + 1;
                    }
                    break;
                }
            }
        }
    }
    let mut constant = BigInt::one();
    let mut out: Vec<(MPolyZ, u32)> = vec![];
    let nonconst = |m: &MPolyZ| m.keys().any(|e| e.iter().any(|&x| x > 0));
    if nonconst(&cur) {
        found.push(cur);
    } else if let Some(c) = cur.values().next() {
        constant *= c;
    }
    for h in found {
        let (c, h) = mp_normalize(&h);
        constant *= c;
        match out.iter_mut().find(|(g, _)| *g == h) {
            Some((_, m)) => *m += 1,
            None => out.push((h, 1)),
        }
    }
    out.sort_by(|a, b| a.0.len().cmp(&b.0.len()).then_with(|| a.0.cmp(&b.0)));
    Some((
        constant,
        out.into_iter()
            .map(|(m, k)| (m.into_iter().collect(), k))
            .collect(),
    ))
}

// ---------- factoring modulo a prime ----------

fn mderiv(f: &[u64], p: u64) -> MPoly {
    mtrim(
        f.iter()
            .enumerate()
            .skip(1)
            .map(|(k, &c)| (k as u64 % p) * c % p)
            .collect(),
    )
}

/// irreducible monic factors of f mod p with multiplicities (f monic, degree >= 1)
fn factor_mod_monic(f: &[u64], p: u64, seed: &mut u64) -> Vec<(MPoly, u32)> {
    if f.len() <= 1 {
        return vec![];
    }
    let df = mderiv(f, p);
    if df.is_empty() {
        // f = g(x)^p: the coefficients sit at multiples of p
        let g: MPoly = f.iter().step_by(p as usize).cloned().collect();
        return factor_mod_monic(&g, p, seed)
            .into_iter()
            .map(|(h, m)| (h, m * p as u32))
            .collect();
    }
    let g = mgcd(f, &df, p);
    let sq = mmonic(&mdivrem(f, &g, p).0, p);
    let mut irr = vec![];
    for (part, d) in ddf(&sq, p) {
        irr.extend(edf(&mmonic(&part, p), d, p, seed));
    }
    let mut rest = f.to_vec();
    let mut out = vec![];
    for h in irr {
        let mut m = 0;
        loop {
            let (q, r) = mdivrem(&rest, &h, p);
            if !r.is_empty() {
                break;
            }
            rest = q;
            m += 1;
        }
        out.push((h, m));
    }
    // what is left has only multiplicities divisible by p
    for (h, m) in factor_mod_monic(&mmonic(&rest, p), p, seed) {
        match out.iter_mut().find(|(g, _)| *g == h) {
            Some((_, k)) => *k += m,
            None => out.push((h, m)),
        }
    }
    out
}

/// factor an integer polynomial modulo a prime p < 2^31: (leading coefficient, [(monic factor, multiplicity)])
pub fn factor_mod_p(f: &[BigInt], p: u64) -> Option<(u64, Vec<(Vec<u64>, u32)>)> {
    let fp = mreduce(f, p);
    if fp.is_empty() {
        return Some((0, vec![]));
    }
    let lead = *fp.last().unwrap();
    let mut seed = 0x2545F4914F6CDD1Du64;
    let mut out = factor_mod_monic(&mmonic(&fp, p), p, &mut seed);
    out.sort_by(|a, b| a.0.len().cmp(&b.0.len()).then_with(|| a.0.cmp(&b.0)));
    Some((lead, out))
}

// ---------- recognizing algebraic forms of a large number ----------

/// How a number relates to a^k ± b^k
#[derive(Clone, Debug)]
pub enum Form {
    /// n = a^k + b^k (plus) or a^k - b^k
    Exact {
        a: BigInt,
        b: BigInt,
        k: u64,
        plus: bool,
    },
    /// n divides b^k + 1 (plus) or b^k - 1, k the smallest such exponent found
    Divides {
        b: u64,
        k: u64,
        plus: bool,
        searched: u64,
    },
    /// n = f(x) for a polynomial f (with integer coefficients) that factors
    Poly { f: ZPoly, x: BigInt },
    /// n = y^d f(x/y), the homogeneous form of a polynomial f of degree d that factors
    Poly2 { f: ZPoly, x: BigInt, y: BigInt },
    /// n = a^2 - b^2 = (a - b)(a + b), found by Fermat's method
    Squares { a: BigInt, b: BigInt },
}

/// Search for an algebraic form of n >= 2. effort (1 = about a second) sets
/// how many bases b and exponents k are tried:
/// * n = a^k ± 1 for every base a (k-th roots of n ∓ 1 for each k),
/// * n = a^k ± b^k for b up to 12*effort,
/// * n divides b^k ± 1 for b up to 12*effort and k up to 3000*effort.
pub fn find_form(n: &BigInt, effort: u64) -> Option<Form> {
    let bits = n.bits();
    if bits < 3 {
        return None;
    }
    let one = BigInt::one();
    // a^k ± 1, largest k first (smallest base)
    for k in (2..=bits).rev() {
        for plus in [true, false] {
            let m = if plus { n - 1u32 } else { n + 1u32 };
            let r = m.nth_root(k as u32);
            if r >= BigInt::from(2) && num_traits::pow(r.clone(), k as usize) == m {
                return Some(Form::Exact {
                    a: r,
                    b: one,
                    k,
                    plus,
                });
            }
        }
    }
    // a^k ± b^k
    let bmax = (12 * effort).min(10_000);
    for b in 2..=bmax {
        let bb = BigInt::from(b);
        let mut bk = bb.clone();
        let mut k = 1u64;
        while &bk * 2u32 < *n {
            bk *= &bb;
            k += 1;
            for plus in [true, false] {
                let m = if plus { n - &bk } else { n + &bk };
                if m.sign() != num_bigint::Sign::Plus {
                    continue;
                }
                let r = m.nth_root(k as u32);
                if r > bb && num_traits::pow(r.clone(), k as usize) == m {
                    return Some(Form::Exact {
                        a: r,
                        b: bb,
                        k,
                        plus,
                    });
                }
            }
        }
    }
    // Sophie Germain: n = a^4 + 4 b^4
    for b in 1..=bmax {
        let f: ZPoly = vec![
            BigInt::from(4) * num_traits::pow(BigInt::from(b), 4),
            BigInt::zero(),
            BigInt::zero(),
            BigInt::zero(),
            BigInt::one(),
        ];
        let m = n - &f[0];
        if m.sign() != num_bigint::Sign::Plus {
            break;
        }
        let r = m.nth_root(4);
        if r > BigInt::one() && num_traits::pow(r.clone(), 4) == m {
            return Some(Form::Poly { f, x: r });
        }
    }
    // n divides b^k ± 1
    let kmax = (3000 * effort).min(10_000_000);
    let dmax = (12 * effort).min(1000);
    let nm1 = n - 1u32;
    let mut best: Option<Form> = None;
    for b in 2..=dmax {
        let bb = BigInt::from(b);
        if !(n.gcd(&bb)).is_one() {
            continue;
        }
        let mut x = BigInt::one();
        let lb = (b as f64).ln();
        for k in 1..=kmax {
            // keep the smallest b^k ± 1 (so 2^1062 + 1 rather than 8^354 + 1)
            if best.as_ref().is_some_and(|f| matches!(f, Form::Divides { b: bb, k: kb, .. } if k as f64 * lb >= *kb as f64 * (*bb as f64).ln() - 1e-9)) {
                break;
            }
            x = &x * &bb % n;
            if x.is_one() || x == nm1 {
                best = Some(Form::Divides {
                    b,
                    k,
                    plus: x == nm1,
                    searched: kmax,
                });
                break;
            }
        }
    }
    best
}

/// The parts of n cut out by the algebraic pieces of b^k ± 1 (n divides it)
pub fn divisor_pieces(n: &BigInt, b: u64, k: u64, plus: bool) -> Vec<Piece> {
    let bb = BigInt::from(b);
    let pieces = algebraic_pieces(&bb, &BigInt::one(), k, plus).unwrap_or_else(|| {
        let v = if plus {
            num_traits::pow(bb.clone(), k as usize) + 1u32
        } else {
            num_traits::pow(bb.clone(), k as usize) - 1u32
        };
        let (text, latex) = if plus {
            (format!("{}^{}+1", b, k), format!("{}^{{{}}}+1", b, k))
        } else {
            (format!("{}^{}-1", b, k), format!("{}^{{{}}}-1", b, k))
        };
        vec![Piece {
            text,
            latex,
            value: v,
            d: if plus { 2 * k } else { k },
        }]
    });
    let mut rest = n.abs();
    let mut out = vec![];
    for p in pieces {
        let g = rest.gcd(&p.value);
        if g.is_one() {
            continue;
        }
        rest /= &g;
        out.push(Piece {
            text: format!("N in {}", p.text),
            latex: format!("N \\cap {}", p.latex),
            value: g,
            d: p.d,
        });
    }
    if !rest.is_one() {
        out.push(Piece {
            text: "rest".into(),
            latex: "\\text{rest}".into(),
            value: rest,
            d: 0,
        });
    }
    out
}

/// an integer x with f(x) = t, for large t (Newton's method from the t^(1/d) estimate, both signs)
pub fn int_root(f: &[BigInt], t: &BigInt) -> Option<BigInt> {
    int_root_from(f, t, None)
}

/// int_root starting Newton from est (the d-th root of t/lc, shared by every
/// family with the same degree and leading coefficient)
pub fn int_root_from(f: &[BigInt], t: &BigInt, est: Option<&BigInt>) -> Option<BigInt> {
    let f = trim(f.to_vec());
    let d = deg(&f);
    if d == 0 {
        return None;
    }
    let lc = f.last().unwrap().clone();
    let df = pderiv(&f);
    let est = match est {
        Some(e) => e.clone(),
        None => (t.abs() / lc.abs()).nth_root(d as u32),
    };
    for sign in [1i32, -1] {
        let mut x = &est * sign;
        for _ in 0..300 {
            let h = peval(&f, &x) - t;
            if h.is_zero() {
                return Some(x);
            }
            let dh = peval(&df, &x);
            if dh.is_zero() {
                break;
            }
            let step = &h / &dh;
            if step.is_zero() {
                break;
            }
            x -= step;
        }
        for dx in -3i32..=3 {
            let y = &x + dx;
            if peval(&f, &y) == *t {
                return Some(y);
            }
        }
    }
    // small x: try directly
    if t.bits() < 64 {
        return (-1000i32..=1000)
            .map(BigInt::from)
            .find(|x| peval(&f, x) == *t);
    }
    None
}

/// the pieces f_i(x) of n = f(x), f = c * prod f_i^m_i over the integers
pub fn poly_pieces(f: &[BigInt], x: &BigInt, var: &str) -> Option<Vec<Piece>> {
    let (c, fs) = factor_zpoly(f)?;
    let mut out = vec![];
    if !c.abs().is_one() {
        out.push(Piece {
            text: c.abs().to_string(),
            latex: c.abs().to_string(),
            value: c.abs(),
            d: 0,
        });
    }
    for (g, m) in fs {
        let v = peval(&g, x).abs();
        let gs = show_zpoly(&g, var);
        for _ in 0..m {
            out.push(Piece {
                text: gs.clone(),
                latex: gs.clone(),
                value: v.clone(),
                d: 0,
            });
        }
    }
    Some(out)
}

/// x^2 - 2*x + 2 style text
pub fn show_zpoly(p: &[BigInt], var: &str) -> String {
    let mut s = String::new();
    for (k, c) in p.iter().enumerate().rev() {
        if c.is_zero() {
            continue;
        }
        let neg = c.is_negative();
        let a = c.abs();
        if s.is_empty() {
            if neg {
                s.push('-');
            }
        } else {
            s.push_str(if neg { " - " } else { " + " });
        }
        let mono = match k {
            0 => String::new(),
            1 => var.to_string(),
            _ => format!("{}^{}", var, k),
        };
        if mono.is_empty() {
            s.push_str(&a.to_string());
        } else if a.is_one() {
            s.push_str(&mono);
        } else {
            s.push_str(&format!("{}*{}", a, mono));
        }
    }
    if s.is_empty() { "0".into() } else { s }
}

// ---------- a catalogue of polynomial families that factor ----------

thread_local! {
    static CATALOG: std::cell::RefCell<Option<Vec<(String, ZPoly)>>> = const { std::cell::RefCell::new(None) };
    static USER_FAMILIES: std::cell::RefCell<Vec<(String, ZPoly)>> = const { std::cell::RefCell::new(Vec::new()) };
}

fn factors_nontrivially(f: &[BigInt]) -> bool {
    factor_zpoly(f).is_some_and(|(_, fs)| fs.iter().map(|(_, m)| *m).sum::<u32>() >= 2)
}

/// Built-in families with large coefficients, which the base-x search below
/// cannot see: the quartics x^4 + k x^2 + 1 that factor (|k| <= 100) and
/// Chernick's Carmichael family (6x + 1)(12x + 1)(18x + 1).
fn builtin_families() -> Vec<(String, ZPoly)> {
    let mut out = vec![];
    for k in -100i32..=100 {
        let f: ZPoly = vec![
            BigInt::one(),
            BigInt::zero(),
            BigInt::from(k),
            BigInt::zero(),
            BigInt::one(),
        ];
        if factors_nontrivially(&f) {
            out.push((format!("quartic {}", show_zpoly(&f, "x")), f));
        }
    }
    let mono = |c: &[i64]| -> ZPoly { c.iter().map(|&v| BigInt::from(v)).collect() };
    for (name, f) in [
        (
            "Sophie Germain x^4 + 4 (with y: x^4 + 4y^4)",
            mono(&[4, 0, 0, 0, 1]),
        ),
        (
            "Lucas x^6 + 27 (with y: x^6 + 27y^6)",
            mono(&[27, 0, 0, 0, 0, 0, 1]),
        ),
        ("x^6 - 27", mono(&[-27, 0, 0, 0, 0, 0, 1])),
        ("x^8 + 4", mono(&[4, 0, 0, 0, 0, 0, 0, 0, 1])),
        ("x^8 + x^4 + 1", mono(&[1, 0, 0, 0, 1, 0, 0, 0, 1])),
        ("x^4 + 2x^2 + 9", mono(&[9, 0, 2, 0, 1])),
    ] {
        if factors_nontrivially(&f) && !out.iter().any(|(_, g)| *g == f) {
            out.push((name.to_string(), f));
        }
    }
    for (name, cs) in crate::auri_families::AURI_FAMILIES {
        let sign = if cs.last().is_some_and(|&v| v < 0) {
            -1
        } else {
            1
        };
        let f: ZPoly = cs.iter().map(|&v| BigInt::from(sign * v)).collect();
        if !out.iter().any(|(_, g)| *g == f) {
            out.push((format!("Aurifeuillean-type {}", name), f));
        }
    }
    let chernick = pmul(
        &pmul(
            &[BigInt::one(), BigInt::from(6)],
            &[BigInt::one(), BigInt::from(12)],
        ),
        &[BigInt::one(), BigInt::from(18)],
    );
    out.push(("Chernick (6x + 1)(12x + 1)(18x + 1)".into(), chernick));
    out
}

/// The base-x search: for each degree d and each base x near (n/c)^(1/d), write
/// n in base x with balanced digits. When all digits are small (|digit| <=
/// 8*effort) and few are nonzero (at most 6 + effort), n = P(x) for a sparse
/// polynomial P with small coefficients, and if P factors over the integers so
/// does n. This finds every trinomial, quadrinomial, ... family at once, of any
/// degree, without a table. Returns P and x.
pub fn base_search(n: &BigInt, effort: u64) -> Option<(ZPoly, BigInt)> {
    let cmax = BigInt::from((8 * effort).min(100_000));
    let tmax = 6 + effort.min(10) as usize;
    let three = BigInt::from(3);
    let bits = n.bits();
    let dmax = (bits as f64 / 3f64.log2()) as usize + 1;
    for d in 2..=dmax {
        let mut lead = BigInt::one();
        while lead <= cmax {
            let r = (n / &lead).nth_root(d as u32);
            if r < three {
                break;
            }
            let delta = 2 + (&cmax / BigInt::from(d)).to_i64().unwrap_or(0).min(1000);
            for off in -delta..=delta {
                let x = &r + off;
                if x < three {
                    continue;
                }
                let half = &x / 2u32;
                let mut m = n.clone();
                let mut coeffs: ZPoly = vec![];
                let mut nonzero = 0;
                let mut ok = true;
                while !m.is_zero() {
                    let mut dgt = m.mod_floor(&x);
                    if dgt > half {
                        dgt -= &x;
                    }
                    if dgt.abs() > cmax || coeffs.len() > d {
                        ok = false;
                        break;
                    }
                    if !dgt.is_zero() {
                        nonzero += 1;
                        if nonzero > tmax {
                            ok = false;
                            break;
                        }
                    }
                    m = (&m - &dgt) / &x;
                    coeffs.push(dgt);
                }
                if !ok || coeffs.len() != d + 1 || nonzero < 3 {
                    continue;
                }
                if factors_nontrivially(&coeffs) {
                    return Some((coeffs, x));
                }
            }
            lead += 1u32;
        }
    }
    None
}

/// add a family to this session's list (algfactor tries it on every number)
pub fn add_family(var: &str, f: ZPoly) -> bool {
    let fresh = USER_FAMILIES.with(|u| !u.borrow().iter().any(|(_, g)| *g == f));
    if fresh {
        USER_FAMILIES.with(|u| u.borrow_mut().push((show_zpoly(&f, var), f)));
    }
    fresh
}

/// the families tried automatically: built-in, then added ones
pub fn families() -> (usize, Vec<(String, ZPoly)>) {
    let builtin = CATALOG.with(|c| c.borrow_mut().get_or_insert_with(builtin_families).clone());
    let nb = builtin.len();
    let mut all = builtin;
    USER_FAMILIES.with(|u| all.extend(u.borrow().iter().cloned()));
    (nb, all)
}

/// the first catalogued family with n = f(x) for a whole number x (|x| >= 2)
pub fn catalogue_match(n: &BigInt) -> Option<(String, ZPoly, BigInt)> {
    let fams = prepared_families();
    let res = Residues::new(n);
    let mut roots = RootCache::default();
    for fam in &fams {
        if !fam.may_hit(&res, 1) {
            continue;
        }
        let est = roots.get(n, fam.d, &fam.f[fam.d]);
        if let Some(x) = int_root_from(&fam.f, n, Some(&est)) {
            if x.abs() >= BigInt::from(2) {
                return Some((fam.name.clone(), fam.f.clone(), x));
            }
        }
    }
    None
}

/// small moduli for the residue filter (all at most 64, so a residue set is a u64 mask)
const PRUNE_MODULI: [u64; 16] = [
    64, 27, 25, 49, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
];

/// n reduced by each pruning modulus
pub struct Residues(Vec<u64>);

impl Residues {
    pub fn new(n: &BigInt) -> Residues {
        Residues(
            PRUNE_MODULI
                .iter()
                .map(|&m| n.mod_floor(&BigInt::from(m)).to_u64().unwrap())
                .collect(),
        )
    }
}

/// A family prepared for fast rejection: for each modulus m and each y mod m,
/// the set of values F(x, y) mod m over all x, as a bit mask. If n mod m is not
/// in the set for the y being tried, no x can give F(x, y) = n, and the family
/// is skipped without any big-number arithmetic.
pub struct Prepared {
    pub name: String,
    pub f: ZPoly,
    pub d: usize,
    masks: Vec<Vec<u64>>,
}

impl Prepared {
    fn new(name: String, f: ZPoly) -> Prepared {
        let f = trim(f);
        let d = f.len() - 1;
        let masks = PRUNE_MODULI
            .iter()
            .map(|&m| {
                let fm: Vec<u64> = f
                    .iter()
                    .map(|c| c.mod_floor(&BigInt::from(m)).to_u64().unwrap())
                    .collect();
                (0..m)
                    .map(|y| {
                        // coefficients of F(x, y) = sum c_i x^i y^(d-i) mod m
                        let mut ypow = vec![1u64; d + 1];
                        for k in 1..=d {
                            ypow[k] = ypow[k - 1] * y % m;
                        }
                        let cs: Vec<u64> = (0..=d).map(|i| fm[i] * ypow[d - i] % m).collect();
                        let mut mask = 0u64;
                        for x in 0..m {
                            let mut v = 0u64;
                            for c in cs.iter().rev() {
                                v = (v * x + c) % m;
                            }
                            mask |= 1 << v;
                        }
                        mask
                    })
                    .collect()
            })
            .collect();
        Prepared { name, f, d, masks }
    }

    /// false when some modulus proves F(x, y) = n has no whole-number solution x
    pub fn may_hit(&self, res: &Residues, y: u64) -> bool {
        PRUNE_MODULI
            .iter()
            .zip(&self.masks)
            .zip(&res.0)
            .all(|((&m, mk), &r)| mk[(y % m) as usize] >> r & 1 == 1)
    }
}

/// one d-th root of n/lc per (degree, leading coefficient), shared by all families and all y
#[derive(Default)]
pub struct RootCache(std::collections::HashMap<(usize, BigInt), BigInt>);

impl RootCache {
    pub fn get(&mut self, n: &BigInt, d: usize, lc: &BigInt) -> BigInt {
        self.0
            .entry((d, lc.clone()))
            .or_insert_with(|| (n.abs() / lc.abs()).nth_root(d as u32))
            .clone()
    }
}

thread_local! {
    static PREPARED: std::cell::RefCell<Vec<std::rc::Rc<Prepared>>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// the families of families(), each prepared once (masks are cached across calls)
pub fn prepared_families() -> Vec<std::rc::Rc<Prepared>> {
    let (_, all) = families();
    PREPARED.with(|p| {
        let mut p = p.borrow_mut();
        let out: Vec<_> = all
            .into_iter()
            .map(
                |(name, f)| match p.iter().find(|q| q.name == name && q.f == trim(f.clone())) {
                    Some(q) => q.clone(),
                    None => {
                        let q = std::rc::Rc::new(Prepared::new(name, f));
                        p.push(q.clone());
                        q
                    }
                },
            )
            .collect();
        out
    })
}

/// the homogeneous form y^d f(x/y) as a polynomial in x for a fixed y
pub fn homogenize(f: &[BigInt], y: &BigInt) -> ZPoly {
    let d = f.len() - 1;
    f.iter()
        .enumerate()
        .map(|(i, c)| c * y.pow((d - i) as u32))
        .collect()
}

/// homogeneous text: x^2 - 3*x*y + 9*y^2
pub fn show_hzpoly(p: &[BigInt], x: &str, y: &str) -> String {
    let d = p.len() - 1;
    let mut s = String::new();
    for (k, c) in p.iter().enumerate().rev() {
        if c.is_zero() {
            continue;
        }
        let a = c.abs();
        if s.is_empty() {
            if c.is_negative() {
                s.push('-');
            }
        } else {
            s.push_str(if c.is_negative() { " - " } else { " + " });
        }
        let pw = |v: &str, e: usize| match e {
            0 => String::new(),
            1 => v.to_string(),
            _ => format!("{}^{}", v, e),
        };
        let mono: Vec<String> = [pw(x, k), pw(y, d - k)]
            .into_iter()
            .filter(|m| !m.is_empty())
            .collect();
        let mono = mono.join("*");
        if mono.is_empty() {
            s.push_str(&a.to_string());
        } else if a.is_one() {
            s.push_str(&mono);
        } else {
            s.push_str(&format!("{}*{}", a, mono));
        }
    }
    s
}

/// Two-variable search: every catalogued family f (built in or added) is
/// homogenized to F(x, y) = y^d f(x/y), and for y = 2, 3, ... up to 50*effort
/// we look for a whole number x with F(x, y) = n and gcd(x, y) = 1.
/// Returns the family's name, f, x and y.
pub fn catalogue_match2(n: &BigInt, effort: u64) -> Option<(String, ZPoly, BigInt, BigInt)> {
    let fams: Vec<_> = prepared_families()
        .into_iter()
        .filter(|q| q.d >= 2 && !q.f[0].is_zero())
        .collect();
    let res = Residues::new(n);
    let mut roots = RootCache::default();
    let ymax = (50 * effort).min(100_000);
    for yy in 2..=ymax {
        let y = BigInt::from(yy);
        for fam in &fams {
            if !fam.may_hit(&res, yy) {
                continue;
            }
            let est = roots.get(n, fam.d, &fam.f[fam.d]);
            let fy = homogenize(&fam.f, &y);
            if let Some(x) = int_root_from(&fy, n, Some(&est)) {
                if x.abs() >= BigInt::from(2) && crate::numtheory::gcd(&x, &y).is_one() {
                    return Some((fam.name.clone(), fam.f.clone(), x, y));
                }
            }
        }
    }
    None
}

/// the pieces of n = F(x, y): the homogenized factors of f, evaluated at (x, y)
pub fn poly2_pieces(f: &[BigInt], x: &BigInt, y: &BigInt) -> Option<Vec<Piece>> {
    let (c, fs) = factor_zpoly(f)?;
    let mut out = vec![];
    if !c.abs().is_one() {
        out.push(Piece {
            text: c.abs().to_string(),
            latex: c.abs().to_string(),
            value: c.abs(),
            d: 0,
        });
    }
    for (g, m) in fs {
        let v = peval(&homogenize(&g, y), x).abs();
        let gs = show_hzpoly(&g, "x", "y");
        for _ in 0..m {
            out.push(Piece {
                text: gs.clone(),
                latex: gs.clone(),
                value: v.clone(),
                d: 0,
            });
        }
    }
    Some(out)
}

/// Fermat's method: n = a^2 - b^2 for a = ceil(sqrt(n)), ceil(sqrt(n)) + 1, ...
/// (steps tries). It succeeds quickly exactly when n has two factors close to
/// sqrt(n), the case Pollard rho and p - 1 are worst at. Skips n = 2 mod 4,
/// which is never a difference of two squares.
pub fn fermat(n: &BigInt, steps: u64) -> Option<(BigInt, BigInt)> {
    if n <= &BigInt::from(3) || n.mod_floor(&BigInt::from(4)) == BigInt::from(2) {
        return None;
    }
    let mut a = n.sqrt();
    if &(&a * &a) < n {
        a += 1u32;
    }
    let mut r = &a * &a - n;
    for _ in 0..steps {
        let b = r.sqrt();
        if &b * &b == r {
            if (&a - &b) > BigInt::one() {
                return Some((a, b));
            }
            return None;
        }
        // (a + 1)^2 - n = r + 2a + 1
        r += &a * 2u32 + 1u32;
        a += 1u32;
    }
    None
}
