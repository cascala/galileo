//! A teaching number field sieve: the general one (GNFS, polynomial by the
//! base-m method) and the special one (SNFS, polynomial from a form b^k ± 1
//! that the algebraic factoring code recognizes).
//!
//! The steps, as the report prints them:
//! 1. Pick a monic polynomial f of degree d and an integer m with f(m) ≡ 0
//!    (mod N). Let α be a root of f. Then φ(α) = m is a ring map Z[α] → Z/N.
//! 2. Factor bases: primes p up to B on the rational side, pairs (p, r) with
//!    f(r) ≡ 0 (mod p) on the algebraic side (the prime ideals of degree one).
//! 3. Line sieve over pairs (a, b), b = 1, 2, ..., |a| <= A: keep the pairs
//!    where both a - b m and the norm F(a, b) = b^d f(a/b) are smooth (one
//!    large prime allowed on each side).
//! 4. Linear algebra mod 2 picks subsets S whose product is a square on both
//!    sides: exponent vectors, two signs and quadratic characters (Legendre
//!    symbols of a - b s mod primes q outside the factor base).
//! 5. Square roots: on the rational side from the exponents; on the algebraic
//!    side β with β² = f'(α)² ∏ (a - b α), computed q-adically at a prime q
//!    where f splits into linear factors (Hensel lifting at each root, then
//!    interpolation), choosing the signs that make β have small coefficients.
//! 6. X = f'(m) ∏ sqrt(a - b m), Y = β(m): X² ≡ Y² (mod N), gcd(X - Y, N).
//!
//! The parameters are small and the code is plain, so it shows the method at
//! 20 to 50 digits rather than competing with CADO-NFS.

use crate::algfactor::{self as af, ZPoly};
use crate::factorlab::{self as lab, legendre_u64, small_primes, sqrt_mod_u64};
use crate::numtheory::{is_prime, modinv};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;

/// log2 of |x| (0 for x = 0)
pub fn lg(x: &BigInt) -> f64 {
    if x.is_zero() {
        return 0.0;
    }
    let b = x.bits();
    if b < 1000 {
        x.abs().to_f64().unwrap().log2()
    } else {
        let sh = b - 60;
        (x.abs() >> sh).to_f64().unwrap().log2() + sh as f64
    }
}

/// A choice of polynomial: f monic, f(m) ≡ 0 mod N
#[derive(Clone, Debug)]
pub struct Choice {
    pub f: ZPoly,
    pub m: BigInt,
    pub how: String,
    /// estimated log2 of |rational norm| + |algebraic norm| over the sieve region
    pub norm_bits: f64,
    /// Murphy's alpha (root property), in log2 units: negative is good
    pub alpha: f64,
}

impl Choice {
    pub fn score(&self) -> f64 {
        self.norm_bits + self.alpha
    }
    pub fn degree(&self) -> usize {
        self.f.len() - 1
    }
}

const SAMPLE_A: f64 = 30000.0;
const SAMPLE_B: f64 = 600.0;

/// estimated log2 sizes of the two norms, averaged over a typical sieve region
pub fn norm_estimate(f: &[BigInt], m: &BigInt) -> (f64, f64) {
    let c: Vec<f64> = f.iter().map(|x| x.to_f64().unwrap_or(f64::MAX)).collect();
    let mf = m.to_f64().unwrap_or(f64::MAX);
    let d = f.len() - 1;
    let (mut sr, mut sa, mut k) = (0.0, 0.0, 0.0);
    for i in 0..24 {
        let b = SAMPLE_B * (i as f64 + 0.5) / 24.0;
        for j in 0..48 {
            let a = SAMPLE_A * ((j as f64 + 0.5) / 24.0 - 1.0);
            sr += (a - b * mf).abs().max(1.0).log2();
            let mut v = 0.0;
            for t in (0..=d).rev() {
                v = v * a + c[t] * b.powi((d - t) as i32);
            }
            sa += v.abs().max(1.0).log2();
            k += 1.0;
        }
    }
    (sr / k, sa / k)
}

/// Murphy's alpha in log2 units over primes below 200: how much more (or
/// less) often values of f are divisible by small primes than random numbers
pub fn alpha(f: &[BigInt]) -> f64 {
    let mut s = 0.0;
    for p in small_primes(200) {
        let roots = roots_mod(f, p).len() as f64;
        let pf = p as f64;
        s += (1.0 - roots * pf / (pf + 1.0)) * pf.log2() / (pf - 1.0);
    }
    s
}

fn make_choice(f: ZPoly, m: BigInt, how: String) -> Choice {
    let (r, a) = norm_estimate(&f, &m);
    let al = alpha(&f);
    Choice {
        f,
        m,
        how,
        norm_bits: r + a,
        alpha: al,
    }
}

/// The digits of N in base m (low first), as a monic polynomial when m^d <= N < 2 m^d;
/// digits above m/2 are made negative (except the top ones) to keep them small
pub fn base_m_poly(n: &BigInt, d: usize, m: &BigInt) -> Option<ZPoly> {
    let mut f = vec![];
    let mut r = n.clone();
    for _ in 0..d {
        let (q, rem) = r.div_mod_floor(m);
        f.push(rem);
        r = q;
    }
    if !r.is_one() {
        return None;
    }
    f.push(r);
    let half = m / 2;
    for i in 0..d.saturating_sub(1) {
        if f[i] > half {
            f[i] -= m;
            f[i + 1] += 1;
        }
    }
    if f[d - 1] > half && d >= 2 {
        // keep monic; nothing to do
    }
    Some(f)
}

/// Base-m polynomials of degree d with m from floor(N^(1/d)) down `tries`
/// steps; returns them sorted by score (best first)
pub fn base_m_search(n: &BigInt, d: usize, tries: usize) -> Vec<Choice> {
    let m0 = n.nth_root(d as u32);
    let mut out = vec![];
    let step = (&m0 / BigInt::from(2000 * tries.max(1) as u64)).max(BigInt::one());
    for k in 0..tries.max(1) {
        let m = &m0 - &step * BigInt::from(k as u64);
        if m < BigInt::from(2) {
            break;
        }
        if let Some(f) = base_m_poly(n, d, &m) {
            out.push(make_choice(f, m, format!("base m, degree {}", d)));
        }
    }
    out.sort_by(|a, b| a.score().partial_cmp(&b.score()).unwrap());
    out
}

/// A polynomial the user gives: f(m) must be divisible by N. A leading
/// coefficient c other than 1 is removed with g(y) = c^(d-1) f(y / c), root c m.
pub fn user_choice(n: &BigInt, f: &[BigInt], m: &BigInt) -> Result<Choice, String> {
    let f = af::trim(f.to_vec());
    if f.len() < 3 {
        return Err("gnfs: f must have degree 2 or more".into());
    }
    if !(af::peval(&f, m) % n).is_zero() {
        return Err(format!("gnfs: f({}) is not divisible by N", m));
    }
    let d = f.len() - 1;
    let c = f[d].clone();
    if c.is_one() {
        return Ok(make_choice(f, m.clone(), "your polynomial".into()));
    }
    if c.is_zero() || c.is_negative() {
        return Err("gnfs: give f with a positive leading coefficient".into());
    }
    let mut g: ZPoly = (0..d)
        .map(|i| &f[i] * num_traits::pow(c.clone(), d - 1 - i))
        .collect();
    g.push(BigInt::one());
    Ok(make_choice(
        g,
        &c * m,
        format!("your polynomial made monic: y = {} x", c),
    ))
}

/// SNFS polynomials from N | b^k ± 1: with k = d t - s, b^s (b^k ± 1) = (b^t)^d ± b^s
pub fn special_choices(n: &BigInt) -> Vec<Choice> {
    let (base, k, plus) = match af::find_form(n, 1) {
        Some(af::Form::Exact { a, b, k, plus }) if b.is_one() => (a, k, plus),
        Some(af::Form::Divides { b, k, plus, .. }) => (BigInt::from(b), k, plus),
        _ => return vec![],
    };
    let mut out = vec![];
    for d in 2..=6u64 {
        let t = k.div_ceil(d);
        let s = d * t - k;
        if t < 2 {
            continue;
        }
        let m = num_traits::pow(base.clone(), t as usize);
        let c = num_traits::pow(base.clone(), s as usize);
        let mut f = vec![BigInt::zero(); d as usize + 1];
        f[0] = if plus { c } else { -c };
        f[d as usize] = BigInt::one();
        let sign = if plus { "+" } else { "-" };
        let how = format!(
            "special form: N divides {}^{} {} 1, so f(x) = {} with m = {}^{}",
            base,
            k,
            sign,
            af::show_zpoly(&f, "x"),
            base,
            t
        );
        if !(af::peval(&f, &m) % n).is_zero() {
            continue;
        }
        out.push(make_choice(f, m, how));
    }
    out.sort_by(|a, b| a.score().partial_cmp(&b.score()).unwrap());
    out
}

/// The polynomials considered for N: special forms first if any, then base m
/// for degrees 3 to 5; sorted by score
pub fn choices(n: &BigInt, tries: usize) -> Vec<Choice> {
    let mut all = special_choices(n);
    let digits = n.to_string().len();
    let degrees: &[usize] = if digits < 30 {
        &[2, 3]
    } else if digits < 70 {
        &[3, 4]
    } else {
        &[4, 5]
    };
    for &d in degrees {
        all.extend(base_m_search(n, d, tries).into_iter().take(1));
    }
    all.sort_by(|a, b| a.score().partial_cmp(&b.score()).unwrap());
    // base m can land on a special form (x^4 + 1 for 2^128 + 1): keep it once, named as the special form
    let mut seen: Vec<(ZPoly, BigInt)> = vec![];
    all.retain(|c| {
        let k = (c.f.clone(), c.m.clone());
        if seen.contains(&k) {
            false
        } else {
            seen.push(k);
            true
        }
    });
    all
}

// ---------- arithmetic mod small primes ----------

/// roots of f mod p (distinct), from the linear factors of f mod p
pub fn roots_mod(f: &[BigInt], p: u64) -> Vec<u64> {
    let Some((lead, facs)) = af::factor_mod_p(f, p) else {
        return vec![];
    };
    if lead == 0 {
        return vec![];
    }
    let mut r: Vec<u64> = facs
        .iter()
        .filter(|(g, _)| g.len() == 2)
        .map(|(g, _)| (p - g[0] % p) % p)
        .collect();
    r.sort();
    r.dedup();
    r
}

fn eval_mod(f: &[BigInt], x: u64, p: u64) -> u64 {
    let mut v: u128 = 0;
    for c in f.iter().rev() {
        let cm = c.mod_floor(&BigInt::from(p)).to_u64().unwrap() as u128;
        v = (v * x as u128 + cm) % p as u128;
    }
    v as u64
}

fn deriv(f: &[BigInt]) -> ZPoly {
    f.iter()
        .enumerate()
        .skip(1)
        .map(|(i, c)| c * BigInt::from(i as u64))
        .collect()
}

/// F(a, b) = b^d f(a/b)
fn norm_alg(f: &[BigInt], a: i64, b: i64) -> BigInt {
    let d = f.len() - 1;
    let (a, b) = (BigInt::from(a), BigInt::from(b));
    let mut v = BigInt::zero();
    let mut bp = BigInt::one();
    // Horner in a with powers of b: sum c_i a^i b^(d-i)
    let mut terms = vec![BigInt::zero(); d + 1];
    for i in (0..=d).rev() {
        terms[i] = &f[i] * &bp;
        bp *= &b;
    }
    for i in (0..=d).rev() {
        v = v * &a + &terms[i];
    }
    v
}

// ---------- parameters ----------

#[derive(Clone, Debug)]
pub struct Params {
    /// factor base bound (both sides)
    pub fb: u64,
    /// sieve half width: a in [-A, A]
    pub half: i64,
    /// large primes up to lp_mult * fb
    pub lp_mult: u64,
    /// number of quadratic characters
    pub chars: usize,
}

/// parameters from the estimated norm sizes (in bits)
pub fn params_for(norm_bits: f64) -> Params {
    // measured: about 4000 at 95 bits (30 digits), doubling every 6.5 bits
    let fb = (4000.0 * ((norm_bits - 95.0) / 6.5).exp2()).clamp(500.0, 400_000.0) as u64;
    let half = (8000.0 + (norm_bits - 80.0).max(0.0) * 1500.0).min(30000.0) as i64;
    Params {
        fb,
        half,
        lp_mult: 40,
        chars: 24,
    }
}

// ---------- sieving ----------

/// primes below this are not sieved (their share of the logarithm is in the slack)
const SMALL_P: u64 = 5;

#[derive(Clone, Debug)]
pub struct Rel {
    pub a: i64,
    pub b: i64,
    /// a - b m < 0
    pub neg_r: bool,
    /// F(a, b) < 0
    pub neg_a: bool,
    /// (column key, exponent)
    pub keys: Vec<(usize, u32)>,
}

/// key of a prime on a side: side 0 rational (p), side 1 algebraic (p, r)
type Key = (u8, u64, u64);

pub struct Stats {
    pub lines: i64,
    pub candidates: u64,
    pub full: usize,
    pub partial: usize,
    pub rfb: usize,
    pub afb: usize,
}

/// What a run did, step by step
pub struct NfsRun {
    pub factor: Option<BigInt>,
    pub log: Vec<String>,
    pub relations: usize,
    pub columns: usize,
    pub lines: i64,
    pub choice: Option<Choice>,
    /// the numbers of this run, for explain = "teach"
    pub teach: Teach,
}

/// What a run saw, kept so that a teaching explanation can show real numbers
#[derive(Clone, Debug, Default)]
pub struct Teach {
    /// f factors over the integers: its factors, and the factor g with gcd(g(m), N)
    pub f_factors: Vec<ZPoly>,
    /// (g, g(m), gcd(g(m), N), f, m)
    pub alg_split: Option<(ZPoly, BigInt, BigInt, ZPoly, BigInt)>,
    /// factor base bound, large prime bound, sieve half width
    pub fb: u64,
    pub lp: u64,
    pub half: i64,
    /// the first rational primes and algebraic pairs (p, r)
    pub rfb: Vec<u64>,
    pub afb: Vec<(u64, u64)>,
    pub nrfb: usize,
    pub nafb: usize,
    /// sieve counts
    pub pairs: u64,
    pub candidates: u64,
    pub full: usize,
    pub partial: usize,
    /// the first relations found, (a, b)
    pub first: Vec<(i64, i64)>,
    /// two relations sharing one large prime: side (0 rational, 1 algebraic), p, the two pairs
    pub lp_pair: Option<(u8, u64, (i64, i64), (i64, i64))>,
    /// relations left after pruning, primes and ideals they use, matrix size
    pub kept: usize,
    pub used: usize,
    pub rows: usize,
    pub cols: usize,
    pub chars: Vec<(u64, u64)>,
    pub nchars: usize,
    pub deps: usize,
    /// the dependency that gave the factor: number tried, size, digits of the rational product
    pub tried: usize,
    pub dep_size: usize,
    pub product_digits: usize,
    /// prime q where f splits, and its roots
    pub q: u64,
    pub q_roots: Vec<u64>,
    /// X and Y mod N
    pub x: BigInt,
    pub y: BigInt,
}

/// splits x into primes up to `limit` (exponents) and the cofactor left over
pub fn small_parts(x: &BigInt, limit: u64) -> (Vec<(u64, u32)>, BigInt) {
    let mut x = x.abs();
    let mut out = vec![];
    if x.is_zero() {
        return (out, x);
    }
    for p in small_primes(limit as usize) {
        let mut e = 0;
        while (&x % p).is_zero() {
            x /= p;
            e += 1;
        }
        if e > 0 {
            out.push((p, e));
        }
        if x.is_one() {
            break;
        }
    }
    (out, x)
}

/// the norm F(a, b) = b^d f(a/b) of a - b α
pub fn norm_of(f: &[BigInt], a: i64, b: i64) -> BigInt {
    norm_alg(f, a, b)
}

fn small_factor(n: &BigInt, limit: u64) -> Option<BigInt> {
    for p in small_primes(limit as usize) {
        if (n % p).is_zero() && n != &BigInt::from(p) {
            return Some(BigInt::from(p));
        }
    }
    None
}

/// prune relations that have a key nobody else has; returns the kept ones and the number of keys in use
fn prune(rels: &[Rel]) -> (Vec<usize>, usize) {
    let mut alive: Vec<usize> = (0..rels.len()).collect();
    loop {
        let mut count: HashMap<usize, u32> = HashMap::new();
        for &i in &alive {
            for &(k, e) in &rels[i].keys {
                if e % 2 == 1 {
                    *count.entry(k).or_insert(0) += 1;
                }
            }
        }
        let before = alive.len();
        alive.retain(|&i| {
            rels[i]
                .keys
                .iter()
                .all(|&(k, e)| e % 2 == 0 || count[&k] >= 2)
        });
        if alive.len() == before {
            return (alive, count.len());
        }
    }
}

/// null space of a GF(2) matrix with rows `rows` (bitsets over ncols): sets of row indices
fn dependencies(rows: &[Vec<u64>], ncols: usize, want: usize) -> Vec<Vec<usize>> {
    let r = rows.len();
    let hw = r.div_ceil(64);
    let cw = ncols.div_ceil(64);
    let mut m: Vec<(Vec<u64>, Vec<u64>)> = rows
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let mut h = vec![0u64; hw];
            h[i / 64] |= 1 << (i % 64);
            (v.clone(), h)
        })
        .collect();
    lab::spend((r * r * (cw + hw)) as f64 / 64.0);
    let mut pr = 0;
    for c in 0..ncols {
        let Some(p) = (pr..r).find(|&i| m[i].0[c / 64] >> (c % 64) & 1 == 1) else {
            continue;
        };
        m.swap(pr, p);
        let (pv, ph) = m[pr].clone();
        for i in 0..r {
            if i != pr && m[i].0[c / 64] >> (c % 64) & 1 == 1 {
                for w in 0..cw {
                    m[i].0[w] ^= pv[w];
                }
                for w in 0..hw {
                    m[i].1[w] ^= ph[w];
                }
            }
        }
        pr += 1;
    }
    let mut out = vec![];
    for (v, h) in m.iter().skip(pr) {
        if v.iter().any(|&w| w != 0) {
            continue;
        }
        out.push((0..r).filter(|&i| h[i / 64] >> (i % 64) & 1 == 1).collect());
        if out.len() >= want {
            break;
        }
    }
    out
}

// ---------- the square root in the number field ----------

/// symmetric difference of two sorted lists
fn symdiff(a: &[usize], b: &[usize]) -> Vec<usize> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0, 0);
    while i < a.len() || j < b.len() {
        if j == b.len() || (i < a.len() && a[i] < b[j]) {
            out.push(a[i]);
            i += 1;
        } else if i == a.len() || b[j] < a[i] {
            out.push(b[j]);
            j += 1;
        } else {
            i += 1;
            j += 1;
        }
    }
    out
}

fn lift_root(f: &[BigInt], fp: &[BigInt], r0: u64, q: &BigInt, e: u32) -> Option<BigInt> {
    let mut r = BigInt::from(r0);
    let mut k = 1u32;
    while k < e {
        k = (2 * k).min(e);
        let mk = q.pow(k);
        let fr = af::peval(f, &r).mod_floor(&mk);
        let dr = af::peval(fp, &r).mod_floor(&mk);
        r = (&r - fr * modinv(&dr, &mk)?).mod_floor(&mk);
    }
    Some(r)
}

fn lift_sqrt(c: &BigInt, s0: u64, q: &BigInt, e: u32) -> Option<BigInt> {
    let mut s = BigInt::from(s0);
    let mut k = 1u32;
    while k < e {
        k = (2 * k).min(e);
        let mk = q.pow(k);
        let g = (&s * &s - c).mod_floor(&mk);
        let inv = modinv(&(BigInt::from(2) * &s), &mk)?;
        s = (&s - g * inv).mod_floor(&mk);
    }
    Some(s)
}

/// a b mod f (f monic)
fn mulmod_f(a: &[BigInt], b: &[BigInt], f: &[BigInt]) -> ZPoly {
    let d = f.len() - 1;
    let mut r = af::pmul(a, b);
    while r.len() > d {
        let top = r.pop().unwrap();
        let k = r.len() - d;
        for i in 0..d {
            r[k + i] -= &top * &f[i];
        }
    }
    r
}

/// a prime q above `from` where f splits into distinct linear factors
fn split_prime(f: &[BigInt], from: u64) -> Option<(u64, Vec<u64>)> {
    let d = f.len() - 1;
    let mut q = from | 1;
    for _ in 0..200_000 {
        q += 2;
        if !is_prime(&BigInt::from(q)) {
            continue;
        }
        let r = roots_mod(f, q);
        if r.len() == d {
            return Some((q, r));
        }
    }
    None
}

/// Y = β(m) mod N where β² = f'(α)² ∏ (a - b α); checked by Y² ≡ X² (mod N)
fn alg_sqrt(
    f: &[BigInt],
    m: &BigInt,
    n: &BigInt,
    pairs: &[(i64, i64)],
    x: &BigInt,
    split: &(u64, Vec<u64>),
    log: &mut Vec<String>,
) -> Option<BigInt> {
    let d = f.len() - 1;
    let fp = deriv(f);
    let (q, roots) = split;
    let qb = BigInt::from(*q);
    // bound on the size of β from the embeddings: |a - b α| <= |a| + b R, R = 1 + max |c_i|
    let rr = 1.0 + f.iter().map(lg).fold(0.0, f64::max).exp2();
    let lr = rr.log2();
    let mut bits: f64 = pairs
        .iter()
        .map(|&(a, b)| ((a.abs() as f64) + (b as f64) * rr).log2())
        .sum::<f64>();
    bits = 0.5 * bits + (d as f64).log2() * 2.0 + (d as f64 - 1.0) * lr + 64.0 + 4.0 * d as f64;
    let x2 = (x * x).mod_floor(n);
    // ∏ (a - b x) mod f, exactly, by a product tree
    let mut layer: Vec<ZPoly> = pairs
        .iter()
        .map(|&(a, b)| vec![BigInt::from(a), BigInt::from(-b)])
        .collect();
    while layer.len() > 1 {
        layer = layer
            .chunks(2)
            .map(|c| {
                if c.len() == 2 {
                    mulmod_f(&c[0], &c[1], f)
                } else {
                    c[0].clone()
                }
            })
            .collect();
        lab::spend(layer.len() as f64 * 4.0);
    }
    let gamma = layer.pop().unwrap_or_else(|| vec![BigInt::one()]);
    for attempt in 0..2 {
        let e = (bits / (*q as f64).log2()).ceil() as u32 + 1;
        let qe = qb.pow(e);
        let mut s = vec![];
        let mut rs = vec![];
        for &r0 in roots {
            let r = lift_root(f, &fp, r0, &qb, e)?;
            let mut c = af::peval(&fp, &r).mod_floor(&qe);
            c = (&c * &c) % &qe;
            c = (c * af::peval(&gamma, &r)).mod_floor(&qe);
            let cq = (&c % &qb).to_u64().unwrap();
            let s0 = sqrt_mod_u64(cq, *q);
            if (s0 as u128 * s0 as u128 % *q as u128) as u64 != cq {
                log.push(format!("    not a square modulo q = {} (the quadratic characters missed something), next dependency", q));
                return None;
            }
            s.push(lift_sqrt(&c, s0, &qb, e)?);
            rs.push(r);
        }
        // Lagrange basis: v_i = s_i prod_{j != i} (x - r_j)/(r_i - r_j)
        let mut v: Vec<Vec<BigInt>> = vec![];
        for i in 0..d {
            let mut num = vec![BigInt::one()];
            let mut den = BigInt::one();
            for j in 0..d {
                if j == i {
                    continue;
                }
                let mut nx = vec![BigInt::zero(); num.len() + 1];
                for (k, c) in num.iter().enumerate() {
                    nx[k + 1] += c;
                    nx[k] -= c * &rs[j];
                }
                num = nx.into_iter().map(|c| c.mod_floor(&qe)).collect();
                den = (den * (&rs[i] - &rs[j])).mod_floor(&qe);
            }
            let w = (modinv(&den, &qe)? * &s[i]) % &qe;
            v.push(num.into_iter().map(|c| (c * &w) % &qe).collect());
        }
        let half = &qe / 2;
        for mask in 0u32..(1 << (d - 1)) {
            let mut beta = vec![BigInt::zero(); d];
            for (i, vi) in v.iter().enumerate() {
                let neg = i > 0 && (mask >> (i - 1)) & 1 == 1;
                for (k, c) in vi.iter().enumerate() {
                    if neg {
                        beta[k] -= c;
                    } else {
                        beta[k] += c;
                    }
                }
            }
            let mut big = 0.0f64;
            for c in beta.iter_mut() {
                *c = c.mod_floor(&qe);
                if *c > half {
                    *c -= &qe;
                }
                big = big.max(lg(c));
            }
            let y = af::peval(&beta, m).mod_floor(n);
            if (&y * &y).mod_floor(n) == x2 {
                log.push(format!(
                    "    square root found {}-adically to {} digits; its coefficients have about {:.0} digits",
                    q,
                    (e as f64 * (*q as f64).log10()).round(),
                    big * std::f64::consts::LOG10_2
                ));
                return Some(y);
            }
        }
        bits *= 2.0;
        if attempt == 1 {
            log.push("    no square root with whole coefficients: the product is not a square in Z[α] after all (the characters only make that likely), next dependency".into());
        }
    }
    None
}

// ---------- the whole run ----------

/// Runs the number field sieve on n with the given polynomial (or the best
/// one found), within the factoring lab budget.
pub fn nfs(n: &BigInt, choice: Option<Choice>, params: Option<Params>) -> NfsRun {
    let mut log: Vec<String> = vec![];
    let clock = lab::Clock::start();
    let at = |c: &lab::Clock| {
        c.seconds()
            .map(|s| format!(" [{:.2} s]", s))
            .unwrap_or_default()
    };
    let mut run = NfsRun {
        factor: None,
        log: vec![],
        relations: 0,
        columns: 0,
        lines: 0,
        choice: None,
        teach: Teach::default(),
    };
    macro_rules! done {
        ($f:expr) => {{
            run.factor = $f;
            run.log = log;
            return run;
        }};
    }
    if n.is_even() {
        done!(Some(BigInt::from(2)));
    }
    if is_prime(n) {
        log.push(format!("{} is prime", n));
        done!(None);
    }
    // 1. polynomial
    let ch = match choice {
        Some(c) => c,
        None => {
            let all = choices(n, 40);
            log.push("1. Polynomials considered (score = estimated bits of the two norms + alpha; lower is better):".into());
            for c in all.iter().take(5) {
                log.push(format!(
                    "    {:>6.1}  f(x) = {}, m = {}  [{}]",
                    c.score(),
                    af::show_zpoly(&c.f, "x"),
                    c.m,
                    c.how
                ));
            }
            all.into_iter().next().unwrap()
        }
    };
    let f = ch.f.clone();
    let m = ch.m.clone();
    let d = f.len() - 1;
    log.push(format!(
        "1. f(x) = {}, m = {} ({}): f(m) ≡ 0 mod N",
        af::show_zpoly(&f, "x"),
        m,
        ch.how
    ));
    if !(af::peval(&f, &m) % n).is_zero() || !f.last().unwrap().is_one() || d < 2 {
        log.push("    f must be monic of degree 2 or more with f(m) ≡ 0 mod N".into());
        done!(None);
    }
    // a polynomial that factors gives the factors of f(m) directly
    if let Some((_, facs)) = af::factor_zpoly(&f) {
        if facs.len() > 1 || facs[0].1 > 1 {
            log.push(format!(
                "    f factors over the integers: {}",
                facs.iter()
                    .map(|(g, _)| format!("({})", af::show_zpoly(g, "x")))
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
            for (g, _) in &facs {
                let gv = af::peval(g, &m).gcd(n);
                if gv > BigInt::one() && &gv < n {
                    run.teach.f_factors = facs.iter().map(|(g, _)| g.clone()).collect();
                    run.teach.alg_split = Some((g.clone(), af::peval(g, &m), gv.clone(), f.clone(), m.clone()));
                    log.push(format!(
                        "    gcd(g(m), N) = {} for the factor g = {}: done without sieving",
                        gv,
                        af::show_zpoly(g, "x")
                    ));
                    done!(Some(gv));
                }
            }
            // N divides one factor's value: continue with that factor
            if let Some((g, _)) = facs.iter().find(|(g, _)| {
                g.len() >= 3 && (af::peval(g, &m) % n).is_zero() && g.last().unwrap().is_one()
            }) {
                log.push(format!(
                    "    N divides g(m) for g = {}; continuing with g",
                    af::show_zpoly(g, "x")
                ));
                let c = make_choice(g.clone(), m.clone(), ch.how.clone());
                let mut r = nfs(n, Some(c), params);
                log.append(&mut r.log);
                r.log = log;
                return r;
            }
            log.push("    no factor of f helps".into());
            done!(None);
        }
    }
    if let Some(p) = small_factor(n, 1000) {
        log.push(format!(
            "    N has the small factor {}: no sieving needed",
            p
        ));
        done!(Some(p));
    }
    let pr = params.unwrap_or_else(|| params_for(ch.norm_bits));
    let (rn, an) = norm_estimate(&f, &m);
    log.push(format!(
        "    typical norms: rational about 10^{:.0}, algebraic about 10^{:.0}; Murphy alpha {:.2}",
        rn * std::f64::consts::LOG10_2,
        an * std::f64::consts::LOG10_2,
        ch.alpha
    ));
    run.choice = Some(ch.clone());
    // 2. factor bases
    let primes = small_primes(pr.fb as usize);
    let rfb: Vec<(u64, u64, u8)> = primes
        .iter()
        .map(|&p| {
            (
                p,
                (&m % p).to_u64().unwrap(),
                (p as f64).log2().round() as u8,
            )
        })
        .collect();
    let mut afb: Vec<(u64, u64, u8)> = vec![];
    for &p in &primes {
        for r in roots_mod(&f, p) {
            afb.push((p, r, (p as f64).log2().round() as u8));
        }
        lab::spend(d as f64 * 2.0);
    }
    let lp = pr.fb * pr.lp_mult;
    run.teach.fb = pr.fb;
    run.teach.lp = lp;
    run.teach.half = pr.half;
    run.teach.rfb = rfb.iter().take(12).map(|t| t.0).collect();
    run.teach.afb = afb.iter().take(12).map(|t| (t.0, t.1)).collect();
    run.teach.nrfb = rfb.len();
    run.teach.nafb = afb.len();
    log.push(format!(
        "2. Factor bases up to {}: {} rational primes, {} algebraic pairs (p, r) with f(r) ≡ 0 mod p; large primes up to {}",
        pr.fb,
        rfb.len(),
        afb.len(),
        lp
    ));
    // quadratic characters
    let mut chars: Vec<(u64, u64)> = vec![];
    let mut q = lp + 1;
    while chars.len() < pr.chars {
        q += 1;
        if !is_prime(&BigInt::from(q)) {
            continue;
        }
        let fp = deriv(&f);
        for r in roots_mod(&f, q) {
            if eval_mod(&fp, r, q) != 0 && chars.len() < pr.chars {
                chars.push((q, r));
            }
        }
    }
    let split = match split_prime(&f, q.max(1000)) {
        Some(s) => s,
        None => {
            log.push("    found no prime where f splits into linear factors".into());
            done!(None);
        }
    };
    // 3. sieve
    let half = pr.half;
    let w = (2 * half + 1) as usize;
    let cf: Vec<f64> = f.iter().map(|c| c.to_f64().unwrap()).collect();
    let mf = m.to_f64().unwrap();
    let lpbits = (lp as f64).log2();
    let mut keys: HashMap<Key, usize> = HashMap::new();
    let mut rels: Vec<Rel> = vec![];
    let mut sr = vec![0u8; w];
    let mut sa = vec![0u8; w];
    let mut stats = Stats {
        lines: 0,
        candidates: 0,
        full: 0,
        partial: 0,
        rfb: rfb.len(),
        afb: afb.len(),
    };
    let mut next_check = 0usize;
    let mut ready: Option<(Vec<usize>, usize)> = None;
    let fudge = 4.0;
    let mut b: i64 = 0;
    let mut cand: Vec<i64> = vec![];
    let mut win = (0i64, 0usize);
    let mut stalled = false;
    'lines: loop {
        b += 1;
        stats.lines = b;
        if lab::out_of_budget() || b > 50_000_000 {
            break;
        }
        sr.iter_mut().for_each(|x| *x = 0);
        sa.iter_mut().for_each(|x| *x = 0);
        let mut updates = 0u64;
        for &(p, mp, l) in &rfb {
            if p < SMALL_P {
                continue;
            }
            let st = ((b as u64 % p) * mp + half as u64 % p) % p;
            let mut i = st as usize;
            while i < w {
                sr[i] += l;
                i += p as usize;
            }
            updates += w as u64 / p + 1;
        }
        for &(p, r, l) in &afb {
            if p < SMALL_P {
                continue;
            }
            let st = ((b as u64 % p) * r + half as u64 % p) % p;
            let mut i = st as usize;
            while i < w {
                sa[i] += l;
                i += p as usize;
            }
            updates += w as u64 / p + 1;
        }
        if lab::spend(updates as f64 / 150.0 + w as f64 / 100.0) {
            break;
        }
        // the smallest possible rational norm on this line
        let bm = b as f64 * mf;
        let bf = b as f64;
        let cb: Vec<f64> = (0..=d).map(|t| cf[t] * bf.powi((d - t) as i32)).collect();
        // rational thresholds per block of 64: the smallest |a - b m| in the block
        let mut i0 = 0usize;
        while i0 < w {
            let i1 = (i0 + 64).min(w);
            let (a0, a1) = ((i0 as i64 - half) as f64, (i1 as i64 - 1 - half) as f64);
            let lo = if a0 <= bm && bm <= a1 {
                1.0
            } else {
                (a0 - bm).abs().min((a1 - bm).abs()).max(1.0)
            };
            let tr = (lo.log2() - lpbits - fudge).max(0.0);
            let tr = if tr > 255.0 { 255u8 } else { tr as u8 };
            for i in i0..i1 {
                if sr[i] < tr {
                    continue;
                }
                let a = i as i64 - half;
                let nr = (a as f64 - bm).abs().max(1.0).log2();
                if (sr[i] as f64) < nr - lpbits - fudge {
                    continue;
                }
                let af_ = a as f64;
                let mut v = 0.0;
                for t in (0..=d).rev() {
                    v = v * af_ + cb[t];
                }
                let na = v.abs().max(1.0).log2();
                if (sa[i] as f64) < na - lpbits - fudge {
                    continue;
                }
                cand.push(a);
            }
            i0 = i1;
        }
        for a in std::mem::take(&mut cand) {
            if a.gcd(&b) != 1 {
                continue;
            }
            stats.candidates += 1;
            lab::spend(((rfb.len() + afb.len()) as f64) / 20.0);
            // 4. check both norms by trial division
            let mut kv: Vec<(usize, u32)> = vec![];
            let mut nr = BigInt::from(a) - BigInt::from(b) * &m;
            let neg_r = nr.is_negative();
            nr = nr.abs();
            if nr.is_zero() {
                continue;
            }
            let am = |p: u64| a.rem_euclid(p as i64) as u64;
            let mut pend: Vec<(Key, u32)> = vec![];
            for &(p, mp, _) in &rfb {
                if (am(p) + p - (b as u64 % p) * mp % p) % p == 0 {
                    let mut e = 0;
                    while (&nr % p).is_zero() {
                        nr /= p;
                        e += 1;
                    }
                    pend.push(((0, p, 0), e));
                }
            }
            let mut large = 0;
            if !nr.is_one() {
                match nr.to_u64() {
                    Some(v) if v < lp => {
                        pend.push(((0, v, 0), 1));
                        large += 1;
                    }
                    _ => continue,
                }
            }
            let mut na_ = norm_alg(&f, a, b);
            let neg_a = na_.is_negative();
            na_ = na_.abs();
            if na_.is_zero() {
                continue;
            }
            for &(p, r, _) in &afb {
                if (am(p) + p - (b as u64 % p) * r % p) % p == 0 {
                    let mut e = 0;
                    while (&na_ % p).is_zero() {
                        na_ /= p;
                        e += 1;
                    }
                    if e > 0 {
                        pend.push(((1, p, r), e));
                    }
                }
            }
            if !na_.is_one() {
                match na_.to_u64() {
                    Some(v) if v < lp && v > pr.fb => {
                        let binv = lab::inv_mod_u64(b as u64 % v, v);
                        let Some(binv) = binv else { continue };
                        let r = (am(v) as u128 * binv as u128 % v as u128) as u64;
                        pend.push(((1, v, r), 1));
                        large += 1;
                    }
                    _ => continue,
                }
            }
            for (k, e) in pend {
                let id = keys.len();
                let id = *keys.entry(k).or_insert(id);
                kv.push((id, e));
            }
            if large == 0 {
                stats.full += 1;
            } else {
                stats.partial += 1;
            }
            rels.push(Rel {
                a,
                b,
                neg_r,
                neg_a,
                keys: kv,
            });
        }
        if rels.len() >= next_check {
            let (alive, used) = prune(&rels);
            let need = used + 2 + chars.len() + 16;
            if alive.len() >= need {
                ready = Some((alive, used));
                break 'lines;
            }
            next_check = rels.len() + (rels.len() / 40).max(20);
            // stalled: at this rate the norms (which grow with b) win
            let (wl, wr) = (b - win.0, rels.len() - win.1);
            if b >= 4000 && wl >= 1000 {
                let raw_need = rels.len() as f64 * need as f64 / alive.len().max(1) as f64;
                let rate = wr as f64 / wl as f64;
                if rate <= 0.0 || (raw_need - rels.len() as f64) / rate > 100.0 * b as f64 {
                    stalled = true;
                    break 'lines;
                }
                win = (b, rels.len());
            }
        }
    }
    run.lines = b;
    run.teach.pairs = b as u64 * w as u64;
    run.teach.candidates = stats.candidates;
    run.teach.full = stats.full;
    run.teach.partial = stats.partial;
    run.teach.first = rels.iter().take(200).map(|r| (r.a, r.b)).collect();
    run.teach.chars = chars.iter().take(4).cloned().collect();
    run.teach.nchars = chars.len();
    {
        // two relations that share a large prime: together they cancel it
        let big: HashMap<usize, Key> = keys
            .iter()
            .filter(|(k, _)| k.1 > pr.fb)
            .map(|(k, &id)| (id, *k))
            .collect();
        let mut owners: HashMap<usize, Vec<usize>> = HashMap::new();
        for (i, r) in rels.iter().enumerate() {
            for (k, _) in &r.keys {
                if big.contains_key(k) {
                    owners.entry(*k).or_default().push(i);
                }
            }
        }
        if let Some((k, o)) = owners
            .iter()
            .filter(|(_, o)| o.len() == 2)
            .min_by_key(|(_, o)| (o[0], o[1]))
        {
            let key = big[k];
            let (r1, r2) = (&rels[o[0]], &rels[o[1]]);
            run.teach.lp_pair = Some((key.0, key.1, (r1.a, r1.b), (r2.a, r2.b)));
        }
    }
    log.push(format!(
        "3. Sieved {} lines b = 1..{} with |a| <= {} ({} pairs): {} candidates, {} relations ({} full, {} with large primes)",
        b,
        b,
        half,
        b as u64 * w as u64,
        stats.candidates,
        rels.len(),
        stats.full,
        stats.partial
    ) + &at(&clock));
    let Some((alive, used)) = ready else {
        if stalled && pr.fb < 1_000_000 {
            let mut p2 = pr.clone();
            p2.fb *= 2;
            log.push(format!("    too few relations for this factor base (the norms grow with b faster than relations come); starting over with factor bases up to {}", p2.fb));
            let mut r = nfs(n, Some(ch), Some(p2));
            log.append(&mut r.log);
            r.log = log;
            return r;
        }
        log.push("    ran out of budget before there were enough relations".into());
        done!(None);
    };
    // 5. matrix: rows are groups of relations; a prime that only two groups have is removed by merging them
    let mut groups: Vec<(Vec<usize>, Vec<usize>, u32)> = alive
        .iter()
        .map(|&i| {
            let r = &rels[i];
            let mut ks: Vec<usize> = r
                .keys
                .iter()
                .filter(|(_, e)| e % 2 == 1)
                .map(|&(k, _)| k)
                .collect();
            ks.sort_unstable();
            let mut extra = (r.neg_r as u32) | (r.neg_a as u32) << 1;
            for (j, &(q, s)) in chars.iter().enumerate() {
                let x = (r.a as i128 - r.b as i128 * s as i128).rem_euclid(q as i128) as u64;
                if legendre_u64(x, q) == -1 {
                    extra |= 1 << (2 + j);
                }
            }
            (vec![i], ks, extra)
        })
        .collect();
    let rows_before = groups.len();
    // structured elimination: a prime that few groups have is removed by adding the
    // sparsest of them to the others (weight 1: that group cannot be in a square)
    for pass in 0..60 {
        let maxw = (2 + pass).min(40);
        let mut owners: HashMap<usize, Vec<usize>> = HashMap::new();
        for (gi, g) in groups.iter().enumerate() {
            for &k in &g.1 {
                owners.entry(k).or_default().push(gi);
            }
        }
        let mut cols: Vec<(usize, Vec<usize>)> = owners
            .into_iter()
            .filter(|(_, v)| v.len() <= maxw)
            .collect();
        cols.sort_by_key(|(k, v)| (v.len(), *k));
        let mut touched = vec![false; groups.len()];
        let mut dead = vec![false; groups.len()];
        let mut changed = false;
        for (_, own) in cols {
            if own.iter().any(|&g| touched[g]) {
                continue;
            }
            let &piv = own.iter().min_by_key(|&&g| groups[g].1.len()).unwrap();
            if own
                .iter()
                .any(|&g| groups[g].1.len() + groups[piv].1.len() > 1500)
            {
                continue;
            }
            for &g in &own {
                touched[g] = true;
                if g != piv {
                    let (k2, m2) = (
                        symdiff(&groups[g].1, &groups[piv].1),
                        symdiff(&groups[g].0, &groups[piv].0),
                    );
                    let e2 = groups[g].2 ^ groups[piv].2;
                    groups[g] = (m2, k2, e2);
                }
            }
            dead[piv] = true;
            changed = true;
        }
        lab::spend(groups.len() as f64 * 4.0);
        groups = groups
            .into_iter()
            .enumerate()
            .filter(|(gi, _)| !dead[*gi])
            .map(|(_, g)| g)
            .collect();
        if !changed && maxw == 40 {
            break;
        }
    }
    // keep 32 more rows than columns, dropping the largest groups
    let ncols0 = 2
        + chars.len()
        + groups
            .iter()
            .flat_map(|g| g.1.iter())
            .collect::<std::collections::HashSet<_>>()
            .len();
    if groups.len() > ncols0 + 32 {
        groups.sort_by_key(|g| g.0.len());
        groups.truncate(ncols0 + 32);
    }
    let mut colmap: HashMap<usize, usize> = HashMap::new();
    for g in &groups {
        for &k in &g.1 {
            let c = colmap.len();
            colmap.entry(k).or_insert(c);
        }
    }
    let ncols = 2 + chars.len() + colmap.len();
    let cw = ncols.div_ceil(64);
    let rows: Vec<Vec<u64>> = groups
        .iter()
        .map(|g| {
            let mut v = vec![0u64; cw];
            for c in 0..2 + chars.len() {
                if g.2 >> c & 1 == 1 {
                    v[c / 64] ^= 1 << (c % 64);
                }
            }
            for &k in &g.1 {
                let c = 2 + chars.len() + colmap[&k];
                v[c / 64] ^= 1 << (c % 64);
            }
            v
        })
        .collect();
    run.relations = alive.len();
    run.columns = ncols;
    run.teach.kept = rows_before;
    run.teach.used = used;
    run.teach.rows = groups.len();
    run.teach.cols = ncols;
    log.push(format!(
        "4. After removing relations with a prime nobody else has: {} relations with {} primes and ideals. Structured elimination (adding relations to remove primes that few of them have): matrix {} x {} (2 signs, {} quadratic characters)",
        rows_before,
        used,
        groups.len(),
        ncols,
        chars.len()
    ));
    let deps: Vec<Vec<usize>> = dependencies(&rows, ncols, 24)
        .into_iter()
        .map(|d| {
            d.into_iter()
                .fold(vec![], |acc, gi| symdiff(&acc, &groups[gi].0))
        })
        .filter(|d: &Vec<usize>| !d.is_empty())
        .collect();
    // The basis the elimination returns can be lopsided: whether a set gives a
    // factor is a +-1 character of the set, and the first basis vectors often all
    // have the same value. Random sums of basis vectors split half and half.
    let basis = deps;
    let mut rng = lab::Rng::new(0x5eed);
    let deps: Vec<Vec<usize>> = (0..basis.len())
        .map(|t| {
            let mut d = basis[t].clone();
            for (u, b) in basis.iter().enumerate() {
                if u != t && rng.below(2) == 1 {
                    d = symdiff(&d, b);
                }
            }
            d
        })
        .filter(|d| !d.is_empty())
        .collect();
    log.push(format!("    linear algebra mod 2: {} dependencies (subsets whose product is a square on both sides)", deps.len()) + &at(&clock));
    // 6. square roots
    let fpm = af::peval(&deriv(&f), &m).mod_floor(n);
    let rkeys: HashMap<usize, u64> = keys
        .iter()
        .filter(|(k, _)| k.0 == 0)
        .map(|(k, &id)| (id, k.1))
        .collect();
    log.push(format!("5. Square roots: rational side from the exponents, algebraic side at q = {} where f splits", split.0));
    run.teach.deps = deps.len();
    run.teach.q = split.0;
    run.teach.q_roots = split.1.clone();
    for (t, dep) in deps.iter().enumerate() {
        if lab::out_of_budget() {
            break;
        }
        let mut tot: HashMap<usize, u32> = HashMap::new();
        let mut pairs = vec![];
        for &ri in dep {
            let r = &rels[ri];
            pairs.push((r.a, r.b));
            for &(k, e) in &r.keys {
                if rkeys.contains_key(&k) {
                    *tot.entry(k).or_insert(0) += e;
                }
            }
        }
        let mut x = fpm.clone();
        for (k, e) in &tot {
            debug_assert!(e % 2 == 0);
            x = x * BigInt::from(rkeys[k]).modpow(&BigInt::from(e / 2), n) % n;
        }
        log.push(format!(
            "    dependency {}: {} relations",
            t + 1,
            pairs.len()
        ));
        let Some(y) = alg_sqrt(&f, &m, n, &pairs, &x, &split, &mut log) else {
            continue;
        };
        let g = (&x - &y).gcd(n);
        if g > BigInt::one() && &g < n {
            run.teach.tried = t + 1;
            run.teach.dep_size = pairs.len();
            run.teach.product_digits = pairs
                .iter()
                .fold(BigInt::one(), |acc, &(a, b)| acc * (BigInt::from(a) - BigInt::from(b) * &m))
                .abs()
                .to_string()
                .len();
            run.teach.x = x.mod_floor(n);
            run.teach.y = y.mod_floor(n);
            log.push(
                format!(
                    "6. X = f'(m) ∏ sqrt(a - b m), Y = β(m): X² ≡ Y² (mod N), gcd(X - Y, N) = {}",
                    g
                ) + &at(&clock),
            );
            done!(Some(g));
        }
        log.push("    X ≡ ±Y: trivial, next dependency".into());
    }
    log.push("    no dependency gave a factor".into());
    done!(None)
}

/// The scoreboard method: GNFS with the best polynomial, no report
pub fn nfs_method(n: &BigInt) -> Option<BigInt> {
    nfs(n, None, None).factor
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_m_is_monic() {
        let (n, _, _) = lab::semiprime(30, 1);
        for d in 2..=5 {
            let c = &base_m_search(&n, d, 5)[0];
            assert!(c.f.last().unwrap().is_one());
            assert_eq!(af::peval(&c.f, &c.m), n);
        }
    }

    #[test]
    fn nfs_factors_small_semiprimes() {
        for (digits, seed) in [(20, 1), (26, 2)] {
            let (n, p, q) = lab::semiprime(digits, seed);
            lab::start_budget(1e12, 60.0);
            let r = nfs(&n, None, None);
            lab::end_budget();
            let f = r.factor.unwrap_or_else(|| panic!("{}", r.log.join("\n")));
            assert!(f == p || f == q, "{}", r.log.join("\n"));
        }
    }
}
