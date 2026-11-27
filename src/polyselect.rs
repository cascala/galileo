//! Polynomial selection for the number field sieve, as an explorer.
//!
//! The number field sieve needs two polynomials with a common root m mod N:
//! here f of degree d and g(x) = x - m, with f(m) = N exactly (the base-m
//! method). Sieving looks for pairs (a, b) where both F(a, b) = b^d f(a/b) and
//! G(a, b) = a - b m are smooth, so f should take small values (small
//! coefficients, after choosing a skew s that balances them) and have many
//! roots modulo small primes (Murphy's alpha, negative is good). Murphy's E
//! score combines both into an estimate of how often F and G are smooth
//! together. Rotations f + (j x + k) g keep the root m, so they are free
//! changes; the root sieve picks the rotation with the best alpha.

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{ToPrimitive, Zero};

pub type Poly = Vec<BigInt>;

fn primes(n: u64) -> Vec<u64> {
    crate::numtheory::small::primes_upto(n as usize)
        .into_iter()
        .map(|p| p as u64)
        .collect()
}

/// f with leading coefficient ad and f(m) = n, by writing n - ad m^d in base m
/// with balanced digits (each |a_i| <= m/2, except a_{d-1} may be larger when m is
/// not close to (n/ad)^(1/d))
pub fn base_m(n: &BigInt, d: usize, ad: &BigInt, m: &BigInt) -> Poly {
    let mut f = vec![BigInt::zero(); d + 1];
    f[d] = ad.clone();
    let mut r = n - ad * m.pow(d as u32);
    for i in (1..d).rev() {
        let mi = m.pow(i as u32);
        // round to nearest
        let num: BigInt = &r * 2u32 + &mi;
        let q: BigInt = num.div_floor(&(&mi * 2u32));
        f[i] = q.clone();
        r -= q * mi;
    }
    f[0] = r;
    f
}

/// the m for leading coefficient ad: round((n/ad)^(1/d))
pub fn root_for(n: &BigInt, d: usize, ad: &BigInt) -> BigInt {
    (n / ad).nth_root(d as u32)
}

pub fn eval(f: &[BigInt], x: &BigInt) -> BigInt {
    f.iter().rev().fold(BigInt::zero(), |acc, c| acc * x + c)
}

/// f + (j x + k)(x - m)
pub fn rotate(f: &[BigInt], m: &BigInt, j: i64, k: i64) -> Poly {
    let mut g = f.to_vec();
    let (j, k) = (BigInt::from(j), BigInt::from(k));
    g[2] += &j;
    g[1] += &k - &j * m;
    g[0] -= &k * m;
    g
}

fn coeffs_f64(f: &[BigInt]) -> Vec<f64> {
    f.iter().map(|c| c.to_f64().unwrap_or(f64::MAX)).collect()
}

/// F(x, y) = sum a_i x^i y^(d-i) in floating point
fn eval_hom(a: &[f64], x: f64, y: f64) -> f64 {
    let d = a.len() - 1;
    let mut s = 0.0;
    let mut xp = 1.0;
    let ypows: Vec<f64> = (0..=d).map(|k| y.powi(k as i32)).collect();
    for (i, c) in a.iter().enumerate() {
        s += c * xp * ypows[d - i];
        xp *= x;
    }
    s
}

/// Murphy's alpha over primes up to b (simple roots only; exact for primes not
/// dividing the discriminant): sum of ln p/(p - 1) * (1 - q_p p/(p + 1)), where
/// q_p counts the roots of f mod p, a projective root included when p divides the
/// leading coefficient. Each root mod p makes f(x) divisible by p more often than
/// a random number, so values are smoother: negative alpha is good.
pub fn alpha(f: &[BigInt], b: u64) -> f64 {
    let d = f.len() - 1;
    let mut s = 0.0;
    for p in primes(b) {
        let pb = BigInt::from(p);
        let fm: Vec<u64> = f
            .iter()
            .map(|c| c.mod_floor(&pb).to_u64().unwrap())
            .collect();
        let mut q = 0u64;
        for r in 0..p {
            let mut v = 0u64;
            for c in fm.iter().rev() {
                v = (v * r + c) % p;
            }
            if v == 0 {
                q += 1;
            }
        }
        if fm[d] == 0 {
            q += 1;
        }
        let pf = p as f64;
        s += pf.ln() / (pf - 1.0) * (1.0 - q as f64 * pf / (pf + 1.0));
    }
    s
}

/// the skew s minimizing the L2 norm of F over an ellipse (Murphy's lognorm), and
/// that lognorm: ln sqrt( integral over theta of F(sqrt(s) cos t, sin t / sqrt(s))^2 )
pub fn skew_lognorm(f: &[BigInt]) -> (f64, f64) {
    let a = coeffs_f64(f);
    let norm = |ls: f64| -> f64 {
        let s = ls.exp();
        let k = 64;
        let mut acc = 0.0;
        for i in 0..k {
            let t = std::f64::consts::PI * (i as f64 + 0.5) / k as f64;
            let v = eval_hom(&a, s.sqrt() * t.cos(), t.sin() / s.sqrt());
            acc += v * v;
        }
        0.5 * (acc / k as f64).ln()
    };
    // golden section search on ln s in [-10, 80]
    let (mut lo, mut hi) = (-10.0f64, 80.0f64);
    // coarse scan first, since the function can have several dips
    let mut best = (0.0, f64::MAX);
    for i in 0..=90 {
        let x = lo + (hi - lo) * i as f64 / 90.0;
        let v = norm(x);
        if v < best.1 {
            best = (x, v);
        }
    }
    lo = best.0 - 1.0;
    hi = best.0 + 1.0;
    let g = (5f64.sqrt() - 1.0) / 2.0;
    for _ in 0..60 {
        let x1 = hi - g * (hi - lo);
        let x2 = lo + g * (hi - lo);
        if norm(x1) < norm(x2) {
            hi = x2;
        } else {
            lo = x1;
        }
    }
    let ls = (lo + hi) / 2.0;
    (ls.exp(), norm(ls))
}

/// Dickman's rho: the probability that a random number near x has no prime
/// factor above x^(1/u). Tabulated by integrating u rho'(u) = -rho(u - 1).
fn rho(u: f64) -> f64 {
    thread_local! {
        static TABLE: Vec<f64> = {
            let h = 1e-3;
            let n = (40.0 / h) as usize;
            let mut t = vec![1.0; n + 1];
            let per = (1.0 / h) as usize;
            for i in per + 1..=n {
                let u0 = (i - 1) as f64 * h;
                let u1 = i as f64 * h;
                let f0 = t[i - 1 - per] / u0;
                let f1 = t[i - per] / u1;
                t[i] = t[i - 1] - h * (f0 + f1) / 2.0;
            }
            t
        };
    }
    if u <= 1.0 {
        return 1.0;
    }
    TABLE.with(|t| {
        let h = 1e-3;
        let x = u / h;
        let i = x.floor() as usize;
        if i + 1 >= t.len() {
            return 0.0;
        }
        let w = x - i as f64;
        (t[i] * (1.0 - w) + t[i + 1] * w).max(0.0)
    })
}

/// sieving parameters for Murphy's E: smoothness bounds for both sides and the
/// sieve area, by the size of n (roughly as CADO-NFS uses them)
pub fn e_params(n: &BigInt) -> (f64, f64, f64) {
    let digits = n.to_string().len() as f64;
    let b = 10f64.powf(5.0 + digits / 50.0); // 1e7 at 100 digits
    let area = 10f64.powf(10.0 + digits * 0.06); // 1e16 at 100 digits
    (b, b, area)
}

/// Murphy's E score of (f, x - m) with skew s: the average over the ellipse of
/// rho(ln|F| + alpha_f over ln Bf) * rho(ln|G| over ln Bg). Larger is better;
/// only ratios between polynomials of the same n matter.
pub fn murphy_e(f: &[BigInt], m: &BigInt, s: f64, alpha_f: f64, n: &BigInt) -> f64 {
    let (bf, bg, area) = e_params(n);
    let a = coeffs_f64(f);
    let mf = m.to_f64().unwrap_or(f64::MAX);
    let k = 500;
    let mut e = 0.0;
    for i in 0..k {
        let t = std::f64::consts::PI * (i as f64 + 0.5) / k as f64;
        let x = (area * s).sqrt() * t.cos();
        let y = (area / s).sqrt() * t.sin();
        let vf = eval_hom(&a, x, y).abs().max(1.0).ln() + alpha_f;
        let vg = (x - y * mf).abs().max(1.0).ln();
        e += rho(vf / bf.ln()) * rho(vg / bg.ln());
    }
    e / k as f64
}

/// one candidate and its scores
#[derive(Clone, Debug)]
pub struct Cand {
    pub f: Poly,
    pub m: BigInt,
    pub ad: i64,
    pub dm: i64,
    pub j: i64,
    pub k: i64,
    pub skew: f64,
    pub lognorm: f64,
    pub alpha: f64,
    pub e: f64,
    /// what the search maximizes: e, or minus the teaching sieve's score in monic mode
    pub fit: f64,
}

pub fn score(n: &BigInt, d: usize, ad: i64, dm: i64, j: i64, k: i64) -> Option<Cand> {
    if ad <= 0 {
        return None;
    }
    let adb = BigInt::from(ad);
    let m = root_for(n, d, &adb) + dm;
    if m < BigInt::from(2) {
        return None;
    }
    let f0 = base_m(n, d, &adb, &m);
    let f = if d >= 2 { rotate(&f0, &m, j, k) } else { f0 };
    debug_assert_eq!(eval(&f, &m), *n);
    if f[0].is_zero() {
        return None;
    }
    let (skew, lognorm) = skew_lognorm(&f);
    let al = alpha(&f, 200);
    let e = murphy_e(&f, &m, skew, al, n);
    Some(Cand {
        f,
        m,
        ad,
        dm,
        j,
        k,
        skew,
        lognorm,
        alpha: al,
        e,
        fit: e,
    })
}

/// Root sieve: for rotations f + (j x + k)(x - m) with |j| <= jmax, |k| <= kmax,
/// estimate alpha by sieving: a root r of the rotated f mod p exists exactly when
/// k = -f_j(r)/(r - m) mod p, so each residue r marks one class of k mod p.
/// Returns the (j, k) with the smallest estimated alpha (primes up to 100).
pub fn root_sieve(f: &[BigInt], m: &BigInt, jmax: i64, kmax: i64) -> (i64, i64, f64) {
    let width = (2 * kmax + 1) as usize;
    let mut best = (0i64, 0i64, f64::MAX);
    let ps = primes(100);
    for j in -jmax..=jmax {
        let fj = rotate(f, m, j, 0);
        let mut arr = vec![0.0f64; width];
        for &p in &ps {
            let pb = BigInt::from(p);
            let pf = p as f64;
            let contrib = pf * pf.ln() / (pf * pf - 1.0);
            let fm: Vec<u64> = fj
                .iter()
                .map(|c| c.mod_floor(&pb).to_u64().unwrap())
                .collect();
            let mm = m.mod_floor(&pb).to_u64().unwrap();
            for r in 0..p {
                let mut v = 0u64;
                for c in fm.iter().rev() {
                    v = (v * r + c) % p;
                }
                let diff = (r + p - mm) % p;
                if diff == 0 {
                    // r = m mod p: a root for every k when f_j(m) = n = 0 mod p, else never
                    if v == 0 {
                        for x in arr.iter_mut() {
                            *x -= contrib;
                        }
                    }
                    continue;
                }
                // k (r - m) = -v mod p
                let inv = crate::numtheory::small::modinv(diff as i128, p as i128).unwrap() as u64;
                let k0 = (p - v) % p * inv % p;
                // indices with k = k0 mod p, k = idx - kmax
                let start = ((k0 as i64 + kmax).rem_euclid(p as i64)) as usize;
                let mut idx = start;
                while idx < width {
                    arr[idx] -= contrib;
                    idx += p as usize;
                }
            }
        }
        for (i, &v) in arr.iter().enumerate() {
            if v < best.2 {
                best = (j, i as i64 - kmax, v);
            }
        }
    }
    best
}

/// A small evolutionary search over (leading coefficient, shift of m, rotation j, k):
/// keeps a population, mutates the best ones, and keeps the fittest by Murphy's E.
/// Returns the best candidate of each generation.
pub fn evolve(
    n: &BigInt,
    d: usize,
    start: &Cand,
    generations: usize,
    pop: usize,
    seed: u64,
) -> Vec<Cand> {
    evolve_with(n, d, start, generations, pop, seed, false)
}

/// evolve, optionally keeping f monic (leading coefficient 1), as the teaching gnfs needs
pub fn evolve_with(
    n: &BigInt,
    d: usize,
    start: &Cand,
    generations: usize,
    pop: usize,
    seed: u64,
    monic: bool,
) -> Vec<Cand> {
    // in monic mode, fitness is the teaching sieve's own estimate (norm bits over its
    // unskewed rectangle plus alpha, lower is better), since that is the region it sieves
    let fitness = |mut c: Cand| -> Cand {
        if monic {
            c.fit = -gnfs_score(&c.f, &c.m);
        }
        c
    };
    let score = |n: &BigInt, d: usize, ad: i64, dm: i64, j: i64, k: i64| {
        score(n, d, ad, dm, j, k).map(fitness)
    };
    let start = &fitness(start.clone());
    let mut rng = seed | 1;
    let mut next = move || {
        rng ^= rng << 13;
        rng ^= rng >> 7;
        rng ^= rng << 17;
        rng
    };
    let mut population: Vec<Cand> = vec![start.clone()];
    let admax = 10_000i64;
    while population.len() < pop {
        let (ad, dm) = if monic {
            (1, (next() % 200_001) as i64 - 100_000)
        } else {
            (1 + (next() % admax as u64) as i64, 0)
        };
        if let Some(c) = score(n, d, ad, dm, 0, 0) {
            population.push(c);
        }
    }
    let mut history = vec![];
    for _ in 0..generations {
        population.sort_by(|a, b| b.fit.partial_cmp(&a.fit).unwrap());
        population.truncate(pop / 3);
        history.push(population[0].clone());
        let parents = population.clone();
        while population.len() < pop {
            let p = &parents[(next() % parents.len() as u64) as usize];
            let (mut ad, mut dm, mut j, mut k) = (p.ad, p.dm, p.j, p.k);
            match next() % 4 {
                0 if !monic => ad = (ad + (next() % 41) as i64 - 20).max(1),
                0 => dm += (next() % 200_001) as i64 - 100_000,
                1 => dm += (next() % 2001) as i64 - 1000,
                2 => j += (next() % 7) as i64 - 3,
                _ => k += (next() % 20001) as i64 - 10000,
            }
            if let Some(c) = score(n, d, ad, dm, j, k) {
                population.push(c);
            }
        }
    }
    population.sort_by(|a, b| b.fit.partial_cmp(&a.fit).unwrap());
    history.push(population[0].clone());
    history
}

/// the teaching sieve's score of a monic f: estimated bits of both norms + alpha
pub fn gnfs_score(f: &[BigInt], m: &BigInt) -> f64 {
    let (r, a) = crate::gnfs::norm_estimate(f, m);
    r + a + crate::gnfs::alpha(f)
}
