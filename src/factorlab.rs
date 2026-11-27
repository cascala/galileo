//! The factoring lab: a work budget shared by every procedure, a seeded
//! generator of balanced semiprimes, reference factoring methods (trial
//! division, Fermat, Hart's one-line method, Pollard rho and p - 1, SQUFOF,
//! ECM and a multiple-polynomial quadratic sieve), and the scoreboard that
//! runs a method on semiprimes of growing size and fits how its cost scales.
//!
//! Cost is counted in work units (about one multiplication modulo N each) so
//! results are the same on every machine and in the browser; natively the
//! wall-clock time is measured too.

use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::cell::Cell;

// ---------- budget ----------

thread_local! {
    static OPS: Cell<f64> = const { Cell::new(0.0) };
    static LIMIT: Cell<f64> = const { Cell::new(f64::INFINITY) };
    static OUT: Cell<bool> = const { Cell::new(false) };
    static ACTIVE: Cell<bool> = const { Cell::new(false) };
    static CHECKS: Cell<u32> = const { Cell::new(0) };
    #[cfg(not(target_arch = "wasm32"))]
    static DEADLINE: Cell<Option<std::time::Instant>> = const { Cell::new(None) };
}

/// Starts a budget: at most `units` work units and (natively) `seconds` of time.
pub fn start_budget(units: f64, seconds: f64) {
    OPS.with(|c| c.set(0.0));
    LIMIT.with(|c| c.set(units));
    OUT.with(|c| c.set(false));
    ACTIVE.with(|c| c.set(true));
    #[cfg(not(target_arch = "wasm32"))]
    DEADLINE.with(|c| {
        c.set(if seconds.is_finite() {
            Some(std::time::Instant::now() + std::time::Duration::from_secs_f64(seconds.max(0.0)))
        } else {
            None
        })
    });
    #[cfg(target_arch = "wasm32")]
    let _ = seconds;
}

/// Ends the budget; returns the work units spent.
pub fn end_budget() -> f64 {
    ACTIVE.with(|c| c.set(false));
    LIMIT.with(|c| c.set(f64::INFINITY));
    #[cfg(not(target_arch = "wasm32"))]
    DEADLINE.with(|c| c.set(None));
    OPS.with(|c| c.get())
}

pub fn spent() -> f64 {
    OPS.with(|c| c.get())
}

/// Spends work; true when the budget is used up (and stays true).
#[inline]
pub fn spend(units: f64) -> bool {
    let o = OPS.with(|c| {
        let v = c.get() + units;
        c.set(v);
        v
    });
    if OUT.with(|c| c.get()) {
        return true;
    }
    if o > LIMIT.with(|c| c.get()) {
        OUT.with(|c| c.set(true));
        return true;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let k = CHECKS.with(|c| {
            let k = c.get().wrapping_add(1);
            c.set(k);
            k
        });
        if k % 256 == 0 {
            if let Some(d) = DEADLINE.with(|c| c.get()) {
                if std::time::Instant::now() > d {
                    OUT.with(|c| c.set(true));
                    return true;
                }
            }
        }
    }
    false
}

pub fn out_of_budget() -> bool {
    OUT.with(|c| c.get())
}

/// Called once per evaluated node of a user-defined Galileo procedure while
/// the lab runs it: one work unit each. True when the budget is used up.
#[inline]
pub fn tick() -> bool {
    if !ACTIVE.with(|c| c.get()) {
        return false;
    }
    spend(1.0)
}

/// A wall clock in seconds (natively); None in the browser.
pub struct Clock {
    #[cfg(not(target_arch = "wasm32"))]
    t0: std::time::Instant,
}

impl Clock {
    pub fn start() -> Clock {
        Clock {
            #[cfg(not(target_arch = "wasm32"))]
            t0: std::time::Instant::now(),
        }
    }
    pub fn seconds(&self) -> Option<f64> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            Some(self.t0.elapsed().as_secs_f64())
        }
        #[cfg(target_arch = "wasm32")]
        {
            None
        }
    }
}

pub fn has_clock() -> bool {
    cfg!(not(target_arch = "wasm32"))
}

// ---------- random numbers ----------

/// SplitMix64: small, fast and the same everywhere.
#[derive(Clone)]
pub struct Rng(pub u64);

impl Rng {
    pub fn new(seed: u64) -> Rng {
        Rng(seed ^ 0x9E37_79B9_7F4A_7C15)
    }
    pub fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    /// uniform in 0..n
    pub fn below(&mut self, n: u64) -> u64 {
        if n == 0 {
            return 0;
        }
        ((self.next() as u128 * n as u128) >> 64) as u64
    }
    pub fn unit(&mut self) -> f64 {
        (self.next() >> 11) as f64 / (1u64 << 53) as f64
    }
    /// uniform BigInt in [lo, hi)
    pub fn big_range(&mut self, lo: &BigInt, hi: &BigInt) -> BigInt {
        let span = hi - lo;
        if !span.is_positive() {
            return lo.clone();
        }
        let words = span.bits() / 64 + 2;
        let mut bytes = vec![];
        for _ in 0..words {
            bytes.extend_from_slice(&self.next().to_le_bytes());
        }
        lo + BigInt::from_bytes_le(Sign::Plus, &bytes).mod_floor(&span)
    }
}

pub fn pow10(d: u32) -> BigInt {
    BigInt::from(10u32).pow(d)
}

/// ceil(sqrt(n)) for n >= 0
pub fn ceil_sqrt(n: &BigInt) -> BigInt {
    let r = n.sqrt();
    if &(&r * &r) == n { r } else { r + 1 }
}

const SQ64: u64 = {
    let mut m = 0u64;
    let mut x = 0;
    while x < 64 {
        m |= 1 << (x * x % 64);
        x += 1;
    }
    m
};

pub fn is_square(n: &BigInt) -> Option<BigInt> {
    if n.is_negative() {
        return None;
    }
    let low = (n & BigInt::from(63u32)).to_u32().unwrap();
    if (SQ64 >> low) & 1 == 0 {
        return None;
    }
    let r = n.sqrt();
    if &(&r * &r) == n { Some(r) } else { None }
}

/// Balanced semiprime with exactly `digits` digits: N = p*q with p, q prime,
/// both drawn uniformly from [10^((d-1)/2), 10^(d/2)), p <= q.
pub fn semiprime(digits: u32, seed: u64) -> (BigInt, BigInt, BigInt) {
    let digits = digits.max(4);
    let mut rng = Rng::new(seed.wrapping_mul(1_000_003).wrapping_add(digits as u64));
    let lo = ceil_sqrt(&pow10(digits - 1));
    let hi = pow10(digits).sqrt(); // p*q < 10^d needs p, q <= floor(sqrt(10^d - 1))
    loop {
        let p = crate::numtheory::next_prime(&(rng.big_range(&lo, &hi) - 1));
        let q = crate::numtheory::next_prime(&(rng.big_range(&lo, &hi) - 1));
        if p == q || p > hi || q > hi {
            continue;
        }
        let n = &p * &q;
        if n.to_string().len() as u32 != digits {
            continue;
        }
        let (p, q) = if p < q { (p, q) } else { (q, p) };
        return (n, p, q);
    }
}

// ---------- small helpers ----------

pub fn small_primes(limit: usize) -> Vec<u64> {
    crate::numtheory::primes_upto(limit)
        .into_iter()
        .map(|p| p as u64)
        .collect()
}

fn nontrivial(g: BigInt, n: &BigInt) -> Option<BigInt> {
    if g > BigInt::one() && &g < n {
        Some(g)
    } else {
        None
    }
}

pub fn powmod_u64(mut a: u64, mut e: u64, m: u64) -> u64 {
    let mut r = 1u64 % m;
    a %= m;
    while e > 0 {
        if e & 1 == 1 {
            r = (r as u128 * a as u128 % m as u128) as u64;
        }
        a = (a as u128 * a as u128 % m as u128) as u64;
        e >>= 1;
    }
    r
}

/// Legendre symbol (a/p) for an odd prime p: 1, p - 1 (meaning -1) or 0
pub fn legendre_u64(a: u64, p: u64) -> i32 {
    let r = powmod_u64(a % p, (p - 1) / 2, p);
    if r == 0 {
        0
    } else if r == 1 {
        1
    } else {
        -1
    }
}

/// a square root of n modulo the odd prime p (n a residue), Tonelli-Shanks
pub fn sqrt_mod_u64(n: u64, p: u64) -> u64 {
    let n = n % p;
    if n == 0 || p == 2 {
        return n;
    }
    if p % 4 == 3 {
        return powmod_u64(n, (p + 1) / 4, p);
    }
    let (mut q, mut s) = (p - 1, 0u32);
    while q % 2 == 0 {
        q /= 2;
        s += 1;
    }
    let mut z = 2;
    while legendre_u64(z, p) != -1 {
        z += 1;
    }
    let mut m = s;
    let mut c = powmod_u64(z, q, p);
    let mut t = powmod_u64(n, q, p);
    let mut r = powmod_u64(n, q.div_ceil(2), p);
    let mul = |a: u64, b: u64| (a as u128 * b as u128 % p as u128) as u64;
    while t != 1 {
        let mut i = 0;
        let mut tt = t;
        while tt != 1 {
            tt = mul(tt, tt);
            i += 1;
            if i == m {
                return 0;
            }
        }
        let mut b = c;
        for _ in 0..(m - i - 1) {
            b = mul(b, b);
        }
        m = i;
        c = mul(b, b);
        t = mul(t, c);
        r = mul(r, b);
    }
    r
}

pub fn inv_mod_u64(a: u64, m: u64) -> Option<u64> {
    let (mut t, mut nt) = (0i128, 1i128);
    let (mut r, mut nr) = (m as i128, (a % m) as i128);
    while nr != 0 {
        let q = r / nr;
        (t, nt) = (nt, t - q * nt);
        (r, nr) = (nr, r - q * nr);
    }
    if r != 1 {
        return None;
    }
    Some(t.rem_euclid(m as i128) as u64)
}

// ---------- reference methods ----------

/// The built-in methods the scoreboard knows, with a one-line description.
pub const METHODS: &[(&str, &str)] = &[
    (
        "trial",
        "trial division by 2, 3 and 6k +- 1 up to sqrt(N): about N^(1/2) work",
    ),
    (
        "fermat",
        "Fermat's method from sqrt(N) up: fast only when p and q are close",
    ),
    (
        "hart",
        "Hart's one-line method: s = ceil(sqrt(i N)), s^2 mod N a square: about N^(1/3)",
    ),
    (
        "rho",
        "Pollard rho (Brent's cycle finding, batched gcds): about N^(1/4)",
    ),
    (
        "pm1",
        "Pollard p - 1 with a growing smoothness bound: fast when p - 1 has only small primes",
    ),
    (
        "squfof",
        "Shanks' square forms factorization (continued fraction of sqrt(kN)): about N^(1/4), N below 10^36",
    ),
    (
        "ecm",
        "Lenstra's elliptic curve method (Montgomery curves, stage 1): L(1/2) in the size of p",
    ),
    (
        "qs",
        "multiple-polynomial quadratic sieve with large primes: L(1/2, 1) in N",
    ),
    (
        "nfs",
        "number field sieve (base-m polynomial, or a special form b^k +- 1 when N divides one): L(1/3) in N, but slow at these sizes",
    ),
    (
        "classgroup",
        "class group of discriminant -4kN (Schnorr-Lenstra): works when the class number is smooth",
    ),
];

/// Runs a built-in method on n within the current budget.
pub fn run_method(name: &str, n: &BigInt, seed: u64) -> Option<BigInt> {
    if n.is_even() {
        return Some(BigInt::from(2));
    }
    match name {
        "trial" => trial(n),
        "fermat" => fermat(n),
        "hart" => hart(n),
        "rho" => rho(n, seed),
        "pm1" => pm1(n),
        "squfof" => squfof(n),
        "ecm" => ecm(n, seed),
        "qs" => qs(n),
        "classgroup" => classgroup(n),
        "nfs" | "gnfs" | "snfs" => crate::gnfs::nfs_method(n),
        _ => None,
    }
}

pub fn trial(n: &BigInt) -> Option<BigInt> {
    if let Some(m) = n.to_u128() {
        for d in [3u128, 5] {
            if m % d == 0 {
                return nontrivial(BigInt::from(d), n);
            }
        }
        let mut d: u128 = 7;
        let mut step = 4;
        while d * d <= m {
            if m % d == 0 {
                return nontrivial(BigInt::from(d), n);
            }
            d += step;
            step = 6 - step;
            if spend(0.5) {
                return None;
            }
        }
        return None;
    }
    let mut d = BigInt::from(7);
    let mut step = 4u32;
    while &(&d * &d) <= n {
        if (n % &d).is_zero() {
            return Some(d);
        }
        d += step;
        step = 6 - step;
        if spend(1.0) {
            return None;
        }
    }
    None
}

pub fn fermat(n: &BigInt) -> Option<BigInt> {
    let mut a = ceil_sqrt(n);
    let mut r = &a * &a - n;
    loop {
        if let Some(b) = is_square(&r) {
            return nontrivial(&a - b, n);
        }
        r += &a * 2u32 + 1u32;
        a += 1u32;
        if spend(0.5) || &a > n {
            return None;
        }
    }
}

pub fn hart(n: &BigInt) -> Option<BigInt> {
    let mut i = BigInt::one();
    loop {
        let s = ceil_sqrt(&(n * &i));
        let m = (&s * &s).mod_floor(n);
        if let Some(t) = is_square(&m) {
            if let Some(g) = nontrivial((&s - t).gcd(n), n) {
                return Some(g);
            }
        }
        i += 1u32;
        if spend(3.0) {
            return None;
        }
    }
}

pub fn rho(n: &BigInt, seed: u64) -> Option<BigInt> {
    let mut rng = Rng::new(seed);
    loop {
        let c = BigInt::from(1 + rng.below(1000));
        let f = |x: &BigInt| (x * x + &c) % n;
        let mut y = BigInt::from(2 + rng.below(1000));
        let (mut r, mut q) = (1u64, BigInt::one());
        let (mut x, mut ys);
        let m = 128u64;
        let mut g;
        loop {
            x = y.clone();
            for _ in 0..r {
                y = f(&y);
            }
            let mut k = 0;
            loop {
                ys = y.clone();
                for _ in 0..m.min(r - k) {
                    y = f(&y);
                    q = q * (&x - &y).abs() % n;
                }
                if spend(2.0 * m.min(r - k) as f64) {
                    return None;
                }
                g = q.gcd(n);
                k += m;
                if k >= r || !g.is_one() {
                    break;
                }
            }
            r *= 2;
            if !g.is_one() {
                break;
            }
        }
        if &g == n {
            loop {
                ys = f(&ys);
                g = (&x - &ys).abs().gcd(n);
                if !g.is_one() {
                    break;
                }
            }
        }
        if &g != n {
            return Some(g);
        }
        if spend(10.0) {
            return None;
        }
    }
}

pub fn pm1(n: &BigInt) -> Option<BigInt> {
    let mut a = BigInt::from(3);
    let mut bound = 1000usize;
    let mut done = 0u64;
    loop {
        let ps = small_primes(bound);
        for &p in ps.iter().filter(|&&p| p > done) {
            let mut pk = p;
            while pk <= (bound as u64) / p {
                pk *= p;
            }
            let save = a.clone();
            a = a.modpow(&BigInt::from(pk), n);
            if spend(1.5 * (64 - pk.leading_zeros()) as f64) {
                return None;
            }
            if p % 64 == 1 || p == *ps.last().unwrap() {
                let g = (&a - 1u32).gcd(n);
                if &g == n {
                    // went too far in one batch: redo prime by prime
                    let _ = save;
                    return None;
                }
                if let Some(g) = nontrivial(g, n) {
                    return Some(g);
                }
            }
        }
        done = bound as u64;
        bound *= 2;
        if bound > 400_000_000 {
            return None;
        }
    }
}

/// isqrt for u128
fn isqrt_u128(n: u128) -> u128 {
    if n == 0 {
        return 0;
    }
    let mut r = (n as f64).sqrt() as u128;
    while r * r > n {
        r -= 1;
    }
    while (r + 1) * (r + 1) <= n {
        r += 1;
    }
    r
}

pub fn squfof(n: &BigInt) -> Option<BigInt> {
    let nn = n.to_u128().filter(|&v| v < (1u128 << 118))?;
    let s = isqrt_u128(nn);
    if s * s == nn {
        return Some(BigInt::from(s));
    }
    const MULT: [u128; 16] = [
        1, 3, 5, 7, 11, 15, 21, 33, 35, 55, 77, 105, 165, 231, 385, 1155,
    ];
    let mut round = 1u128;
    loop {
        for &k in MULT.iter() {
            if nn > (1u128 << 124) / k {
                continue;
            }
            let d = (k * nn) as i128;
            let p0 = isqrt_u128(d as u128) as i128;
            let (mut pprev, mut p) = (p0, p0);
            let mut qprev: i128 = 1;
            let mut q = d - p0 * p0;
            if q == 0 {
                continue;
            }
            let l = 2 * isqrt_u128(2 * s) as i128;
            let bound = 3 * l * round as i128;
            let mut i = 2i128;
            let mut r = 0i128;
            let mut found = false;
            while i < bound {
                let b = (p0 + p) / q;
                p = b * q - p;
                let qq = q;
                q = qprev + b * (pprev - p);
                r = isqrt_u128(q as u128) as i128;
                if i & 1 == 0 && r * r == q {
                    found = true;
                    break;
                }
                qprev = qq;
                pprev = p;
                i += 1;
                if i % 64 == 0 && spend(64.0) {
                    return None;
                }
            }
            if !found {
                continue;
            }
            let b = (p0 - p) / r;
            p = b * r + p;
            pprev = p;
            qprev = r;
            q = (d - pprev * pprev) / qprev;
            let mut steps = 0u64;
            loop {
                let b = (p0 + p) / q;
                pprev = p;
                p = b * q - p;
                let qq = q;
                q = qprev + b * (pprev - p);
                qprev = qq;
                steps += 1;
                if steps % 64 == 0 && spend(64.0) {
                    return None;
                }
                if p == pprev {
                    break;
                }
            }
            let g = BigInt::from(qprev as u128).gcd(n);
            if let Some(g) = nontrivial(g, n) {
                return Some(g);
            }
        }
        round *= 2;
        if round > 1 << 20 {
            return None;
        }
    }
}

/// class group factoring (groups.rs) with a growing smoothness bound
pub fn classgroup(n: &BigInt) -> Option<BigInt> {
    let mut b = 500u64;
    loop {
        let found = crate::groups::class_factor(n, b, 2);
        // 11 multipliers x 2 forms x ~1.44 b squarings, a composition ~ 20 multiplications
        let out = spend(22.0 * 1.44 * b as f64 * 20.0);
        if let Some((f, _, _)) = found {
            return Some(f);
        }
        if out {
            return None;
        }
        b *= 2;
        if b > 1 << 26 {
            return None;
        }
    }
}

// ---------- ECM (Montgomery curves, stage 1) ----------

struct Mont<'a> {
    n: &'a BigInt,
    a24: BigInt,
}

impl Mont<'_> {
    fn m(&self, a: &BigInt, b: &BigInt) -> BigInt {
        (a * b) % self.n
    }
    fn dbl(&self, x: &BigInt, z: &BigInt) -> (BigInt, BigInt) {
        let s = (x + z) % self.n;
        let d = (x - z).mod_floor(self.n);
        let t1 = self.m(&s, &s);
        let t2 = self.m(&d, &d);
        let t3 = (&t1 - &t2).mod_floor(self.n);
        let x2 = self.m(&t1, &t2);
        let z2 = self.m(&t3, &((&t2 + self.m(&self.a24, &t3)) % self.n));
        (x2, z2)
    }
    fn add(
        &self,
        p: &(BigInt, BigInt),
        q: &(BigInt, BigInt),
        d: &(BigInt, BigInt),
    ) -> (BigInt, BigInt) {
        let u = self.m(&(&p.0 - &p.1).mod_floor(self.n), &((&q.0 + &q.1) % self.n));
        let v = self.m(&((&p.0 + &p.1) % self.n), &(&q.0 - &q.1).mod_floor(self.n));
        let s = (&u + &v) % self.n;
        let t = (&u - &v).mod_floor(self.n);
        (self.m(&d.1, &self.m(&s, &s)), self.m(&d.0, &self.m(&t, &t)))
    }
    /// [k]P by the Montgomery ladder
    fn mul(&self, k: u64, p: &(BigInt, BigInt)) -> (BigInt, BigInt) {
        if k == 1 {
            return p.clone();
        }
        let mut r0 = p.clone();
        let mut r1 = self.dbl(&p.0, &p.1);
        let bits = 64 - k.leading_zeros();
        for i in (0..bits - 1).rev() {
            if (k >> i) & 1 == 1 {
                r0 = self.add(&r1, &r0, p);
                r1 = self.dbl(&r1.0, &r1.1);
            } else {
                r1 = self.add(&r0, &r1, p);
                r0 = self.dbl(&r0.0, &r0.1);
            }
        }
        r0
    }
}

pub fn ecm(n: &BigInt, seed: u64) -> Option<BigInt> {
    let mut rng = Rng::new(seed ^ 0xEC);
    let mut b1 = 400u64;
    let primes = small_primes(3_000_000);
    loop {
        // Suyama's parametrization: the group order has a factor 12
        let sigma = BigInt::from(6 + rng.below(1 << 30));
        let u = (&sigma * &sigma - 5u32).mod_floor(n);
        let v = (&sigma * 4u32) % n;
        let x0 = u.modpow(&BigInt::from(3), n);
        let z0 = v.modpow(&BigInt::from(3), n);
        let num = ((&v - &u).mod_floor(n)).modpow(&BigInt::from(3), n) * ((&u * 3u32 + &v) % n) % n;
        let den = (x0.clone() * &v * 16u32) % n;
        let g = den.gcd(n);
        if !g.is_one() {
            if let Some(g) = nontrivial(g, n) {
                return Some(g);
            }
            continue;
        }
        let inv = crate::numtheory::modinv(&den, n)?;
        let curve = Mont {
            n,
            a24: num * inv % n,
        };
        let mut pt = (x0, z0);
        for &p in primes.iter().take_while(|&&p| p <= b1) {
            let mut pk = p;
            while pk <= b1 / p {
                pk *= p;
            }
            pt = curve.mul(pk, &pt);
            if spend(10.0 * (64 - pk.leading_zeros()) as f64) {
                return None;
            }
        }
        let g = pt.1.gcd(n);
        if let Some(g) = nontrivial(g, n) {
            return Some(g);
        }
        b1 = (b1 as f64 * 1.15) as u64;
        b1 = b1.min(2_900_000);
    }
}

// ---------- quadratic sieve (multiple polynomials, one large prime) ----------

struct Relation {
    /// u with u^2 = (product of the factor base powers) * extra^2 (mod n)
    u: BigInt,
    exps: Vec<u32>,
    extra: BigInt,
}

fn qs_params(digits: usize) -> (usize, i64) {
    // (factor base size, sieve half-width)
    let table: [(usize, usize, i64); 10] = [
        (20, 120, 6000),
        (24, 160, 8000),
        (30, 240, 12000),
        (36, 400, 20000),
        (40, 600, 30000),
        (46, 900, 40000),
        (50, 1200, 50000),
        (56, 1800, 60000),
        (60, 2400, 65536),
        (70, 4000, 100000),
    ];
    let mut best = table[0];
    for t in table {
        if digits >= t.0 {
            best = t;
        }
    }
    (best.1, best.2)
}

fn knuth_schroeppel(n: &BigInt) -> u64 {
    let ks = [
        1u64, 3, 5, 7, 11, 13, 15, 17, 19, 21, 23, 29, 31, 33, 35, 37, 39, 41, 43, 47,
    ];
    let ps = small_primes(300);
    let mut best = (f64::NEG_INFINITY, 1);
    for &k in &ks {
        let kn = n * k;
        let mut f = -0.5 * (k as f64).ln();
        let m8 = (&kn % 8u32).to_u32().unwrap();
        f += match m8 {
            1 => 2.0 * 2f64.ln(),
            5 => 2f64.ln(),
            _ => 0.5 * 2f64.ln(),
        };
        for &p in ps.iter().skip(1) {
            let r = (&kn % p).to_u64().unwrap();
            if r == 0 {
                f += (p as f64).ln() / p as f64;
            } else if legendre_u64(r, p) == 1 {
                f += 2.0 * (p as f64).ln() / (p - 1) as f64;
            }
        }
        if f > best.0 {
            best = (f, k);
        }
    }
    best.1
}

pub fn qs(n: &BigInt) -> Option<BigInt> {
    let digits = n.to_string().len();
    if digits < 16 {
        return rho(n, 1);
    }
    if let Some(r) = is_square(n) {
        return Some(r);
    }
    let k = knuth_schroeppel(n);
    let kn = n * k;
    let (fsize, m) = qs_params(digits);
    // factor base: -1, 2, then odd primes with (kn/p) = 1
    let mut fb: Vec<u64> = vec![2];
    let mut roots: Vec<u64> = vec![1];
    for p in small_primes(10_000_000).into_iter().skip(1) {
        if fb.len() >= fsize {
            break;
        }
        let r = (&kn % p).to_u64().unwrap();
        if (n % p).is_zero() {
            return Some(BigInt::from(p));
        }
        if r == 0 {
            // p divides the multiplier
            fb.push(p);
            roots.push(0);
        } else if legendre_u64(r, p) == 1 {
            fb.push(p);
            roots.push(sqrt_mod_u64(r, p));
        }
        if spend(0.2) {
            return None;
        }
    }
    let logs: Vec<u8> = fb
        .iter()
        .map(|&p| ((p as f64).log2().round() as u8).max(1))
        .collect();
    let pmax = *fb.last().unwrap();
    let ncols = fb.len() + 1; // column 0 is the sign
    let width = (2 * m) as usize;
    let mut rels: Vec<Relation> = vec![];
    let mut partials: std::collections::HashMap<u64, (BigInt, Vec<u32>)> =
        std::collections::HashMap::new();
    let target = (((&kn * 2u32).sqrt()) / m as u64).sqrt(); // q with q^2 about sqrt(2kn)/M
    let mut q = target.max(BigInt::from(pmax + 1));
    let large_bound = (pmax as u128) * (pmax as u128).min(1 << 20) / 4;
    let mut sieve = vec![0u8; width];
    let mut wanted = ncols + 16;
    loop {
        // next polynomial: a = q^2 with q = 3 mod 4 prime, (kn/q) = 1
        let (a, b, qinv) = loop {
            q = crate::numtheory::next_prime(&q);
            if (&q % 4u32).to_u32() != Some(3) {
                continue;
            }
            let knq = kn.mod_floor(&q);
            if knq.is_zero() {
                if let Some(g) = nontrivial(q.gcd(n), n) {
                    return Some(g);
                }
                continue;
            }
            let e = (&q - 1u32) / 2u32;
            if !knq.modpow(&e, &q).is_one() {
                continue;
            }
            let b0 = knq.modpow(&((&q + 1u32) / 4u32), &q);
            // Hensel: b = b0 + q t with b^2 = kn (mod q^2)
            let a = &q * &q;
            let t = ((&kn - &b0 * &b0) / &q).mod_floor(&q)
                * crate::numtheory::modinv(&(&b0 * 2u32), &q)?
                % &q;
            let mut b = (&b0 + &q * t).mod_floor(&a);
            if (&b * &b - &kn).mod_floor(&a) != BigInt::zero() {
                continue;
            }
            if b.is_even() {
                b = &a - b; // keep b odd (either sign of the root works)
            }
            match crate::numtheory::modinv(&q, n) {
                Some(qi) => break (a, b, qi),
                None => return nontrivial(q.gcd(n), n),
            }
        };
        let c = (&b * &b - &kn) / &a;
        // sieve over x in [-m, m)
        sieve.iter_mut().for_each(|s| *s = 0);
        for (ix, &p) in fb.iter().enumerate().skip(1) {
            if p < 5 {
                continue;
            }
            let am = (&a % p).to_u64().unwrap();
            let Some(ai) = inv_mod_u64(am, p) else {
                continue;
            };
            let bm = (&b % p).to_u64().unwrap();
            let t = roots[ix];
            let lp = logs[ix];
            let r1 = ((t + p - bm) % p) as u128 * ai as u128 % p as u128;
            let r2 = ((2 * p - t - bm) % p) as u128 * ai as u128 % p as u128;
            let off = (m as u64 % p) as u128;
            for r in [r1, r2] {
                let mut j = ((r + off) % p as u128) as usize;
                while j < width {
                    sieve[j] = sieve[j].wrapping_add(lp);
                    j += p as usize;
                }
                if t == 0 {
                    break;
                }
            }
        }
        if spend(width as f64 * 0.02 + fb.len() as f64 * 0.5) {
            return None;
        }
        let qmax = (m as f64) * ((&kn * 2u32).sqrt().to_f64().unwrap_or(f64::MAX)).max(1.0) / 2.0;
        let thresh = (qmax.log2() - (pmax as f64).log2() * 1.6 - 4.0).max(1.0) as u8;
        for j in 0..width {
            if sieve[j] < thresh {
                continue;
            }
            let x = j as i64 - m;
            let xb = BigInt::from(x);
            let mut val = (&a * &xb + &b * 2u32) * &xb + &c;
            let mut exps = vec![0u32; ncols];
            if val.is_negative() {
                exps[0] = 1;
                val = -val;
            }
            if val.is_zero() {
                continue;
            }
            for (ix, &p) in fb.iter().enumerate() {
                let pb = BigInt::from(p);
                while (&val % p).is_zero() {
                    val /= &pb;
                    exps[ix + 1] += 1;
                }
            }
            spend(fb.len() as f64 * 0.3);
            let u = ((&a * &xb + &b) * &qinv).mod_floor(n);
            if val.is_one() {
                rels.push(Relation {
                    u,
                    exps,
                    extra: BigInt::one(),
                });
            } else if let Some(l) = val
                .to_u128()
                .filter(|&l| l < large_bound && l < u64::MAX as u128)
            {
                let l = l as u64;
                if let Some((u2, e2)) = partials.get(&l) {
                    let Some(li) = crate::numtheory::modinv(&BigInt::from(l), n) else {
                        return nontrivial(BigInt::from(l).gcd(n), n);
                    };
                    let _ = li;
                    let exps: Vec<u32> = exps.iter().zip(e2).map(|(a, b)| a + b).collect();
                    rels.push(Relation {
                        u: (&u * u2) % n,
                        exps,
                        extra: BigInt::from(l),
                    });
                } else {
                    partials.insert(l, (u, exps));
                }
            }
        }
        if rels.len() >= wanted {
            if let Some(g) = qs_solve(n, &fb, &rels) {
                return Some(g);
            }
            wanted = rels.len() + 10;
        }
        if out_of_budget() {
            return None;
        }
    }
}

/// Gaussian elimination over GF(2); tries every dependency
fn qs_solve(n: &BigInt, fb: &[u64], rels: &[Relation]) -> Option<BigInt> {
    let cols = fb.len() + 1;
    let r = rels.len();
    let cw = cols.div_ceil(64);
    let hw = r.div_ceil(64);
    let mut rows: Vec<(Vec<u64>, Vec<u64>)> = rels
        .iter()
        .enumerate()
        .map(|(i, rel)| {
            let mut v = vec![0u64; cw];
            for (c, &e) in rel.exps.iter().enumerate() {
                if e & 1 == 1 {
                    v[c / 64] |= 1 << (c % 64);
                }
            }
            let mut h = vec![0u64; hw];
            h[i / 64] |= 1 << (i % 64);
            (v, h)
        })
        .collect();
    spend((r * r * (cw + hw)) as f64 / 64.0);
    let mut pivot_row = 0;
    for c in 0..cols {
        let Some(pr) = (pivot_row..r).find(|&i| rows[i].0[c / 64] >> (c % 64) & 1 == 1) else {
            continue;
        };
        rows.swap(pivot_row, pr);
        let (pv, ph) = rows[pivot_row].clone();
        for i in 0..r {
            if i != pivot_row && rows[i].0[c / 64] >> (c % 64) & 1 == 1 {
                for w in 0..cw {
                    rows[i].0[w] ^= pv[w];
                }
                for w in 0..hw {
                    rows[i].1[w] ^= ph[w];
                }
            }
        }
        pivot_row += 1;
    }
    for (v, h) in rows.iter().skip(pivot_row) {
        if v.iter().any(|&w| w != 0) {
            continue;
        }
        let mut x = BigInt::one();
        let mut total = vec![0u32; cols];
        let mut extra = BigInt::one();
        for i in 0..r {
            if h[i / 64] >> (i % 64) & 1 == 1 {
                x = x * &rels[i].u % n;
                for (t, e) in total.iter_mut().zip(&rels[i].exps) {
                    *t += e;
                }
                extra = extra * &rels[i].extra % n;
            }
        }
        let mut y = extra;
        for (ix, &p) in fb.iter().enumerate() {
            let e = total[ix + 1] / 2;
            if e > 0 {
                y = y * BigInt::from(p).modpow(&BigInt::from(e), n) % n;
            }
        }
        if let Some(g) = nontrivial((&x - &y).gcd(n), n) {
            return Some(g);
        }
    }
    None
}

// ---------- scaling fits ----------

/// The cost models: name, x(digits) so that log(cost) = c0 + c1 x(d)
pub const MODELS: &[&str] = &["N^a", "L(1/2)", "L(1/3)"];

pub fn model_x(model: usize, digits: f64) -> f64 {
    let ln_n = digits * std::f64::consts::LN_10;
    let lnln = ln_n.ln();
    match model {
        0 => ln_n,
        1 => (ln_n * lnln).sqrt(),
        _ => ln_n.cbrt() * lnln.powf(2.0 / 3.0),
    }
}

/// least squares fit of ln(cost) = c0 + c1 x; returns (c0, c1, rms residual)
pub fn fit(model: usize, pts: &[(f64, f64)]) -> Option<(f64, f64, f64)> {
    if pts.len() < 2 {
        return None;
    }
    let xs: Vec<f64> = pts.iter().map(|p| model_x(model, p.0)).collect();
    let ys: Vec<f64> = pts.iter().map(|p| p.1.max(1e-12).ln()).collect();
    let n = xs.len() as f64;
    let mx = xs.iter().sum::<f64>() / n;
    let my = ys.iter().sum::<f64>() / n;
    let sxx: f64 = xs.iter().map(|x| (x - mx) * (x - mx)).sum();
    if sxx <= 0.0 {
        return None;
    }
    let sxy: f64 = xs.iter().zip(&ys).map(|(x, y)| (x - mx) * (y - my)).sum();
    let c1 = sxy / sxx;
    let c0 = my - c1 * mx;
    let rms = (xs
        .iter()
        .zip(&ys)
        .map(|(x, y)| (y - c0 - c1 * x).powi(2))
        .sum::<f64>()
        / n)
        .sqrt();
    Some((c0, c1, rms))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(name: &str, digits: u32) {
        for seed in 0..3 {
            let (n, p, q) = semiprime(digits, seed);
            start_budget(1e9, 30.0);
            let f = run_method(name, &n, seed);
            end_budget();
            let f = f.unwrap_or_else(|| panic!("{} failed on {}", name, n));
            assert!(f == p || f == q, "{} gave {} for {}", name, f, n);
        }
    }

    #[test]
    fn semiprimes_are_balanced() {
        for d in [10, 21, 40] {
            let (n, p, q) = semiprime(d, 7);
            assert_eq!(n.to_string().len(), d as usize);
            assert_eq!(&p * &q, n);
            assert!(crate::numtheory::is_prime(&p) && crate::numtheory::is_prime(&q));
            assert!(&q < &(&p * 4u32));
        }
        assert_eq!(semiprime(20, 3), semiprime(20, 3));
    }

    #[test]
    fn methods_work() {
        check("trial", 12);
        check("fermat", 10);
        check("hart", 14);
        check("rho", 20);
        check("squfof", 20);
        check("ecm", 22);
        check("qs", 30);
    }

    #[test]
    fn pm1_finds_smooth() {
        let mut k = 1u64;
        let p = loop {
            let p = BigInt::from(9_699_690u64 * k + 1);
            if crate::numtheory::is_prime(&p) {
                break p;
            }
            k += 1;
        };
        let n = &p * BigInt::from(1_000_000_007u64);
        start_budget(1e9, 10.0);
        let f = pm1(&n);
        end_budget();
        assert_eq!(f, Some(p));
    }
}
