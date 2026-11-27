//! Integer arithmetic for number theory: primality, factoring, modular
//! arithmetic, on whole numbers of any size. Numbers below 2^126 use the fast
//! u128 routines in `small`; larger ones use big integers.

use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

pub use small::primes_upto;

/// work limit for Pollard's rho on big numbers: multiplications mod n, times
/// the number of 64-bit limbs of n (about a second of work)
const BIG_RHO_WORK: u64 = 5_000_000;

static EFFORT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/// Scale the work rho and p - 1 may spend on a large number (1 = about a second)
pub fn set_effort(e: u64) {
    EFFORT.store(e.clamp(1, 10_000), std::sync::atomic::Ordering::Relaxed);
}

pub fn effort() -> u64 {
    EFFORT.load(std::sync::atomic::Ordering::Relaxed)
}

fn limbs(n: &BigInt) -> u64 {
    n.bits() / 64 + 1
}

/// below 2^64 the u128 routines are fast (a product fits in u128)
fn word_of(n: &BigInt) -> Option<u128> {
    n.to_u64().map(|v| v as u128)
}

fn small_of(n: &BigInt) -> Option<u128> {
    n.to_u128().filter(|&v| v < 1u128 << 126)
}

fn big(n: u128) -> BigInt {
    BigInt::from(n)
}

pub fn gcd(a: &BigInt, b: &BigInt) -> BigInt {
    a.gcd(b)
}

/// a^e mod m for e >= 0, m > 0 (result in 0..m)
pub fn powmod(a: &BigInt, e: &BigInt, m: &BigInt) -> BigInt {
    if m.is_one() {
        return BigInt::zero();
    }
    if let (Some(a), Some(e), Some(mm)) = (small_of(&a.mod_floor(m)), e.to_u128(), small_of(m)) {
        return big(small::powmod(a, e, mm));
    }
    a.mod_floor(m).modpow(e, m)
}

/// a^-1 mod m, if gcd(a, m) = 1
pub fn modinv(a: &BigInt, m: &BigInt) -> Option<BigInt> {
    let e = a.mod_floor(m).extended_gcd(m);
    if e.gcd.is_one() {
        Some(e.x.mod_floor(m))
    } else {
        None
    }
}

const BASES: [u32; 20] = [
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
];

/// Miller-Rabin: certain below 3.3e24, and for larger numbers a strong
/// probable-prime test to 20 bases (no composite is known to pass it)
pub fn is_prime(n: &BigInt) -> bool {
    if n.sign() != Sign::Plus {
        return false;
    }
    if let Some(v) = small_of(n) {
        return small::is_prime(v);
    }
    for p in small::primes_upto(1000) {
        if (n % p as u32).is_zero() {
            return false;
        }
    }
    let nm1 = n - 1u32;
    let s = nm1.trailing_zeros().unwrap_or(0);
    let d = &nm1 >> s;
    'outer: for a in BASES {
        let mut x = BigInt::from(a).modpow(&d, n);
        if x.is_one() || x == nm1 {
            continue;
        }
        for _ in 1..s {
            x = &x * &x % n;
            if x == nm1 {
                continue 'outer;
            }
        }
        return false;
    }
    true
}

pub fn next_prime(n: &BigInt) -> BigInt {
    if n < &BigInt::from(2) {
        return BigInt::from(2);
    }
    let mut k = n + 1u32;
    if k.is_even() && k != BigInt::from(2) {
        k += 1u32;
    }
    while !is_prime(&k) {
        k += 2u32;
    }
    k
}

pub fn prev_prime(n: &BigInt) -> Option<BigInt> {
    if let Some(v) = small_of(n) {
        return small::prev_prime(v).map(big);
    }
    let mut k = n - 1u32;
    if k.is_even() {
        k -= 1u32;
    }
    while !is_prime(&k) {
        k -= 2u32;
    }
    Some(k)
}

/// one nontrivial factor of an odd composite n (Pollard's rho, Brent's variant)
pub fn pollard_rho(n: &BigInt) -> Option<BigInt> {
    if let Some(v) = word_of(n) {
        return small::pollard_rho(v).map(big);
    }
    pollard_rho_pow(n, 2)
}

/// Pollard–Brent rho with the walk x -> x^k + c. When every prime factor p
/// of n has p = 1 (mod k), the walk takes only (p - 1)/k values mod p, so the
/// repeat comes about sqrt(k) times sooner (Brent and Pollard, 1981).
pub fn pollard_rho_pow(n: &BigInt, k: u64) -> Option<BigInt> {
    if n.is_even() {
        return Some(BigInt::from(2));
    }
    let k = k.max(2);
    let kb = BigInt::from(k);
    // multiplications per step
    let cost = if k == 2 {
        1
    } else {
        (64 - k.leading_zeros() as u64) + k.count_ones() as u64 - 1
    };
    let mut budget = BIG_RHO_WORK.saturating_mul(effort()) / limbs(n);
    for c in 1..20u32 {
        let c = BigInt::from(c);
        let f = |x: &BigInt| {
            if k == 2 {
                (x * x + &c) % n
            } else {
                (x.modpow(&kb, n) + &c) % n
            }
        };
        let (mut y, mut r, mut q, m) = (BigInt::from(2), 1u64, BigInt::one(), 128u64);
        let (mut g, mut x, mut ys) = (BigInt::one(), BigInt::zero(), BigInt::zero());
        while g.is_one() {
            x = y.clone();
            for _ in 0..r {
                y = f(&y);
            }
            let mut k = 0;
            while k < r && g.is_one() {
                ys = y.clone();
                for _ in 0..m.min(r - k) {
                    y = f(&y);
                    q = q * (&x - &y).abs() % n;
                }
                g = q.gcd(n);
                k += m;
            }
            budget = budget.checked_sub(2 * r * cost)?;
            r *= 2;
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
    }
    None
}

/// one factor of n by Pollard's p - 1 method: finds a prime factor p when
/// all prime factors of p - 1 are below 10^5, except one that may go up to
/// 5*10^6 (10^6 for numbers above 2^256)
pub fn pollard_pm1(n: &BigInt) -> Option<BigInt> {
    pollard_pm1_with(n, 1)
}

/// Pollard's p - 1 when a factor of p - 1 is known in advance (for a prime
/// p dividing a cyclotomic piece Phi_d(b), d divides p - 1)
pub fn pollard_pm1_with(n: &BigInt, known: u64) -> Option<BigInt> {
    let e = effort() as usize;
    let b1: usize = 100_000 * e.min(1000);
    let b2: usize = ((if limbs(n) <= 4 { 5_000_000 } else { 1_000_000 }) * e)
        .min(50_000_000)
        .max(b1 + 1);
    let ps = small::primes_upto(b2);
    let mut a = BigInt::from(2).modpow(&BigInt::from(known.max(1)), n);
    for (i, &q) in ps.iter().take_while(|&&q| q <= b1).enumerate() {
        let mut qk = q as u64;
        while qk * (q as u64) <= b1 as u64 {
            qk *= q as u64;
        }
        a = a.modpow(&BigInt::from(qk), n);
        if i % 1000 == 999 || q > b1 - 1000 {
            let g = (&a - 1u32).gcd(n);
            if &g == n {
                return None;
            }
            if !g.is_one() {
                return Some(g);
            }
        }
    }
    // stage 2: one larger prime q, stepping a^q from prime to prime
    let first = ps.iter().position(|&q| q > b1)?;
    let mut steps: Vec<BigInt> = vec![BigInt::one()];
    let a2 = &a * &a % n;
    for k in 1..=200 {
        let next = &steps[k - 1] * &a2 % n;
        steps.push(next);
    }
    let mut x = a.modpow(&BigInt::from(ps[first]), n);
    let mut acc = BigInt::one();
    for i in first..ps.len() {
        if i > first {
            let gap = (ps[i] - ps[i - 1]) / 2;
            let step = if gap <= 200 {
                steps[gap].clone()
            } else {
                a2.modpow(&BigInt::from(gap), n)
            };
            x = x * step % n;
        }
        acc = acc * (&x - 1u32) % n;
        if i % 2000 == 0 || i + 1 == ps.len() {
            let g = acc.gcd(n);
            if &g == n {
                return None;
            }
            if !g.is_one() {
                return Some(g);
            }
        }
    }
    None
}

/// prime factorization as (prime, exponent), primes increasing
pub fn factorize(n: &BigInt) -> Result<Vec<(BigInt, u32)>, String> {
    let n = n.abs();
    if let Some(v) = word_of(&n) {
        return Ok(small::factorize(v)?
            .into_iter()
            .map(|(p, e)| (big(p), e))
            .collect());
    }
    let mut primes: Vec<BigInt> = vec![];
    let mut n = n;
    for p in small::primes_upto(10_000) {
        let p = p as u32;
        while (&n % p).is_zero() {
            primes.push(BigInt::from(p));
            n /= p;
        }
    }
    let mut todo = if n.is_one() { vec![] } else { vec![n] };
    while let Some(m) = todo.pop() {
        if let Some(v) = word_of(&m) {
            for (p, e) in small::factorize(v)? {
                primes.extend(std::iter::repeat_n(big(p), e as usize));
            }
        } else if is_prime(&m) {
            primes.push(m);
        } else {
            let mut found = primes.clone();
            found.sort();
            let found: Vec<String> = found.iter().map(|p| p.to_string()).collect();
            let d = pollard_pm1(&m).or_else(|| pollard_rho(&m)).ok_or_else(|| {
                let digits = m.to_string().len();
                if found.is_empty() {
                    format!("could not find a factor of this {}-digit number in reasonable time", digits)
                } else {
                    format!("found the factors {}, but could not split the remaining {}-digit number in reasonable time", found.join(", "), digits)
                }
            })?;
            todo.push(&m / &d);
            todo.push(d);
        }
    }
    primes.sort();
    let mut out: Vec<(BigInt, u32)> = vec![];
    for p in primes {
        match out.last_mut() {
            Some((q, e)) if *q == p => *e += 1,
            _ => out.push((p, 1)),
        }
    }
    Ok(out)
}

pub fn divisors(n: &BigInt) -> Result<Vec<BigInt>, String> {
    let mut ds = vec![BigInt::one()];
    for (p, e) in factorize(n)? {
        let mut next = vec![];
        for d in &ds {
            let mut pk = BigInt::one();
            for _ in 0..=e {
                next.push(d * &pk);
                pk *= &p;
            }
        }
        ds = next;
        if ds.len() > 1_000_000 {
            return Err("more than a million divisors".into());
        }
    }
    ds.sort();
    Ok(ds)
}

pub fn totient(n: &BigInt) -> Result<BigInt, String> {
    let mut t = n.clone();
    for (p, _) in factorize(n)? {
        t = t / &p * (&p - 1u32);
    }
    Ok(t)
}

/// smallest k > 0 with a^k = 1 mod n
pub fn mult_order(a: &BigInt, n: &BigInt) -> Result<BigInt, String> {
    if !a.gcd(n).is_one() {
        return Err(format!("{} has no order mod {}: they share a factor", a, n));
    }
    let phi = totient(n)?;
    let mut k = phi.clone();
    for (p, _) in factorize(&phi)? {
        while (&k % &p).is_zero() && powmod(a, &(&k / &p), n).is_one() {
            k /= &p;
        }
    }
    Ok(k)
}

/// smallest primitive root mod n, if there is one
pub fn primitive_root(n: &BigInt) -> Result<Option<BigInt>, String> {
    if *n == BigInt::from(2) {
        return Ok(Some(BigInt::one()));
    }
    let phi = totient(n)?;
    let ps: Vec<BigInt> = factorize(&phi)?.into_iter().map(|(p, _)| p).collect();
    let mut g = BigInt::from(2);
    let limit = n.clone().min(BigInt::from(100_000));
    while g < limit {
        if g.gcd(n).is_one() && ps.iter().all(|p| !powmod(&g, &(&phi / p), n).is_one()) {
            return Ok(Some(g));
        }
        g += 1u32;
    }
    Ok(None)
}

/// integer square root
pub fn isqrt(n: &BigInt) -> BigInt {
    n.sqrt()
}

/// x with x = r_i mod m_i for all i (moduli pairwise coprime); (x, product of moduli)
pub fn crt(rs: &[BigInt], ms: &[BigInt]) -> Option<(BigInt, BigInt)> {
    let (mut x, mut m) = (BigInt::zero(), BigInt::one());
    for (r, mi) in rs.iter().zip(ms) {
        let inv = modinv(&m, mi)?;
        let t = (r - &x).mod_floor(mi);
        let k = t * inv % mi;
        x += &m * k;
        m *= mi;
        x = x.mod_floor(&m);
    }
    Some((x, m))
}

/// the u128 routines, for numbers below 2^126
pub mod small {
    /// a*b mod m without overflow
    pub fn mulmod(a: u128, b: u128, m: u128) -> u128 {
        if m == 0 {
            return 0;
        }
        let (a, b) = (a % m, b % m);
        if m <= u64::MAX as u128 {
            return a * b % m;
        }
        // double-and-add for big moduli
        let (mut r, mut x, mut y) = (0u128, a, b);
        while y > 0 {
            if y & 1 == 1 {
                r = addmod(r, x, m);
            }
            x = addmod(x, x, m);
            y >>= 1;
        }
        r
    }

    fn addmod(a: u128, b: u128, m: u128) -> u128 {
        if a >= m - b { a - (m - b) } else { a + b }
    }

    /// a^e mod m
    pub fn powmod(a: u128, mut e: u128, m: u128) -> u128 {
        if m == 1 {
            return 0;
        }
        let (mut r, mut b) = (1u128, a % m);
        while e > 0 {
            if e & 1 == 1 {
                r = mulmod(r, b, m);
            }
            b = mulmod(b, b, m);
            e >>= 1;
        }
        r
    }

    pub fn gcd(a: u128, b: u128) -> u128 {
        let (mut a, mut b) = (a, b);
        while b != 0 {
            let t = a % b;
            a = b;
            b = t;
        }
        a
    }

    /// (g, x, y) with a*x + b*y = g = gcd(a, b)
    pub fn ext_gcd(a: i128, b: i128) -> (i128, i128, i128) {
        if b == 0 {
            if a < 0 { (-a, -1, 0) } else { (a, 1, 0) }
        } else {
            let (g, x, y) = ext_gcd(b, a.rem_euclid(b));
            (g, y, x - a.div_euclid(b) * y)
        }
    }

    /// a^-1 mod m, if gcd(a, m) = 1
    pub fn modinv(a: i128, m: i128) -> Option<i128> {
        let (g, x, _) = ext_gcd(a.rem_euclid(m), m);
        if g == 1 { Some(x.rem_euclid(m)) } else { None }
    }

    const SMALL: [u128; 12] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];

    /// Miller-Rabin; deterministic below 3.3e24 with these bases, and
    /// a strong probable-prime test beyond
    pub fn is_prime(n: u128) -> bool {
        if n < 2 {
            return false;
        }
        for p in SMALL {
            if n % p == 0 {
                return n == p;
            }
        }
        let mut d = n - 1;
        let mut s = 0;
        while d % 2 == 0 {
            d /= 2;
            s += 1;
        }
        let bases: &[u128] = &[
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
        ];
        'outer: for &a in bases {
            if a % n == 0 {
                // a prime base equal to n says nothing
                continue;
            }
            let mut x = powmod(a, d, n);
            if x == 1 || x == n - 1 {
                continue;
            }
            for _ in 1..s {
                x = mulmod(x, x, n);
                if x == n - 1 {
                    continue 'outer;
                }
            }
            return false;
        }
        true
    }

    pub fn next_prime(n: u128) -> u128 {
        let mut k = n + 1;
        while !is_prime(k) {
            k += 1;
        }
        k
    }

    pub fn prev_prime(n: u128) -> Option<u128> {
        let mut k = n.checked_sub(1)?;
        while k >= 2 {
            if is_prime(k) {
                return Some(k);
            }
            k -= 1;
        }
        None
    }

    /// one nontrivial factor of an odd composite n (Pollard's rho, Brent's variant)
    pub fn pollard_rho(n: u128) -> Option<u128> {
        if n % 2 == 0 {
            return Some(2);
        }
        for c in 1..200u128 {
            let f = |x: u128| addmod(mulmod(x, x, n), c % n, n);
            let (mut y, mut r, mut q, m) = (2u128, 1u64, 1u128, 128u64);
            let (mut g, mut x, mut ys) = (1u128, 0u128, 0u128);
            let mut steps: u64 = 0;
            while g == 1 {
                x = y;
                for _ in 0..r {
                    y = f(y);
                }
                let mut k = 0;
                while k < r && g == 1 {
                    ys = y;
                    for _ in 0..m.min(r - k) {
                        y = f(y);
                        q = mulmod(q, x.abs_diff(y), n);
                    }
                    g = gcd(q, n);
                    k += m;
                }
                r *= 2;
                steps += r;
                if steps > 50_000_000 {
                    return None;
                }
            }
            if g == n {
                loop {
                    ys = f(ys);
                    g = gcd(x.abs_diff(ys), n);
                    if g > 1 {
                        break;
                    }
                }
            }
            if g != n {
                return Some(g);
            }
        }
        None
    }

    /// prime factorization as (prime, exponent), primes increasing
    pub fn factorize(n: u128) -> Result<Vec<(u128, u32)>, String> {
        let mut primes = vec![];
        let mut n = n;
        for p in [2u128, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47] {
            while n % p == 0 {
                primes.push(p);
                n /= p;
            }
        }
        let mut p = 53u128;
        while p * p <= n && p < 10_000 {
            while n % p == 0 {
                primes.push(p);
                n /= p;
            }
            p += 2;
        }
        let mut todo = if n > 1 { vec![n] } else { vec![] };
        while let Some(m) = todo.pop() {
            if is_prime(m) {
                primes.push(m);
            } else {
                let d = pollard_rho(m)
                    .ok_or_else(|| format!("could not factor {} in reasonable time", m))?;
                todo.push(d);
                todo.push(m / d);
            }
        }
        primes.sort();
        let mut out: Vec<(u128, u32)> = vec![];
        for p in primes {
            match out.last_mut() {
                Some((q, e)) if *q == p => *e += 1,
                _ => out.push((p, 1)),
            }
        }
        Ok(out)
    }

    pub fn divisors(n: u128) -> Result<Vec<u128>, String> {
        let mut ds = vec![1u128];
        for (p, e) in factorize(n)? {
            let mut next = vec![];
            for d in &ds {
                let mut pk = 1u128;
                for _ in 0..=e {
                    next.push(d * pk);
                    pk *= p;
                }
            }
            ds = next;
        }
        ds.sort();
        Ok(ds)
    }

    pub fn totient(n: u128) -> Result<u128, String> {
        let mut t = n;
        for (p, _) in factorize(n)? {
            t = t / p * (p - 1);
        }
        Ok(t)
    }

    /// smallest k > 0 with a^k = 1 mod n
    pub fn mult_order(a: u128, n: u128) -> Result<u128, String> {
        if gcd(a, n) != 1 {
            return Err(format!("{} has no order mod {}: they share a factor", a, n));
        }
        let phi = totient(n)?;
        let mut k = phi;
        for (p, _) in factorize(phi)? {
            while k % p == 0 && powmod(a, k / p, n) == 1 {
                k /= p;
            }
        }
        Ok(k)
    }

    /// smallest primitive root mod n, if there is one
    pub fn primitive_root(n: u128) -> Result<Option<u128>, String> {
        if n == 2 {
            return Ok(Some(1));
        }
        let phi = totient(n)?;
        let ps: Vec<u128> = factorize(phi)?.into_iter().map(|(p, _)| p).collect();
        for g in 2..n.min(100_000) {
            if gcd(g, n) == 1 && ps.iter().all(|p| powmod(g, phi / p, n) != 1) {
                return Ok(Some(g));
            }
        }
        Ok(None)
    }

    /// primes up to n (sieve of Eratosthenes)
    pub fn primes_upto(n: usize) -> Vec<usize> {
        if n < 2 {
            return vec![];
        }
        let mut sieve = vec![true; n + 1];
        sieve[0] = false;
        sieve[1] = false;
        let mut i = 2;
        while i * i <= n {
            if sieve[i] {
                let mut j = i * i;
                while j <= n {
                    sieve[j] = false;
                    j += i;
                }
            }
            i += 1;
        }
        (0..=n).filter(|&k| sieve[k]).collect()
    }

    /// integer square root
    pub fn isqrt(n: u128) -> u128 {
        if n < 2 {
            return n;
        }
        let mut x = (n as f64).sqrt() as u128;
        while x * x > n {
            x -= 1;
        }
        while (x + 1).checked_mul(x + 1).is_some_and(|y| y <= n) {
            x += 1;
        }
        x
    }

    /// x with x = r_i mod m_i for all i (moduli pairwise coprime)
    pub fn crt(rs: &[i128], ms: &[i128]) -> Option<(i128, i128)> {
        let (mut x, mut m) = (0i128, 1i128);
        for (&r, &mi) in rs.iter().zip(ms) {
            let inv = modinv(m.rem_euclid(mi), mi)?;
            let t = (r - x).rem_euclid(mi);
            let k = mulmod(t as u128, inv as u128, mi as u128) as i128;
            x += m.checked_mul(k)?;
            m = m.checked_mul(mi)?;
            x = x.rem_euclid(m);
        }
        Some((x, m))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn b(s: &str) -> BigInt {
        s.parse().unwrap()
    }
    #[test]
    fn primes_and_factors() {
        use small::*;
        assert!(is_prime(1_000_000_007));
        assert!(!is_prime(561));
        assert!(is_prime(170141183460469231731687303715884105727)); // 2^127 - 1
        assert_eq!(factorize(360).unwrap(), vec![(2, 3), (3, 2), (5, 1)]);
        let n = 1_000_000_007u128 * 998_244_353;
        assert_eq!(
            factorize(n).unwrap(),
            vec![(998_244_353, 1), (1_000_000_007, 1)]
        );
        assert_eq!(totient(36).unwrap(), 12);
        assert_eq!(modinv(3, 11), Some(4));
        assert_eq!(mult_order(2, 7).unwrap(), 3);
        assert_eq!(primitive_root(7).unwrap(), Some(3));
        assert_eq!(crt(&[2, 3, 2], &[3, 5, 7]), Some((23, 105)));
        assert_eq!(divisors(12).unwrap(), vec![1, 2, 3, 4, 6, 12]);
    }
    #[test]
    fn big_numbers() {
        let two = BigInt::from(2);
        let m127 = num_traits::pow(two.clone(), 127) - 1u32;
        assert!(super::is_prime(&m127));
        assert!(super::is_prime(&(num_traits::pow(two.clone(), 521) - 1u32)));
        assert!(!super::is_prime(
            &(num_traits::pow(two.clone(), 523) - 1u32)
        ));
        let f = super::factorize(&(num_traits::pow(two.clone(), 128) - 1u32)).unwrap();
        assert_eq!(f.len(), 9);
        assert_eq!(f[8], (b("67280421310721"), 1));
        // p - 1 = 2^3 * 3^4 * 5 * 7 * 11 * 13 * 1999993 is smooth, so p - 1 finds p in a big product
        let p = b("6486457297321");
        assert!(super::is_prime(&p));
        let q = super::next_prime(&num_traits::pow(BigInt::from(10), 40));
        assert_eq!(super::pollard_pm1(&(&p * &q)), Some(p));
        assert_eq!(
            super::powmod(&BigInt::from(3), &BigInt::from(100), &BigInt::from(7)),
            BigInt::from(4)
        );
        assert_eq!(
            super::next_prime(&num_traits::pow(BigInt::from(10), 40)),
            b("10000000000000000000000000000000000000121")
        );
    }
}
