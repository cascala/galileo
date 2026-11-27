//! Factoring with groups: Pollard p - 1 with an explicit bound, Lenstra's
//! elliptic curve method, Shanks' SQUFOF, and binary quadratic forms (the class
//! group of a negative discriminant) with a Schnorr–Lenstra style factorer.
//!
//! The common idea: for N = p q, (Z/N)* is (Z/p)* x (Z/q)*. An exponent that
//! kills the group mod p but not mod q gives gcd(a^E - 1, N) = p. p - 1 uses
//! (Z/p)*, whose order p - 1 is fixed; ECM swaps in the group of an elliptic
//! curve mod p, whose order varies with the curve; class groups and SQUFOF use
//! forms of discriminant -4N or kN, where elements of order 2 (ambiguous forms)
//! are factorizations.

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

fn primes_upto(n: usize) -> Vec<u64> {
    crate::numtheory::small::primes_upto(n)
        .into_iter()
        .map(|p| p as u64)
        .collect()
}

/// a small xorshift generator, so runs are repeatable
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
}

// ---------- Pollard p - 1 with an explicit bound ----------

/// Stage 1 of p - 1 with bound b: a = 3^E with E = prod of prime powers <= b.
/// Returns a proper factor, or None. Also returns the last gcd checked (1 when
/// no prime had a b-smooth p - 1, n when every prime did).
pub fn pm1(n: &BigInt, b: u64) -> (Option<BigInt>, BigInt) {
    // base 3, not 2: for n dividing 2^k - 1 (Mersenne and Cunningham numbers)
    // the order of 2 is tiny for every prime, so 2^E = 1 mod n at once
    let mut a = BigInt::from(3);
    let mut g = BigInt::one();
    for (i, q) in primes_upto(b as usize).into_iter().enumerate() {
        let mut qk = q;
        while qk.saturating_mul(q) <= b {
            qk *= q;
        }
        a = a.modpow(&BigInt::from(qk), n);
        if i % 200 == 199 {
            g = (&a - 1u32).gcd(n);
            if !g.is_one() {
                break;
            }
        }
    }
    if g.is_one() {
        g = (&a - 1u32).gcd(n);
    }
    let f = (!g.is_one() && &g != n).then(|| g.clone());
    (f, g)
}

// ---------- Lenstra's elliptic curve method (Montgomery curves, Suyama) ----------

/// x-only Montgomery ladder point (X : Z)
#[derive(Clone)]
struct Pt(BigInt, BigInt);

fn xdbl(p: &Pt, a24: &BigInt, n: &BigInt) -> Pt {
    let s = (&p.0 + &p.1) % n;
    let d = (&p.0 - &p.1).mod_floor(n);
    let s2 = &s * &s % n;
    let d2 = &d * &d % n;
    let t = (&s2 - &d2).mod_floor(n);
    let x = &s2 * &d2 % n;
    let z = &t * ((&d2 + a24 * &t) % n) % n;
    Pt(x, z)
}

fn xadd(p: &Pt, q: &Pt, diff: &Pt, n: &BigInt) -> Pt {
    let u = ((&p.0 - &p.1).mod_floor(n) * ((&q.0 + &q.1) % n)) % n;
    let v = ((&p.0 + &p.1) % n * (&q.0 - &q.1).mod_floor(n)) % n;
    let s = (&u + &v) % n;
    let d = (&u - &v).mod_floor(n);
    Pt(&diff.1 * (&s * &s % n) % n, &diff.0 * (&d * &d % n) % n)
}

fn ladder(p: &Pt, k: u64, a24: &BigInt, n: &BigInt) -> Pt {
    let mut r0 = p.clone();
    let mut r1 = xdbl(p, a24, n);
    for i in (0..63 - k.leading_zeros()).rev() {
        if (k >> i) & 1 == 1 {
            r0 = xadd(&r1, &r0, p, n);
            r1 = xdbl(&r1, a24, n);
        } else {
            r1 = xadd(&r0, &r1, p, n);
            r0 = xdbl(&r0, a24, n);
        }
    }
    r0
}

/// Lenstra's ECM, stage 1 with bound b1 plus a stage 2 for one more prime
/// up to 25*b1, on `curves` random Suyama curves. Returns a proper factor and
/// the number of curves used.
pub fn ecm(n: &BigInt, curves: u64, b1: u64) -> Option<(BigInt, u64)> {
    if n.is_even() {
        return Some((BigInt::from(2), 0));
    }
    let b2 = b1 * 25;
    let ps = primes_upto(b2 as usize);
    let mut rng = Rng(0x9E3779B97F4A7C15 ^ n.to_u64_digits().1.first().copied().unwrap_or(1));
    for c in 1..=curves {
        let sigma = BigInt::from(6 + rng.next() % 1_000_000_000);
        let u = (&sigma * &sigma - 5u32).mod_floor(n);
        let v = (&sigma * 4u32) % n;
        let x0 = u.modpow(&BigInt::from(3), n);
        let z0 = v.modpow(&BigInt::from(3), n);
        // a24 = (A + 2)/4 = (v - u)^3 (3u + v) / (16 u^3 v)
        let num = (&v - &u).mod_floor(n).modpow(&BigInt::from(3), n) * ((&u * 3u32 + &v) % n) % n;
        let den = (&x0 * &v % n) * 16u32 % n;
        let g = den.gcd(n);
        if !g.is_one() {
            if &g != n {
                return Some((g, c));
            }
            continue;
        }
        let inv = crate::numtheory::modinv(&den, n)?;
        let a24 = num * inv % n;
        let mut q = Pt(x0, z0);
        for &p in ps.iter().take_while(|&&p| p <= b1) {
            let mut pk = p;
            while pk.saturating_mul(p) <= b1 {
                pk *= p;
            }
            q = ladder(&q, pk, &a24, n);
        }
        let g = q.1.gcd(n);
        if !g.is_one() {
            if &g != n {
                return Some((g, c));
            }
            continue;
        }
        // stage 2: walk R = [m]Q over odd m from b1 to b2 with one addition per
        // step ([m + 2]Q = [m]Q + [2]Q, difference [m - 2]Q), and multiply up the
        // Z coordinate at each prime m: Z = 0 mod p exactly when [m]Q is the
        // identity mod p, that is when the curve's order mod p is b1-smooth
        // apart from one prime m.
        let q2 = xdbl(&q, &a24, n);
        let m0 = if b1 % 2 == 0 { b1 + 1 } else { b1 + 2 };
        let mut prev = ladder(&q, m0 - 2, &a24, n);
        let mut cur = ladder(&q, m0, &a24, n);
        let mut acc = BigInt::one();
        let mut pi = ps.partition_point(|&p| p < m0);
        let mut m = m0;
        let mut count = 0u64;
        while m <= b2 && pi < ps.len() {
            if ps[pi] == m {
                acc = acc * &cur.1 % n;
                pi += 1;
                count += 1;
                if count % 200 == 0 {
                    let g = acc.gcd(n);
                    if !g.is_one() {
                        break;
                    }
                }
            }
            let next = xadd(&cur, &q2, &prev, n);
            prev = cur;
            cur = next;
            m += 2;
        }
        let g = acc.gcd(n);
        if !g.is_one() && &g != n {
            return Some((g, c));
        }
    }
    None
}

// ---------- SQUFOF ----------

/// Shanks' square forms factorization: walks the continued fraction of
/// sqrt(kN) until a Q value with even index is a perfect square r^2, then a
/// second walk from that square form reaches a symmetry point whose P shares a
/// factor with N. Returns (factor, multiplier k, steps).
pub fn squfof(n: &BigInt) -> Option<(BigInt, u64, u64)> {
    if n.is_even() {
        return Some((BigInt::from(2), 1, 0));
    }
    let r = n.sqrt();
    if &(&r * &r) == n {
        return Some((r, 1, 0));
    }
    const KS: [u64; 16] = [
        1, 3, 5, 7, 11, 15, 21, 33, 35, 55, 77, 105, 165, 231, 385, 1155,
    ];
    for &k in &KS {
        let kn = n * k;
        let s = kn.sqrt();
        let mut p = s.clone();
        let mut qprev = BigInt::one();
        let mut q = &kn - &p * &p;
        if q.is_zero() {
            continue;
        }
        let l = (&s * 2u32).sqrt() * 2u32;
        let bound = (l * 3u32).to_u64().unwrap_or(u64::MAX).min(50_000_000);
        let mut found = None;
        for i in 2..bound {
            let b = (&s + &p) / &q;
            let pn = &b * &q - &p;
            let qn = &qprev + &b * (&p - &pn);
            qprev = q;
            q = qn;
            p = pn;
            if i % 2 == 0 {
                let rq = q.sqrt();
                if &rq * &rq == q {
                    found = Some((rq, i));
                    break;
                }
            }
        }
        let Some((rq, steps)) = found else { continue };
        let b = (&s - &p) / &rq;
        let mut p = &b * &rq + &p;
        let mut qprev = rq.clone();
        let mut q = (&kn - &p * &p) / &qprev;
        if q.is_zero() {
            continue;
        }
        let mut extra = 0u64;
        loop {
            let b = (&s + &p) / &q;
            let pprev = p.clone();
            p = &b * &q - &p;
            let qn = &qprev + &b * (&pprev - &p);
            qprev = q;
            q = qn;
            extra += 1;
            if p == pprev || extra > bound {
                break;
            }
        }
        let f = n.gcd(&qprev);
        if !f.is_one() && &f != n {
            return Some((f, k, steps + extra));
        }
    }
    None
}

// ---------- binary quadratic forms ----------

/// a x^2 + b x y + c y^2, positive definite (discriminant b^2 - 4ac < 0, a > 0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Form {
    pub a: BigInt,
    pub b: BigInt,
    pub c: BigInt,
}

impl Form {
    pub fn new(a: BigInt, b: BigInt, c: BigInt) -> Form {
        Form { a, b, c }
    }
    pub fn disc(&self) -> BigInt {
        &self.b * &self.b - BigInt::from(4) * &self.a * &self.c
    }
    /// the reduced form equivalent to this one: |b| <= a <= c, b >= 0 when |b| = a or a = c
    pub fn reduce(&self) -> Form {
        let (mut a, mut b, mut c) = (self.a.clone(), self.b.clone(), self.c.clone());
        loop {
            // b into (-a, a]
            if b > a || -&b >= a {
                let two_a = &a * 2u32;
                let mut t = (&a - &b).div_floor(&two_a);
                let b2 = &b + &two_a * &t;
                // b2 in (-a, a]
                let b2 = if b2 <= -&a.clone() {
                    t += 1;
                    &b + &two_a * &t
                } else {
                    b2
                };
                c = &c + &t * (&b + &a * &t);
                b = b2;
            }
            if a > c {
                std::mem::swap(&mut a, &mut c);
                b = -b;
                continue;
            }
            if (a == c) && b.is_negative() {
                b = -b;
            }
            return Form { a, b, c };
        }
    }
    pub fn identity(d: &BigInt) -> Form {
        let b = if d.is_odd() {
            BigInt::one()
        } else {
            BigInt::zero()
        };
        let c = (&b * &b - d) / 4u32;
        Form {
            a: BigInt::one(),
            b,
            c,
        }
    }
    pub fn is_identity(&self) -> bool {
        self.a.is_one()
    }
    /// an element of order <= 2: b = 0, a = b or a = c (for a reduced form)
    pub fn is_ambiguous(&self) -> bool {
        self.b.is_zero() || self.a == self.b || self.a == self.c
    }
    /// Gauss composition (Shanks / Cohen, Algorithm 5.4.7), then reduction
    pub fn compose(&self, other: &Form) -> Form {
        let d = self.disc();
        let (f1, f2) = if self.a > other.a {
            (other, self)
        } else {
            (self, other)
        };
        let (a1, b1) = (&f1.a, &f1.b);
        let (a2, b2, c2) = (&f2.a, &f2.b, &f2.c);
        let s: BigInt = (b1 + b2) / 2;
        let n: BigInt = b2 - &s;
        let (y1, dd): (BigInt, BigInt) = if (a2 % a1).is_zero() {
            (BigInt::zero(), a1.clone())
        } else {
            let e = a2.extended_gcd(a1);
            (e.x, e.gcd)
        };
        let (x2, y2, d1) = if (&s % &dd).is_zero() {
            (BigInt::zero(), BigInt::from(-1), dd.clone())
        } else {
            let e = s.extended_gcd(&dd);
            (e.x, -e.y, e.gcd)
        };
        let v1 = a1 / &d1;
        let v2 = a2 / &d1;
        let r = (&y1 * &y2 * &n - &x2 * c2).mod_floor(&v1);
        let b3 = b2 + &v2 * &r * 2u32;
        let a3 = &v1 * &v2;
        let c3 = (&b3 * &b3 - &d) / (&a3 * 4u32);
        Form {
            a: a3,
            b: b3,
            c: c3,
        }
        .reduce()
    }
    pub fn square(&self) -> Form {
        self.compose(self)
    }
    pub fn pow(&self, e: &BigInt) -> Form {
        let mut result = Form::identity(&self.disc());
        let mut base = self.clone();
        let mut e = e.clone();
        while !e.is_zero() {
            if e.is_odd() {
                result = result.compose(&base);
            }
            base = base.square();
            e >>= 1;
        }
        result
    }
    pub fn text(&self) -> String {
        format!("({}, {}, {})", self.a, self.b, self.c)
    }
}

/// all reduced forms of discriminant d < 0 (one per class), in order of a
pub fn reduced_forms(d: i64) -> Vec<Form> {
    let mut out = vec![];
    let ad = -d;
    let mut a = 1i64;
    while 3 * a * a <= ad {
        for b in -a + 1..=a {
            if (b - d).rem_euclid(2) != 0 {
                continue;
            }
            let num = b * b - d;
            if num % (4 * a) != 0 {
                continue;
            }
            let c = num / (4 * a);
            if c < a || (c == a && b < 0) {
                continue;
            }
            // primitive forms only: gcd(a, b, c) = 1
            if a.gcd(&b.abs()).gcd(&c) != 1 {
                continue;
            }
            out.push(Form::new(a.into(), b.into(), c.into()));
        }
        a += 1;
    }
    out
}

/// order of a form in the class group (repeated composition), up to limit
pub fn form_order(f: &Form, limit: u64) -> Option<u64> {
    let mut g = f.clone();
    for k in 1..=limit {
        if g.is_identity() {
            return Some(k);
        }
        g = g.compose(f);
    }
    None
}

/// the factor of n that an ambiguous form of discriminant -4kn reveals, if any
pub fn ambiguous_factor(f: &Form, n: &BigInt) -> Option<BigInt> {
    for v in [&f.a, &f.b, &f.c] {
        let g = v.gcd(n);
        if !g.is_one() && &g != n {
            return Some(g);
        }
    }
    let g = (&f.b - &f.a * 2u32).abs().gcd(n);
    (!g.is_one() && &g != n).then_some(g)
}

/// a prime form (p, b, c) of discriminant d: the smallest prime p with
/// (d/p) = 1, b^2 = d mod 4p
fn prime_form(d: &BigInt, start: u64) -> Option<Form> {
    for p in primes_upto(100_000)
        .into_iter()
        .filter(|&p| p >= start && p > 2)
    {
        let pb = BigInt::from(p);
        let dm = d.mod_floor(&pb);
        if dm.is_zero() {
            continue;
        }
        // brute force the square root mod p (p small)
        let dmu = dm.to_u64().unwrap();
        let Some(r) = (1..p).find(|&x| x * x % p == dmu) else {
            continue;
        };
        let mut b = BigInt::from(r);
        // b must have the parity of d
        if (&b - d).is_odd() {
            b = &pb - &b;
        }
        let num = &b * &b - d;
        if (&num % (&pb * 4u32)).is_zero() {
            return Some(Form::new(pb.clone(), b.clone(), num / (&pb * 4u32)).reduce());
        }
    }
    None
}

/// Class group factoring (in the spirit of Schnorr and Lenstra, 1984): take a
/// form f of discriminant d = -4kn, raise it to E = product of the odd prime
/// powers up to b, then square until the identity appears. The form just
/// before it has order 2, so it is ambiguous, and ambiguous forms of
/// discriminant -4kn are factorizations of kn. Works when the odd part of the
/// class number h(d) is b-smooth. Returns (factor, k, the ambiguous form).
pub fn class_factor(n: &BigInt, b: u64, tries: u64) -> Option<(BigInt, u64, Form)> {
    let mut e = BigInt::one();
    for q in primes_upto(b as usize).into_iter().filter(|&q| q > 2) {
        let mut qk = q;
        while qk.saturating_mul(q) <= b {
            qk *= q;
        }
        e *= qk;
    }
    let steps = n.bits() + 8;
    for k in [1u64, 3, 5, 7, 11, 13, 15, 17, 19, 21, 23] {
        let d = -(n * k * 4u32);
        for t in 0..tries {
            let Some(f) = prime_form(&d, 3 + 10 * t) else {
                break;
            };
            let mut g = f.pow(&e);
            for _ in 0..steps {
                if g.is_identity() {
                    break;
                }
                let h = g.square();
                if h.is_identity() {
                    if let Some(p) = ambiguous_factor(&g, n) {
                        return Some((p, k, g));
                    }
                    break;
                }
                g = h;
            }
        }
    }
    None
}
