//! Group programs for the factoring lab: `grp(world, e, r)`.
//!
//! A world is a group (or monoid) of objects modulo N. Start from an element
//! P; at step i replace P by P^e(i), where e is a whole-number expression in
//! i (so e = i gives P^(i!) after i steps). Modulo the hidden prime p the
//! world is a finite group; once its order divides the exponent built so far,
//! P is the identity modulo p, and a gcd with N reveals p. With r > 0, every
//! r steps the program starts over with a fresh element and, for worlds with
//! a parameter, a fresh parameter (that is how ECM tries many curves).
//!
//! Worlds and their sizes modulo p (which decide when a program succeeds):
//! * `mul`: numbers mod N, order p - 1 (Pollard p - 1)
//! * `quad(d)`: a + b sqrt(d), order p^2 - 1 or (p - 1)^2 (Williams p + 1 lives inside)
//! * `cubic(c)`: polynomials mod t^3 - c, order p^3 - 1, (p - 1)(p^2 - 1) or (p - 1)^3
//!   (Bach and Shallit's cyclotomic methods)
//! * `mat`: 2 x 2 matrices, GL(2, p) of order (p^2 - 1)(p^2 - p)
//! * `ec(a)`: the elliptic curve b y^2 = x^3 + a x^2 + x, order p + 1 - t with |t| < 2 sqrt(p) (ECM)

use crate::factorlab as lab;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Zero};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum World {
    Mul,
    Quad(i64),
    Cubic(i64),
    Mat,
    Ec(i64),
}

impl std::fmt::Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            World::Mul => write!(f, "mul"),
            World::Quad(d) => write!(f, "quad({})", d),
            World::Cubic(c) => write!(f, "cubic({})", c),
            World::Mat => write!(f, "mat"),
            World::Ec(a) => write!(f, "ec({})", a),
        }
    }
}

pub const PARAMS: [i64; 8] = [2, 3, 5, 6, 7, 10, -1, -3];

pub fn random_world(rng: &mut lab::Rng) -> World {
    let k = PARAMS[rng.below(PARAMS.len() as u64) as usize];
    match rng.below(5) {
        0 => World::Mul,
        1 => World::Quad(k),
        2 => World::Cubic(k),
        3 => World::Mat,
        _ => World::Ec(k.abs() + 3),
    }
}

pub fn all_worlds() -> Vec<World> {
    let mut v = vec![World::Mul, World::Mat];
    for k in [2, 3, 5, -1] {
        v.push(World::Quad(k));
        v.push(World::Cubic(k));
    }
    for a in [3, 5, 6] {
        v.push(World::Ec(a));
    }
    v
}

struct M<'a> {
    n: &'a BigInt,
}

impl M<'_> {
    fn m(&self, a: &BigInt, b: &BigInt) -> BigInt {
        lab::spend(1.0);
        (a * b) % self.n
    }
    fn s(&self, a: BigInt) -> BigInt {
        a.mod_floor(self.n)
    }
}

/// product in the world (not for ec)
fn mul(w: World, md: &M, x: &[BigInt], y: &[BigInt]) -> Vec<BigInt> {
    match w {
        World::Mul => vec![md.m(&x[0], &y[0])],
        World::Quad(d) => {
            let bd = md.m(&x[1], &y[1]) * d;
            vec![
                md.s(md.m(&x[0], &y[0]) + bd),
                md.s(md.m(&x[0], &y[1]) + md.m(&x[1], &y[0])),
            ]
        }
        World::Cubic(c) => {
            // (x0 + x1 t + x2 t^2)(y0 + y1 t + y2 t^2), t^3 = c
            let p = |i: usize, j: usize| md.m(&x[i], &y[j]);
            let c0 = p(0, 0) + (p(1, 2) + p(2, 1)) * c;
            let c1 = p(0, 1) + p(1, 0) + p(2, 2) * c;
            let c2 = p(0, 2) + p(1, 1) + p(2, 0);
            vec![md.s(c0), md.s(c1), md.s(c2)]
        }
        World::Mat => {
            let p = |i: usize, j: usize| md.m(&x[i], &y[j]);
            vec![
                md.s(p(0, 0) + p(1, 2)),
                md.s(p(0, 1) + p(1, 3)),
                md.s(p(2, 0) + p(3, 2)),
                md.s(p(2, 1) + p(3, 3)),
            ]
        }
        World::Ec(_) => unreachable!(),
    }
}

fn power(w: World, md: &M, a24: &BigInt, x: &[BigInt], e: u64) -> Vec<BigInt> {
    if e <= 1 {
        return x.to_vec();
    }
    if let World::Ec(_) = w {
        return ec_mul(md, a24, (&x[0], &x[1]), e);
    }
    let bits = 64 - e.leading_zeros();
    let mut r = x.to_vec();
    for k in (0..bits - 1).rev() {
        r = mul(w, md, &r, &r);
        if (e >> k) & 1 == 1 {
            r = mul(w, md, &r, x);
        }
    }
    r
}

fn ec_mul(md: &M, a24: &BigInt, p: (&BigInt, &BigInt), k: u64) -> Vec<BigInt> {
    let dbl = |x: &BigInt, z: &BigInt| {
        let s = md.s(x + z);
        let d = md.s(x - z);
        let t1 = md.m(&s, &s);
        let t2 = md.m(&d, &d);
        let t3 = md.s(&t1 - &t2);
        let x2 = md.m(&t1, &t2);
        let z2 = md.m(&t3, &md.s(&t2 + md.m(a24, &t3)));
        (x2, z2)
    };
    let add = |p: &(BigInt, BigInt), q: &(BigInt, BigInt), d: (&BigInt, &BigInt)| {
        let u = md.m(&md.s(&p.0 - &p.1), &md.s(&q.0 + &q.1));
        let v = md.m(&md.s(&p.0 + &p.1), &md.s(&q.0 - &q.1));
        let s = md.s(&u + &v);
        let t = md.s(&u - &v);
        (md.m(d.1, &md.m(&s, &s)), md.m(d.0, &md.m(&t, &t)))
    };
    let mut r0 = (p.0.clone(), p.1.clone());
    let mut r1 = dbl(p.0, p.1);
    let bits = 64 - k.leading_zeros();
    for i in (0..bits - 1).rev() {
        if (k >> i) & 1 == 1 {
            r0 = add(&r1, &r0, p);
            r1 = dbl(&r1.0, &r1.1);
        } else {
            r1 = add(&r0, &r1, p);
            r0 = dbl(&r0.0, &r0.1);
        }
    }
    vec![r0.0, r0.1]
}

/// the start element for attempt `t`, and the curve constant for ec
fn start(w: World, n: &BigInt, t: i64) -> Option<(World, Vec<BigInt>, BigInt)> {
    let b = |v: i64| BigInt::from(v).mod_floor(n);
    let s = 2 + t;
    Some(match w {
        World::Mul => (w, vec![b(s + 1)], BigInt::zero()),
        World::Quad(d) => (World::Quad(d + t), vec![b(s), b(1)], BigInt::zero()),
        World::Cubic(c) => (World::Cubic(c + t), vec![b(s), b(1), b(0)], BigInt::zero()),
        World::Mat => (w, vec![b(s), b(1), b(1), b(1)], BigInt::zero()),
        World::Ec(a) => {
            // a24 = (A + 2)/4 with A = a + t; point x = s + 1
            let aa = a + 7 * t;
            let inv4 = crate::numtheory::modinv(&BigInt::from(4), n)?;
            (
                World::Ec(aa),
                vec![b(s + 1), b(1)],
                (BigInt::from(aa + 2) * inv4).mod_floor(n),
            )
        }
    })
}

/// the coordinate that is 0 mod p once the element is the identity mod p
fn witness(w: World, x: &[BigInt]) -> BigInt {
    match w {
        World::Mul => &x[0] - 1,
        World::Quad(_) | World::Cubic(_) | World::Mat => x[1].clone(),
        World::Ec(_) => x[1].clone(),
    }
}

/// Runs grp(world, e, restart) on n within the current budget. `e(i)` gives
/// the exponent of step i (at least 1).
pub fn run(w: World, e: &dyn Fn(u64) -> u64, restart: u64, n: &BigInt) -> Option<BigInt> {
    let md = M { n };
    let mut attempt = 0i64;
    loop {
        let (wt, mut x, a24) = start(w, n, attempt)?;
        let mut acc = BigInt::one();
        let mut i = 0u64;
        loop {
            i += 1;
            let k = e(i).max(1);
            x = power(wt, &md, &a24, &x, k);
            acc = md.m(&acc, &witness(wt, &x).mod_floor(n));
            let last = restart > 0 && i >= restart;
            if i % 16 == 0 || last || lab::out_of_budget() {
                let g = acc.gcd(n);
                lab::spend(4.0);
                if g > BigInt::one() && &g < n {
                    return Some(g);
                }
                if &g == n {
                    // both primes at once (or a degenerate start): check the witness alone
                    let g2 = witness(wt, &x).mod_floor(n).gcd(n);
                    if g2 > BigInt::one() && &g2 < n {
                        return Some(g2);
                    }
                    break;
                }
            }
            if lab::out_of_budget() {
                return None;
            }
            if last {
                break;
            }
        }
        attempt += 1;
        if attempt > 1_000_000 {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn worlds_find_smooth_orders() {
        // p - 1 smooth: mul finds it
        let mut k = 1u64;
        let p = loop {
            let p = BigInt::from(9_699_690u64 * k + 1);
            if crate::numtheory::is_prime(&p) {
                break p;
            }
            k += 1;
        };
        let n = &p * BigInt::from(1_000_000_007u64);
        lab::start_budget(1e8, 10.0);
        assert_eq!(run(World::Mul, &|i| i, 0, &n), Some(p.clone()));
        lab::end_budget();
        // ECM with restarts finds a factor of a 20-digit semiprime
        let (n, p, q) = lab::semiprime(20, 2);
        lab::start_budget(1e9, 20.0);
        let f = run(World::Ec(3), &|i| i, 150, &n).unwrap();
        lab::end_budget();
        assert!(f == p || f == q);
    }
}
