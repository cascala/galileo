//! Factoring polynomials over a quadratic field Q(sqrt(d)), d a squarefree
//! integer (d = -1 gives the Gaussian numbers, factors with i), by Trager's
//! norm method: shift so the norm g(x - k*sqrt(d)) * g(x + k*sqrt(d)) is
//! square-free, factor the norm over the integers, and take gcds over Q(sqrt(d)).

use crate::algfactor::{self as af, ZPoly};
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Zero};

/// a + b*sqrt(d)
#[derive(Clone, Debug, PartialEq)]
pub struct Qd {
    pub a: BigRational,
    pub b: BigRational,
}

type KPoly = Vec<Qd>;

impl Qd {
    fn zero() -> Qd {
        Qd {
            a: BigRational::zero(),
            b: BigRational::zero(),
        }
    }
    fn rat(a: BigRational) -> Qd {
        Qd {
            a,
            b: BigRational::zero(),
        }
    }
    fn is_zero(&self) -> bool {
        self.a.is_zero() && self.b.is_zero()
    }
    fn add(&self, o: &Qd) -> Qd {
        Qd {
            a: &self.a + &o.a,
            b: &self.b + &o.b,
        }
    }
    fn sub(&self, o: &Qd) -> Qd {
        Qd {
            a: &self.a - &o.a,
            b: &self.b - &o.b,
        }
    }
    fn mul(&self, o: &Qd, d: &BigRational) -> Qd {
        Qd {
            a: &self.a * &o.a + &self.b * &o.b * d,
            b: &self.a * &o.b + &self.b * &o.a,
        }
    }
    fn inv(&self, d: &BigRational) -> Qd {
        let n = &self.a * &self.a - &self.b * &self.b * d;
        Qd {
            a: &self.a / &n,
            b: -&self.b / &n,
        }
    }
    fn conj(&self) -> Qd {
        Qd {
            a: self.a.clone(),
            b: -self.b.clone(),
        }
    }
}

fn ktrim(mut p: KPoly) -> KPoly {
    while p.last().is_some_and(|c| c.is_zero()) {
        p.pop();
    }
    p
}

fn kmul(a: &[Qd], b: &[Qd], d: &BigRational) -> KPoly {
    if a.is_empty() || b.is_empty() {
        return vec![];
    }
    let mut r = vec![Qd::zero(); a.len() + b.len() - 1];
    for (i, x) in a.iter().enumerate() {
        for (j, y) in b.iter().enumerate() {
            r[i + j] = r[i + j].add(&x.mul(y, d));
        }
    }
    ktrim(r)
}

fn kdivrem(a: &[Qd], b: &[Qd], d: &BigRational) -> (KPoly, KPoly) {
    let mut r = a.to_vec();
    if r.len() < b.len() {
        return (vec![], r);
    }
    let inv = b.last().unwrap().inv(d);
    let mut q = vec![Qd::zero(); a.len() - b.len() + 1];
    for i in (0..q.len()).rev() {
        let c = r[i + b.len() - 1].mul(&inv, d);
        for (j, y) in b.iter().enumerate() {
            r[i + j] = r[i + j].sub(&c.mul(y, d));
        }
        q[i] = c;
    }
    (ktrim(q), ktrim(r))
}

fn kmonic(a: &[Qd], d: &BigRational) -> KPoly {
    let inv = a.last().unwrap().inv(d);
    a.iter().map(|c| c.mul(&inv, d)).collect()
}

fn kgcd(a: &[Qd], b: &[Qd], d: &BigRational) -> KPoly {
    let (mut a, mut b) = (ktrim(a.to_vec()), ktrim(b.to_vec()));
    while !b.is_empty() {
        let r = kdivrem(&a, &b, d).1;
        a = b;
        b = r;
    }
    kmonic(&a, d)
}

/// p(x + c)
fn kshift(p: &[Qd], c: &Qd, d: &BigRational) -> KPoly {
    let mut r: KPoly = vec![];
    for coef in p.iter().rev() {
        // r = r*(x + c) + coef
        let mut next = vec![Qd::zero(); r.len() + 1];
        for (i, x) in r.iter().enumerate() {
            next[i + 1] = next[i + 1].add(x);
            next[i] = next[i].add(&x.mul(c, d));
        }
        next[0] = next[0].add(coef);
        r = ktrim(next);
    }
    r
}

/// Irreducible monic factors over Q(sqrt(d)) of an irreducible integer polynomial g
fn split_irreducible(g: &ZPoly, d: &BigInt) -> Option<Vec<KPoly>> {
    let dr = BigRational::from_integer(d.clone());
    let gk: KPoly = g
        .iter()
        .map(|c| Qd::rat(BigRational::from_integer(c.clone())))
        .collect();
    if g.len() <= 2 {
        return Some(vec![kmonic(&gk, &dr)]);
    }
    for k in 0..20i64 {
        let shift = Qd {
            a: BigRational::zero(),
            b: BigRational::from_integer(BigInt::from(-k)),
        };
        let a = kshift(&gk, &shift, &dr);
        let abar: KPoly = a.iter().map(|c| c.conj()).collect();
        let norm = kmul(&a, &abar, &dr);
        if norm.iter().any(|c| !c.b.is_zero()) {
            return None;
        }
        let den = norm.iter().fold(BigInt::one(), |acc, c| {
            num_integer::Integer::lcm(&acc, c.a.denom())
        });
        let nz: ZPoly = norm
            .iter()
            .map(|c| (&c.a * BigRational::from_integer(den.clone())).to_integer())
            .collect();
        let (_, fs) = af::factor_zpoly(&nz)?;
        if fs.iter().any(|(_, m)| *m > 1) {
            continue;
        }
        let back = Qd {
            a: BigRational::zero(),
            b: BigRational::from_integer(BigInt::from(k)),
        };
        let mut out = vec![];
        for (nj, _) in fs {
            let njk: KPoly = nj
                .iter()
                .map(|c| Qd::rat(BigRational::from_integer(c.clone())))
                .collect();
            let h = kgcd(&a, &njk, &dr);
            if h.len() > 1 {
                out.push(kshift(&h, &back, &dr));
            }
        }
        return Some(out);
    }
    None
}

/// Factor an integer polynomial over Q(sqrt(d)): (rational constant, [(monic factor, multiplicity)])
pub fn factor_ext(f: &[BigInt], d: &BigInt) -> Option<(BigRational, Vec<(KPoly, u32)>)> {
    let (c, fs) = af::factor_zpoly(f)?;
    let mut constant = BigRational::from_integer(c);
    let mut out = vec![];
    for (g, m) in fs {
        let lead = g.last().unwrap().clone();
        constant *= BigRational::from_integer(num_traits::pow(lead, m as usize));
        for h in split_irreducible(&g, d)? {
            out.push((h, m));
        }
    }
    Some((constant, out))
}
