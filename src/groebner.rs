//! Gröbner bases of polynomial ideals over Q or Z/p (Buchberger's algorithm
//! with the coprime-leading-terms criterion), and normal forms modulo them.

use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Zero};
use std::cmp::Ordering;

pub type R<T> = Result<T, String>;
pub type Mono = Vec<u32>;
/// terms sorted by the monomial order, largest first; no zero coefficients
pub type Poly = Vec<(Mono, BigRational)>;

/// stop after this many basis elements
const MAX_BASIS: usize = 400;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Order {
    Lex,
    Grlex,
    Grevlex,
}

impl Order {
    pub fn parse(s: &str) -> Option<Order> {
        match s {
            "lex" => Some(Order::Lex),
            "grlex" | "deglex" => Some(Order::Grlex),
            "grevlex" | "degrevlex" => Some(Order::Grevlex),
            _ => None,
        }
    }
    pub fn cmp(self, a: &Mono, b: &Mono) -> Ordering {
        let deg = |m: &Mono| m.iter().sum::<u32>();
        match self {
            Order::Lex => a.cmp(b),
            Order::Grlex => deg(a).cmp(&deg(b)).then_with(|| a.cmp(b)),
            Order::Grevlex => deg(a).cmp(&deg(b)).then_with(|| {
                for i in (0..a.len()).rev() {
                    if a[i] != b[i] {
                        return b[i].cmp(&a[i]);
                    }
                }
                Ordering::Equal
            }),
        }
    }
}

/// The coefficient field: Q, or Z/p
#[derive(Clone, Debug)]
pub struct Ring {
    pub p: Option<BigInt>,
    pub order: Order,
}

impl Ring {
    fn norm(&self, c: BigRational) -> BigRational {
        match &self.p {
            None => c,
            Some(p) => {
                let d = crate::numtheory::modinv(&c.denom().mod_floor(p), p).unwrap_or_default();
                BigRational::from_integer((c.numer() * d).mod_floor(p))
            }
        }
    }
    fn inv(&self, c: &BigRational) -> BigRational {
        self.norm(c.recip())
    }
    pub fn poly(&self, mut t: Vec<(Mono, BigRational)>) -> R<Poly> {
        if let Some(p) = &self.p {
            if t.iter().any(|(_, c)| c.denom().gcd(p) != BigInt::one()) {
                return Err(format!(
                    "a coefficient has a denominator divisible by {}",
                    p
                ));
            }
        }
        t.sort_by(|a, b| self.order.cmp(&b.0, &a.0));
        let mut out: Poly = vec![];
        for (m, c) in t {
            match out.last_mut() {
                Some((lm, lc)) if *lm == m => *lc += c,
                _ => out.push((m, c)),
            }
        }
        Ok(out
            .into_iter()
            .map(|(m, c)| (m, self.norm(c)))
            .filter(|(_, c)| !c.is_zero())
            .collect())
    }
    fn monic(&self, f: Poly) -> Poly {
        match f.first() {
            None => f,
            Some((_, c)) => {
                let k = self.inv(c);
                f.into_iter().map(|(m, c)| (m, self.norm(c * &k))).collect()
            }
        }
    }
    /// f - c*x^m*g
    fn sub_mul(&self, f: &Poly, c: &BigRational, m: &Mono, g: &Poly) -> Poly {
        let mut t = f.clone();
        for (gm, gc) in g {
            t.push((mono_mul(gm, m), -(c * gc)));
        }
        self.poly(t).unwrap()
    }

    /// Remainder of f on division by the list g (every term reduced)
    pub fn reduce(&self, f: &Poly, g: &[Poly]) -> Poly {
        let mut f = f.clone();
        let mut rem: Poly = vec![];
        while let Some((m, c)) = f.first().cloned() {
            let div = g.iter().find(|h| !h.is_empty() && divides(&h[0].0, &m));
            match div {
                Some(h) => {
                    let q = self.norm(&c * self.inv(&h[0].1));
                    f = self.sub_mul(&f, &q, &mono_div(&m, &h[0].0), h);
                }
                None => {
                    rem.push((m, c));
                    f.remove(0);
                }
            }
        }
        rem
    }

    fn spoly(&self, f: &Poly, g: &Poly) -> Poly {
        let l = mono_lcm(&f[0].0, &g[0].0);
        let a = mono_div(&l, &f[0].0);
        let b = mono_div(&l, &g[0].0);
        let mut t: Vec<(Mono, BigRational)> = vec![];
        let (fi, gi) = (self.inv(&f[0].1), self.inv(&g[0].1));
        for (m, c) in f {
            t.push((mono_mul(m, &a), c * &fi));
        }
        for (m, c) in g {
            t.push((mono_mul(m, &b), -(c * &gi)));
        }
        self.poly(t).unwrap()
    }

    /// The reduced Gröbner basis, sorted by leading term, largest first
    pub fn groebner(&self, fs: Vec<Poly>) -> R<Vec<Poly>> {
        let mut g: Vec<Poly> = fs
            .into_iter()
            .filter(|f| !f.is_empty())
            .map(|f| self.monic(f))
            .collect();
        let mut pairs: Vec<(usize, usize)> = vec![];
        for j in 0..g.len() {
            for i in 0..j {
                pairs.push((i, j));
            }
        }
        while let Some((i, j)) = pairs.pop() {
            let (a, b) = (&g[i][0].0, &g[j][0].0);
            if a.iter().zip(b).all(|(x, y)| *x == 0 || *y == 0) {
                continue; // coprime leading terms: S reduces to 0
            }
            let s = self.reduce(&self.spoly(&g[i], &g[j]), &g);
            if s.is_empty() {
                continue;
            }
            if g.len() >= MAX_BASIS {
                return Err(format!("groebner: stopped at {} basis elements", MAX_BASIS));
            }
            let s = self.monic(s);
            if s[0].0.iter().all(|&e| e == 0) {
                return Ok(vec![s]); // the ideal is everything
            }
            let k = g.len();
            g.push(s);
            for i in 0..k {
                pairs.insert(0, (i, k));
            }
        }
        // minimal: drop elements whose leading term another one divides
        let mut keep: Vec<Poly> = vec![];
        for (i, f) in g.iter().enumerate() {
            let redundant = g
                .iter()
                .enumerate()
                .any(|(j, h)| j != i && divides(&h[0].0, &f[0].0) && (h[0].0 != f[0].0 || j < i));
            if !redundant {
                keep.push(f.clone());
            }
        }
        // reduced: reduce each by the others
        let mut out = vec![];
        for i in 0..keep.len() {
            let others: Vec<Poly> = keep
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, h)| h.clone())
                .collect();
            let lead = keep[i][0].clone();
            let tail = self.reduce(&keep[i][1..].to_vec(), &others);
            let mut f = vec![lead];
            f.extend(tail);
            out.push(self.monic(f));
        }
        out.sort_by(|a, b| self.order.cmp(&b[0].0, &a[0].0));
        Ok(out)
    }
}

fn mono_mul(a: &Mono, b: &Mono) -> Mono {
    a.iter().zip(b).map(|(x, y)| x + y).collect()
}
fn mono_div(a: &Mono, b: &Mono) -> Mono {
    a.iter().zip(b).map(|(x, y)| x - y).collect()
}
fn mono_lcm(a: &Mono, b: &Mono) -> Mono {
    a.iter().zip(b).map(|(x, y)| *x.max(y)).collect()
}
fn divides(a: &Mono, b: &Mono) -> bool {
    a.iter().zip(b).all(|(x, y)| x <= y)
}

/// Whether the ideal has finitely many common zeros (over the algebraic
/// closure): every variable has a pure power among the leading terms
pub fn zero_dimensional(g: &[Poly], nv: usize) -> bool {
    (0..nv).all(|v| {
        g.iter().any(|f| {
            let m = &f[0].0;
            m[v] > 0 && m.iter().enumerate().all(|(i, &e)| i == v || e == 0)
        })
    })
}
