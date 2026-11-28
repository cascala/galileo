//! Roots of a polynomial in Z/N: `solve(f, x, N)`.
//!
//! N is factored; modulo each prime p the roots are the linear factors of
//! gcd(f, x^p − x) (Fermat: every element of Z/p is a root of x^p − x), split by
//! Cantor–Zassenhaus; they are lifted to p^e by Hensel's lemma; the Chinese
//! remainder theorem combines the prime powers. The steps are kept for the notes.

use crate::numtheory as nt;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, ToPrimitive, Zero};

pub type R<T> = Result<T, String>;

/// at most this many solutions are listed
pub const MAX_ROOTS: usize = 10_000;

type P = Vec<BigInt>; // coefficients low to high, reduced mod p, no leading zeros

pub(crate) fn trim(mut a: P) -> P {
    while a.last().is_some_and(|c| c.is_zero()) {
        a.pop();
    }
    a
}

pub(crate) fn reduce(a: &[BigInt], p: &BigInt) -> P {
    trim(a.iter().map(|c| c.mod_floor(p)).collect())
}

pub(crate) fn sub(a: &[BigInt], b: &[BigInt], p: &BigInt) -> P {
    let n = a.len().max(b.len());
    let z = BigInt::zero();
    trim(
        (0..n)
            .map(|i| (a.get(i).unwrap_or(&z) - b.get(i).unwrap_or(&z)).mod_floor(p))
            .collect(),
    )
}

/// remainder of a by b (b nonzero) mod p
pub(crate) fn rem(a: &[BigInt], b: &[BigInt], p: &BigInt) -> P {
    let mut r = reduce(a, p);
    let lead_inv = nt::modinv(b.last().unwrap(), p).expect("p prime");
    while r.len() >= b.len() && !r.is_empty() {
        let shift = r.len() - b.len();
        let c = (r.last().unwrap() * &lead_inv).mod_floor(p);
        for (i, bc) in b.iter().enumerate() {
            r[shift + i] = (&r[shift + i] - &c * bc).mod_floor(p);
        }
        r = trim(r);
    }
    r
}

pub(crate) fn quo(a: &[BigInt], b: &[BigInt], p: &BigInt) -> P {
    let mut r = reduce(a, p);
    let lead_inv = nt::modinv(b.last().unwrap(), p).expect("p prime");
    let mut q = vec![BigInt::zero(); r.len().saturating_sub(b.len()) + 1];
    while r.len() >= b.len() && !r.is_empty() {
        let shift = r.len() - b.len();
        let c = (r.last().unwrap() * &lead_inv).mod_floor(p);
        for (i, bc) in b.iter().enumerate() {
            r[shift + i] = (&r[shift + i] - &c * bc).mod_floor(p);
        }
        q[shift] = c;
        r = trim(r);
    }
    trim(q)
}

pub(crate) fn mulmod(a: &[BigInt], b: &[BigInt], m: &[BigInt], p: &BigInt) -> P {
    if a.is_empty() || b.is_empty() {
        return vec![];
    }
    let mut out = vec![BigInt::zero(); a.len() + b.len() - 1];
    for (i, x) in a.iter().enumerate() {
        for (j, y) in b.iter().enumerate() {
            out[i + j] += x * y;
        }
    }
    rem(&out, m, p)
}

pub(crate) fn monic(a: P, p: &BigInt) -> P {
    match a.last() {
        None => a,
        Some(l) => {
            let inv = nt::modinv(l, p).expect("p prime");
            a.iter().map(|c| (c * &inv).mod_floor(p)).collect()
        }
    }
}

pub(crate) fn gcd(a: &[BigInt], b: &[BigInt], p: &BigInt) -> P {
    let (mut a, mut b) = (reduce(a, p), reduce(b, p));
    while !b.is_empty() {
        let r = rem(&a, &b, p);
        a = b;
        b = r;
    }
    monic(a, p)
}

/// base^e mod (m, p)
pub(crate) fn powmod(base: &[BigInt], e: &BigInt, m: &[BigInt], p: &BigInt) -> P {
    let mut result: P = rem(&[BigInt::one()], m, p);
    let mut b = rem(base, m, p);
    let bits = e.bits();
    for i in 0..bits {
        if e.bit(i) {
            result = mulmod(&result, &b, m, p);
        }
        if i + 1 < bits {
            b = mulmod(&b, &b, m, p);
        }
    }
    result
}

fn eval(f: &[BigInt], x: &BigInt, m: &BigInt) -> BigInt {
    f.iter()
        .rev()
        .fold(BigInt::zero(), |acc, c| (acc * x + c).mod_floor(m))
}

fn deriv(f: &[BigInt]) -> P {
    f.iter()
        .enumerate()
        .skip(1)
        .map(|(k, c)| c * BigInt::from(k))
        .collect()
}

/// split a monic product of distinct linear factors mod an odd prime
fn split(g: &[BigInt], p: &BigInt, out: &mut Vec<BigInt>) {
    if g.len() <= 1 {
        return;
    }
    if g.len() == 2 {
        out.push((-&g[0]).mod_floor(p));
        return;
    }
    let half = (p - 1u32) / 2u32;
    let mut a = BigInt::zero();
    loop {
        a += 1u32;
        // gcd(g, (x + a)^((p-1)/2) - 1): the roots r with r + a a nonzero square
        let t = powmod(&[a.clone(), BigInt::one()], &half, g, p);
        let h = gcd(g, &sub(&t, &[BigInt::one()], p), p);
        if h.len() > 1 && h.len() < g.len() {
            let other = monic(quo(g, &h, p), p);
            split(&h, p, out);
            split(&other, p, out);
            return;
        }
    }
}

/// How the roots modulo one prime power were found
#[derive(Clone, Debug)]
pub struct PrimeStep {
    pub p: BigInt,
    pub e: u32,
    /// f reduced mod p is zero: every residue is a root mod p
    pub all: bool,
    /// tried every residue (small p) rather than the gcd
    pub brute: bool,
    /// degree of gcd(f, x^p - x) (the number of distinct roots mod p)
    pub gcd_degree: usize,
    pub roots_p: Vec<BigInt>,
    /// roots mod p where f'(r) = 0 mod p (Hensel's lemma does not give one lift)
    pub singular: Vec<BigInt>,
    pub roots: Vec<BigInt>,
}

#[derive(Clone, Debug)]
pub struct ModSolve {
    pub n: BigInt,
    pub f: Vec<BigInt>,
    pub steps: Vec<PrimeStep>,
    pub roots: Vec<BigInt>,
}

/// roots of f mod p
pub(crate) fn roots_mod_p(f: &[BigInt], p: &BigInt) -> (Vec<BigInt>, bool, bool, usize) {
    let fp = reduce(f, p);
    if fp.is_empty() {
        let all = if p.bits() <= 14 {
            (0..p.to_u64().unwrap()).map(BigInt::from).collect()
        } else {
            vec![]
        };
        return (all, true, false, 0);
    }
    if fp.len() == 1 {
        return (vec![], false, false, 0);
    }
    if p == &BigInt::from(2) {
        let r: Vec<BigInt> = (0..p.to_u64().unwrap())
            .map(BigInt::from)
            .filter(|x| eval(&fp, x, p).is_zero())
            .collect();
        let n = r.len();
        return (r, false, true, n);
    }
    // x^p - x mod f, then gcd
    let xp = powmod(&[BigInt::zero(), BigInt::one()], p, &fp, p);
    let g = gcd(&fp, &sub(&xp, &[BigInt::zero(), BigInt::one()], p), p);
    let mut r = vec![];
    split(&g, p, &mut r);
    r.sort();
    (r, false, false, g.len().saturating_sub(1))
}

/// all roots of f (integer coefficients) mod n
pub fn solve(f: &[BigInt], n: &BigInt) -> R<ModSolve> {
    if n < &BigInt::from(2) {
        return Err("solve(f, x, N): N must be an integer of at least 2".into());
    }
    let fac = nt::factorize(n).map_err(|e| {
        format!(
            "solve(f, x, N) needs the prime factors of N, but {}. Solving mod N without them is as hard as factoring N (for x^2 = a this is Rabin's theorem).",
            e
        )
    })?;
    let fprime = deriv(f);
    let mut steps = vec![];
    for (p, e) in fac {
        let (roots_p, all, brute, gd) = roots_mod_p(f, &p);
        if all && roots_p.is_empty() {
            return Err(format!(
                "every residue mod {} is a root (all coefficients are multiples of {}), more than {} solutions",
                p, p, MAX_ROOTS
            ));
        }
        let singular: Vec<BigInt> = roots_p
            .iter()
            .filter(|r| eval(&fprime, r, &p).is_zero())
            .cloned()
            .collect();
        // lift from p^k to p^(k+1)
        let mut roots = roots_p.clone();
        let mut pk = p.clone();
        for _ in 1..e {
            let next = &pk * &p;
            let mut lifted = vec![];
            for r in &roots {
                let d = eval(&fprime, r, &p);
                if !d.is_zero() {
                    // Newton: r - f(r)/f'(r) mod p^(k+1)
                    let inv = nt::modinv(&eval(&fprime, r, &next), &next).unwrap();
                    lifted.push((r - eval(f, r, &next) * inv).mod_floor(&next));
                } else if eval(f, r, &next).is_zero() {
                    // f(r + t p^k) = f(r) mod p^(k+1) for every t
                    let mut t = BigInt::zero();
                    while &t < &p {
                        lifted.push(r + &t * &pk);
                        t += 1u32;
                        if lifted.len() > MAX_ROOTS {
                            return Err(format!("more than {} solutions", MAX_ROOTS));
                        }
                    }
                }
            }
            lifted.sort();
            roots = lifted;
            pk = next;
        }
        steps.push(PrimeStep {
            p,
            e,
            all,
            brute,
            gcd_degree: gd,
            roots_p,
            singular,
            roots,
        });
    }
    // the Chinese remainder theorem
    let mut total: usize = 1;
    for s in &steps {
        total = total.saturating_mul(s.roots.len());
    }
    if total > MAX_ROOTS {
        return Err(format!(
            "{} solutions, more than {} to list",
            total, MAX_ROOTS
        ));
    }
    let mut combos: Vec<(BigInt, BigInt)> = vec![(BigInt::zero(), BigInt::one())];
    for s in &steps {
        let m = s.p.pow(s.e);
        let mut next = vec![];
        for (x, mm) in &combos {
            for r in &s.roots {
                next.push(nt::crt(&[x.clone(), r.clone()], &[mm.clone(), m.clone()]).unwrap());
            }
        }
        combos = next;
    }
    let mut roots: Vec<BigInt> = if steps.iter().any(|s| s.roots.is_empty()) {
        vec![]
    } else {
        combos.into_iter().map(|(x, _)| x).collect()
    };
    roots.sort();
    Ok(ModSolve {
        n: n.clone(),
        f: f.to_vec(),
        steps,
        roots,
    })
}

// ---------- explanations ----------

fn list(v: &[BigInt]) -> String {
    if v.is_empty() {
        return "none".into();
    }
    let mut s: Vec<String> = v.iter().take(12).map(|x| x.to_string()).collect();
    if v.len() > 12 {
        s.push(format!("… ({} in all)", v.len()));
    }
    s.join(", ")
}

fn modulus(s: &PrimeStep) -> String {
    if s.e > 1 {
        format!("{}^{}", s.p, s.e)
    } else {
        s.p.to_string()
    }
}

fn ptext(f: &[BigInt], x: &str) -> String {
    crate::algfactor::show_zpoly(&trim(f.to_vec()), x)
}

/// the notes for solve(f, x, N) (Markdown)
pub fn notes(t: &ModSolve, shown: &str, x: &str, teach: bool) -> String {
    let prime = t.steps.len() == 1 && t.steps[0].e == 1;
    let fs = shown.to_string();
    if !teach {
        if prime {
            let s = &t.steps[0];
            return format!(
                "Roots of `{}` mod the prime {}: {}{}.",
                fs,
                t.n,
                list(&t.roots),
                if s.brute || s.all {
                    " (every residue tried)".to_string()
                } else if s.gcd_degree == 0 {
                    format!("; gcd(f, {x}^p − {x}) = 1, so there are none")
                } else {
                    format!(
                        "; gcd(f, {x}^p − {x}) has degree {}, one factor per root",
                        s.gcd_degree
                    )
                }
            );
        }
        let parts: Vec<String> = t
            .steps
            .iter()
            .map(|s| format!("mod {}: {}", modulus(s), list(&s.roots)))
            .collect();
        return format!(
            "Roots of `{}` mod {} = {}: {}; the Chinese remainder theorem combines them into {} root{} mod {}.",
            fs,
            t.n,
            t.steps.iter().map(modulus).collect::<Vec<_>>().join("·"),
            parts.join("; "),
            t.roots.len(),
            if t.roots.len() == 1 { "" } else { "s" },
            t.n
        );
    }
    let mut md = vec![format!("**Solving `{}` = 0 in Z/{}.**", fs, t.n)];
    let deg = trim(t.f.clone()).len().saturating_sub(1);
    if !prime {
        md.push(format!(
            "**Step 1: split N into prime powers.** {} = {}. By the Chinese remainder theorem, knowing a number mod N is the same as knowing it mod each of these, and f(x) = 0 mod N exactly when it is 0 mod each. So we solve mod each prime power and combine.",
            t.n,
            t.steps.iter().map(modulus).collect::<Vec<_>>().join(" · ")
        ));
    }
    for (i, s) in t.steps.iter().enumerate() {
        let head = if prime {
            "**Mod the prime ".to_string()
        } else {
            format!("**Step {}: mod ", i + 2)
        };
        let fp = reduce(&t.f, &s.p);
        let mut txt = format!("{}{}.** ", head, s.p);
        if s.all {
            txt.push_str(&format!(
                "Every coefficient is a multiple of {}, so every residue is a root mod {}.",
                s.p, s.p
            ));
        } else if s.brute {
            txt.push_str(&format!(
                "Mod {} the polynomial is {}. With only {} residues we just try them both: the roots are {}.",
                s.p,
                ptext(&fp, x),
                s.p,
                list(&s.roots_p)
            ));
        } else {
            txt.push_str(&format!(
                "Mod {} the polynomial is {} (coefficients reduced mod {}). Z/{} is a field, so a polynomial of degree {} has at most {} roots. By Fermat's little theorem every residue r has r^{} = r, so {x}^{} − {x} is the product of ({x} − r) over all of Z/{}; its gcd with f is the product of ({x} − r) over the roots of f alone. Here that gcd has degree {}, so f has {} distinct root{} mod {}.",
                s.p,
                ptext(&fp, x),
                s.p,
                s.p,
                fp.len() - 1,
                fp.len() - 1,
                s.p,
                s.p,
                s.p,
                s.gcd_degree,
                s.gcd_degree,
                if s.gcd_degree == 1 { "" } else { "s" },
                s.p
            ));
            if s.gcd_degree > 1 {
                txt.push_str(&format!(
                    " To split the gcd into its linear factors, take gcd with ({x} + a)^((p−1)/2) − 1 for a = 1, 2, …: that keeps the roots r for which r + a is a nonzero square mod p, which is about half of them (Cantor–Zassenhaus). Repeating gives the roots {}.",
                    list(&s.roots_p)
                ));
            } else if s.gcd_degree == 1 {
                txt.push_str(&format!(" The root is {}.", list(&s.roots_p)));
            }
        }
        if s.e > 1 {
            txt.push_str(&format!(
                " **Lifting to {}.** Hensel's lemma: if f(r) = 0 mod p^k and f'(r) is not 0 mod p, then exactly one r' = r − f(r)/f'(r) (mod p^(k+1)) above r is a root, Newton's method with p-adic numbers.",
                modulus(s)
            ));
            if !s.singular.is_empty() {
                txt.push_str(&format!(
                    " For {} we have f'(r) = 0 mod {}, and then f(r + t·p^k) = f(r) mod p^(k+1) for every t: either all {} lifts are roots or none is. That is how a prime power can have more roots than the degree.",
                    list(&s.singular),
                    s.p,
                    s.p
                ));
            }
            txt.push_str(&format!(
                " The roots mod {} are {}.",
                modulus(s),
                list(&s.roots)
            ));
        }
        md.push(txt);
    }
    if !prime {
        let mut txt = format!(
            "**Step {}: combine.** Each choice of one root per prime power gives one root mod {} (Chinese remainder theorem), so there are {} = {} root{}: {}.",
            t.steps.len() + 2,
            t.n,
            t.steps
                .iter()
                .map(|s| s.roots.len().to_string())
                .collect::<Vec<_>>()
                .join("·"),
            t.roots.len(),
            if t.roots.len() == 1 { "" } else { "s" },
            list(&t.roots)
        );
        if t.roots.len() > deg {
            txt.push_str(&format!(
                " That is more than the degree {}: Z/{} is not a field (it has zero divisors), so the rule \"at most n roots\" does not hold.",
                deg, t.n
            ));
        }
        let mixed = t.roots.iter().find(|r| {
            let v: Vec<BigInt> = t.steps.iter().map(|s| r.mod_floor(&s.p.pow(s.e))).collect();
            v.windows(2).any(|w| w[0] != w[1])
        });
        if let Some(r) = mixed.or(t.roots.first()) {
            let parts: Vec<String> = t
                .steps
                .iter()
                .map(|s| format!("{} mod {}", r.mod_floor(&s.p.pow(s.e)), modulus(s)))
                .collect();
            txt.push_str(&format!(
                " For example {} is the number that is {}.",
                r,
                parts.join(", ")
            ));
        }
        md.push(txt);
    }
    if let Some(r) = t
        .roots
        .iter()
        .find(|r| !eval_int(&t.f, r).is_zero())
        .or(t.roots.first())
    {
        md.push(format!(
            "**Check.** f({}) = {} = {} · {} + 0, so f({}) = 0 mod {}.",
            r,
            eval_int(&t.f, r),
            eval_int(&t.f, r) / &t.n,
            t.n,
            r,
            t.n
        ));
    }
    // Rabin: two roots of a quadratic that are not partners reveal a factor of N
    if deg == 2 && t.steps.len() > 1 {
        let a = t.f[2].mod_floor(&t.n);
        if let Some(ainv) = nt::modinv(&a, &t.n) {
            let sum = (-&t.f[1] * ainv).mod_floor(&t.n);
            let r = &t.roots[0];
            if let Some(s) = t
                .roots
                .iter()
                .find(|s| *s != r && **s != (&sum - r).mod_floor(&t.n))
            {
                let g = nt::gcd(&(r - s), &t.n);
                md.push(format!(
                    "**Why this needs the factors of N.** A quadratic has two roots mod each prime, so mod N it has more, and they come in partner pairs r, (−b/a) − r. Two roots that are not partners, like {} and {}, give a factor: gcd({} − {}, {}) = {}. So anyone who could solve quadratics mod N without its factors could factor N (Rabin, 1979); that is why Galileo factors N first, and why this is slow for an N like an RSA modulus.",
                    r, s, r, s, t.n, g
                ));
            }
        }
    }
    md.join("\n\n")
}

fn eval_int(f: &[BigInt], x: &BigInt) -> BigInt {
    f.iter().rev().fold(BigInt::zero(), |acc, c| acc * x + c)
}
