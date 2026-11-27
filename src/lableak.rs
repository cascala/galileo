//! Leakage hunt: does any cheap function of N = p q (p < q) tell us anything
//! about p? Many features of N are computed on semiprimes with known
//! factors and every (feature, target bit) pair is tested for dependence
//! with a chi-squared test; p-values are corrected for the number of tests
//! (Benjamini-Hochberg) and anything that looks significant is re-tested on
//! a held-out half. A naive Bayes predictor using all features at once
//! checks whether weak signals add up to a better-than-chance guess.
//!
//! Positive controls make sure the test can see a leak when there is one:
//! targets like "p = q (mod 3)" are decided by N mod 3, and the second bit
//! of p is tied to how large N is.

use crate::factorlab as lab;
use num_traits::ToPrimitive;

// ---------- statistics ----------

fn ln_gamma(x: f64) -> f64 {
    // Lanczos approximation
    const G: [f64; 9] = [
        0.999_999_999_999_809_9,
        676.520_368_121_885_1,
        -1_259.139_216_722_402_8,
        771.323_428_777_653_1,
        -176.615_029_162_140_6,
        12.507_343_278_686_905,
        -0.138_571_095_265_720_12,
        9.984_369_578_019_572e-6,
        1.505_632_735_149_311_6e-7,
    ];
    if x < 0.5 {
        return (std::f64::consts::PI / (std::f64::consts::PI * x).sin()).ln() - ln_gamma(1.0 - x);
    }
    let x = x - 1.0;
    let mut a = G[0];
    let t = x + 7.5;
    for (i, g) in G.iter().enumerate().skip(1) {
        a += g / (x + i as f64);
    }
    0.5 * (2.0 * std::f64::consts::PI).ln() + (x + 0.5) * t.ln() - t + a.ln()
}

/// regularized upper incomplete gamma Q(a, x)
pub fn gamma_q(a: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return 1.0;
    }
    if x < a + 1.0 {
        // series for P
        let (mut sum, mut del, mut ap) = (1.0 / a, 1.0 / a, a);
        for _ in 0..1000 {
            ap += 1.0;
            del *= x / ap;
            sum += del;
            if del.abs() < sum.abs() * 1e-15 {
                break;
            }
        }
        let p = sum * (-x + a * x.ln() - ln_gamma(a)).exp();
        (1.0 - p).max(0.0)
    } else {
        // continued fraction for Q
        let tiny = 1e-300;
        let mut b = x + 1.0 - a;
        let mut c = 1.0 / tiny;
        let mut d = 1.0 / b;
        let mut h = d;
        for i in 1..1000 {
            let an = -(i as f64) * (i as f64 - a);
            b += 2.0;
            d = an * d + b;
            if d.abs() < tiny {
                d = tiny;
            }
            c = b + an / c;
            if c.abs() < tiny {
                c = tiny;
            }
            d = 1.0 / d;
            let del = d * c;
            h *= del;
            if (del - 1.0).abs() < 1e-15 {
                break;
            }
        }
        (-x + a * x.ln() - ln_gamma(a)).exp() * h
    }
}

pub fn chi2_pvalue(x: f64, dof: f64) -> f64 {
    if dof <= 0.0 {
        return 1.0;
    }
    gamma_q(dof / 2.0, x / 2.0)
}

/// regularized incomplete beta I_x(a, b) (continued fraction)
fn beta_i(a: f64, b: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    if x >= 1.0 {
        return 1.0;
    }
    let bt = (ln_gamma(a + b) - ln_gamma(a) - ln_gamma(b) + a * x.ln() + b * (1.0 - x).ln()).exp();
    let cf = |a: f64, b: f64, x: f64| {
        let tiny = 1e-300;
        let (qab, qap, qam) = (a + b, a + 1.0, a - 1.0);
        let mut c = 1.0;
        let mut d = 1.0 - qab * x / qap;
        if d.abs() < tiny {
            d = tiny;
        }
        d = 1.0 / d;
        let mut h = d;
        for m in 1..1000 {
            let m = m as f64;
            let m2 = 2.0 * m;
            let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
            d = 1.0 + aa * d;
            if d.abs() < tiny {
                d = tiny;
            }
            c = 1.0 + aa / c;
            if c.abs() < tiny {
                c = tiny;
            }
            d = 1.0 / d;
            h *= d * c;
            let aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
            d = 1.0 + aa * d;
            if d.abs() < tiny {
                d = tiny;
            }
            c = 1.0 + aa / c;
            if c.abs() < tiny {
                c = tiny;
            }
            d = 1.0 / d;
            let del = d * c;
            h *= del;
            if (del - 1.0).abs() < 1e-15 {
                break;
            }
        }
        h
    };
    if x < (a + 1.0) / (a + b + 2.0) {
        bt * cf(a, b, x) / a
    } else {
        1.0 - bt * cf(b, a, 1.0 - x) / b
    }
}

/// two-sided p-value of Student's t with dof degrees of freedom
pub fn t_pvalue(t: f64, dof: f64) -> f64 {
    beta_i(dof / 2.0, 0.5, dof / (dof + t * t))
}

/// one-sided p-value of k or more successes in n trials with probability p
pub fn binom_tail(k: usize, n: usize, p: f64) -> f64 {
    if k == 0 {
        return 1.0;
    }
    // P(X >= k) = I_p(k, n - k + 1)
    beta_i(k as f64, (n - k + 1) as f64, p)
}

/// chi-squared test of independence for a 2 x k table (target bit x category)
pub fn chi2_table(xs: &[(usize, bool)], k: usize) -> (f64, f64) {
    let mut t = vec![[0f64; 2]; k];
    for &(c, b) in xs {
        t[c][b as usize] += 1.0;
    }
    let n = xs.len() as f64;
    let col = [
        t.iter().map(|r| r[0]).sum::<f64>(),
        t.iter().map(|r| r[1]).sum::<f64>(),
    ];
    // merge sparse categories into one
    let mut rows: Vec<[f64; 2]> = vec![];
    let mut other = [0f64; 2];
    for r in t {
        let tot = r[0] + r[1];
        if tot == 0.0 {
            continue;
        }
        if tot * col[0].min(col[1]) / n < 5.0 {
            other[0] += r[0];
            other[1] += r[1];
        } else {
            rows.push(r);
        }
    }
    if other[0] + other[1] > 0.0 {
        rows.push(other);
    }
    if rows.len() < 2 || col[0] == 0.0 || col[1] == 0.0 {
        return (0.0, 1.0);
    }
    let mut x2 = 0.0;
    for r in &rows {
        let tot = r[0] + r[1];
        for j in 0..2 {
            let e = tot * col[j] / n;
            if e > 0.0 {
                x2 += (r[j] - e).powi(2) / e;
            }
        }
    }
    (x2, chi2_pvalue(x2, (rows.len() - 1) as f64))
}

// ---------- features ----------

fn jacobi(a: i128, mut n: u128) -> i32 {
    let mut a = a.rem_euclid(n as i128) as u128;
    let mut s = 1;
    while a != 0 {
        while a % 2 == 0 {
            a /= 2;
            if n % 8 == 3 || n % 8 == 5 {
                s = -s;
            }
        }
        std::mem::swap(&mut a, &mut n);
        if a % 4 == 3 && n % 4 == 3 {
            s = -s;
        }
        a %= n;
    }
    if n == 1 { s } else { 0 }
}

fn isqrt(n: u128) -> u128 {
    let mut r = (n as f64).sqrt() as u128;
    while r * r > n {
        r -= 1;
    }
    while (r + 1) * (r + 1) <= n {
        r += 1;
    }
    r
}

/// continued fraction of sqrt(n): first terms and the period length (None if longer than cap)
fn cf_sqrt(n: u128, terms: usize, cap: usize) -> (Vec<u128>, Option<usize>) {
    let a0 = isqrt(n);
    if a0 * a0 == n {
        return (vec![], Some(0));
    }
    let (mut m, mut d, mut a) = (0u128, 1u128, a0);
    let mut out = vec![];
    for k in 1..=cap {
        m = d * a - m;
        d = (n - m * m) / d;
        a = (a0 + m) / d;
        if out.len() < terms {
            out.push(a);
        }
        if a == 2 * a0 {
            return (out, Some(k));
        }
    }
    (out, None)
}

/// class number of the discriminant -4n (reduced forms), for small n
fn class_number(n: u128) -> u64 {
    let d = 4 * n;
    let mut h = 0u64;
    let mut a = 1u128;
    while 3 * a * a <= d {
        let mut b = 0u128;
        while b <= a {
            let num = b * b + d;
            if num % (4 * a) == 0 {
                let c = num / (4 * a);
                if c >= a {
                    h += if b == 0 || b == a || a == c { 1 } else { 2 };
                }
            }
            b += 2;
        }
        a += 1;
    }
    h
}

pub struct Feature {
    pub name: String,
    pub cats: usize,
}

impl Feature {
    /// features that take about sqrt(N) steps: more than factoring N by rho
    pub fn costly(&self) -> bool {
        self.name.contains("period") || self.name.contains("class number")
    }
}

/// names and category counts of the features for numbers with `digits` digits
pub fn feature_list(digits: u32) -> Vec<Feature> {
    let mut f = vec![];
    let mut add = |name: String, cats: usize| f.push(Feature { name, cats });
    for m in [
        3u32, 4, 5, 7, 8, 9, 11, 12, 13, 16, 17, 19, 23, 24, 29, 30, 31, 32, 37, 64,
    ] {
        add(format!("N mod {}", m), m as usize);
    }
    for a in [3, 5, 7, 11, 13, 17, 19, 23] {
        add(format!("Jacobi ({}/N)", a), 2);
    }
    add("least a with (a/N) = -1".into(), 12);
    add("leading digit of N".into(), 10);
    add("second digit of N".into(), 10);
    add("top 3 bits of N after the leading 1".into(), 8);
    add("popcount(N) mod 2".into(), 2);
    add("popcount(N) mod 3".into(), 3);
    add("number of digits 7 in N, mod 2".into(), 2);
    add("zero digits of N in base 3, mod 2".into(), 2);
    add("leading digit of N in base 7".into(), 7);
    add("floor(sqrt N) mod 4".into(), 4);
    add("floor(sqrt N) mod 3".into(), 3);
    add("frac(sqrt N) in eighths".into(), 8);
    add("N - floor(sqrt N)^2, mod 4".into(), 4);
    add("CF of sqrt N: a1 mod 4".into(), 4);
    add("CF of sqrt N: a2 mod 4".into(), 4);
    add("CF of sqrt N: a3 mod 4".into(), 4);
    add("CF of sqrt N: log2(largest of first 40 terms)".into(), 8);
    add(
        "CF of sqrt N: period length mod 4 (3 = longer than 4000)".into(),
        5,
    );
    if digits <= 6 {
        add("class number h(-4N) mod 4".into(), 4);
        add("class number h(-4N) mod 8".into(), 8);
        add("class number h(-4N) mod 3".into(), 3);
    }
    f
}

pub fn features(n: u128, digits: u32) -> Vec<usize> {
    let mut v = vec![];
    for m in [
        3u128, 4, 5, 7, 8, 9, 11, 12, 13, 16, 17, 19, 23, 24, 29, 30, 31, 32, 37, 64,
    ] {
        v.push((n % m) as usize);
    }
    for a in [3, 5, 7, 11, 13, 17, 19, 23] {
        v.push((jacobi(a, n) == 1) as usize);
    }
    v.push(
        (2..13)
            .find(|&a| jacobi(a as i128, n) == -1)
            .map_or(11, |a| a as usize - 2)
            .min(11),
    );
    let s = n.to_string();
    let dg: Vec<usize> = s.bytes().map(|b| (b - b'0') as usize).collect();
    v.push(dg[0]);
    v.push(dg.get(1).copied().unwrap_or(0));
    let bits = 128 - n.leading_zeros();
    v.push(((n >> (bits - 4)) & 7) as usize);
    v.push((n.count_ones() % 2) as usize);
    v.push((n.count_ones() % 3) as usize);
    v.push(dg.iter().filter(|&&d| d == 7).count() % 2);
    let (mut m, mut zeros) = (n, 0);
    while m > 0 {
        zeros += (m % 3 == 0) as usize;
        m /= 3;
    }
    v.push(zeros % 2);
    let mut m = n;
    while m >= 7 {
        m /= 7;
    }
    v.push(m as usize);
    let r = isqrt(n);
    v.push((r % 4) as usize);
    v.push((r % 3) as usize);
    let frac = (n - r * r) as f64 / (2 * r + 1) as f64;
    v.push(((frac * 8.0) as usize).min(7));
    v.push(((n - r * r) % 4) as usize);
    let (cf, period) = cf_sqrt(n, 40, 4000);
    for k in 0..3 {
        v.push(cf.get(k).map_or(0, |a| (a % 4) as usize));
    }
    let big = cf.iter().copied().max().unwrap_or(1);
    v.push(((128 - big.leading_zeros()) as usize).min(7));
    v.push(period.map_or(4, |p| p % 4));
    if digits <= 6 {
        let h = class_number(n);
        v.push((h % 4) as usize);
        v.push((h % 8) as usize);
        v.push((h % 3) as usize);
    }
    v
}

// ---------- targets ----------

pub const TARGETS: &[(&str, bool)] = &[
    // (name, is a positive control expected to leak)
    ("p = 1 (mod 4)", false),
    ("p = 1 (mod 3)", false),
    ("p = +-1 (mod 8)", false),
    ("p = +-1 (mod 5)", false),
    ("middle bit of p", false),
    ("lowest bit of floor(p / 10)", false),
    ("popcount(p) odd", false),
    ("second bit of p (below the leading 1)", true),
    ("p = q (mod 4)", true),
    ("p = q (mod 3)", true),
    ("Legendre (p/q) = 1", true),
];

fn targets(p: u128, q: u128) -> Vec<bool> {
    let bits = 128 - p.leading_zeros();
    vec![
        p % 4 == 1,
        p % 3 == 1,
        p % 8 == 1 || p % 8 == 7,
        p % 5 == 1 || p % 5 == 4,
        (p >> (bits / 2)) & 1 == 1,
        (p / 10) & 1 == 1,
        p.count_ones() % 2 == 1,
        (p >> (bits - 2)) & 1 == 1,
        p % 4 == q % 4,
        p % 3 == q % 3,
        jacobi(p as i128, q) == 1,
    ]
}

// ---------- the hunt ----------

pub struct Hit {
    pub target: usize,
    pub feature: usize,
    pub p_train: f64,
    pub q_bh: f64,
    pub p_test: f64,
}

pub struct Predict {
    pub target: usize,
    pub accuracy: f64,
    pub baseline: f64,
    pub p: f64,
}

pub struct Hunt {
    pub digits: u32,
    pub samples: usize,
    pub features: Vec<Feature>,
    pub tests: usize,
    /// smallest p-values, with their corrected values and the held-out check
    pub hits: Vec<Hit>,
    pub predict: Vec<Predict>,
}

pub fn hunt(digits: u32, samples: usize, seed: u64) -> Result<Hunt, String> {
    if !(5..=36).contains(&digits) {
        return Err("leakhunt: digits from 5 to 36".into());
    }
    let feats = feature_list(digits);
    let mut rows: Vec<(Vec<usize>, Vec<bool>)> = vec![];
    for k in 0..samples {
        let (n, p, q) = lab::semiprime(
            digits,
            seed.wrapping_mul(1_000_033).wrapping_add(k as u64 + 17),
        );
        let (n, p, q) = (
            n.to_u128().unwrap(),
            p.to_u128().unwrap(),
            q.to_u128().unwrap(),
        );
        rows.push((features(n, digits), targets(p, q)));
        if lab::spend(50.0) {
            return Err("leakhunt: out of time; use fewer samples".into());
        }
    }
    let half = samples / 2;
    let (train, test) = rows.split_at(half);
    let mut all: Vec<(usize, usize, f64)> = vec![];
    for t in 0..TARGETS.len() {
        for (f, feat) in feats.iter().enumerate() {
            let xs: Vec<(usize, bool)> = train.iter().map(|r| (r.0[f], r.1[t])).collect();
            let (_, p) = chi2_table(&xs, feat.cats);
            all.push((t, f, p));
        }
    }
    // Benjamini-Hochberg
    let m = all.len();
    let mut order: Vec<usize> = (0..m).collect();
    order.sort_by(|&a, &b| all[a].2.partial_cmp(&all[b].2).unwrap());
    let mut q = vec![1.0; m];
    let mut running: f64 = 1.0;
    for (rank, &ix) in order.iter().enumerate().rev() {
        running = running.min(all[ix].2 * m as f64 / (rank + 1) as f64);
        q[ix] = running.min(1.0);
    }
    // best feature per target, plus every pair with q < 0.05
    let mut picked: Vec<usize> = vec![];
    for t in 0..TARGETS.len() {
        if let Some(&ix) = order.iter().find(|&&ix| all[ix].0 == t) {
            picked.push(ix);
        }
    }
    for &ix in &order {
        if q[ix] < 0.05 && !picked.contains(&ix) {
            picked.push(ix);
        }
    }
    let hits = picked
        .into_iter()
        .map(|ix| {
            let (t, f, p) = all[ix];
            let xs: Vec<(usize, bool)> = test.iter().map(|r| (r.0[f], r.1[t])).collect();
            Hit {
                target: t,
                feature: f,
                p_train: p,
                q_bh: q[ix],
                p_test: chi2_table(&xs, feats[f].cats).1,
            }
        })
        .collect();
    // naive Bayes on all features
    let mut predict = vec![];
    for t in 0..TARGETS.len() {
        let pos = train.iter().filter(|r| r.1[t]).count() as f64;
        let neg = train.len() as f64 - pos;
        let mut counts: Vec<Vec<[f64; 2]>> =
            feats.iter().map(|f| vec![[1.0, 1.0]; f.cats]).collect();
        for r in train {
            for (f, &c) in r.0.iter().enumerate() {
                counts[f][c][r.1[t] as usize] += 1.0;
            }
        }
        let mut right = 0;
        for r in test {
            let mut score = (pos / neg.max(1.0)).ln();
            for (f, &c) in r.0.iter().enumerate() {
                let k = feats[f].cats as f64;
                score +=
                    ((counts[f][c][1]) / (pos + k)).ln() - ((counts[f][c][0]) / (neg + k)).ln();
            }
            if (score > 0.0) == r.1[t] {
                right += 1;
            }
        }
        let base_rate = test.iter().filter(|r| r.1[t]).count() as f64 / test.len() as f64;
        let baseline = base_rate.max(1.0 - base_rate);
        let acc = right as f64 / test.len() as f64;
        let k = right;
        let p = binom_tail(k, test.len(), baseline);
        predict.push(Predict {
            target: t,
            accuracy: acc,
            baseline,
            p,
        });
    }
    Ok(Hunt {
        digits,
        samples,
        features: feats,
        tests: m,
        hits,
        predict,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stats() {
        assert!((chi2_pvalue(3.841, 1.0) - 0.05).abs() < 1e-3);
        assert!((chi2_pvalue(11.07, 5.0) - 0.05).abs() < 1e-3);
        assert!((t_pvalue(2.0, 30.0) - 0.0546).abs() < 1e-3);
        assert!((binom_tail(60, 100, 0.5) - 0.0284).abs() < 1e-3);
        assert_eq!(class_number(5), 2); // h(-20) = 2
        assert_eq!(class_number(14), 4); // h(-56) = 4
        assert_eq!(jacobi(2, 7), 1);
        assert_eq!(jacobi(3, 7), -1);
    }

    #[test]
    fn controls_leak_and_others_do_not() {
        let h = hunt(14, 2000, 1).unwrap();
        let find = |t: usize| h.hits.iter().find(|x| x.target == t).unwrap();
        assert!(find(9).p_train < 1e-10); // p = q (mod 3) from N mod 3
        assert!(find(8).p_train < 1e-10);
    }
}
