//! Numerics for probability and statistics: the log-gamma function, the
//! regularized incomplete beta and gamma functions, and through them the
//! cumulative distribution functions of the t and chi-square distributions,
//! with inverses by bisection.

/// log Γ(x) for x > 0 (Lanczos, g = 7, n = 9)
#[allow(clippy::excessive_precision)]
pub fn ln_gamma(x: f64) -> f64 {
    const G: [f64; 9] = [
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7,
    ];
    if x < 0.5 {
        let pi = std::f64::consts::PI;
        return (pi / (pi * x).sin()).ln() - ln_gamma(1.0 - x);
    }
    let x = x - 1.0;
    let mut a = G[0];
    let t = x + 7.5;
    for (i, g) in G.iter().enumerate().skip(1) {
        a += g / (x + i as f64);
    }
    0.5 * (2.0 * std::f64::consts::PI).ln() + (x + 0.5) * t.ln() - t + a.ln()
}

/// continued fraction for the incomplete beta function (Lentz)
fn beta_cf(a: f64, b: f64, x: f64) -> f64 {
    let tiny = 1e-300;
    let (qab, qap, qam) = (a + b, a + 1.0, a - 1.0);
    let mut c = 1.0;
    let mut d = 1.0 - qab * x / qap;
    if d.abs() < tiny {
        d = tiny;
    }
    d = 1.0 / d;
    let mut h = d;
    for m in 1..400 {
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
}

/// regularized incomplete beta I_x(a, b)
pub fn beta_inc(a: f64, b: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    if x >= 1.0 {
        return 1.0;
    }
    let lbt = ln_gamma(a + b) - ln_gamma(a) - ln_gamma(b) + a * x.ln() + b * (1.0 - x).ln();
    if x < (a + 1.0) / (a + b + 2.0) {
        lbt.exp() * beta_cf(a, b, x) / a
    } else {
        1.0 - lbt.exp() * beta_cf(b, a, 1.0 - x) / b
    }
}

/// regularized lower incomplete gamma P(a, x)
pub fn gamma_inc(a: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    let lg = ln_gamma(a);
    if x < a + 1.0 {
        // series
        let mut sum = 1.0 / a;
        let mut term = sum;
        let mut ap = a;
        for _ in 0..1000 {
            ap += 1.0;
            term *= x / ap;
            sum += term;
            if term.abs() < sum.abs() * 1e-16 {
                break;
            }
        }
        sum * (-x + a * x.ln() - lg).exp()
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
            if (del - 1.0).abs() < 1e-16 {
                break;
            }
        }
        1.0 - (-x + a * x.ln() - lg).exp() * h
    }
}

pub fn norm_cdf(z: f64) -> f64 {
    0.5 * crate::special::erfc(-z / std::f64::consts::SQRT_2)
}

/// Student t with df degrees of freedom
pub fn t_cdf(t: f64, df: f64) -> f64 {
    let ib = beta_inc(df / 2.0, 0.5, df / (df + t * t));
    if t >= 0.0 { 1.0 - 0.5 * ib } else { 0.5 * ib }
}

pub fn chi2_cdf(x: f64, k: f64) -> f64 {
    gamma_inc(k / 2.0, x / 2.0)
}

/// the x with cdf(x) = p, for an increasing cdf, by bracketing and bisection
pub fn invert(cdf: impl Fn(f64) -> f64, p: f64, lo: f64, hi: f64) -> f64 {
    let (mut lo, mut hi) = (lo, hi);
    while cdf(lo) > p {
        lo = if lo < 0.0 { lo * 2.0 } else { lo / 2.0 - 1.0 };
    }
    while cdf(hi) < p {
        hi = if hi > 0.0 { hi * 2.0 } else { hi / 2.0 + 1.0 };
    }
    for _ in 0..200 {
        let mid = 0.5 * (lo + hi);
        if cdf(mid) < p {
            lo = mid;
        } else {
            hi = mid;
        }
        if hi - lo < 1e-14 * (1.0 + mid.abs()) {
            break;
        }
    }
    0.5 * (lo + hi)
}

/// a standard normal sample (Box-Muller) from two uniforms in (0, 1)
pub fn box_muller(u1: f64, u2: f64) -> f64 {
    let u1 = u1.max(1e-300);
    (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_values() {
        assert!((ln_gamma(5.0) - 24f64.ln()).abs() < 1e-12);
        assert!((norm_cdf(1.96) - 0.9750021048517795).abs() < 1e-12);
        // t with 10 df: 97.5% quantile 2.228139
        let q = invert(|t| t_cdf(t, 10.0), 0.975, -10.0, 10.0);
        assert!((q - 2.2281388519649).abs() < 1e-8);
        // chi-square with 3 df: 95% quantile 7.814728
        let c = invert(|x| chi2_cdf(x, 3.0), 0.95, 0.0, 50.0);
        assert!((c - 7.814727903251178).abs() < 1e-8);
    }
}
