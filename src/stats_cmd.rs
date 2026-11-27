//! Probability and statistics commands: descriptive statistics, random
//! samples, distributions, confidence intervals, tests, least-squares lines,
//! histograms and bar charts. Exact data (integers, fractions) give exact
//! answers; decimal data give decimals.

use crate::{calc, cas, numeric};
use crate::expr::Func;
use crate::build::*;
use crate::calc::{evalf, simplify};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::num::Number;
use crate::parser::Node;
use crate::plot::{Figure, Series};
use crate::print::show;
use crate::stats;
use std::cmp::Ordering;
use std::rc::Rc;

#[derive(Clone)]
enum Data {
    Exact(Vec<Number>),
    Float(Vec<f64>),
}

impl Data {
    fn len(&self) -> usize {
        match self {
            Data::Exact(v) => v.len(),
            Data::Float(v) => v.len(),
        }
    }
    fn floats(&self) -> Vec<f64> {
        match self {
            Data::Exact(v) => v.iter().map(|n| n.to_f64()).collect(),
            Data::Float(v) => v.clone(),
        }
    }
    fn sorted(&self) -> Data {
        match self {
            Data::Exact(v) => {
                let mut v = v.clone();
                v.sort_by(|a, b| a.cmp_value(b));
                Data::Exact(v)
            }
            Data::Float(v) => {
                let mut v = v.clone();
                v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
                Data::Float(v)
            }
        }
    }
    fn get(&self, i: usize) -> Expr {
        match self {
            Data::Exact(v) => Expr::Num(v[i].clone()),
            Data::Float(v) => Expr::float(v[i]),
        }
    }
    fn exprs(&self) -> Vec<Expr> {
        (0..self.len()).map(|i| self.get(i)).collect()
    }
}

fn num(e: &Expr) -> Option<Number> {
    e.num()
}

fn to_data(vals: Vec<Expr>, f: &str) -> R<Data> {
    if vals.is_empty() {
        return Err(format!("{}: no data", f));
    }
    let mut exact = vec![];
    let mut all_exact = true;
    let mut floats = vec![];
    for v in &vals {
        match num(v) {
            Some(n) if n.is_exact() => {
                floats.push(n.to_f64());
                exact.push(n);
            }
            _ => {
                all_exact = false;
                let x = evalf(v).num().map(|n| n.to_f64()).ok_or_else(|| format!("{} needs numbers, got {}", f, show(v)))?;
                floats.push(x);
            }
        }
    }
    Ok(if all_exact { Data::Exact(exact) } else { Data::Float(floats) })
}

fn flat(e: Expr) -> Vec<Expr> {
    match e {
        Expr::Matrix(m) => m.into_iter().flatten().collect(),
        v => vec![v],
    }
}

fn row(v: Vec<Expr>) -> Expr {
    Expr::Matrix(vec![v])
}

fn sum_n(v: &[Number]) -> Number {
    v.iter().fold(Number::zero(), |a, b| a.add(b))
}

fn mean_of(d: &Data) -> Expr {
    match d {
        Data::Exact(v) => Expr::Num(sum_n(v).div(Number::int(v.len() as i128)).unwrap()),
        Data::Float(v) => Expr::float(v.iter().sum::<f64>() / v.len() as f64),
    }
}

/// sample variance (divide by n - 1)
fn var_of(d: &Data) -> R<Expr> {
    let n = d.len();
    if n < 2 {
        return Err("var: needs at least two values".into());
    }
    Ok(match d {
        Data::Exact(v) => {
            let m = sum_n(v).div(Number::int(n as i128)).unwrap();
            let ss = v.iter().fold(Number::zero(), |a, x| {
                let d = x.sub(&m);
                a.add(d.mul(&d))
            });
            Expr::Num(ss.div(Number::int(n as i128 - 1)).unwrap())
        }
        Data::Float(v) => {
            let m = v.iter().sum::<f64>() / n as f64;
            Expr::float(v.iter().map(|x| (x - m) * (x - m)).sum::<f64>() / (n - 1) as f64)
        }
    })
}

fn sqrt_of(e: &Expr) -> Expr {
    match e {
        Expr::Num(n) if !n.is_exact() => Expr::float(n.to_f64().sqrt()),
        _ => simplify(&pow(e.clone(), Expr::Num(Number::rat(1, 2).unwrap()))),
    }
}

/// quantile with linear interpolation between order statistics (R's type 7)
fn quantile_of(d: &Data, p: &Number) -> R<Expr> {
    let pf = p.to_f64();
    if !(0.0..=1.0).contains(&pf) {
        return Err("quantile: p must be between 0 and 1".into());
    }
    let s = d.sorted();
    let n = s.len();
    match &s {
        Data::Exact(v) if p.is_exact() => {
            let h = Number::int(n as i128 - 1).mul(p);
            let lo = h.to_f64().floor() as usize;
            let frac = h.sub(Number::int(lo as i128));
            let hi = (lo + 1).min(n - 1);
            Ok(Expr::Num(v[lo].add(frac.mul(v[hi].sub(&v[lo])))))
        }
        _ => {
            let v = s.floats();
            let h = (n - 1) as f64 * pf;
            let lo = h.floor() as usize;
            let hi = (lo + 1).min(n - 1);
            Ok(Expr::float(v[lo] + (h - lo as f64) * (v[hi] - v[lo])))
        }
    }
}

fn arg_num(e: &Expr, f: &str) -> R<f64> {
    let v = evalf(e).num().map(|n| n.to_f64()).ok_or_else(|| format!("{} needs a number, got {}", f, show(e)))?;
    if v.is_finite() { Ok(v) } else { Err(format!("{} needs a finite number", f)) }
}

fn is_numeric(e: &Expr) -> bool {
    e.symbols().is_empty() && matches!(e, Expr::Num(_)) || evalf(e).num().is_some() && e.symbols().is_empty()
}

fn has_float(es: &[&Expr]) -> bool {
    es.iter().any(|e| crate::vcalc::has_float(e))
}

impl Session {
    /// Shannon entropy of a probability list, von Neumann entropy of a density matrix
    fn entropy_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: entropy(p) for probabilities p = [p1 p2 ...], or entropy(rho) for a density matrix; entropy(p, 2) in bits";
        let (m, base) = match args {
            [p] => (self.eval(p)?, None),
            [p, b] => (self.eval(p)?, Some(self.eval(b)?)),
            _ => return Err(usage.into()),
        };
        let Expr::Matrix(rows) = m else { return Err(usage.into()) };
        let (r, c) = (rows.len(), rows.first().map_or(0, |x| x.len()));
        let matrix = r > 1 && c > 1;
        let p: Vec<Expr> = if !matrix {
            rows.into_iter().flatten().collect()
        } else {
            if r != c {
                return Err("entropy: a density matrix must be square".into());
            }
            let diagonal = (0..r).all(|i| (0..c).all(|j| i == j || cas::is_zero(&rows[i][j])));
            if diagonal {
                (0..r).map(|i| rows[i][i].clone()).collect()
            } else if rows.iter().flatten().all(|e| e.symbols().is_empty()) {
                let v: Vec<Vec<f64>> = rows.iter().map(|x| x.iter().map(numeric::value).collect()).collect();
                if (0..r).any(|i| (0..c).any(|j| (v[i][j] - v[j][i]).abs() > 1e-9)) {
                    return Err("entropy: a density matrix must be symmetric (Hermitian)".into());
                }
                crate::vcalc::sym_eigenvalues(&v).into_iter().map(|x| Expr::float(if x.abs() < 1e-13 { 0.0 } else { x })).collect()
            } else {
                crate::decomp::exact_eigenvalues(&rows, "entropy")?
            }
        };
        let vals: Vec<f64> = p.iter().map(numeric::value).collect();
        if vals.iter().all(|v| v.is_finite()) {
            if vals.iter().any(|v| *v < -1e-12) {
                return Err("entropy: probabilities can't be negative".into());
            }
            let t: f64 = vals.iter().sum();
            if (t - 1.0).abs() > 1e-9 {
                return Err(format!("entropy: the probabilities add up to {}, not 1; divide by their sum", t));
            }
        }
        let terms: Vec<Expr> = p
            .iter()
            .filter(|q| !q.is_zero() && numeric::value(q) != 0.0)
            .map(|q| neg(mul2(q.clone(), func(Func::Log, q.clone()))))
            .collect();
        let mut h = calc::simplify(&add(terms));
        if matches!(&h, Expr::Num(n) if !n.is_exact() && n.to_f64().abs() < 1e-12) {
            h = Expr::zero();
        }
        let unit = match &base {
            Some(b) => {
                h = calc::simplify(&div(h, func(Func::Log, b.clone())));
                if b.num().is_some_and(|n| n.to_f64() == 2.0) { "bits" } else { "units of log base" }
            }
            None => "nats (natural log)",
        };
        self.note(if matrix {
            format!("von Neumann entropy −tr(ρ log ρ) = −Σ λ log λ over the eigenvalues λ of ρ, in {}. 0 for a pure state, log n for the maximally mixed state of n levels.", unit)
        } else {
            format!("Shannon entropy −Σ p log p, in {}. 0 when one outcome is certain, log n for n equally likely outcomes.", unit)
        });
        Ok(h)
    }

    pub(crate) fn stats_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "mean" | "median" | "mode" | "var" | "std" | "summary" | "sort" | "numel" | "tally" | "cumsum" => {
                self.describe(f, args)
            }
            "quantile" => self.quantile_cmd(args),
            "cov" | "corr" => self.cov_cmd(f, args),
            "seed" => self.seed_cmd(args),
            "randn" | "randint" | "randbin" | "randpois" => self.random_cmd(f, args),
            "normpdf" | "normcdf" | "norminv" | "binopdf" | "binocdf" | "poisspdf" | "poisscdf" | "tpdf" | "tcdf"
            | "tinv" | "chi2pdf" | "chi2cdf" | "chi2inv" => self.dist_cmd(f, args),
            "ci" => self.ci_cmd(args),
            "ttest" => self.ttest_cmd(args),
            "chi2test" => self.chi2test_cmd(args),
            "linfit" => self.linfit_cmd(args),
            "hist" | "histogram" => self.hist_cmd(args),
            "bar" => self.bar_cmd(args),
            "entropy" => self.entropy_cmd(args),
            _ => return None,
        };
        Some(r)
    }

    fn data_arg(&mut self, n: &Node, f: &str) -> R<Data> {
        let v = self.eval(n)?;
        to_data(flat(v), f)
    }

    fn describe(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a] = args else {
            return Err(format!("usage: {}(data), where data is a row or column of numbers", f));
        };
        let d = self.data_arg(a, f)?;
        let n = d.len();
        Ok(match f {
            "mean" => {
                let m = mean_of(&d);
                self.note(format!("Mean: the sum of the {} values divided by {}.", n, n));
                m
            }
            "median" => {
                let m = quantile_of(&d, &Number::rat(1, 2).unwrap())?;
                self.note("Median: the middle value after sorting (the average of the two middle values when the count is even). Half the data lie below it, half above; unlike the mean it ignores how extreme the extremes are.");
                m
            }
            "mode" => {
                let s = d.sorted();
                let (mut best, mut best_c, mut i) = (0usize, 0usize, 0usize);
                let mut ties = 0;
                while i < n {
                    let mut j = i;
                    while j < n && s.get(j) == s.get(i) {
                        j += 1;
                    }
                    if j - i > best_c {
                        best = i;
                        best_c = j - i;
                        ties = 0;
                    } else if j - i == best_c {
                        ties += 1;
                    }
                    i = j;
                }
                self.note(format!(
                    "Mode: the most frequent value ({} times){}.",
                    best_c,
                    if ties > 0 { format!("; {} other values are as frequent, and the smallest is shown", ties) } else { String::new() }
                ));
                s.get(best)
            }
            "var" => {
                let v = var_of(&d)?;
                self.note(format!("Sample variance: the sum of squared deviations from the mean divided by n − 1 = {} (dividing by n − 1 instead of n makes it an unbiased estimate of the population variance).", n - 1));
                v
            }
            "std" => {
                let v = var_of(&d)?;
                self.note(format!("Sample standard deviation: the square root of the sample variance (sum of squared deviations divided by n − 1 = {}).", n - 1));
                sqrt_of(&v)
            }
            "summary" => {
                let s = d.sorted();
                let q = |p: (i128, i128)| quantile_of(&d, &Number::rat(p.0, p.1).unwrap());
                let out = vec![s.get(0), q((1, 4))?, q((1, 2))?, q((3, 4))?, s.get(n - 1)];
                let m = mean_of(&d);
                let sd = if n > 1 { show(&sqrt_of(&var_of(&d)?)) } else { "-".into() };
                self.note(format!(
                    "Five-number summary [minimum, first quartile, median, third quartile, maximum] of {} values. Mean {}, standard deviation {}, interquartile range {}.",
                    n,
                    show(&m),
                    sd,
                    show(&simplify(&sub(out[3].clone(), out[1].clone())))
                ));
                row(out)
            }
            "sort" => row(d.sorted().exprs()),
            "numel" => Expr::int(n as i128),
            "cumsum" => {
                let mut acc = Expr::zero();
                let mut out = vec![];
                for e in d.exprs() {
                    acc = simplify(&add2(acc, e));
                    out.push(acc.clone());
                }
                row(out)
            }
            _ => {
                // tally: values and how often each occurs
                let s = d.sorted();
                let (mut vals, mut counts) = (vec![], vec![]);
                let mut i = 0;
                while i < n {
                    let mut j = i;
                    while j < n && s.get(j) == s.get(i) {
                        j += 1;
                    }
                    vals.push(s.get(i));
                    counts.push(Expr::int((j - i) as i128));
                    i = j;
                }
                self.note("tally: the first row lists the distinct values, the second how often each occurs.");
                Expr::Matrix(vec![vals, counts])
            }
        })
    }

    fn quantile_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [a, p] = args else {
            return Err("usage: quantile(data, p) with 0 <= p <= 1, or a list of p".into());
        };
        let d = self.data_arg(a, "quantile")?;
        let ps = flat(self.eval(p)?);
        let mut out = vec![];
        for p in &ps {
            let pn = num(p).or_else(|| evalf(p).num()).ok_or("quantile: p must be a number")?;
            out.push(quantile_of(&d, &pn)?);
        }
        self.note("Quantile: the value below which a fraction p of the data lie, interpolating linearly between sorted values (the default of R and NumPy).");
        Ok(if out.len() == 1 { out.pop().unwrap() } else { row(out) })
    }

    fn cov_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [a, b] = args else {
            return Err(format!("usage: {}(x, y) for two lists of the same length", f));
        };
        let x = self.data_arg(a, f)?;
        let y = self.data_arg(b, f)?;
        if x.len() != y.len() || x.len() < 2 {
            return Err(format!("{}: x and y need the same length, at least 2", f));
        }
        let n = x.len();
        let (sxy, sxx, syy) = sums(&x, &y);
        let nm1 = Expr::int(n as i128 - 1);
        if f == "cov" {
            self.note("Sample covariance: the sum of (x − mean x)(y − mean y) divided by n − 1. Positive when x and y tend to be large together.");
            return Ok(simplify(&div(sxy, nm1)));
        }
        let r = simplify(&div(sxy, sqrt_of(&simplify(&mul2(sxx, syy)))));
        self.note(format!(
            "Correlation coefficient r = cov(x, y)/(std(x) std(y)), between −1 and 1: ±1 for points exactly on a line, near 0 for no linear relation. r² = {} is the fraction of the variance of y explained by a straight line.",
            show(&simplify(&pow(r.clone(), Expr::int(2))))
        ));
        Ok(r)
    }

    fn seed_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [a] = args else { return Err("usage: seed(n): restart the random numbers from seed n".into()) };
        let n = self.eval(a)?.num().and_then(|k| k.as_int()).ok_or("seed: give a whole number")?;
        // splitmix64, so nearby seeds give unrelated sequences
        let mut z = (n as u64).wrapping_add(0x9e37_79b9_7f4a_7c15);
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^= z >> 31;
        crate::eval::seed_random(z);
        self.note("seed: rand, randn, randint, randbin and randpois now repeat the same sequence every time this seed is set.");
        Ok(Expr::Str(String::new()))
    }

    fn count_args(&mut self, args: &[Node], f: &str) -> R<(usize, usize)> {
        let get = |s: &mut Session, n: &Node| -> R<usize> {
            match s.eval(n)?.num().and_then(|k| k.as_int()) {
                Some(k) if (1..=1_000_000).contains(&k) => Ok(k as usize),
                _ => Err(format!("{}: the size must be a whole number from 1 to 1000000", f)),
            }
        };
        Ok(match args {
            [n] => (1, get(self, n)?),
            [n, m] => (get(self, n)?, get(self, m)?),
            _ => (1, 1),
        })
    }

    fn random_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let rnd = crate::eval::rand_f64;
        let shape = |s: &mut Session, rest: &[Node]| -> R<(usize, usize)> {
            if rest.is_empty() { Ok((1, 1)) } else { s.count_args(rest, f) }
        };
        let (rows, cols, draw): (usize, usize, Box<dyn Fn() -> Expr>) = match f {
            "randn" => {
                if args.len() > 2 {
                    return Err("usage: randn(n) for n standard normal numbers in a row, randn(n, m) for a matrix".into());
                }
                let (r, c) = shape(self, args)?;
                (r, c, Box::new(move || Expr::float(stats::box_muller(rnd(), rnd()))))
            }
            "randint" => {
                if args.len() < 2 || args.len() > 4 {
                    return Err("usage: randint(a, b, n): n random whole numbers from a to b".into());
                }
                let a = self.eval(&args[0])?.num().and_then(|k| k.as_int()).ok_or("randint: a must be whole")?;
                let b = self.eval(&args[1])?.num().and_then(|k| k.as_int()).ok_or("randint: b must be whole")?;
                if b < a {
                    return Err("randint: need a <= b".into());
                }
                let (r, c) = shape(self, &args[2..])?;
                let span = (b - a + 1) as f64;
                (r, c, Box::new(move || Expr::int(a + ((rnd() * span).floor() as i128).min(b - a))))
            }
            "randbin" => {
                if args.len() < 2 || args.len() > 4 {
                    return Err("usage: randbin(n, p, count): count samples of the number of successes in n trials".into());
                }
                let n = self.eval(&args[0])?.num().and_then(|k| k.as_int()).filter(|k| (0..=100_000).contains(k)).ok_or("randbin: n must be a whole number up to 100000")?;
                let p = arg_num(&self.eval(&args[1])?, "randbin")?;
                if !(0.0..=1.0).contains(&p) {
                    return Err("randbin: p must be between 0 and 1".into());
                }
                let (r, c) = shape(self, &args[2..])?;
                (r, c, Box::new(move || Expr::int((0..n).filter(|_| rnd() < p).count() as i128)))
            }
            _ => {
                if args.is_empty() || args.len() > 3 {
                    return Err("usage: randpois(lambda, count): count Poisson samples with mean lambda".into());
                }
                let lam = arg_num(&self.eval(&args[0])?, "randpois")?;
                if !(0.0..=500.0).contains(&lam) {
                    return Err("randpois: lambda must be between 0 and 500".into());
                }
                let (r, c) = shape(self, &args[1..])?;
                (r, c, Box::new(move || {
                    // multiply uniforms until the product drops below e^-lambda (Knuth)
                    let l = (-lam).exp();
                    let (mut k, mut p) = (0i128, 1.0);
                    loop {
                        p *= rnd();
                        if p <= l {
                            return Expr::int(k);
                        }
                        k += 1;
                    }
                }))
            }
        };
        if rows * cols > 1_000_000 {
            return Err(format!("{}: at most a million numbers", f));
        }
        let m: Vec<Vec<Expr>> = (0..rows).map(|_| (0..cols).map(|_| draw()).collect()).collect();
        Ok(if rows == 1 && cols == 1 { m[0][0].clone() } else { Expr::Matrix(m) })
    }

    fn dist_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let vals: Vec<Expr> = args.iter().map(|a| self.eval(a)).collect::<R<_>>()?;
        // a matrix first argument: apply entry by entry, with one note
        if let Some(Expr::Matrix(m)) = vals.first() {
            let m = m.clone();
            let mut first = true;
            let mut out = Vec::with_capacity(m.len());
            for row in &m {
                let mut r = Vec::with_capacity(row.len());
                for x in row {
                    let mut v = vals.clone();
                    v[0] = x.clone();
                    let keep = self.notes.len();
                    r.push(self.dist_vals(f, v)?);
                    if !first {
                        self.notes.truncate(keep);
                    }
                    first = false;
                }
                out.push(r);
            }
            return Ok(Expr::Matrix(out));
        }
        self.dist_vals(f, vals)
    }

    fn dist_vals(&mut self, f: &str, vals: Vec<Expr>) -> R<Expr> {
        let usage = match f {
            "normpdf" | "normcdf" => "(x) or (x, mu, sigma)",
            "norminv" => "(p) or (p, mu, sigma)",
            "binopdf" | "binocdf" => "(k, n, p)",
            "poisspdf" | "poisscdf" => "(k, lambda)",
            "tpdf" | "tcdf" => "(x, df)",
            "tinv" | "chi2inv" => "(p, df)",
            _ => "(x, df)",
        };
        let bad = || format!("usage: {}{}", f, usage);
        let pi = Expr::Const(crate::expr::Const::Pi);
        let half = Expr::Num(Number::rat(1, 2).unwrap());
        let numeric_all = vals.iter().all(is_numeric);
        let floaty = has_float(&vals.iter().collect::<Vec<_>>());
        let finish = |e: Expr| if floaty { evalf(&e) } else { simplify(&e) };
        match f {
            "normpdf" | "normcdf" | "norminv" => {
                let (x, mu, s) = match vals.as_slice() {
                    [x] => (x.clone(), Expr::zero(), Expr::one()),
                    [x, m, s] => (x.clone(), m.clone(), s.clone()),
                    _ => return Err(bad()),
                };
                if f == "norminv" {
                    let p = arg_num(&x, f)?;
                    if !(0.0 < p && p < 1.0) {
                        return Err("norminv: p must be strictly between 0 and 1".into());
                    }
                    let z = stats::invert(stats::norm_cdf, p, -40.0, 40.0);
                    let r = arg_num(&mu, f)? + arg_num(&s, f)? * z;
                    self.note(format!("norminv: the value with probability {} below it; z = {:.6} standard deviations from the mean.", p, z));
                    return Ok(Expr::float(r));
                }
                let zz = div(sub(x.clone(), mu.clone()), s.clone());
                if f == "normpdf" {
                    self.note("Normal density exp(−(x − μ)²/(2σ²))/(σ√(2π)).");
                    return Ok(finish(div(exp(neg(mul2(half, pow(zz, Expr::int(2))))), mul2(s, pow(mul2(Expr::int(2), pi), Expr::Num(Number::rat(1, 2).unwrap()))))));
                }
                self.note("Normal cumulative probability P(X ≤ x) = (1 + erf((x − μ)/(σ√2)))/2.");
                if numeric_all {
                    let z = arg_num(&zz, f)?;
                    return Ok(Expr::float(stats::norm_cdf(z)));
                }
                let arg = div(zz, pow(Expr::int(2), Expr::Num(Number::rat(1, 2).unwrap())));
                Ok(simplify(&mul2(half, add2(Expr::one(), crate::special::apply("erf", vec![arg])))))
            }
            "binopdf" | "binocdf" => {
                let [k, n, p] = vals.as_slice() else { return Err(bad()) };
                let n_i = n.num().and_then(|x| x.as_int()).filter(|x| *x >= 0).ok_or("binopdf: n must be a whole number")?;
                let pmf = |k: i128| -> Expr {
                    if k < 0 || k > n_i {
                        return Expr::zero();
                    }
                    mul(vec![
                        crate::cas::binomial(n_i, k),
                        pow(p.clone(), Expr::int(k)),
                        pow(sub(Expr::one(), p.clone()), Expr::int(n_i - k)),
                    ])
                };
                let k_i = k.num().and_then(|x| x.as_int()).ok_or_else(|| format!("{}: k must be a whole number", f))?;
                let r = if f == "binopdf" {
                    self.note(format!("Binomial probability P(X = k) = C(n, k) p^k (1 − p)^(n − k): exactly k successes in n = {} independent trials.", n_i));
                    pmf(k_i)
                } else {
                    self.note("Binomial cumulative probability P(X ≤ k): the sum of C(n, j) p^j (1 − p)^(n − j) for j = 0 .. k.");
                    add((0..=k_i.min(n_i)).map(pmf).collect())
                };
                Ok(finish(r))
            }
            "poisspdf" | "poisscdf" => {
                let [k, lam] = vals.as_slice() else { return Err(bad()) };
                let k_i = k.num().and_then(|x| x.as_int()).ok_or_else(|| format!("{}: k must be a whole number", f))?;
                let pmf = |j: i128| -> Expr {
                    if j < 0 {
                        return Expr::zero();
                    }
                    let fact: Expr = Expr::Num(Number::from_bigint((1..=j).fold(num_bigint::BigInt::from(1), |a, b| a * b)));
                    div(mul2(pow(lam.clone(), Expr::int(j)), exp(neg(lam.clone()))), fact)
                };
                let r = if f == "poisspdf" {
                    self.note("Poisson probability P(X = k) = λ^k e^(−λ)/k!: the number of rare events in an interval when λ are expected.");
                    pmf(k_i)
                } else {
                    self.note("Poisson cumulative probability P(X ≤ k) = e^(−λ) Σ λ^j/j! for j = 0 .. k.");
                    add((0..=k_i).map(pmf).collect())
                };
                Ok(finish(r))
            }
            "tpdf" | "chi2pdf" => {
                let [x, df] = vals.as_slice() else { return Err(bad()) };
                let g = |e: Expr| crate::special::apply("gamma", vec![e]);
                let r = if f == "tpdf" {
                    let nu1 = mul2(half.clone(), add2(df.clone(), Expr::one()));
                    self.note("Student t density Γ((ν+1)/2)/(√(νπ) Γ(ν/2)) (1 + x²/ν)^(−(ν+1)/2), with ν degrees of freedom.");
                    mul(vec![
                        g(nu1.clone()),
                        pow(mul(vec![g(mul2(half.clone(), df.clone())), pow(mul2(df.clone(), pi), half.clone())]), Expr::int(-1)),
                        pow(add2(Expr::one(), div(pow(x.clone(), Expr::int(2)), df.clone())), neg(nu1)),
                    ])
                } else {
                    let k2 = mul2(half.clone(), df.clone());
                    self.note("Chi-square density x^(k/2 − 1) e^(−x/2)/(2^(k/2) Γ(k/2)), with k degrees of freedom.");
                    div(
                        mul2(pow(x.clone(), sub(k2.clone(), Expr::one())), exp(neg(mul2(half.clone(), x.clone())))),
                        mul2(pow(Expr::int(2), k2.clone()), g(k2)),
                    )
                };
                Ok(finish(r))
            }
            _ => {
                let [x, df] = vals.as_slice() else { return Err(bad()) };
                let (x, nu) = (arg_num(x, f)?, arg_num(df, f)?);
                if nu <= 0.0 {
                    return Err(format!("{}: the degrees of freedom must be positive", f));
                }
                let v = match f {
                    "tcdf" => stats::t_cdf(x, nu),
                    "chi2cdf" => stats::chi2_cdf(x, nu),
                    _ => {
                        if !(0.0 < x && x < 1.0) {
                            return Err(format!("{}: p must be strictly between 0 and 1", f));
                        }
                        if f == "tinv" {
                            stats::invert(|t| stats::t_cdf(t, nu), x, -50.0, 50.0)
                        } else {
                            stats::invert(|c| stats::chi2_cdf(c, nu), x, 0.0, 10.0 * nu + 50.0)
                        }
                    }
                };
                self.note(match f {
                    "tcdf" => "P(T ≤ x) for Student's t distribution.",
                    "chi2cdf" => "P(X ≤ x) for the chi-square distribution.",
                    "tinv" => "The t value with probability p below it (a critical value for confidence intervals and tests).",
                    _ => "The chi-square value with probability p below it.",
                });
                Ok(Expr::float(v))
            }
        }
    }

    fn ci_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (d, level) = match args {
            [a] => (self.data_arg(a, "ci")?, 0.95),
            [a, l] => (self.data_arg(a, "ci")?, arg_num(&self.eval(l)?, "ci")?),
            _ => return Err("usage: ci(data) or ci(data, level), level like 0.95".into()),
        };
        if !(0.0 < level && level < 1.0) {
            return Err("ci: the level must be between 0 and 1, like 0.95".into());
        }
        let x = d.floats();
        let n = x.len();
        if n < 2 {
            return Err("ci: needs at least two values".into());
        }
        let m = x.iter().sum::<f64>() / n as f64;
        let s = (x.iter().map(|v| (v - m) * (v - m)).sum::<f64>() / (n - 1) as f64).sqrt();
        let t = stats::invert(|t| stats::t_cdf(t, (n - 1) as f64), (1.0 + level) / 2.0, -50.0, 50.0);
        let e = t * s / (n as f64).sqrt();
        self.note(format!(
            "{}% confidence interval for the mean: mean ± t·s/√n = {:.6} ± {:.6} × {:.6}/√{}, with t = tinv({}, {}) = {:.4} from Student's t distribution with n − 1 = {} degrees of freedom.",
            level * 100.0, m, t, s, n, (1.0 + level) / 2.0, n - 1, t, n - 1
        ));
        Ok(row(vec![Expr::float(m - e), Expr::float(m + e)]))
    }

    fn ttest_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [a, b] = args else {
            return Err("usage: ttest(data, mu0) for one sample, ttest(x, y) to compare two samples".into());
        };
        let x = self.data_arg(a, "ttest")?.floats();
        let bv = self.eval(b)?;
        let stat = |v: &[f64]| {
            let n = v.len() as f64;
            let m = v.iter().sum::<f64>() / n;
            let s2 = v.iter().map(|x| (x - m) * (x - m)).sum::<f64>() / (n - 1.0);
            (n, m, s2)
        };
        if x.len() < 2 {
            return Err("ttest: needs at least two values".into());
        }
        let (n, m, s2) = stat(&x);
        let (t, df, what) = if let Expr::Matrix(_) = bv {
            let y = to_data(flat(bv), "ttest")?.floats();
            if y.len() < 2 {
                return Err("ttest: needs at least two values in each sample".into());
            }
            let (n2, m2, s22) = stat(&y);
            let se2 = s2 / n + s22 / n2;
            let t = (m - m2) / se2.sqrt();
            let df = se2 * se2 / ((s2 / n).powi(2) / (n - 1.0) + (s22 / n2).powi(2) / (n2 - 1.0));
            (t, df, format!("Welch two-sample t-test of equal means: means {:.6} and {:.6}", m, m2))
        } else {
            let mu0 = arg_num(&bv, "ttest")?;
            let t = (m - mu0) / (s2 / n).sqrt();
            (t, n - 1.0, format!("One-sample t-test of mean = {}: sample mean {:.6}", mu0, m))
        };
        let p = 2.0 * (1.0 - stats::t_cdf(t.abs(), df));
        self.note(format!(
            "{}, t = {:.4} with {} {}. The answer is the two-sided p-value: the probability of a |t| at least this large if the null hypothesis were true. {}",
            what,
            t,
            if df.fract() == 0.0 { format!("{}", df) } else { format!("{:.2}", df) },
            if df == 1.0 { "degree of freedom" } else { "degrees of freedom" },
            if p < 0.05 { "p < 0.05: significant at the 5% level." } else { "p ≥ 0.05: not significant at the 5% level." }
        ));
        Ok(Expr::float(p))
    }

    fn chi2test_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let obs_e = match args.first() {
            Some(a) => self.eval(a)?,
            None => return Err("usage: chi2test(observed, expected) or chi2test(table)".into()),
        };
        let (stat, df, what) = match args {
            [_, e] => {
                let o = to_data(flat(obs_e), "chi2test")?.floats();
                let mut ex = to_data(flat(self.eval(e)?), "chi2test")?.floats();
                if o.len() != ex.len() || o.len() < 2 {
                    return Err("chi2test: observed and expected need the same length, at least 2".into());
                }
                let (so, se) = (o.iter().sum::<f64>(), ex.iter().sum::<f64>());
                if (se - 1.0).abs() < 1e-9 && (so - 1.0).abs() > 1e-9 {
                    ex.iter_mut().for_each(|p| *p *= so);
                }
                if ex.iter().any(|e| *e <= 0.0) {
                    return Err("chi2test: expected counts must be positive".into());
                }
                let small = ex.iter().filter(|e| **e < 5.0).count();
                let s: f64 = o.iter().zip(&ex).map(|(o, e)| (o - e) * (o - e) / e).sum();
                (s, (o.len() - 1) as f64, format!(
                    "Goodness of fit: χ² = Σ (observed − expected)²/expected = {:.4} over {} categories{}",
                    s,
                    o.len(),
                    if small > 0 { format!(" ({} expected counts are below 5, so the chi-square approximation is rough)", small) } else { String::new() }
                ))
            }
            [_] => {
                let Expr::Matrix(t) = obs_e else { return Err("chi2test: give a table (a matrix of counts) or observed and expected".into()) };
                let r = t.len();
                let c = t[0].len();
                if r < 2 || c < 2 {
                    return Err("chi2test: a contingency table needs at least 2 rows and 2 columns".into());
                }
                let o: Vec<Vec<f64>> = t.iter().map(|row| row.iter().map(|v| arg_num(v, "chi2test")).collect::<R<_>>()).collect::<R<_>>()?;
                let rs: Vec<f64> = o.iter().map(|row| row.iter().sum()).collect();
                let cs: Vec<f64> = (0..c).map(|j| o.iter().map(|row| row[j]).sum()).collect();
                let tot: f64 = rs.iter().sum();
                let mut s = 0.0;
                for i in 0..r {
                    for j in 0..c {
                        let e = rs[i] * cs[j] / tot;
                        if e <= 0.0 {
                            return Err("chi2test: a row or column of the table is all zero".into());
                        }
                        s += (o[i][j] - e).powi(2) / e;
                    }
                }
                (s, ((r - 1) * (c - 1)) as f64, format!("Test of independence in a {}×{} table: expected counts (row total × column total)/{}, χ² = {:.4}", r, c, tot, s))
            }
            _ => return Err("usage: chi2test(observed, expected) or chi2test(table)".into()),
        };
        let p = 1.0 - stats::chi2_cdf(stat, df);
        self.note(format!(
            "{} with {} degree{} of freedom. The answer is the p-value P(χ² ≥ {:.4}). {}",
            what,
            df,
            if df == 1.0 { "" } else { "s" },
            stat,
            if p < 0.05 { "p < 0.05: the data are unlikely under the hypothesis." } else { "p ≥ 0.05: the data are consistent with the hypothesis." }
        ));
        Ok(Expr::float(p))
    }

    fn linfit_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (a, b, var) = match args {
            [a, b] => (a, b, "x".to_string()),
            [a, b, v] => (a, b, crate::commands::ident(v, "linfit: variable")?),
            _ => return Err("usage: linfit(xs, ys) or linfit(xs, ys, x): the least-squares line a + b x".into()),
        };
        let x = self.data_arg(a, "linfit")?;
        let y = self.data_arg(b, "linfit")?;
        if x.len() != y.len() || x.len() < 2 {
            return Err("linfit: xs and ys need the same length, at least 2".into());
        }
        let (sxy, sxx, syy) = sums(&x, &y);
        if crate::cas::is_zero(&sxx) {
            return Err("linfit: all x values are equal, so the slope is undefined".into());
        }
        let slope = simplify(&div(sxy.clone(), sxx.clone()));
        let icpt = simplify(&sub(mean_of(&y), mul2(slope.clone(), mean_of(&x))));
        let r2 = if crate::cas::is_zero(&syy) { Expr::one() } else { simplify(&div(pow(sxy, Expr::int(2)), mul2(sxx, syy))) };
        self.note(format!(
            "Least-squares line: slope b = Sxy/Sxx = {}, intercept a = mean(y) − b·mean(x) = {}. It minimizes the sum of squared vertical distances. r² = {}: the fraction of the variance of y that the line explains.",
            show(&slope),
            show(&icpt),
            show(&r2)
        ));
        Ok(simplify(&add2(icpt, mul2(slope, Expr::sym(&var)))))
    }

    fn hist_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: hist(data), hist(data, bins), hist(data, [edges]) and a last \"density\" to scale the area to 1";
        let mut args = args.to_vec();
        let mut density = false;
        if let Some(Node::Str(s)) = args.last() {
            if s == "density" {
                density = true;
                args.pop();
            } else {
                return Err(usage.into());
            }
        }
        let (d, spec) = match args.as_slice() {
            [a] => (self.data_arg(a, "hist")?, None),
            [a, b] => (self.data_arg(a, "hist")?, Some(self.eval(b)?)),
            _ => return Err(usage.into()),
        };
        let x = d.floats();
        let n = x.len();
        let (lo, hi) = x.iter().fold((f64::INFINITY, f64::NEG_INFINITY), |(a, b), v| (a.min(*v), b.max(*v)));
        let edges: Vec<f64> = match spec {
            Some(Expr::Matrix(m)) => {
                let e: Vec<f64> = m.into_iter().flatten().map(|v| arg_num(&v, "hist")).collect::<R<_>>()?;
                if e.len() < 2 || e.windows(2).any(|w| w[1] <= w[0]) {
                    return Err("hist: the edges must increase".into());
                }
                e
            }
            other => {
                let k = match other {
                    Some(v) => v.num().and_then(|k| k.as_int()).filter(|k| (1..=500).contains(k)).ok_or("hist: the number of bins must be a whole number from 1 to 500")? as usize,
                    None => ((n as f64).log2().ceil() as usize + 1).max(1),
                };
                let (lo, hi) = if hi > lo { (lo, hi) } else { (lo - 0.5, hi + 0.5) };
                (0..=k).map(|i| lo + (hi - lo) * i as f64 / k as f64).collect()
            }
        };
        let k = edges.len() - 1;
        let mut counts = vec![0usize; k];
        let mut outside = 0;
        for v in &x {
            if *v < edges[0] || *v > edges[k] {
                outside += 1;
                continue;
            }
            let mut i = edges.partition_point(|e| e <= v).saturating_sub(1);
            if i >= k {
                i = k - 1;
            }
            counts[i] += 1;
        }
        let heights: Vec<f64> = counts
            .iter()
            .enumerate()
            .map(|(i, c)| if density { *c as f64 / (n as f64 * (edges[i + 1] - edges[i])) } else { *c as f64 })
            .collect();
        let pts = bar_outline(&edges, &heights);
        self.note(format!(
            "Histogram of {} values in {} bins from {} to {}{}{}. Counts: {:?}.",
            n,
            k,
            crate::num::fmt_float(edges[0]),
            crate::num::fmt_float(edges[k]),
            if density { "; heights are scaled so the total area is 1, to compare with a density" } else { "" },
            if outside > 0 { format!("; {} values outside the edges are left out", outside) } else { String::new() },
            counts
        ));
        let s = Series { label: if density { "histogram (density)".into() } else { "histogram".into() }, points: pts, fill: true, ..Default::default() };
        Ok(Expr::Obj(Rc::new(Obj::Figure(Figure::new(vec![s], "", if density { "density" } else { "count" }, false)))))
    }

    fn bar_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (xs, hs) = match args {
            [h] => {
                let h = to_data(flat(self.eval(h)?), "bar")?.floats();
                ((1..=h.len()).map(|i| i as f64).collect::<Vec<_>>(), h)
            }
            [x, h] | [x, h, _] => (to_data(flat(self.eval(x)?), "bar")?.floats(), to_data(flat(self.eval(h)?), "bar")?.floats()),
            _ => return Err("usage: bar(heights), bar(xs, heights) or bar(xs, heights, width)".into()),
        };
        let given = match args {
            [_, _, wd] => Some(arg_num(&self.eval(wd)?, "bar").ok().filter(|w| *w > 0.0).ok_or("bar: the width must be a positive number")?),
            _ => None,
        };
        if xs.len() != hs.len() {
            return Err("bar: xs and heights need the same length".into());
        }
        let mut gap = f64::INFINITY;
        let mut sorted = xs.clone();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
        for w in sorted.windows(2) {
            if w[1] > w[0] {
                gap = gap.min(w[1] - w[0]);
            }
        }
        let w = given.unwrap_or(if gap.is_finite() { 0.8 * gap } else { 0.8 });
        let mut pts = vec![];
        for (x, h) in xs.iter().zip(&hs) {
            let (l, r) = (x - w / 2.0, x + w / 2.0);
            pts.extend([(l, 0.0), (l, *h), (r, *h), (r, 0.0), (f64::NAN, f64::NAN)]);
        }
        pts.pop();
        self.note(format!("Bar chart: {} bars of width {} centred on the given values; the heights are drawn as given.", xs.len(), crate::num::fmt_float(w)));
        let s = Series { label: "bars".into(), points: pts, fill: true, ..Default::default() };
        Ok(Expr::Obj(Rc::new(Obj::Figure(Figure::new(vec![s], "", "", false)))))
    }
}

/// (Sxy, Sxx, Syy): sums of products of deviations from the means
fn sums(x: &Data, y: &Data) -> (Expr, Expr, Expr) {
    match (x, y) {
        (Data::Exact(a), Data::Exact(b)) => {
            let n = Number::int(a.len() as i128);
            let ma = sum_n(a).div(&n).unwrap();
            let mb = sum_n(b).div(&n).unwrap();
            let (mut sxy, mut sxx, mut syy) = (Number::zero(), Number::zero(), Number::zero());
            for (u, v) in a.iter().zip(b) {
                let (du, dv) = (u.sub(&ma), v.sub(&mb));
                sxy = sxy.add(du.mul(&dv));
                sxx = sxx.add(du.mul(&du));
                syy = syy.add(dv.mul(&dv));
            }
            (Expr::Num(sxy), Expr::Num(sxx), Expr::Num(syy))
        }
        _ => {
            let (a, b) = (x.floats(), y.floats());
            let n = a.len() as f64;
            let ma = a.iter().sum::<f64>() / n;
            let mb = b.iter().sum::<f64>() / n;
            let (mut sxy, mut sxx, mut syy) = (0.0, 0.0, 0.0);
            for (u, v) in a.iter().zip(&b) {
                sxy += (u - ma) * (v - mb);
                sxx += (u - ma) * (u - ma);
                syy += (v - mb) * (v - mb);
            }
            (Expr::float(sxy), Expr::float(sxx), Expr::float(syy))
        }
    }
}

/// the outline of adjacent bars: up and down each edge, along each top
fn bar_outline(edges: &[f64], h: &[f64]) -> Vec<(f64, f64)> {
    let mut pts = vec![(edges[0], 0.0)];
    for (i, hi) in h.iter().enumerate() {
        pts.push((edges[i], *hi));
        pts.push((edges[i + 1], *hi));
        pts.push((edges[i + 1], 0.0));
    }
    pts
}
