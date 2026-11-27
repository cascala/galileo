//! Stochastic processes: Markov chains (exact stationary distributions,
//! absorption probabilities, expected hitting times, simulation), random walks,
//! Poisson arrivals, Brownian motion, stochastic differential equations
//! (Euler–Maruyama), the Ising model (Metropolis) and Black–Scholes prices.

use crate::eval::{R, Session, rand_f64};
use crate::expr::{Expr, Obj};
use crate::num::Number;
use crate::parser::Node;
use crate::print::show;
use std::rc::Rc;

const NAMES: &[&str] = &[
    "stationary",
    "absorption",
    "hitting",
    "randchain",
    "randwalk",
    "arrivals",
    "brownian",
    "sde",
    "ising",
    "bscall",
    "bsput",
];

fn row(v: Vec<Expr>) -> Expr {
    Expr::Matrix(vec![v])
}

fn floats_row(v: &[f64]) -> Expr {
    row(v.iter().map(|&x| Expr::float(x)).collect())
}

fn normal() -> f64 {
    // Box–Muller
    let u = rand_f64().max(1e-300);
    let v = rand_f64();
    (-2.0 * u.ln()).sqrt() * (std::f64::consts::TAU * v).cos()
}

/// A transition matrix: square, entries from 0 to 1, rows adding up to 1
fn transition(e: &Expr, f: &str) -> R<Vec<Vec<Number>>> {
    let usage = || {
        format!(
            "{}: needs a transition matrix P, square, with P(i, j) the probability of going from state i to state j and every row adding up to 1, e.g. [1/2 1/2; 1/4 3/4]",
            f
        )
    };
    let Expr::Matrix(m) = e else { return Err(usage()) };
    let n = m.len();
    if n == 0 || m.iter().any(|r| r.len() != n) {
        return Err(usage());
    }
    let mut p = vec![];
    for (i, r) in m.iter().enumerate() {
        let mut out = vec![];
        let mut sum = Number::zero();
        for x in r {
            let v = x
                .num()
                .or_else(|| crate::calc::evalf(x).num())
                .ok_or_else(usage)?;
            if v.is_negative() || v.cmp_value(Number::one()) == std::cmp::Ordering::Greater {
                return Err(format!("{}: entry {} in row {} is not a probability", f, show(x), i + 1));
            }
            sum = sum.add(&v);
            out.push(v);
        }
        let off = (sum.to_f64() - 1.0).abs();
        if (sum.is_exact() && !sum.is_one()) || off > 1e-9 {
            return Err(format!("{}: row {} adds up to {}, not 1", f, i + 1, show(&Expr::Num(sum))));
        }
        p.push(out);
    }
    Ok(p)
}

/// Solve A x = b exactly (fractions); None if singular
fn solve(mut a: Vec<Vec<Number>>, mut b: Vec<Vec<Number>>) -> Option<Vec<Vec<Number>>> {
    let n = a.len();
    let k = b.first().map_or(0, |r| r.len());
    for c in 0..n {
        let p = if (c..n).all(|r| a[r][c].is_exact()) {
            (c..n).find(|&r| !a[r][c].is_zero())?
        } else {
            (c..n).max_by(|&x, &y| a[x][c].to_f64().abs().partial_cmp(&a[y][c].to_f64().abs()).unwrap_or(std::cmp::Ordering::Equal))?
        };
        if a[p][c].is_zero() || (!a[p][c].is_exact() && a[p][c].to_f64().abs() < 1e-14) {
            return None;
        }
        a.swap(p, c);
        b.swap(p, c);
        let piv = a[c][c].clone();
        for j in c..n {
            a[c][j] = a[c][j].div(&piv)?;
        }
        for j in 0..k {
            b[c][j] = b[c][j].div(&piv)?;
        }
        for r in 0..n {
            if r == c || a[r][c].is_zero() {
                continue;
            }
            let f = a[r][c].clone();
            for j in c..n {
                let x = a[c][j].mul(&f);
                a[r][j] = a[r][j].sub(&x);
            }
            for j in 0..k {
                let x = b[c][j].mul(&f);
                b[r][j] = b[r][j].sub(&x);
            }
        }
    }
    Some(b)
}

/// Which states can be reached from which (P(i, j) > 0 steps)
fn reach_from(p: &[Vec<Number>], starts: &[usize], avoid: &[bool]) -> Vec<bool> {
    let n = p.len();
    let mut seen = vec![false; n];
    let mut st: Vec<usize> = starts.to_vec();
    for &s in starts {
        seen[s] = true;
    }
    while let Some(u) = st.pop() {
        if avoid[u] {
            continue;
        }
        for v in 0..n {
            if !p[u][v].is_zero() && !seen[v] {
                seen[v] = true;
                st.push(v);
            }
        }
    }
    seen
}

/// Closed communicating classes (no way out), each sorted
fn closed_classes(p: &[Vec<Number>]) -> Vec<Vec<usize>> {
    let n = p.len();
    let none = vec![false; n];
    let reach: Vec<Vec<bool>> = (0..n).map(|i| reach_from(p, &[i], &none)).collect();
    let mut out: Vec<Vec<usize>> = vec![];
    for i in 0..n {
        // i is in a closed class when everything it reaches can reach it back
        if (0..n).all(|j| !reach[i][j] || reach[j][i]) {
            let class: Vec<usize> = (0..n).filter(|&j| reach[i][j]).collect();
            if !out.contains(&class) {
                out.push(class);
            }
        }
    }
    out
}

fn states(v: &[usize]) -> String {
    v.iter().map(|i| (i + 1).to_string()).collect::<Vec<_>>().join(", ")
}

impl Session {
    pub(crate) fn stoch_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        if !NAMES.contains(&f) {
            return None;
        }
        Some(self.stoch(f, args))
    }

    fn num_arg(&mut self, n: &Node, f: &str, what: &str) -> R<f64> {
        let v = self.eval(n)?;
        crate::calc::evalf(&v)
            .num()
            .map(|x| x.to_f64())
            .filter(|x| x.is_finite())
            .ok_or_else(|| format!("{}: {} must be a number, got {}", f, what, show(&v)))
    }

    fn count_arg(&mut self, n: &Node, f: &str, what: &str, hi: i128) -> R<usize> {
        let v = self.eval(n)?;
        let k = crate::extra_cmd::int_of(&v, f)?;
        if k < 1 || k > hi {
            return Err(format!("{}: {} must be a whole number from 1 to {}", f, what, hi));
        }
        Ok(k as usize)
    }

    fn stoch(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        match f {
            "stationary" => {
                let [pn] = args else { return Err("usage: stationary(P) for a transition matrix P".into()) };
                let p = transition(&self.eval(pn)?, f)?;
                let n = p.len();
                let classes = closed_classes(&p);
                let mut rows = vec![];
                for c in &classes {
                    // pi P = pi on the class, sum 1: replace the last equation by the sum
                    let m = c.len();
                    let mut a = vec![vec![Number::zero(); m]; m];
                    let mut b = vec![vec![Number::zero()]; m];
                    for (r, &j) in c.iter().enumerate() {
                        for (k, &i) in c.iter().enumerate() {
                            // equation for state j: sum_i pi_i P(i, j) - pi_j = 0
                            a[r][k] = p[i][j].clone();
                        }
                        a[r][r] = a[r][r].sub(Number::one());
                    }
                    for k in 0..m {
                        a[m - 1][k] = Number::one();
                    }
                    b[m - 1][0] = Number::one();
                    let x = solve(a, b).ok_or("stationary: could not solve for the stationary distribution")?;
                    let mut full = vec![Expr::int(0); n];
                    for (k, &i) in c.iter().enumerate() {
                        full[i] = Expr::Num(x[k][0].clone());
                    }
                    rows.push(full);
                }
                if rows.len() == 1 {
                    let transient: Vec<usize> = (0..n).filter(|i| !classes[0].contains(i)).collect();
                    self.note(format!(
                        "the long-run share of time in each state: pi P = pi with the entries adding up to 1{}",
                        if transient.is_empty() {
                            String::new()
                        } else {
                            format!("; states {} are left for good, so they get 0", states(&transient))
                        }
                    ));
                } else {
                    self.note(format!(
                        "the chain has {} closed classes ({}), so the stationary distribution is not unique: one row for each class, and any mixture of the rows is stationary too",
                        rows.len(),
                        classes.iter().map(|c| format!("{{{}}}", states(c))).collect::<Vec<_>>().join(", ")
                    ));
                }
                Ok(Expr::Matrix(rows))
            }
            "absorption" => {
                let [pn] = args else { return Err("usage: absorption(P) for a transition matrix P with absorbing states (P(i, i) = 1)".into()) };
                let p = transition(&self.eval(pn)?, f)?;
                let n = p.len();
                let abs: Vec<usize> = (0..n).filter(|&i| p[i][i].is_one() || (p[i][i].to_f64() - 1.0).abs() < 1e-12).collect();
                if abs.is_empty() {
                    return Err("absorption: no state is absorbing (an absorbing state i has P(i, i) = 1)".into());
                }
                let tr: Vec<usize> = (0..n).filter(|i| !abs.contains(i)).collect();
                let mut is_abs = vec![false; n];
                for &a in &abs {
                    is_abs[a] = true;
                }
                // every transient state must be able to reach an absorbing one
                let back = (0..n).filter(|&i| !reach_from(&p, &[i], &is_abs).iter().enumerate().any(|(j, &r)| r && is_abs[j])).collect::<Vec<_>>();
                if !back.is_empty() {
                    return Err(format!("absorption: from state {} no absorbing state can be reached", back[0] + 1));
                }
                let m = tr.len();
                let mut a = vec![vec![Number::zero(); m]; m];
                let mut b = vec![vec![Number::zero(); abs.len() + 1]; m];
                for (r, &i) in tr.iter().enumerate() {
                    for (k, &j) in tr.iter().enumerate() {
                        a[r][k] = if i == j { Number::one().sub(&p[i][j]) } else { p[i][j].neg() };
                    }
                    for (k, &j) in abs.iter().enumerate() {
                        b[r][k] = p[i][j].clone();
                    }
                    b[r][abs.len()] = Number::one();
                }
                let x = solve(a, b).ok_or("absorption: (I - Q) is singular")?;
                let mut rows = vec![];
                for i in 0..n {
                    rows.push(if let Some(r) = tr.iter().position(|&t| t == i) {
                        (0..abs.len()).map(|k| Expr::Num(x[r][k].clone())).collect()
                    } else {
                        abs.iter().map(|&a| Expr::int((a == i) as i128)).collect()
                    });
                }
                let steps: Vec<String> = tr.iter().enumerate().map(|(r, &i)| format!("{}: {}", i + 1, show(&Expr::Num(x[r][abs.len()].clone())))).collect();
                self.note(format!(
                    "row i: the probabilities that the chain started in state i ends in absorbing state {} (one column each); B = (I - Q)^-1 R. Expected steps until absorption, from each other state: {}",
                    states(&abs),
                    steps.join(", ")
                ));
                Ok(Expr::Matrix(rows))
            }
            "hitting" => {
                let [pn, tn] = args else { return Err("usage: hitting(P, targets): expected steps to reach one of the target states, from each state".into()) };
                let p = transition(&self.eval(pn)?, f)?;
                let n = p.len();
                let tv = self.eval(tn)?;
                let ts: Vec<Expr> = match &tv {
                    Expr::Matrix(m) => m.iter().flatten().cloned().collect(),
                    Expr::Obj(o) => match &**o {
                        Obj::List(l) => l.clone(),
                        _ => vec![tv.clone()],
                    },
                    e => vec![e.clone()],
                };
                let mut target = vec![false; n];
                for t in &ts {
                    let k = crate::extra_cmd::int_of(t, f)?;
                    if k < 1 || k as usize > n {
                        return Err(format!("hitting: the states are 1 to {}", n));
                    }
                    target[k as usize - 1] = true;
                }
                // states that can never reach a target
                let lost: Vec<bool> = (0..n).map(|i| !target[i] && !reach_from(&p, &[i], &target).iter().enumerate().any(|(j, &r)| r && target[j])).collect();
                let lost_list: Vec<usize> = (0..n).filter(|&i| lost[i]).collect();
                // states that may get lost (reach a lost state before a target): infinite expectation
                let infinite: Vec<bool> = (0..n)
                    .map(|i| !target[i] && reach_from(&p, &[i], &target).iter().enumerate().any(|(j, &r)| r && lost[j]))
                    .collect();
                let free: Vec<usize> = (0..n).filter(|&i| !target[i] && !infinite[i]).collect();
                let m = free.len();
                let mut a = vec![vec![Number::zero(); m]; m];
                let mut b = vec![vec![Number::one()]; m];
                for (r, &i) in free.iter().enumerate() {
                    for (k, &j) in free.iter().enumerate() {
                        a[r][k] = if i == j { Number::one().sub(&p[i][j]) } else { p[i][j].neg() };
                    }
                }
                let x = if m > 0 { solve(a, std::mem::take(&mut b)).ok_or("hitting: could not solve")? } else { vec![] };
                let out: Vec<Expr> = (0..n)
                    .map(|i| {
                        if target[i] {
                            Expr::int(0)
                        } else if infinite[i] {
                            Expr::sym("inf")
                        } else {
                            Expr::Num(x[free.iter().position(|&j| j == i).unwrap()][0].clone())
                        }
                    })
                    .collect();
                self.note(format!(
                    "expected number of steps to first reach {}: h = 0 there and h(i) = 1 + sum_j P(i, j) h(j) elsewhere{}",
                    if ts.len() == 1 { format!("state {}", show(&ts[0])) } else { "one of the target states".to_string() },
                    if lost_list.is_empty() { String::new() } else { "; inf where the chain may never get there".to_string() }
                ));
                Ok(row(out))
            }
            "randchain" => {
                let [pn, sn, kn] = args else { return Err("usage: randchain(P, start, n): n random steps of the Markov chain with transition matrix P".into()) };
                let p = transition(&self.eval(pn)?, f)?;
                let s = self.count_arg(sn, f, "the start state", p.len() as i128)? - 1;
                let k = self.count_arg(kn, f, "the number of steps", 10_000_000)?;
                let pf: Vec<Vec<f64>> = p.iter().map(|r| r.iter().map(|x| x.to_f64()).collect()).collect();
                let mut x = s;
                let mut out = vec![Expr::int(s as i128 + 1)];
                for _ in 0..k {
                    let u = rand_f64();
                    let mut acc = 0.0;
                    let mut next = pf[x].len() - 1;
                    for (j, &q) in pf[x].iter().enumerate() {
                        acc += q;
                        if u < acc {
                            next = j;
                            break;
                        }
                    }
                    // never step to a state with probability 0 (rounding at the end of the row)
                    while pf[x][next] == 0.0 && next > 0 {
                        next -= 1;
                    }
                    x = next;
                    out.push(Expr::int(x as i128 + 1));
                }
                self.note(format!("states visited, starting in {}; seed(k) first makes the run repeatable", s + 1));
                Ok(row(out))
            }
            "randwalk" => {
                let (kn, dn) = match args {
                    [k] => (k, None),
                    [k, d] => (k, Some(d)),
                    _ => return Err("usage: randwalk(n) for n steps of +1 or -1 from 0, randwalk(n, d) for a walk on the d-dimensional grid (one row per position)".into()),
                };
                let k = self.count_arg(kn, f, "the number of steps", 10_000_000)?;
                let d = match dn {
                    Some(d) => self.count_arg(d, f, "the dimension", 3)?,
                    None => 1,
                };
                if d == 1 {
                    let mut x = 0i128;
                    let mut out = vec![Expr::int(0)];
                    for _ in 0..k {
                        x += if rand_f64() < 0.5 { 1 } else { -1 };
                        out.push(Expr::int(x));
                    }
                    self.note(format!("positions after 0, 1, ..., {} steps of +1 or -1 with equal chance; ends at {}", k, x));
                    return Ok(row(out));
                }
                let mut pos = vec![0i128; d];
                let mut rows = vec![pos.iter().map(|&v| Expr::int(v)).collect::<Vec<_>>()];
                for _ in 0..k {
                    let u = rand_f64() * (2 * d) as f64;
                    let axis = (u as usize / 2).min(d - 1);
                    pos[axis] += if (u as usize) % 2 == 0 { 1 } else { -1 };
                    rows.push(pos.iter().map(|&v| Expr::int(v)).collect());
                }
                self.note(format!(
                    "one row per position, [x, y{}]; each step moves one unit along a random axis. plot(column(W, 1), column(W, 2)) draws the path",
                    if d == 3 { ", z" } else { "" }
                ));
                Ok(Expr::Matrix(rows))
            }
            "arrivals" => {
                let [rn, tn] = args else { return Err("usage: arrivals(rate, T): the times of a Poisson process with that rate up to time T".into()) };
                let rate = self.num_arg(rn, f, "the rate")?;
                let t = self.num_arg(tn, f, "the time T")?;
                if rate <= 0.0 || t <= 0.0 {
                    return Err("arrivals: the rate and T must be positive".into());
                }
                if rate * t > 5e6 {
                    return Err("arrivals: rate * T is too large (at most 5000000 arrivals expected)".into());
                }
                let mut s = 0.0;
                let mut out = vec![];
                loop {
                    s += -(1.0 - rand_f64()).ln() / rate;
                    if s > t {
                        break;
                    }
                    out.push(s);
                }
                self.note(format!(
                    "{} arrivals (on average rate * T = {}); the gaps between them are exponential with mean 1/rate",
                    out.len(),
                    crate::num::fmt_float(rate * t)
                ));
                Ok(floats_row(&out))
            }
            "brownian" => {
                let (tn, kn, pn) = match args {
                    [t, k] => (t, k, None),
                    [t, k, p] => (t, k, Some(p)),
                    _ => return Err("usage: brownian(T, n) for a Brownian path at the n + 1 times 0, T/n, ..., T; brownian(T, n, paths) for several, one per row".into()),
                };
                let t = self.num_arg(tn, f, "the time T")?;
                let k = self.count_arg(kn, f, "the number of steps", 10_000_000)?;
                let m = match pn {
                    Some(p) => self.count_arg(p, f, "the number of paths", 100_000)?,
                    None => 1,
                };
                if t <= 0.0 {
                    return Err("brownian: T must be positive".into());
                }
                if k * m > 20_000_000 {
                    return Err("brownian: too many points (steps times paths at most 20000000)".into());
                }
                let sd = (t / k as f64).sqrt();
                let rows: Vec<Vec<Expr>> = (0..m)
                    .map(|_| {
                        let mut w = 0.0;
                        let mut r = vec![Expr::float(0.0)];
                        for _ in 0..k {
                            w += sd * normal();
                            r.push(Expr::float(w));
                        }
                        r
                    })
                    .collect();
                self.note(format!(
                    "W(0) = 0 and independent normal steps with variance T/n = {}; W(T) has mean 0 and variance T. linspace(0, T, n + 1) gives the times",
                    crate::num::fmt_float(t / k as f64)
                ));
                Ok(Expr::Matrix(rows))
            }
            "sde" => {
                let (fnode, gnode, xn, tn, kn, pn) = match args {
                    [a, b, x, t, k] => (a, b, x, t, k, None),
                    [a, b, x, t, k, p] => (a, b, x, t, k, Some(p)),
                    _ => return Err("usage: sde(f, g, x0, T, n): dx = f(x, t) dt + g(x, t) dW from x(0) = x0 to time T in n steps (Euler-Maruyama); sde(f, g, x0, T, n, paths) for several paths, one per row".into()),
                };
                let names = vec!["x".to_string(), "t".to_string()];
                let (fc, _) = self.function(fnode, &names)?;
                let (gc, _) = self.function(gnode, &names)?;
                let x0 = self.num_arg(xn, f, "x0")?;
                let t = self.num_arg(tn, f, "the time T")?;
                let k = self.count_arg(kn, f, "the number of steps", 10_000_000)?;
                let m = match pn {
                    Some(p) => self.count_arg(p, f, "the number of paths", 100_000)?,
                    None => 1,
                };
                if t <= 0.0 {
                    return Err("sde: T must be positive".into());
                }
                if k * m > 20_000_000 {
                    return Err("sde: too many points (steps times paths at most 20000000)".into());
                }
                let dt = t / k as f64;
                let sd = dt.sqrt();
                let mut blew = false;
                let rows: Vec<Vec<Expr>> = (0..m)
                    .map(|_| {
                        let mut x = x0;
                        let mut r = vec![Expr::float(x)];
                        for i in 0..k {
                            let ti = i as f64 * dt;
                            let a = crate::numeric::run(&fc, &[x, ti]);
                            let b = crate::numeric::run(&gc, &[x, ti]);
                            x += a * dt + b * sd * normal();
                            if !x.is_finite() {
                                blew = true;
                            }
                            r.push(Expr::float(x));
                        }
                        r
                    })
                    .collect();
                if blew {
                    return Err("sde: the solution blew up (not a finite number); try more steps or check f and g".into());
                }
                self.note(format!(
                    "Euler-Maruyama: x <- x + f dt + g sqrt(dt) Z with dt = {} and Z standard normal; values at the times linspace(0, T, n + 1){}",
                    crate::num::fmt_float(dt),
                    if m > 1 { ", one path per row" } else { "" }
                ));
                Ok(Expr::Matrix(rows))
            }
            "ising" => {
                let [nn, tn, sn] = args else { return Err("usage: ising(n, T, sweeps): the Ising model on an n by n grid at temperature T (Metropolis, J = 1, k = 1)".into()) };
                let n = self.count_arg(nn, f, "the size n", 400)?;
                let temp = self.num_arg(tn, f, "the temperature T")?;
                let sweeps = self.count_arg(sn, f, "the number of sweeps", 1_000_000)?;
                if temp <= 0.0 {
                    return Err("ising: the temperature must be positive".into());
                }
                if n * n * sweeps > 400_000_000 {
                    return Err("ising: n^2 * sweeps is too large (at most 400000000)".into());
                }
                // start ordered (all up): low temperatures settle quickly
                let mut s = vec![1i8; n * n];
                let at = |i: usize, j: usize| i * n + j;
                let w4 = (-4.0 / temp).exp();
                let w8 = (-8.0 / temp).exp();
                let (mut msum, mut esum, mut count) = (0.0, 0.0, 0usize);
                for sw in 0..sweeps {
                    for _ in 0..n * n {
                        let i = ((rand_f64() * n as f64) as usize).min(n - 1);
                        let j = ((rand_f64() * n as f64) as usize).min(n - 1);
                        let nb = s[at((i + 1) % n, j)] as i32 + s[at((i + n - 1) % n, j)] as i32 + s[at(i, (j + 1) % n)] as i32 + s[at(i, (j + n - 1) % n)] as i32;
                        let de = 2 * s[at(i, j)] as i32 * nb;
                        if de <= 0 || rand_f64() < if de == 4 { w4 } else { w8 } {
                            s[at(i, j)] = -s[at(i, j)];
                        }
                    }
                    // measure over the second half, after settling
                    if sw >= sweeps / 2 {
                        let m: i64 = s.iter().map(|&x| x as i64).sum();
                        let mut e: i64 = 0;
                        for i in 0..n {
                            for j in 0..n {
                                e -= s[at(i, j)] as i64 * (s[at((i + 1) % n, j)] as i64 + s[at(i, (j + 1) % n)] as i64);
                            }
                        }
                        msum += (m as f64).abs() / (n * n) as f64;
                        esum += e as f64 / (n * n) as f64;
                        count += 1;
                    }
                }
                let spins: Vec<Vec<Expr>> = (0..n).map(|i| (0..n).map(|j| Expr::int(s[at(i, j)] as i128)).collect()).collect();
                let tc = 2.0 / (1.0 + 2f64.sqrt()).ln();
                self.note(format!(
                    "m: the average |magnetization| per spin, e: the average energy per spin (both over the last {} sweeps), spins: the final grid of +1 and -1. Onsager's exact critical temperature is 2 / ln(1 + sqrt(2)) = {:.4}: below it the spins line up (m near 1), above it they do not (m near 0)",
                    count, tc
                ));
                Ok(Expr::Obj(Rc::new(Obj::Record(vec![
                    ("m".into(), Expr::float(msum / count as f64)),
                    ("e".into(), Expr::float(esum / count as f64)),
                    ("spins".into(), Expr::Matrix(spins)),
                ]))))
            }
            "bscall" | "bsput" => {
                let [s, k, r, sig, t] = args else {
                    return Err(format!("usage: {}(S, K, r, sigma, T): stock price S, strike K, interest rate r, volatility sigma, years to expiry T", f));
                };
                let s = self.num_arg(s, f, "S")?;
                let k = self.num_arg(k, f, "K")?;
                let r = self.num_arg(r, f, "r")?;
                let sig = self.num_arg(sig, f, "sigma")?;
                let t = self.num_arg(t, f, "T")?;
                if s <= 0.0 || k <= 0.0 || sig <= 0.0 || t <= 0.0 {
                    return Err(format!("{}: S, K, sigma and T must be positive", f));
                }
                let d1 = ((s / k).ln() + (r + sig * sig / 2.0) * t) / (sig * t.sqrt());
                let d2 = d1 - sig * t.sqrt();
                let ncdf = |x: f64| 0.5 * crate::special::erfc(-x / std::f64::consts::SQRT_2);
                let call = s * ncdf(d1) - k * (-r * t).exp() * ncdf(d2);
                let put = k * (-r * t).exp() * ncdf(-d2) - s * ncdf(-d1);
                self.note(format!(
                    "Black-Scholes: d1 = {:.4}, d2 = {:.4}; call = S N(d1) - K e^(-rT) N(d2), put = K e^(-rT) N(-d2) - S N(-d1); call - put = S - K e^(-rT) (put-call parity)",
                    d1, d2
                ));
                Ok(Expr::float(if f == "bscall" { call } else { put }))
            }
            _ => unreachable!(),
        }
    }
}
