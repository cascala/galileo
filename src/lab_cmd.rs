//! Galileo commands for the factoring lab: semiprime, semifactors, labrun,
//! scoreboard, scoreplot, algosearch, leaderboard, leakhunt.

use crate::discover::{Report, with_value};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::extra_cmd::{big_of, bnum};
use crate::factorlab::{self as lab, Clock};
use crate::gnfs;
use crate::lableak;
use crate::labsearch::{self as ls, Prog};
use crate::parser::Node;
use crate::plot::{Figure, Series};
use crate::print::show;
use num_bigint::BigInt;
use num_traits::{One, Signed, Zero};
use std::rc::Rc;

fn report(
    kind: &'static str,
    text: String,
    markdown: String,
    formulas: Vec<Expr>,
    vars: Vec<String>,
) -> Expr {
    Expr::Obj(Rc::new(Obj::Report(Report {
        kind,
        text,
        markdown,
        formulas,
        best: 0,
        vars,
        order: 0,
        target: 0,
        value: None,
    })))
}

/// how much work a second of budget buys (also in the browser, which has no clock)
const UNITS_PER_SECOND: f64 = 3.0e6;

fn fmt_units(u: f64) -> String {
    if u < 1e4 {
        format!("{:.0}", u)
    } else {
        let e = u.log10().floor();
        format!("{:.1}e{}", u / 10f64.powf(e), e)
    }
}

fn fmt_secs(s: Option<f64>) -> String {
    match s {
        Some(s) if s < 0.001 => "<1 ms".into(),
        Some(s) if s < 1.0 => format!("{:.0} ms", s * 1000.0),
        Some(s) => format!("{:.2} s", s),
        None => "-".into(),
    }
}

fn pval(p: f64) -> String {
    if p < 1e-300 {
        "< 1e-300".into()
    } else if p < 0.001 {
        format!("{:.1e}", p)
    } else {
        format!("{:.3}", p)
    }
}

fn median(v: &mut [f64]) -> f64 {
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = v.len();
    if n % 2 == 1 {
        v[n / 2]
    } else {
        (v[n / 2 - 1] + v[n / 2]) / 2.0
    }
}

/// A method: a built-in name, a lab program, or the name of a user function f(n)
enum Method {
    Builtin(String),
    Program(Prog),
    User(String),
}

impl Method {
    fn label(&self) -> String {
        match self {
            Method::Builtin(s) | Method::User(s) => s.clone(),
            Method::Program(p) => p.to_string(),
        }
    }
}

struct Run {
    digits: u32,
    solved: usize,
    trials: usize,
    work: Vec<f64>,
    secs: Vec<f64>,
}

impl Session {
    pub(crate) fn lab_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "semiprime" | "semifactors" => self.semiprime_cmd(f, args),
            "labrun" => self.labrun(args),
            "scoreboard" => self.scoreboard(args),
            "scoreplot" => self.scoreplot(args),
            "algosearch" => self.algosearch(args),
            "leaderboard" => self.leaderboard(args),
            "leakhunt" => self.leakhunt(args),
            "gnfs" | "nfs" => self.gnfs_cmd(args),
            "nfspoly" => self.nfspoly(args),
            _ => return None,
        };
        Some(r)
    }

    fn small_arg(&mut self, n: &Node, f: &str, lo: f64, hi: f64) -> R<f64> {
        let v = self.eval(n)?;
        let x = v
            .num()
            .map(|n| n.to_f64())
            .ok_or_else(|| format!("{}: expected a number, got {}", f, show(&v)))?;
        if !(lo..=hi).contains(&x) {
            return Err(format!("{}: {} is outside {} to {}", f, x, lo, hi));
        }
        Ok(x)
    }

    fn semiprime_cmd(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        if args.is_empty() || args.len() > 2 {
            return Err(format!("usage: {}(digits) or {}(digits, seed)", f, f));
        }
        let d = self.small_arg(&args[0], f, 4.0, 2000.0)? as u32;
        let seed = if args.len() == 2 {
            self.small_arg(&args[1], f, 0.0, 1e15)? as u64
        } else {
            0
        };
        let (n, p, q) = lab::semiprime(d, seed);
        Ok(if f == "semiprime" {
            bnum(n)
        } else {
            crate::factor_cmd::factor_rows(vec![p, q])
        })
    }

    fn method_of(&mut self, v: &Expr) -> R<Method> {
        let Expr::Str(s) = v else {
            return Err(format!(
                "a method is a name in quotes like \"rho\", a program like \"iter(x^2 + 1, x - y, 2)\", or the name of your function f(n) in quotes; got {}",
                show(v)
            ));
        };
        let s = s.trim();
        if ls::is_prog_text(s) {
            return Ok(Method::Program(ls::parse_prog(s)?));
        }
        if lab::METHODS.iter().any(|m| m.0 == s) {
            return Ok(Method::Builtin(s.to_string()));
        }
        match self.funcs.get(s) {
            Some((params, _)) if params.len() == 1 => Ok(Method::User(s.to_string())),
            Some(_) => Err(format!(
                "{} must take one argument, the number to factor",
                s
            )),
            None => Err(format!(
                "unknown method {}: use one of {}, a program iter(f, h, x0) or rel(s, fb), or define f(n) := ... and pass \"f\"",
                s,
                lab::METHODS
                    .iter()
                    .map(|m| m.0)
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }

    /// runs a method on n with a budget; (factor if found, work, seconds)
    fn run_one(
        &mut self,
        m: &Method,
        n: &BigInt,
        seconds: f64,
        seed: u64,
    ) -> (Option<BigInt>, f64, Option<f64>) {
        let clock = Clock::start();
        // with a clock the limit is time; in the browser it is work
        let units = if lab::has_clock() {
            f64::INFINITY
        } else {
            seconds * UNITS_PER_SECOND
        };
        lab::start_budget(units, seconds);
        let f = match m {
            Method::Builtin(name) => lab::run_method(name, n, seed),
            Method::Program(p) => ls::run_prog(p, n),
            Method::User(name) => {
                match self.eval(&Node::Call(name.clone(), vec![Node::Num(n.to_string())])) {
                    Ok(v) => v.num().and_then(|x| x.as_bigint()),
                    Err(_) => None,
                }
            }
        };
        let w = lab::end_budget();
        let f = f
            .map(|f| f.abs())
            .filter(|f| f > &BigInt::one() && f < n && (n % f).is_zero());
        (f, w, clock.seconds())
    }

    fn gnfs_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: gnfs(N), gnfs(N, seconds) or gnfs(N, f, m) or gnfs(N, f, m, seconds) with f a polynomial in x and f(m) divisible by N";
        if args.is_empty() || args.len() > 4 {
            return Err(usage.into());
        }
        let n = big_of(&self.eval(&args[0])?, "gnfs")?;
        if n < BigInt::from(4) {
            return Err("gnfs: N must be 4 or more".into());
        }
        let (choice, secs) = match args.len() {
            1 => (None, 60.0),
            2 => (None, self.small_arg(&args[1], "gnfs", 0.001, 36000.0)?),
            _ => {
                let fe = self.eval(&args[1])?;
                let f = crate::factor_cmd::int_poly(&fe).ok_or_else(|| format!("gnfs: f must be a polynomial in one variable with integer coefficients, got {}", show(&fe)))?;
                let m = big_of(&self.eval(&args[2])?, "gnfs")?;
                let secs = if args.len() == 4 {
                    self.small_arg(&args[3], "gnfs", 0.001, 36000.0)?
                } else {
                    60.0
                };
                (Some(gnfs::user_choice(&n, &f, &m)?), secs)
            }
        };
        let clock = Clock::start();
        let units = if lab::has_clock() {
            f64::INFINITY
        } else {
            secs * UNITS_PER_SECOND
        };
        lab::start_budget(units, secs);
        let run = gnfs::nfs(&n, choice, None);
        let w = lab::end_budget();
        // carry the split on to primes, like factor: the engine of factor
        // first, then the number field sieve again on what it leaves
        let (value, more) = crate::factor_cmd::complete_split(&n, run.factor.as_ref(), &mut |c| {
            lab::start_budget(units, secs);
            let f = gnfs::nfs(c, None, None).factor;
            lab::end_budget();
            f
        });
        let head = match &run.factor {
            Some(f) => format!(
                "The number field sieve split {} = {} * {} in {} work units ({}).",
                n,
                f,
                &n / f,
                fmt_units(w),
                fmt_secs(clock.seconds())
            ),
            None => format!(
                "The number field sieve found no factor of {} in {} work units ({}).",
                n,
                fmt_units(w),
                fmt_secs(clock.seconds())
            ),
        };
        let (text, md) = if self.teaching() {
            let md = crate::teach::gnfs(&n, &run, &format!("{} work units", fmt_units(w)), &fmt_secs(clock.seconds()));
            (crate::teach::plain(&md), md)
        } else {
            (
                format!("{}\n{}", head, run.log.join("\n")),
                format!("{}\n\n```\n{}\n```", head, run.log.join("\n")),
            )
        };
        let (text, md) = if more.is_empty() {
            (text, md)
        } else {
            (format!("{}\n{}", text, more), format!("{}\n\n{}", md, more.replace('\n', "\n\n")))
        };
        let formulas: Vec<Expr> = run.factor.iter().map(|f| bnum(f.clone())).collect();
        Ok(with_value(report("NFS", text, md, formulas, vec![]), value))
    }

    fn nfspoly(&mut self, args: &[Node]) -> R<Expr> {
        if args.is_empty() || args.len() > 2 {
            return Err("usage: nfspoly(N) or nfspoly(N, degree)".into());
        }
        let n = big_of(&self.eval(&args[0])?, "nfspoly")?;
        if n < BigInt::from(1000) {
            return Err("nfspoly: N must be 1000 or more".into());
        }
        let list = if args.len() == 2 {
            let d = self.small_arg(&args[1], "nfspoly", 2.0, 8.0)? as usize;
            gnfs::base_m_search(&n, d, 40)
                .into_iter()
                .take(5)
                .collect::<Vec<_>>()
        } else {
            gnfs::choices(&n, 40)
        };
        let mut md = vec![
            "| score | norm bits | alpha | f(x) | m | how |".to_string(),
            "|---|---|---|---|---|---|".into(),
        ];
        let mut tx = vec![];
        let mut formulas = vec![];
        for c in &list {
            let fs = crate::algfactor::show_zpoly(&c.f, "x");
            md.push(format!(
                "| {:.1} | {:.1} | {:.2} | ${}$ | {} | {} |",
                c.score(),
                c.norm_bits,
                c.alpha,
                fs,
                c.m,
                c.how
            ));
            tx.push(format!(
                "{:>6.1}  f(x) = {}, m = {}  [{}]",
                c.score(),
                fs,
                c.m,
                c.how
            ));
            formulas.push(Expr::Matrix(vec![vec![
                crate::eval::value_of(&fs),
                bnum(c.m.clone()),
                Expr::float((c.score() * 10.0).round() / 10.0),
            ]]));
        }
        let note = "score = estimated bits of the rational plus algebraic norm over a typical sieve region, plus Murphy's alpha (in bits); lower sieves faster".to_string();
        md.push(String::new());
        md.push(note.clone());
        tx.push(note);
        let value = Expr::Matrix(
            formulas
                .iter()
                .filter_map(|f| match f {
                    Expr::Matrix(m) => m.first().cloned(),
                    _ => None,
                })
                .collect(),
        );
        md.push(String::new());
        md.push("The value has one row per polynomial: f(x), m and the score; `gnfs(N, r(1, 1), r(1, 2))` sieves with the first.".into());
        if self.teaching() {
            let t = crate::teach::nfspoly(&n, &list);
            md.push(String::new());
            md.push(t.clone());
            tx.push(crate::teach::plain(&t));
        }
        Ok(with_value(
            report(
                "NFS polynomials",
                tx.join("\n"),
                md.join("\n"),
                formulas,
                vec![],
            ),
            value,
        ))
    }

    fn labrun(&mut self, args: &[Node]) -> R<Expr> {
        if args.len() < 2 || args.len() > 3 {
            return Err("usage: labrun(method, N) or labrun(method, N, seconds)".into());
        }
        let mv = self.eval(&args[0])?;
        let m = self.method_of(&mv)?;
        let n = big_of(&self.eval(&args[1])?, "labrun")?;
        if n < BigInt::from(4) {
            return Err("labrun: N must be 4 or more".into());
        }
        let secs = if args.len() == 3 {
            self.small_arg(&args[2], "labrun", 0.001, 36000.0)?
        } else {
            5.0
        };
        let (f, w, t) = self.run_one(&m, &n, secs, 1);
        let label = m.label();
        match f {
            Some(f) => {
                let mut text = format!(
                    "{} found {} in {} work units ({})",
                    label,
                    f,
                    fmt_units(w),
                    fmt_secs(t)
                );
                // carry the split on to primes, like factor
                let (value, more) = crate::factor_cmd::complete_split(&n, Some(&f), &mut |c| {
                    self.run_one(&m, c, secs, 1).0
                });
                if !more.is_empty() {
                    text = format!("{}\n{}", text, more);
                }
                Ok(with_value(
                    report("LabRun", text.clone(), text, vec![bnum(f)], vec![]),
                    value,
                ))
            }
            None => {
                let text = format!(
                    "{} found no factor of {} in {} work units ({})",
                    label,
                    n,
                    fmt_units(w),
                    fmt_secs(t)
                );
                Ok(with_value(
                    report("LabRun", text.clone(), text, vec![], vec![]),
                    Expr::Matrix(vec![]),
                ))
            }
        }
    }

    fn scoreboard(&mut self, args: &[Node]) -> R<Expr> {
        if args.is_empty() || args.len() > 4 {
            return Err("usage: scoreboard(method, digits, trials, seconds), e.g. scoreboard([\"rho\" \"qs\"], 12:4:40)".into());
        }
        let mv = self.eval(&args[0])?;
        let items: Vec<Expr> = match mv {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            o => vec![o],
        };
        let methods: Vec<Method> = items.iter().map(|v| self.method_of(v)).collect::<R<_>>()?;
        let digits: Vec<u32> = if args.len() >= 2 {
            let v = self.eval(&args[1])?;
            let xs: Vec<Expr> = match v {
                Expr::Matrix(m) => m.into_iter().flatten().collect(),
                o => vec![o],
            };
            xs.iter()
                .map(|x| {
                    x.num()
                        .and_then(|n| n.as_int())
                        .filter(|d| (6..=300).contains(d))
                        .map(|d| d as u32)
                        .ok_or_else(|| {
                            "scoreboard: digits are whole numbers from 6 to 300".to_string()
                        })
                })
                .collect::<R<_>>()?
        } else {
            (12..=40).step_by(4).collect()
        };
        let trials = if args.len() >= 3 {
            self.small_arg(&args[2], "scoreboard", 1.0, 100.0)? as usize
        } else {
            3
        };
        let secs = if args.len() >= 4 {
            self.small_arg(&args[3], "scoreboard", 0.001, 3600.0)?
        } else {
            1.0
        };
        let timed = lab::has_clock();
        let mut text = String::new();
        let mut md = String::new();
        let mut formulas = vec![];
        let mut names = vec![];
        md.push_str(&format!(
            "**Scoreboard**: balanced semiprimes (p and q of the same size, seeds 0 to {}), at most {} per run.\n\n",
            trials - 1,
            if timed { fmt_secs(Some(secs)) } else { format!("{} work units", fmt_units(secs * UNITS_PER_SECOND)) }
        ));
        md.push_str(
            "| method | digits | solved | median work | median time |\n|---|---|---|---|---|\n",
        );
        let mut fits_md = String::new();
        for m in &methods {
            let label = m.label();
            let mut runs: Vec<Run> = vec![];
            for &d in &digits {
                let mut run = Run {
                    digits: d,
                    solved: 0,
                    trials,
                    work: vec![],
                    secs: vec![],
                };
                for seed in 0..trials as u64 {
                    let (n, _, _) = lab::semiprime(d, seed);
                    let (f, w, t) = self.run_one(m, &n, secs, seed);
                    if f.is_some() {
                        run.solved += 1;
                        run.work.push(w);
                        if let Some(t) = t {
                            run.secs.push(t);
                        }
                    }
                }
                let stop = run.solved == 0;
                runs.push(run);
                if stop {
                    break; // larger numbers will not go better
                }
            }
            let mut rows = vec![];
            let mut pts = vec![];
            for r in &mut runs {
                let mw = if r.work.is_empty() {
                    None
                } else {
                    Some(median(&mut r.work))
                };
                let ms = if r.secs.is_empty() {
                    None
                } else {
                    Some(median(&mut r.secs))
                };
                md.push_str(&format!(
                    "| `{}` | {} | {}/{} | {} | {} |\n",
                    label,
                    r.digits,
                    r.solved,
                    r.trials,
                    mw.map_or("-".into(), fmt_units),
                    if mw.is_some() {
                        fmt_secs(ms)
                    } else {
                        "-".into()
                    }
                ));
                text.push_str(&format!(
                    "{:>24} {:>4} digits  {}/{}  work {:>8}  time {}\n",
                    label,
                    r.digits,
                    r.solved,
                    r.trials,
                    mw.map_or("-".into(), fmt_units),
                    fmt_secs(ms)
                ));
                let cost = if timed { ms } else { mw };
                if 2 * r.solved >= r.trials {
                    if let Some(c) = cost {
                        pts.push((r.digits as f64, c));
                    }
                }
                rows.push(vec![
                    Expr::int(r.digits as i128),
                    Expr::int(r.solved as i128),
                    cost.map_or(Expr::int(0), Expr::float),
                ]);
            }
            formulas.push(Expr::Matrix(rows));
            names.push(label.clone());
            // fits: sizes that cost under 1/1000 of the largest measure fixed overhead, not growth
            let top = pts.iter().map(|p| p.1).fold(0.0, f64::max);
            pts.retain(|p| p.1 >= top / 1000.0);
            let fits: Vec<(usize, (f64, f64, f64))> = (0..3)
                .filter_map(|k| lab::fit(k, &pts).map(|f| (k, f)))
                .collect();
            if pts.len() >= 3 {
                let best = fits
                    .iter()
                    .min_by(|a, b| a.1.2.partial_cmp(&b.1.2).unwrap())
                    .unwrap();
                let a = fits[0].1.1;
                fits_md.push_str(&format!(
                    "- `{}`: cost grows like N^{:.3} (fit on {} sizes); L(1/2) constant {:.2}, L(1/3) constant {:.2}; best fit: {}{}\n",
                    label,
                    a,
                    pts.len(),
                    fits[1].1 .1,
                    fits[2].1 .1,
                    lab::MODELS[best.0],
                    if pts.len() < 5 { " (few sizes: the models are hard to tell apart)" } else { "" }
                ));
            } else {
                fits_md.push_str(&format!(
                    "- `{}`: solved too few sizes to fit a curve\n",
                    label
                ));
            }
        }
        md.push_str("\n**How the cost grows** (");
        md.push_str(if timed { "time" } else { "work units" });
        md.push_str("; for reference: trial division N^0.5, Hart N^0.33, rho and SQUFOF N^0.25, quadratic sieve L(1/2) with constant 1, number field sieve L(1/3) with constant 1.92):\n\n");
        md.push_str(&fits_md);
        text.push('\n');
        text.push_str(&fits_md.replace('`', ""));
        // the value: one row per method and size: method, digits, solved, trials, cost
        let mut value = vec![];
        for (name, f) in names.iter().zip(&formulas) {
            if let Expr::Matrix(rows) = f {
                for r in rows {
                    let mut row = vec![Expr::Str(name.clone())];
                    row.push(r[0].clone());
                    row.push(r[1].clone());
                    row.push(Expr::int(trials as i128));
                    row.push(r[2].clone());
                    value.push(row);
                }
            }
        }
        md.push_str("\nThe value has one row per method and size: method, digits, solved, trials, cost (seconds, or work units in the browser).");
        Ok(with_value(
            report("Scoreboard", text, md, formulas, names),
            Expr::Matrix(value),
        ))
    }

    fn scoreplot(&mut self, args: &[Node]) -> R<Expr> {
        if args.is_empty() {
            return Err(
                "usage: scoreplot(s) or scoreplot(s1, s2, ...) with results of scoreboard".into(),
            );
        }
        let mut series: Vec<Series> = vec![];
        let mut first: Option<(f64, f64)> = None;
        let mut dmax: f64 = 0.0;
        let (mut ymin, mut ymax) = (f64::INFINITY, f64::NEG_INFINITY);
        let unit = if lab::has_clock() {
            "seconds"
        } else {
            "work units"
        };
        for a in args {
            let v = self.eval(a)?;
            let Expr::Obj(o) = &v else {
                return Err("scoreplot takes results of scoreboard".into());
            };
            let Obj::Report(r) = &**o else {
                return Err("scoreplot takes results of scoreboard".into());
            };
            if r.kind != "Scoreboard" {
                return Err("scoreplot takes results of scoreboard".into());
            }
            for (name, data) in r.vars.iter().zip(&r.formulas) {
                let Expr::Matrix(rows) = data else { continue };
                let mut pts = vec![];
                for row in rows {
                    let d = row[0].num().map(|n| n.to_f64()).unwrap_or(0.0);
                    let c = row[2].num().map(|n| n.to_f64()).unwrap_or(0.0);
                    if c > 0.0 {
                        pts.push((d, c.log10()));
                        dmax = dmax.max(d);
                    }
                }
                if pts.is_empty() {
                    continue;
                }
                if first.is_none() {
                    first = Some(pts[0]);
                }
                ymin = ymin.min(pts.iter().map(|p| p.1).fold(f64::INFINITY, f64::min));
                ymax = ymax.max(pts.iter().map(|p| p.1).fold(f64::NEG_INFINITY, f64::max));
                series.push(Series {
                    label: name.clone(),
                    points: pts,
                    markers: true,
                    errors: vec![],
                    ..Default::default()
                });
            }
        }
        // reference curves through the first point (the fitted laws are in the scoreboard table)
        if let Some((d0, l0)) = first {
            let d1 = dmax.max(d0 + 1.0);
            let refs: [(&str, Box<dyn Fn(f64) -> f64>); 4] = [
                ("N^(1/2) (trial division)", Box::new(|d: f64| 0.5 * d)),
                ("N^(1/4) (rho)", Box::new(|d: f64| 0.25 * d)),
                (
                    "L(1/2, 1) (quadratic sieve)",
                    Box::new(|d: f64| lab::model_x(1, d) / std::f64::consts::LN_10),
                ),
                (
                    "L(1/3, 1.92) (number field sieve)",
                    Box::new(|d: f64| 1.923 * lab::model_x(2, d) / std::f64::consts::LN_10),
                ),
            ];
            for (name, g) in refs.iter() {
                // only the part inside the range of the data, so the data stays readable
                let pts: Vec<(f64, f64)> = crate::plot::steps(d0, d1, 80)
                    .into_iter()
                    .map(|d| (d, l0 + g(d) - g(d0)))
                    .filter(|p| p.1 <= ymax + 1.0 && p.1 >= ymin - 1.0)
                    .collect();
                if pts.len() < 2 {
                    continue;
                }
                series.push(Series {
                    label: format!("reference {}", name),
                    points: pts,
                    markers: false,
                    errors: vec![],
                    ..Default::default()
                });
            }
        }
        if series.is_empty() {
            return Err("scoreplot: nothing was solved, so there is nothing to plot".into());
        }
        let mut fig = Figure::new(series, "digits of N", &format!("log10 {}", unit), false);
        fig.title = "Factoring scoreboard".into();
        Ok(Expr::Obj(Rc::new(Obj::Figure(fig))))
    }

    fn algosearch(&mut self, args: &[Node]) -> R<Expr> {
        if args.len() > 4 {
            return Err("usage: algosearch(seconds, mode, seed, digits) with mode \"evolve\", \"random\" or \"exhaustive\" and digits the training sizes, e.g. [24 28 32]".into());
        }
        let secs = if !args.is_empty() {
            self.small_arg(&args[0], "algosearch", 0.1, 36000.0)?
        } else {
            5.0
        };
        let mode = if args.len() >= 2 {
            match self.eval(&args[1])? {
                Expr::Str(s) if ["evolve", "random", "exhaustive"].contains(&s.as_str()) => s,
                o => {
                    return Err(format!(
                        "algosearch: mode is \"evolve\", \"random\" or \"exhaustive\", got {}",
                        show(&o)
                    ));
                }
            }
        } else {
            "evolve".to_string()
        };
        let seed = if args.len() >= 3 {
            self.small_arg(&args[2], "algosearch", 0.0, 1e15)? as u64
        } else {
            1
        };
        let sizes: Vec<u32> = if args.len() == 4 {
            let v = self.eval(&args[3])?;
            let xs: Vec<Expr> = match v {
                Expr::Matrix(m) => m.into_iter().flatten().collect(),
                o => vec![o],
            };
            xs.iter()
                .map(|x| {
                    x.num()
                        .and_then(|n| n.as_int())
                        .filter(|d| (10..=60).contains(d))
                        .map(|d| d as u32)
                        .ok_or_else(|| {
                            "algosearch: training digits are whole numbers from 10 to 60"
                                .to_string()
                        })
                })
                .collect::<R<_>>()?
        } else {
            ls::TRAIN.to_vec()
        };
        let r = ls::search_with(&mode, secs, seed, &sizes);
        let rho = &r.classics[0].1;
        let mut md = format!(
            "**Algorithm search** ({}, seed {}): {} programs scored, {} distinct candidates generated, {}.\n\n\
             Training set: {} balanced semiprimes of {} digits; work is counted in multiplications mod N; a miss costs 8 times the cap. \
             The best programs are then re-run on {} numbers the search never saw ({} digits), next to Pollard rho.\n\n",
            mode,
            seed,
            r.evaluated,
            r.distinct,
            match r.seconds {
                Some(s) => format!("{:.1} s", s),
                None => format!("{} work units", fmt_units(r.units)),
            },
            r.train_count,
            r.train_sizes
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            r.test_count,
            r.test_sizes
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        );
        if r.seeded {
            md.push_str("On numbers this big random programs almost never solve anything, so the first population also holds the classics.\n\n");
        }
        md.push_str("| program | solved (training) | work vs rho (training) | solved (held out) | work vs rho (held out) | p-value |\n|---|---|---|---|---|---|\n");
        let mut text = String::new();
        let mut formulas = vec![];
        let row = |name: &str, e: &ls::Entry, md: &mut String, text: &mut String| {
            let train = ls::speedup(&e.score, &rho.score);
            let (held, held_solved, p) = match &e.test {
                Some(t) => (
                    format!("{:.2}x less", ls::speedup(t, &r.rho_test)),
                    format!("{}/{}", t.solved, t.total),
                    pval(ls::paired(t, &r.rho_test).1),
                ),
                None => ("-".into(), "-".into(), "-".into()),
            };
            md.push_str(&format!(
                "| {}`{}` | {}/{} | {:.2}x less | {} | {} | {} |\n",
                name, e.prog, e.score.solved, e.score.total, train, held_solved, held, p
            ));
            text.push_str(&format!(
                "{:<60} train {}/{} {:.2}x  held out {} {} p={}\n",
                e.prog.to_string(),
                e.score.solved,
                e.score.total,
                train,
                held_solved,
                held,
                p
            ));
        };
        for e in r.best.iter().take(8) {
            row("", e, &mut md, &mut text);
            formulas.push(Expr::Str(e.prog.to_string()));
        }
        md.push_str("\nFor comparison, the classics on the same training set:\n\n| classic | program | solved | work vs rho |\n|---|---|---|---|\n");
        for (name, e) in &r.classics {
            md.push_str(&format!(
                "| {} | `{}` | {}/{} | {:.2}x less |\n",
                name,
                e.prog,
                e.score.solved,
                e.score.total,
                ls::speedup(&e.score, &rho.score)
            ));
        }
        let best_held = r
            .best
            .iter()
            .filter_map(|e| {
                e.test
                    .as_ref()
                    .map(|t| (e, ls::speedup(t, &r.rho_test), ls::paired(t, &r.rho_test).1))
            })
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
        md.push_str("\n**Verdict**: ");
        md.push_str(&match best_held {
            Some((e, s, p)) if s > 1.0 && p * 5.0 < 0.05 => format!("`{}` needs {:.2}x less work than rho on held-out numbers (p = {}, still significant after testing 5 programs). Check it at larger sizes with scoreboard before believing it.", e.prog, s, pval(p)),
            Some((e, s, p)) => format!(
                "nothing beats Pollard rho on numbers the search did not train on: the best held-out ratio is {:.2}x (`{}`, p = {}, and 5 programs were tested, so a p-value above 0.01 is what chance gives). Gains on the training set are mostly luck: rho's cost varies a lot from number to number, and a search that tries thousands of variants finds some that were lucky on those {} numbers.",
                s,
                e.prog,
                pval(p),
                rho.score.total
            ),
            None => "no program solved anything; try a longer search.".into(),
        });
        md.push_str("\n\nr[k] is program k as text: scoreboard(r[0], 12:2:22) runs it on bigger numbers. leaderboard() keeps the best programs of all searches in this session.");
        Ok(report("Leaderboard", text, md, formulas, vec![]))
    }

    fn leaderboard(&mut self, args: &[Node]) -> R<Expr> {
        if !args.is_empty() {
            return Err("leaderboard() takes no arguments".into());
        }
        let b = ls::board();
        if b.is_empty() {
            return Err("leaderboard: run algosearch first".into());
        }
        let mut md = "**Leaderboard** of this session (checked programs first, by how much less work than rho they need on numbers no search trained on)\n\n| # | program | training digits | training score | held out vs rho | p-value | found by |\n|---|---|---|---|---|---|---|\n".to_string();
        let mut text = String::new();
        let mut formulas = vec![];
        for (k, e) in b.iter().enumerate().take(15) {
            let (held, p) = match e.held {
                Some((x, p)) => (format!("{:.2}x less work", x), pval(p)),
                None => ("-".into(), "-".into()),
            };
            md.push_str(&format!(
                "| {} | `{}` | {} | {:.1} | {} | {} | {} |\n",
                k, e.prog, e.sizes, e.score.score, held, p, e.found_by
            ));
            text.push_str(&format!(
                "{:>2} {:<60} {:.1} {} p={}\n",
                k,
                e.prog.to_string(),
                e.score.score,
                held,
                p
            ));
            formulas.push(Expr::Str(e.prog.to_string()));
        }
        Ok(report("Leaderboard", text, md, formulas, vec![]))
    }

    fn leakhunt(&mut self, args: &[Node]) -> R<Expr> {
        if args.len() > 3 {
            return Err("usage: leakhunt(digits, samples, seed)".into());
        }
        let digits = if !args.is_empty() {
            self.small_arg(&args[0], "leakhunt", 5.0, 36.0)? as u32
        } else {
            16
        };
        let samples = if args.len() >= 2 {
            self.small_arg(&args[1], "leakhunt", 100.0, 1e6)? as usize
        } else {
            4000
        };
        let seed = if args.len() == 3 {
            self.small_arg(&args[2], "leakhunt", 0.0, 1e15)? as u64
        } else {
            1
        };
        lab::start_budget(1e12, f64::INFINITY);
        let h = lableak::hunt(digits, samples, seed);
        lab::end_budget();
        let h = h?;
        let mut md = format!(
            "**Leakage hunt**: {} balanced semiprimes N = p q of {} digits (p the smaller prime). {} features of N against {} properties of p: {} tests on the first half, the best per property re-tested on the second half. ",
            h.samples,
            h.digits,
            h.features.len(),
            lableak::TARGETS.len(),
            h.tests
        );
        md.push_str("Properties marked *control* are known to depend on N and show that the test can see a leak.\n\n");
        md.push_str("| property of p | best feature of N | p-value | corrected (BH) | held-out p-value | naive Bayes accuracy (chance) | p-value |\n|---|---|---|---|---|---|---|\n");
        let mut text = String::new();
        for (t, (name, control)) in lableak::TARGETS.iter().enumerate() {
            let hit = h.hits.iter().find(|x| x.target == t).unwrap();
            let pr = &h.predict[t];
            let tag = if *control { " (control)" } else { "" };
            md.push_str(&format!(
                "| {}{} | {} | {} | {} | {} | {:.1}% ({:.1}%) | {} |\n",
                name,
                tag,
                h.features[hit.feature].name,
                pval(hit.p_train),
                pval(hit.q_bh),
                pval(hit.p_test),
                100.0 * pr.accuracy,
                100.0 * pr.baseline,
                pval(pr.p)
            ));
            text.push_str(&format!(
                "{:<40} {:<45} p={} q={} held-out p={} NB {:.1}% vs {:.1}% p={}\n",
                format!("{}{}", name, tag),
                h.features[hit.feature].name,
                pval(hit.p_train),
                pval(hit.q_bh),
                pval(hit.p_test),
                100.0 * pr.accuracy,
                100.0 * pr.baseline,
                pval(pr.p)
            ));
        }
        if h.digits < 10 {
            md.push_str(&format!(
                "\n**Small numbers**: with {} digits there are only a few hundred primes of the size of p, so N almost pins p down and nearly every feature of N says something about it (its leading digits say how big p is, and in a short range of primes, size and residues are not independent). Leaks found here do not carry over to large N; use 10 digits or more for real conclusions.\n",
                h.digits
            ));
        }
        let extra: Vec<&lableak::Hit> = h
            .hits
            .iter()
            .filter(|x| x.q_bh < 0.05 && !lableak::TARGETS[x.target].1)
            .collect();
        md.push_str("\n**Verdict**: ");
        let real: Vec<&&lableak::Hit> = extra.iter().filter(|x| x.p_test < 0.01).collect();
        let nb: Vec<usize> = (0..lableak::TARGETS.len())
            .filter(|&t| {
                !lableak::TARGETS[t].1 && h.predict[t].p < 0.05 / lableak::TARGETS.len() as f64
            })
            .collect();
        if real.is_empty() && nb.is_empty() {
            md.push_str(&format!(
                "no feature predicts any property of p alone better than chance after correcting for {} tests, and combining all features does not either. The controls do show up, so the test would have seen a leak of that strength.",
                h.tests
            ));
        } else {
            for x in &real {
                let f = &h.features[x.feature];
                md.push_str(&format!(
                    "{} depends on {} (corrected p = {}, held-out p = {}){}. ",
                    lableak::TARGETS[x.target].0,
                    f.name,
                    pval(x.q_bh),
                    pval(x.p_test),
                    if f.costly() { ", but computing this feature takes about sqrt(N) steps, more than factoring N with rho" } else { "" }
                ));
            }
            for t in &nb {
                md.push_str(&format!("All features together predict {} at {:.1}% against {:.1}% by chance (p = {}). ", lableak::TARGETS[*t].0, 100.0 * h.predict[*t].accuracy, 100.0 * h.predict[*t].baseline, pval(h.predict[*t].p)));
            }
            md.push_str("Before calling this a leak, check whether it only reflects the size of N (p is the smaller factor; with few digits, p ranges over a short interval and its bits depend on its size) and repeat with another seed and more digits.");
        }
        // the value: the significant (feature, property) pairs with corrected and held-out p-values
        let value: Vec<Vec<Expr>> = h
            .hits
            .iter()
            .filter(|x| x.q_bh < 0.05)
            .map(|x| {
                vec![
                    Expr::Str(h.features[x.feature].name.clone()),
                    Expr::Str(lableak::TARGETS[x.target].0.to_string()),
                    Expr::float(x.q_bh),
                    Expr::float(x.p_test),
                ]
            })
            .collect();
        md.push_str("\n\nThe value lists the significant pairs: feature, property of p, corrected p-value, held-out p-value.");
        Ok(with_value(
            report("LeakHunt", text, md, vec![], vec![]),
            Expr::Matrix(value),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::Session;

    #[test]
    fn lab_commands() {
        let mut s = Session::new();
        let out = s.run_to_string("N = semiprime(20, 1); f = semifactors(20, 1); f(1, 1)*f(2, 1) == N");
        assert!(out.trim().ends_with("true"), "{}", out);
        let out = s.run_to_string("labrun(\"rho\", semiprime(20, 3))");
        assert!(out.contains("found"), "{}", out);
        let out = s.run_to_string("myf(n) := pollard(n); sb = scoreboard([\"rho\" \"myf\" \"iter(x^2 + 3, x - y, 2)\"], 10:2:14, 2, 1)");
        assert!(
            out.contains("rho") && out.contains("myf") && !out.contains("rror:"),
            "{}",
            out
        );
        let out = s.run_to_string("scoreplot(sb)");
        assert!(!out.contains("rror:"), "{}", out);
        let out = s.run_to_string("gnfs(semiprime(22, 5))");
        assert!(out.contains("gcd(X - Y, N)"), "{}", out);
        let out = s.run_to_string("nfspoly(2^128 + 1)");
        assert!(out.contains("special form"), "{}", out);
        let out = s.run_to_string("gnfs(2^128 + 1, x^3 + 2, 2^43)");
        assert!(out.contains("gcd(X - Y, N)"), "{}", out);
        let out = s.run_to_string("gnfs(2^78 + 1, x^4 + 4, 2^20)");
        assert!(out.contains("factors over the integers"), "{}", out);
        let out = s.run_to_string("r = algosearch(3); leaderboard()");
        assert!(
            out.contains("iter(") || out.contains("rel(") || out.contains("grp("),
            "{}",
            out
        );
        let out = s.run_to_string("leakhunt(10, 400)");
        assert!(out.contains("control"), "{}", out);
    }

    /// Factoring methods give every prime, like factor, not just one split
    #[test]
    fn methods_factor_completely() {
        let mut s = Session::new();
        s.run_to_string("explain = false; N = semiprime(15) * semiprime(13); M = semiprime(12, 1) * semiprime(10, 2) * 7^2");
        let want = s.run_to_string("factor(N)");
        assert_eq!(want.matches(", 1").count(), 4, "{}", want);
        for cmd in ["gnfs(N)", "labrun(\"rho\", N)", "ecm(N)", "squfof(N)", "classfactor(N)"] {
            assert_eq!(s.run_to_string(cmd), want, "{}", cmd);
        }
        let want = s.run_to_string("factor(M)");
        for cmd in ["pm1(M, 10^6)", "ecm(M)", "gnfs(M)"] {
            let got = s.run_to_string(cmd);
            assert!(got == want || got == "[]", "{}: {} vs {}", cmd, got, want);
        }
    }
}
