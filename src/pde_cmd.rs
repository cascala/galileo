//! Partial differential equations by finite differences: the heat equation
//! (Crank–Nicolson), the wave equation (leapfrog), the Laplace and Poisson
//! equations on a rectangle (successive over-relaxation), and pdeplot to show
//! the results as an animation, a set of snapshots, a color map or a surface.

use crate::commands::{ident, obj};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::mv_cmd::{auto_levels, march};
use crate::numeric::{Code, compile, run};
use crate::parser::Node;
use crate::plot::{Figure, Figure3D, Frame, Raster, Series, Surface};
use crate::print::show;
use std::rc::Rc;

const NAMES: &[&str] = &["heat", "wave", "laplace", "poisson", "pdeplot"];

fn floats_row(v: &[f64]) -> Expr {
    Expr::Matrix(vec![v.iter().map(|&x| Expr::float(x)).collect()])
}

fn floats_matrix(m: &[Vec<f64>]) -> Expr {
    Expr::Matrix(m.iter().map(|r| r.iter().map(|&x| Expr::float(x)).collect()).collect())
}

fn record(v: Vec<(&str, Expr)>) -> Expr {
    Expr::Obj(Rc::new(Obj::Record(v.into_iter().map(|(k, x)| (k.to_string(), x)).collect())))
}

fn fmt(v: f64) -> String {
    let r = (v * 1e4).round() / 1e4;
    if r == r.trunc() && r.abs() < 1e15 {
        format!("{}", r as i64)
    } else {
        format!("{}", r)
    }
}

/// A boundary condition at one end of an interval
enum End {
    /// u = g(t)
    Fixed(Code),
    /// no flux: du/dx = 0
    Flux,
}

/// Solve a tridiagonal system (sub, diag, sup) x = r in place (Thomas)
fn thomas(sub: &[f64], diag: &[f64], sup: &[f64], r: &mut [f64]) {
    let n = diag.len();
    let mut c = vec![0.0; n];
    let mut d = diag[0];
    c[0] = sup[0] / d;
    r[0] /= d;
    for i in 1..n {
        d = diag[i] - sub[i] * c[i - 1];
        c[i] = sup[i] / d;
        r[i] = (r[i] - sub[i] * r[i - 1]) / d;
    }
    for i in (0..n - 1).rev() {
        r[i] -= c[i] * r[i + 1];
    }
}

/// Numbers out of a row or matrix
fn grid_of(e: &Expr) -> Option<Vec<Vec<f64>>> {
    let Expr::Matrix(m) = e else { return None };
    m.iter()
        .map(|r| {
            r.iter()
                .map(|x| x.num().or_else(|| crate::calc::evalf(x).num()).map(|n| n.to_f64()))
                .collect::<Option<Vec<f64>>>()
        })
        .collect()
}

/// Blue to red through light grey, as RGB
fn ramp_rgb(t: f64) -> [u8; 3] {
    let stops = [
        (59.0, 76.0, 192.0),
        (124.0, 159.0, 249.0),
        (221.0, 221.0, 221.0),
        (238.0, 132.0, 104.0),
        (180.0, 4.0, 38.0),
    ];
    let t = if t.is_finite() { t.clamp(0.0, 1.0) } else { 0.5 } * (stops.len() - 1) as f64;
    let i = (t.floor() as usize).min(stops.len() - 2);
    let u = t - i as f64;
    let (a, b) = (stops[i], stops[i + 1]);
    let c = |p: f64, q: f64| (p + (q - p) * u).round() as u8;
    [c(a.0, b.0), c(a.1, b.1), c(a.2, b.2)]
}

impl Session {
    pub(crate) fn pde_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        // laplace with 1 or 3 arguments is the Laplace transform
        if !NAMES.contains(&f) || (f == "laplace" && args.len() < 7) {
            return None;
        }
        Some(match f {
            "heat" => self.heat(args),
            "wave" => self.wave(args),
            "laplace" => self.laplace(args, false),
            "poisson" => self.laplace(args, true),
            _ => self.pdeplot(args),
        })
    }

    fn pnum(&mut self, n: &Node, f: &str, what: &str) -> R<f64> {
        let v = self.eval(n)?;
        crate::calc::evalf(&v)
            .num()
            .map(|x| x.to_f64())
            .filter(|x| x.is_finite())
            .ok_or_else(|| format!("{}: {} must be a number, got {}", f, what, show(&v)))
    }

    /// The options record: a literal {k: 0.1, left: "insulated"} (its values may
    /// use the variables in `vars`), or a name holding a record
    fn pde_opts(&mut self, n: Option<&Node>, vars: &[String], f: &str, known: &[&str]) -> R<Vec<(String, Expr)>> {
        let Some(n) = n else { return Ok(vec![]) };
        let fields = match n {
            Node::Record(fs) => {
                let mut out = vec![];
                for (k, v) in fs {
                    out.push((k.clone(), self.eval_hidden(v, vars)?));
                }
                out
            }
            _ => match self.eval(n)? {
                Expr::Obj(o) => match &*o {
                    Obj::Record(v) => v.clone(),
                    _ => return Err(format!("{}: the options are a record such as {{{}: ...}}", f, known[0])),
                },
                v => return Err(format!("{}: the options are a record such as {{{}: ...}}, got {}", f, known[0], show(&v))),
            },
        };
        for (k, _) in &fields {
            if !known.contains(&k.as_str()) {
                return Err(format!("{}: unknown option {}; the options are {}", f, k, known.join(", ")));
            }
        }
        Ok(fields)
    }

    fn opt_num(opts: &[(String, Expr)], key: &str, f: &str, default: f64) -> R<f64> {
        match opts.iter().find(|(k, _)| k == key) {
            None => Ok(default),
            Some((_, v)) => crate::calc::evalf(v)
                .num()
                .map(|x| x.to_f64())
                .filter(|x| x.is_finite())
                .ok_or_else(|| format!("{}: {} must be a number, got {}", f, key, show(v))),
        }
    }

    fn opt_count(opts: &[(String, Expr)], key: &str, f: &str, default: usize, lo: usize, hi: usize) -> R<usize> {
        let v = Self::opt_num(opts, key, f, default as f64)?;
        if v.fract() != 0.0 || v < lo as f64 || v > hi as f64 {
            return Err(format!("{}: {} must be a whole number from {} to {}", f, key, lo, hi));
        }
        Ok(v as usize)
    }

    /// An expression option in the given variables (missing: the default)
    fn opt_fn(opts: &[(String, Expr)], key: &str, vars: &[&str], f: &str) -> R<Option<Code>> {
        let Some((_, v)) = opts.iter().find(|(k, _)| k == key) else { return Ok(None) };
        if !v.is_scalar() {
            return Err(format!("{}: {} must be a number or an expression in {}", f, key, vars.join(" and ")));
        }
        let stray: Vec<String> = v.symbols().into_iter().filter(|s| !vars.contains(&s.as_str()) && s != "pi" && s != "e").collect();
        if !stray.is_empty() {
            return Err(format!("{}: {} may only use {}; give {} a value first", f, key, vars.join(" and "), stray.join(", ")));
        }
        Ok(Some(compile(v, vars)))
    }

    fn opt_end(opts: &[(String, Expr)], key: &str, t: &str, f: &str, flux: &str) -> R<End> {
        if let Some((_, Expr::Str(s))) = opts.iter().find(|(k, _)| k == key) {
            if s == flux || s == "insulated" || s == "free" || s == "neumann" {
                return Ok(End::Flux);
            }
            return Err(format!("{}: {} is a value, an expression in {}, or \"{}\"", f, key, t, flux));
        }
        Ok(End::Fixed(Self::opt_fn(opts, key, &[t], f)?.unwrap_or_else(|| compile(&Expr::int(0), &[t]))))
    }

    /// heat(f, x, a, b, T[, options]): u_t = k u_xx (+ source) on [a, b] for
    /// 0 <= t <= T with u(x, 0) = f, by Crank–Nicolson
    fn heat(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: heat(f, x, a, b, T) solves u_t = u_xx on a <= x <= b up to time T, starting from u = f(x) with u = 0 at both ends; heat(f, x, a, b, T, {k: 0.1, left: 1, right: \"insulated\", source: ..., n: 100, frames: 50}) sets the diffusivity, the ends (a value, an expression in t, or \"insulated\"), a heat source in x and t, the grid and the number of snapshots";
        let (fnode, xn, an, bn, tn, on) = match args {
            [f, x, a, b, t] => (f, x, a, b, t, None),
            [f, x, a, b, t, o] => (f, x, a, b, t, Some(o)),
            _ => return Err(usage.into()),
        };
        let x = ident(xn, "x")?;
        let (a, b) = (self.pnum(an, "heat", "a")?, self.pnum(bn, "heat", "b")?);
        if a >= b {
            return Err("heat: need a < b".into());
        }
        let tmax = self.pnum(tn, "heat", "the time T")?;
        if tmax <= 0.0 {
            return Err("heat: the time T must be positive".into());
        }
        let vars = vec![x.clone(), "t".to_string()];
        let opts = self.pde_opts(on, &vars, "heat", &["k", "left", "right", "source", "n", "frames"])?;
        let k = Self::opt_num(&opts, "k", "heat", 1.0)?;
        if k <= 0.0 {
            return Err("heat: the diffusivity k must be positive".into());
        }
        let n = Self::opt_count(&opts, "n", "heat", 100, 4, 2000)?;
        let frames = Self::opt_count(&opts, "frames", "heat", 50, 1, 1000)?;
        let left = Self::opt_end(&opts, "left", "t", "heat", "insulated")?;
        let right = Self::opt_end(&opts, "right", "t", "heat", "insulated")?;
        let source = Self::opt_fn(&opts, "source", &[x.as_str(), "t"], "heat")?;
        let (fc, _) = self.function(fnode, std::slice::from_ref(&x))?;

        let h = (b - a) / n as f64;
        let xs: Vec<f64> = (0..=n).map(|i| a + i as f64 * h).collect();
        let frame_dt = tmax / frames as f64;
        // time steps with k dt / h^2 <= 1 keep Crank–Nicolson free of wiggles
        let per = ((frame_dt * k / (h * h)).ceil() as usize).max(1);
        if per * frames > 2_000_000 {
            return Err("heat: too many time steps; use a coarser grid (n) or a shorter time".into());
        }
        let dt = frame_dt / per as f64;
        let r = k * dt / (h * h);
        let src = |xv: f64, tv: f64| source.as_ref().map_or(0.0, |c| run(c, &[xv, tv]));
        let mut u: Vec<f64> = xs.iter().map(|&xv| run(&fc, &[xv])).collect();
        if u.iter().any(|v| !v.is_finite()) {
            return Err("heat: the starting values f(x) are not all numbers on [a, b]".into());
        }
        let set_ends = |u: &mut [f64], t: f64| {
            if let End::Fixed(g) = &left {
                u[0] = run(g, &[t]);
            }
            if let End::Fixed(g) = &right {
                u[n] = run(g, &[t]);
            }
        };
        set_ends(&mut u, 0.0);
        let m = n + 1;
        let (mut sub, mut diag, mut sup) = (vec![-r / 2.0; m], vec![1.0 + r; m], vec![-r / 2.0; m]);
        sub[0] = 0.0;
        sup[n] = 0.0;
        match left {
            End::Fixed(_) => {
                diag[0] = 1.0;
                sup[0] = 0.0;
            }
            End::Flux => sup[0] = -r,
        }
        match right {
            End::Fixed(_) => {
                diag[n] = 1.0;
                sub[n] = 0.0;
            }
            End::Flux => sub[n] = -r,
        }
        let mut out = vec![u.clone()];
        let mut ts = vec![0.0];
        let mut t = 0.0;
        let mut rhs = vec![0.0; m];
        for fr in 1..=frames {
            for s in 0..per {
                let t1 = (fr - 1) as f64 * frame_dt + (s + 1) as f64 * dt;
                for i in 0..m {
                    let lap = if i == 0 {
                        2.0 * (u[1] - u[0])
                    } else if i == n {
                        2.0 * (u[n - 1] - u[n])
                    } else {
                        u[i - 1] - 2.0 * u[i] + u[i + 1]
                    };
                    rhs[i] = u[i] + r / 2.0 * lap + dt * (src(xs[i], t) + src(xs[i], t1)) / 2.0;
                }
                if let End::Fixed(g) = &left {
                    rhs[0] = run(g, &[t1]);
                }
                if let End::Fixed(g) = &right {
                    rhs[n] = run(g, &[t1]);
                }
                thomas(&sub, &diag, &sup, &mut rhs);
                u.copy_from_slice(&rhs);
                t = t1;
            }
            if u.iter().any(|v| !v.is_finite()) {
                return Err(format!("heat: the solution is not a number at t = {}; check the source and the ends", fmt(t)));
            }
            out.push(u.clone());
            ts.push(fr as f64 * frame_dt);
        }
        let end = |e: &End| match e {
            End::Fixed(_) => "a fixed temperature",
            End::Flux => "insulated (no heat flows through)",
        };
        self.note(format!(
            "Heat equation u_t = {} u_xx{} on [{}, {}] up to t = {}, solved by Crank–Nicolson on {} intervals (dx = {}) with {} time steps (dt = {}, k dt/dx^2 = {}). Left end: {}; right end: {}. x holds the grid, t the {} snapshot times, u one row per snapshot (u(1) is the start); pdeplot shows it.",
            fmt(k),
            if source.is_some() { " + source" } else { "" },
            fmt(a),
            fmt(b),
            fmt(tmax),
            n,
            fmt(h),
            per * frames,
            format!("{:.3e}", dt),
            fmt(r),
            end(&left),
            end(&right),
            frames + 1
        ));
        Ok(record(vec![("x", floats_row(&xs)), ("t", floats_row(&ts)), ("u", floats_matrix(&out))]))
    }

    /// wave(f, g, x, a, b, T[, options]): u_tt = c^2 u_xx on [a, b] with
    /// u(x, 0) = f and u_t(x, 0) = g, by the leapfrog scheme
    fn wave(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: wave(f, g, x, a, b, T) solves u_tt = u_xx on a <= x <= b up to time T, starting from u = f(x) and u_t = g(x) with u = 0 at both ends; wave(f, g, x, a, b, T, {c: 2, left: \"free\", right: sin(t), damping: 0.1, n: 200, frames: 50}) sets the speed, the ends (a value, an expression in t, or \"free\"), damping, the grid and the number of snapshots";
        let (fnode, gnode, xn, an, bn, tn, on) = match args {
            [f, g, x, a, b, t] => (f, g, x, a, b, t, None),
            [f, g, x, a, b, t, o] => (f, g, x, a, b, t, Some(o)),
            _ => return Err(usage.into()),
        };
        let x = ident(xn, "x")?;
        let (a, b) = (self.pnum(an, "wave", "a")?, self.pnum(bn, "wave", "b")?);
        if a >= b {
            return Err("wave: need a < b".into());
        }
        let tmax = self.pnum(tn, "wave", "the time T")?;
        if tmax <= 0.0 {
            return Err("wave: the time T must be positive".into());
        }
        let vars = vec![x.clone(), "t".to_string()];
        let opts = self.pde_opts(on, &vars, "wave", &["c", "left", "right", "damping", "n", "frames"])?;
        let c = Self::opt_num(&opts, "c", "wave", 1.0)?;
        if c <= 0.0 {
            return Err("wave: the speed c must be positive".into());
        }
        let gamma = Self::opt_num(&opts, "damping", "wave", 0.0)?;
        if gamma < 0.0 {
            return Err("wave: damping must be 0 or more".into());
        }
        let n = Self::opt_count(&opts, "n", "wave", 200, 4, 5000)?;
        let frames = Self::opt_count(&opts, "frames", "wave", 50, 1, 1000)?;
        let left = Self::opt_end(&opts, "left", "t", "wave", "free")?;
        let right = Self::opt_end(&opts, "right", "t", "wave", "free")?;
        let (fc, _) = self.function(fnode, std::slice::from_ref(&x))?;
        let (gc, _) = self.function(gnode, std::slice::from_ref(&x))?;

        let h = (b - a) / n as f64;
        let xs: Vec<f64> = (0..=n).map(|i| a + i as f64 * h).collect();
        let frame_dt = tmax / frames as f64;
        // Courant number c dt / h at most 0.9 keeps the scheme stable
        let per = ((frame_dt * c / (0.9 * h)).ceil() as usize).max(1);
        if per * frames * (n + 1) > 50_000_000 {
            return Err("wave: too many grid steps; use a coarser grid (n) or a shorter time".into());
        }
        let dt = frame_dt / per as f64;
        let cfl = c * dt / h;
        let q = cfl * cfl;
        let lap = |u: &[f64], i: usize| -> f64 {
            if i == 0 {
                2.0 * (u[1] - u[0])
            } else if i == n {
                2.0 * (u[n - 1] - u[n])
            } else {
                u[i - 1] - 2.0 * u[i] + u[i + 1]
            }
        };
        let fix = |u: &mut [f64], t: f64| {
            if let End::Fixed(g) = &left {
                u[0] = run(g, &[t]);
            }
            if let End::Fixed(g) = &right {
                u[n] = run(g, &[t]);
            }
        };
        let mut u: Vec<f64> = xs.iter().map(|&xv| run(&fc, &[xv])).collect();
        let v0: Vec<f64> = xs.iter().map(|&xv| run(&gc, &[xv])).collect();
        if u.iter().chain(&v0).any(|v| !v.is_finite()) {
            return Err("wave: the starting values f(x) and g(x) are not all numbers on [a, b]".into());
        }
        fix(&mut u, 0.0);
        // first step from the Taylor expansion
        let mut prev = u.clone();
        let mut cur: Vec<f64> = (0..=n)
            .map(|i| u[i] + dt * v0[i] * (1.0 - gamma * dt / 2.0) + q / 2.0 * lap(&u, i))
            .collect();
        fix(&mut cur, dt);
        let mut out = vec![u.clone()];
        let mut ts = vec![0.0];
        let mut step = 1usize;
        let (ap, am) = (1.0 + gamma * dt / 2.0, 1.0 - gamma * dt / 2.0);
        for fr in 1..=frames {
            while step < fr * per {
                let next: Vec<f64> = (0..=n)
                    .map(|i| (2.0 * cur[i] - am * prev[i] + q * lap(&cur, i)) / ap)
                    .collect();
                prev = std::mem::replace(&mut cur, next);
                step += 1;
                fix(&mut cur, step as f64 * dt);
            }
            if cur.iter().any(|v| !v.is_finite()) {
                return Err(format!("wave: the solution is not a number at t = {}", fmt(fr as f64 * frame_dt)));
            }
            out.push(cur.clone());
            ts.push(fr as f64 * frame_dt);
        }
        let end = |e: &End| match e {
            End::Fixed(_) => "held (fixed)",
            End::Flux => "free",
        };
        self.note(format!(
            "Wave equation u_tt = {}^2 u_xx{} on [{}, {}] up to t = {}, solved by the leapfrog scheme on {} intervals (dx = {}) with {} time steps (dt = {}, Courant number c dt/dx = {}; above 1 the scheme blows up). Left end: {}; right end: {}. x holds the grid, t the {} snapshot times, u one row per snapshot; pdeplot shows it.",
            fmt(c),
            if gamma > 0.0 { format!(" - {} u_t", fmt(gamma)) } else { String::new() },
            fmt(a),
            fmt(b),
            fmt(tmax),
            n,
            fmt(h),
            per * frames,
            format!("{:.3e}", dt),
            fmt(cfl),
            end(&left),
            end(&right),
            frames + 1
        ));
        Ok(record(vec![("x", floats_row(&xs)), ("t", floats_row(&ts)), ("u", floats_matrix(&out))]))
    }

    /// laplace(g, x, a, b, y, c, d[, options]): u_xx + u_yy = 0 inside the
    /// rectangle, u = g on its edge; poisson(f, g, ...) solves u_xx + u_yy = f
    fn laplace(&mut self, args: &[Node], poisson: bool) -> R<Expr> {
        let f = if poisson { "poisson" } else { "laplace" };
        let usage = if poisson {
            "usage: poisson(f, g, x, a, b, y, c, d) solves u_xx + u_yy = f(x, y) on the rectangle a <= x <= b, c <= y <= d with u = g(x, y) on its edge; a last record {left: ..., right: ..., bottom: ..., top: ..., n: 40} sets single edges (an expression in y or x) and the grid"
        } else {
            "usage: laplace(g, x, a, b, y, c, d) solves u_xx + u_yy = 0 on the rectangle a <= x <= b, c <= y <= d with u = g(x, y) on its edge; a last record {left: ..., right: ..., bottom: ..., top: ..., n: 40} sets single edges (an expression in y or x) and the grid"
        };
        let (fnode, rest) = if poisson {
            match args.split_first() {
                Some((p, r)) => (Some(p), r),
                None => return Err(usage.into()),
            }
        } else {
            (None, args)
        };
        let (gnode, xn, an, bn, yn, cn, dn, on) = match rest {
            [g, x, a, b, y, c, d] => (g, x, a, b, y, c, d, None),
            [g, x, a, b, y, c, d, o] => (g, x, a, b, y, c, d, Some(o)),
            _ => return Err(usage.into()),
        };
        let (x, y) = (ident(xn, "x")?, ident(yn, "y")?);
        if x == y {
            return Err(format!("{}: x and y must be different names", f));
        }
        let (a, b) = (self.pnum(an, f, "a")?, self.pnum(bn, f, "b")?);
        let (c, d) = (self.pnum(cn, f, "c")?, self.pnum(dn, f, "d")?);
        if a >= b || c >= d {
            return Err(format!("{}: need a < b and c < d", f));
        }
        let vars = vec![x.clone(), y.clone()];
        let opts = self.pde_opts(on, &vars, f, &["left", "right", "bottom", "top", "n"])?;
        let n = Self::opt_count(&opts, "n", f, 40, 4, 400)?;
        let (gc, _) = self.function(gnode, &vars)?;
        let fc = match fnode {
            Some(p) => Some(self.function(p, &vars)?.0),
            None => None,
        };
        let (xv, yv) = (x.as_str(), y.as_str());
        let edges = [
            ("left", Self::opt_fn(&opts, "left", &[yv], f)?),
            ("right", Self::opt_fn(&opts, "right", &[yv], f)?),
            ("bottom", Self::opt_fn(&opts, "bottom", &[xv], f)?),
            ("top", Self::opt_fn(&opts, "top", &[xv], f)?),
        ];
        // the longer side gets n intervals, the other in proportion
        let (nx, ny) = if b - a >= d - c {
            (n, ((n as f64 * (d - c) / (b - a)).round() as usize).max(4))
        } else {
            (((n as f64 * (b - a) / (d - c)).round() as usize).max(4), n)
        };
        let (hx, hy) = ((b - a) / nx as f64, (d - c) / ny as f64);
        let xs: Vec<f64> = (0..=nx).map(|i| a + i as f64 * hx).collect();
        let ys: Vec<f64> = (0..=ny).map(|j| c + j as f64 * hy).collect();
        // u[j][i] = u(x_i, y_j)
        let mut u = vec![vec![0.0; nx + 1]; ny + 1];
        let edge = |k: usize, s: f64| edges[k].1.as_ref().map(|code| run(code, &[s]));
        for j in 0..=ny {
            for i in 0..=nx {
                if i == 0 || i == nx || j == 0 || j == ny {
                    let mut v = run(&gc, &[xs[i], ys[j]]);
                    if i == 0 {
                        v = edge(0, ys[j]).unwrap_or(v);
                    }
                    if i == nx {
                        v = edge(1, ys[j]).unwrap_or(v);
                    }
                    if j == 0 {
                        v = edge(2, xs[i]).unwrap_or(v);
                    }
                    if j == ny {
                        v = edge(3, xs[i]).unwrap_or(v);
                    }
                    if !v.is_finite() {
                        return Err(format!("{}: the edge value at ({}, {}) is not a number", f, fmt(xs[i]), fmt(ys[j])));
                    }
                    u[j][i] = v;
                }
            }
        }
        let rhs: Vec<Vec<f64>> = (0..=ny)
            .map(|j| (0..=nx).map(|i| fc.as_ref().map_or(0.0, |p| run(p, &[xs[i], ys[j]]))).collect())
            .collect();
        if rhs.iter().flatten().any(|v| !v.is_finite()) {
            return Err(format!("{}: f(x, y) is not a number everywhere inside the rectangle", f));
        }
        // start the inside at the average edge value
        let (mut sum, mut cnt) = (0.0, 0.0);
        for j in 0..=ny {
            for i in 0..=nx {
                if i == 0 || i == nx || j == 0 || j == ny {
                    sum += u[j][i];
                    cnt += 1.0;
                }
            }
        }
        for row in u.iter_mut().take(ny).skip(1) {
            for v in row.iter_mut().take(nx).skip(1) {
                *v = sum / cnt;
            }
        }
        let (ax, ay) = (1.0 / (hx * hx), 1.0 / (hy * hy));
        let den = 2.0 * (ax + ay);
        let m = nx.max(ny) as f64;
        let omega = 2.0 / (1.0 + (std::f64::consts::PI / m).sin());
        let big = |m: &[Vec<f64>]| m.iter().flatten().fold(0.0f64, |s, v| s.max(v.abs()));
        let scale = big(&u).max(big(&rhs) * (b - a).max(d - c).powi(2)).max(1e-300);
        let mut iters = 0;
        let mut change = f64::INFINITY;
        while change > 1e-11 * scale && iters < 100_000 {
            change = 0.0;
            for j in 1..ny {
                for i in 1..nx {
                    let gs = (ax * (u[j][i - 1] + u[j][i + 1]) + ay * (u[j - 1][i] + u[j + 1][i]) - rhs[j][i]) / den;
                    let du = omega * (gs - u[j][i]);
                    u[j][i] += du;
                    change = change.max(du.abs());
                }
            }
            iters += 1;
        }
        if iters >= 100_000 {
            return Err(format!("{}: the iteration did not settle; try a coarser grid (n)", f));
        }
        self.note(format!(
            "{} u_xx + u_yy = {} on [{}, {}] x [{}, {}], solved with the five-point difference formula on a {} by {} grid (dx = {}, dy = {}) by successive over-relaxation ({} sweeps, omega = {}). x and y hold the grid, u(j) is the row at y = y(j), so u(j, i) is the value at (x(i), y(j)); pdeplot shows it.",
            if poisson { "Poisson equation" } else { "Laplace equation" },
            if poisson { "f" } else { "0" },
            fmt(a),
            fmt(b),
            fmt(c),
            fmt(d),
            nx,
            ny,
            fmt(hx),
            fmt(hy),
            iters,
            fmt(omega)
        ));
        Ok(record(vec![("x", floats_row(&xs)), ("y", floats_row(&ys)), ("u", floats_matrix(&u))]))
    }

    /// pdeplot(s[, kind]) for a result of heat, wave, laplace or poisson
    fn pdeplot(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: pdeplot(s) for a result s of heat, wave, laplace or poisson; pdeplot(s, \"lines\"), pdeplot(s, \"map\") or pdeplot(s, \"surface\") for snapshots, a color map or a surface";
        let (sn, kind) = match args {
            [s] => (s, None),
            [s, k] => (s, Some(self.eval(k)?)),
            _ => return Err(usage.into()),
        };
        let kind = match kind {
            None => None,
            Some(Expr::Str(k)) => Some(k),
            Some(v) => return Err(format!("pdeplot: the kind is \"animate\", \"lines\", \"map\" or \"surface\", got {}", show(&v))),
        };
        let s = self.eval(sn)?;
        let fields = match &s {
            Expr::Obj(o) => match &**o {
                Obj::Record(v) => v.clone(),
                _ => return Err(usage.into()),
            },
            _ => return Err(usage.into()),
        };
        let get = |k: &str| fields.iter().find(|(n, _)| n == k).and_then(|(_, v)| grid_of(v));
        let (Some(xs), Some(u)) = (get("x"), get("u")) else { return Err(usage.into()) };
        let xs = xs.into_iter().next().unwrap_or_default();
        let (other, timed) = match (get("t"), get("y")) {
            (Some(t), _) => (t.into_iter().next().unwrap_or_default(), true),
            (None, Some(y)) => (y.into_iter().next().unwrap_or_default(), false),
            _ => return Err(usage.into()),
        };
        if xs.len() < 2 || other.is_empty() || u.len() != other.len() || u.iter().any(|r| r.len() != xs.len()) {
            return Err("pdeplot: x, u and t (or y) do not fit together: u needs one row per t (or y) and one column per x".into());
        }
        let ylab = if timed { "t" } else { "y" };
        let kind = kind.unwrap_or_else(|| if timed { "animate".into() } else { "map".into() });
        let (lo, hi) = u.iter().flatten().filter(|v| v.is_finite()).fold((f64::INFINITY, f64::NEG_INFINITY), |(l, h), &v| (l.min(v), h.max(v)));
        let pad = if hi > lo { 0.05 * (hi - lo) } else { 1.0 };
        let pts = |r: &[f64]| xs.iter().copied().zip(r.iter().copied()).collect::<Vec<_>>();
        match kind.as_str() {
            "animate" => {
                if !timed {
                    return Err("pdeplot: \"animate\" needs a result with times (heat or wave)".into());
                }
                let mut fig = Figure::new(vec![Series { label: "u".into(), points: pts(&u[0]), ..Default::default() }], "x", "u", false);
                fig.frames = u
                    .iter()
                    .zip(&other)
                    .map(|(r, &t)| Frame { label: format!("t = {}", fmt(t)), series: vec![(0, pts(r))] })
                    .collect();
                fig.window = Some((xs[0], xs[xs.len() - 1], lo - pad, hi + pad));
                self.note("pdeplot: the solution u(x) moving in time; press play or drag the slider (a still picture shows a few faint frames). pdeplot(s, \"lines\") draws snapshots side by side, pdeplot(s, \"map\") the whole history as colors.");
                Ok(obj(Obj::Figure(fig)))
            }
            "lines" => {
                let k = u.len();
                let pick: Vec<usize> = if k <= 6 { (0..k).collect() } else { (0..6).map(|i| (i * (k - 1) + 2) / 5).map(|i| i.min(k - 1)).collect() };
                let mut pick = pick;
                pick.dedup();
                let series = pick
                    .iter()
                    .map(|&i| Series { label: format!("{} = {}", ylab, fmt(other[i])), points: pts(&u[i]), ..Default::default() })
                    .collect();
                self.note(format!("pdeplot: u against x at {} values of {}.", pick.len(), ylab));
                Ok(obj(Obj::Figure(Figure::new(series, "x", "u", false))))
            }
            "map" => {
                let (x0, x1) = (xs[0], xs[xs.len() - 1]);
                let (y0, y1) = (other[0], other[other.len() - 1]);
                let (w, h) = (xs.len().clamp(2, 400), other.len().clamp(2, 400));
                // pixels: sample the grid (nearest node), top row first
                let mut px = Vec::with_capacity(w * h * 3);
                let span = if hi > lo { hi - lo } else { 1.0 };
                for jj in 0..h {
                    let j = ((h - 1 - jj) * (other.len() - 1) + (h - 1) / 2) / (h - 1);
                    for ii in 0..w {
                        let i = (ii * (xs.len() - 1) + (w - 1) / 2) / (w - 1);
                        px.extend_from_slice(&ramp_rgb((u[j][i] - lo) / span));
                    }
                }
                // half a cell beyond the outer nodes so every node gets its pixel
                let (dx, dy) = ((x1 - x0) / (w - 1) as f64 / 2.0, (y1 - y0) / (h - 1) as f64 / 2.0);
                let mut series = vec![Series {
                    label: format!("u from {} (blue) to {} (red)", fmt(lo), fmt(hi)),
                    points: vec![],
                    color: Some("#b40426".into()),
                    ..Default::default()
                }];
                if !timed {
                    // level curves on top: z indexed [x][y]
                    let z: Vec<Vec<f64>> = (0..xs.len()).map(|i| (0..other.len()).map(|j| u[j][i]).collect()).collect();
                    let mut fin: Vec<f64> = u.iter().flatten().copied().filter(|v| v.is_finite()).collect();
                    fin.sort_by(|p, q| p.total_cmp(q));
                    let mut pts = vec![];
                    if !fin.is_empty() && fin[0] < fin[fin.len() - 1] {
                        for lvl in auto_levels(&fin, 10) {
                            pts.extend(march(&xs, &other, &z, lvl));
                            pts.push((f64::NAN, f64::NAN));
                        }
                    }
                    series.push(Series { label: "level curves".into(), points: pts, color: Some("#1f2937".into()), thin: true, ..Default::default() });
                }
                let mut fig = Figure::new(series, "x", ylab, !timed);
                fig.image = Some(Raster { w, h, rgb: px, x0: x0 - dx, x1: x1 + dx, y0: y0 - dy, y1: y1 + dy });
                self.note(if timed {
                    "pdeplot: the whole history as colors, x across and time t upward; blue is low, red is high.".to_string()
                } else {
                    "pdeplot: u(x, y) as colors, blue low and red high, with level curves on top.".to_string()
                });
                Ok(obj(Obj::Figure(fig)))
            }
            "surface" => {
                let (step_i, step_j) = (xs.len().div_ceil(80).max(1), other.len().div_ceil(80).max(1));
                let is: Vec<usize> = (0..xs.len()).step_by(step_i).chain(std::iter::once(xs.len() - 1)).collect::<std::collections::BTreeSet<_>>().into_iter().collect();
                let js: Vec<usize> = (0..other.len()).step_by(step_j).chain(std::iter::once(other.len() - 1)).collect::<std::collections::BTreeSet<_>>().into_iter().collect();
                let sx = js.iter().map(|_| is.iter().map(|&i| xs[i]).collect()).collect();
                let sy = js.iter().map(|&j| is.iter().map(|_| other[j]).collect()).collect();
                let sz = js.iter().map(|&j| is.iter().map(|&i| u[j][i]).collect()).collect();
                self.note(format!("pdeplot: u as a surface over the (x, {}) plane.", ylab));
                Ok(obj(Obj::Figure3D(Figure3D::new(
                    vec![Surface { label: "u".into(), x: sx, y: sy, z: sz }],
                    vec![],
                    ["x", ylab, "u"],
                    false,
                ))))
            }
            k => Err(format!("pdeplot: unknown kind \"{}\"; use \"animate\", \"lines\", \"map\" or \"surface\"", k)),
        }
    }
}
