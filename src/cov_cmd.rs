//! Calculus of variations and Lagrangian mechanics: the Euler–Lagrange
//! equations of a Lagrangian L(x, y, y'), the conserved energy (Beltrami
//! identity), the value of a functional on a given path, a numerical extremal
//! between two fixed end points (Newton's method on the discretized
//! functional), and the motion of a mechanical system from its Lagrangian.

use crate::build::*;
use crate::calc::{deriv, evalf, simplify, subst};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::numeric::{Code, compile, run};
use crate::parser::Node;
use crate::print::show;
use std::rc::Rc;

const NAMES: &[&str] = &["eulerlagrange", "energy", "action", "extremal", "motion"];

fn ph(q: &str, k: usize) -> String {
    format!("{}__{}", q, k)
}

fn floats_row(v: &[f64]) -> Expr {
    Expr::Matrix(vec![v.iter().map(|&x| Expr::float(x)).collect()])
}

fn record(v: Vec<(&str, Expr)>) -> Expr {
    Expr::Obj(Rc::new(Obj::Record(v.into_iter().map(|(k, x)| (k.to_string(), x)).collect())))
}

fn fmt(v: f64) -> String {
    let r = (v * 1e6).round() / 1e6;
    if r == r.trunc() && r.abs() < 1e15 { format!("{}", r as i64) } else { format!("{}", r) }
}

/// The unknowns: one name or a row of names
fn unknowns(n: &Node, f: &str) -> R<Vec<String>> {
    let bad = || format!("{}: the unknowns are a name such as y, or a row of names such as [x y]", f);
    match n {
        Node::Ident(s) => Ok(vec![s.clone()]),
        Node::Matrix(rows) => {
            let mut out = vec![];
            for r in rows {
                for e in r {
                    match e {
                        Node::Ident(s) if !out.contains(s) => out.push(s.clone()),
                        _ => return Err(bad()),
                    }
                }
            }
            if out.is_empty() { Err(bad()) } else { Ok(out) }
        }
        _ => Err(bad()),
    }
}

/// Write q__0, q__1, q__2 back as q, q', q''
fn primes(e: &Expr, qs: &[String]) -> Expr {
    let mut e = e.clone();
    for q in qs {
        for k in 0..3 {
            e = subst(&e, &ph(q, k), &Expr::sym(&format!("{}{}", q, "'".repeat(k))));
        }
    }
    e
}

/// Solve a small dense linear system in place (partial pivoting); false if singular
fn gauss(a: &mut [Vec<f64>], b: &mut [f64]) -> bool {
    let n = b.len();
    for c in 0..n {
        let p = (c..n).max_by(|&i, &j| a[i][c].abs().total_cmp(&a[j][c].abs())).unwrap();
        if a[p][c].abs() < 1e-300 || !a[p][c].is_finite() {
            return false;
        }
        a.swap(c, p);
        b.swap(c, p);
        for r in c + 1..n {
            let m = a[r][c] / a[c][c];
            for k in c..n {
                a[r][k] -= m * a[c][k];
            }
            b[r] -= m * b[c];
        }
    }
    for c in (0..n).rev() {
        let mut s = b[c];
        for k in c + 1..n {
            s -= a[c][k] * b[k];
        }
        b[c] = s / a[c][c];
    }
    true
}

/// A Lagrangian read with its unknowns written as q__0 (value) and q__1 (derivative)
struct Lag {
    qs: Vec<String>,
    x: String,
    l: Expr,
}

impl Lag {
    fn d(&self, e: &Expr, v: &str) -> Expr {
        simplify(&deriv(e, v))
    }
    /// the total derivative d/dx of an expression in x, q, q'
    fn total(&self, e: &Expr) -> Expr {
        let mut terms = vec![self.d(e, &self.x)];
        for q in &self.qs {
            terms.push(mul(vec![self.d(e, &ph(q, 0)), Expr::sym(&ph(q, 1))]));
            terms.push(mul(vec![self.d(e, &ph(q, 1)), Expr::sym(&ph(q, 2))]));
        }
        simplify(&add(terms))
    }
    /// sum of q' dL/dq' - L
    fn energy(&self) -> Expr {
        let mut terms = vec![neg(self.l.clone())];
        for q in &self.qs {
            terms.push(mul(vec![Expr::sym(&ph(q, 1)), self.d(&self.l, &ph(q, 1))]));
        }
        simplify(&add(terms))
    }
    /// names for compiled code: q__0..., q__1..., x
    fn vars(&self) -> Vec<String> {
        let mut v: Vec<String> = self.qs.iter().map(|q| ph(q, 0)).collect();
        v.extend(self.qs.iter().map(|q| ph(q, 1)));
        v.push(self.x.clone());
        v
    }
    fn code(&self, e: &Expr) -> Code {
        let v = self.vars();
        let names: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
        compile(e, &names)
    }
}

impl Session {
    pub(crate) fn cov_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        if !NAMES.contains(&f) {
            return None;
        }
        Some(match f {
            "eulerlagrange" => self.eulerlagrange(args),
            "energy" => self.energy_cmd(args),
            "action" => self.action(args),
            "extremal" => self.extremal(args),
            _ => self.motion(args),
        })
    }

    /// Read L with its unknowns and independent variable
    fn lagrangian(&mut self, ln: &Node, qn: &Node, xn: &Node, f: &str) -> R<Lag> {
        let qs = unknowns(qn, f)?;
        let x = match xn {
            Node::Ident(s) => s.clone(),
            _ => return Err(format!("{}: the independent variable is a name such as x or t", f)),
        };
        if qs.contains(&x) {
            return Err(format!("{}: {} can't be both an unknown and the variable", f, x));
        }
        let mut node = ln.clone();
        let mut order = 0;
        for q in &qs {
            let mut o = 0;
            node = crate::extra_cmd::ode_node(&node, q, &x, &mut o)?;
            order = order.max(o);
        }
        if order > 1 {
            return Err(format!("{}: L may use the unknowns and their first derivatives ({}'), not higher ones", f, qs[0]));
        }
        let mut hid: Vec<String> = qs.iter().flat_map(|q| [ph(q, 0), ph(q, 1)]).collect();
        hid.push(x.clone());
        let l = crate::eval::equation_lhs(&self.eval_hidden(&node, &hid)?);
        if !l.is_scalar() {
            return Err(format!("{}: L must be a single expression", f));
        }
        // free parameters (m, g, ...) are fine for symbolic work; numeric commands check them
        Ok(Lag { qs, x, l })
    }

    fn eulerlagrange(&mut self, args: &[Node]) -> R<Expr> {
        let [ln, qn, xn] = args else {
            return Err("usage: eulerlagrange(L, y, x) for a Lagrangian L in x, y and y', or eulerlagrange(L, [q1 q2], t) for several unknowns".into());
        };
        let lag = self.lagrangian(ln, qn, xn, "eulerlagrange")?;
        let mut eqs = vec![];
        let mut notes = vec![];
        // M q'' = f: with a diagonal M (no products of different velocities) write q'' = f / M
        let diag = lag.qs.iter().all(|qi| {
            let p = lag.d(&lag.l, &ph(qi, 1));
            lag.qs.iter().all(|qj| qj == qi || lag.d(&p, &ph(qj, 1)).is_zero())
        });
        for q in &lag.qs {
            let p = lag.d(&lag.l, &ph(q, 1));
            let mm = lag.d(&p, &ph(q, 1));
            if diag && !mm.is_zero() {
                let mut terms = vec![lag.d(&lag.l, &ph(q, 0)), neg(lag.d(&p, &lag.x))];
                for qj in &lag.qs {
                    terms.push(neg(mul(vec![lag.d(&p, &ph(qj, 0)), Expr::sym(&ph(qj, 1))])));
                }
                let rhs = simplify(&div(add(terms), mm));
                eqs.push(Expr::Apply("==".into(), vec![Expr::sym(&format!("{}''", q)), primes(&rhs, &lag.qs)]));
            }
        }
        if eqs.len() == lag.qs.len() {
            notes.push("The equations are solved for the second derivatives.".to_string());
        } else {
            eqs.clear();
        }
        for q in &lag.qs {
            let p = lag.d(&lag.l, &ph(q, 1));
            let lhs = lag.total(&p);
            let rhs = lag.d(&lag.l, &ph(q, 0));
            if rhs.is_zero() {
                notes.push(format!(
                    "L does not contain {q} itself, so dL/d{q}' = {} stays constant along every extremal (a conserved momentum).",
                    show(&primes(&p, &lag.qs))
                ));
            }
            if eqs.len() < lag.qs.len() || !diag {
                eqs.push(Expr::Apply("==".into(), vec![primes(&lhs, &lag.qs), primes(&rhs, &lag.qs)]));
            }
        }
        if !lag.l.contains_sym(&lag.x) {
            notes.push(format!(
                "L does not contain {} itself, so energy(L, ...) = {} stays constant along every extremal (Beltrami's identity, conservation of energy).",
                lag.x,
                show(&primes(&lag.energy(), &lag.qs))
            ));
        }
        self.note(format!(
            "Euler–Lagrange equation{}: d/d{x}(dL/d{q}') = dL/d{q}{}. A path that makes the integral of L smallest (or stationary) with fixed end points satisfies {}. {}",
            if lag.qs.len() > 1 { "s, one per unknown" } else { "" },
            if lag.qs.len() > 1 { " for each unknown q" } else { "" },
            if lag.qs.len() > 1 { "all of them" } else { "it" },
            notes.join(" "),
            x = lag.x,
            q = if lag.qs.len() > 1 { "q".to_string() } else { lag.qs[0].clone() },
        ));
        if eqs.len() == 1 {
            Ok(eqs.pop().unwrap())
        } else {
            Ok(Expr::Matrix(vec![eqs]))
        }
    }

    fn energy_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [ln, qn, xn] = args else {
            return Err("usage: energy(L, y, x) or energy(L, [q1 q2], t): the sum of q' dL/dq' minus L".into());
        };
        let lag = self.lagrangian(ln, qn, xn, "energy")?;
        let e = primes(&lag.energy(), &lag.qs);
        self.note(format!(
            "E = {} - L, the sum of q' dL/dq' over the unknowns minus L. When L does not contain {} itself, E is constant along every solution of the Euler–Lagrange equations: in mechanics it is the total energy, in geometry Beltrami's first integral.",
            lag.qs.iter().map(|q| format!("{q}' dL/d{q}'")).collect::<Vec<_>>().join(" + "),
            lag.x
        ));
        Ok(e)
    }

    /// action(L, y, x, f, a, b): the integral of L along y = f(x) from a to b
    fn action(&mut self, args: &[Node]) -> R<Expr> {
        let [ln, qn, xn, pn, an, bn] = args else {
            return Err("usage: action(L, y, x, f, a, b): the integral of L from x = a to b along the path y = f(x); for several unknowns give a row of paths".into());
        };
        let lag = self.lagrangian(ln, qn, xn, "action")?;
        let paths: Vec<Expr> = match self.eval_hidden(pn, std::slice::from_ref(&lag.x))? {
            Expr::Matrix(m) => m.into_iter().flatten().collect(),
            v => vec![v],
        };
        if paths.len() != lag.qs.len() {
            return Err(format!("action: give one path per unknown ({})", lag.qs.len()));
        }
        let (a, b) = (self.eval(an)?, self.eval(bn)?);
        let mut e = lag.l.clone();
        for (q, p) in lag.qs.iter().zip(&paths) {
            e = subst(&e, &ph(q, 1), &simplify(&deriv(p, &lag.x)));
            e = subst(&e, &ph(q, 0), p);
        }
        let e = simplify(&e);
        let exact = crate::cas_cmd::definite(&e, &lag.x, &a, &b, 0).ok().filter(|v| evalf(v).num().is_some_and(|n| n.to_f64().is_finite()));
        let val = match exact {
            Some(v) => v,
            None => {
                let (lo, hi) = (
                    evalf(&a).num().map(|n| n.to_f64()).ok_or("action: a must be a number")?,
                    evalf(&b).num().map(|n| n.to_f64()).ok_or("action: b must be a number")?,
                );
                let c = compile(&e, &[&lag.x]);
                // Gauss–Legendre, 5 points on 400 panels
                let (gx, gw) = (
                    [0.0, -0.538469310105683, 0.538469310105683, -0.906179845938664, 0.906179845938664],
                    [0.568888888888889, 0.478628670499366, 0.478628670499366, 0.236926885056189, 0.236926885056189],
                );
                let n = 400;
                let h = (hi - lo) / n as f64;
                let mut s = 0.0;
                for k in 0..n {
                    let m = lo + (k as f64 + 0.5) * h;
                    for j in 0..5 {
                        s += gw[j] * run(&c, &[m + gx[j] * h / 2.0]);
                    }
                }
                let s = s * h / 2.0;
                if !s.is_finite() {
                    return Err(format!("action: the integral of {} is not a finite number", show(&e)));
                }
                Expr::float(s)
            }
        };
        self.note(format!("The integral of L = {} from {} = {} to {} along the given path.", show(&primes(&lag.l, &lag.qs)), lag.x, show(&a), show(&b)));
        Ok(val)
    }

    /// extremal(L, y, x, a, b, ya, yb[, n]): a numerical extremal with fixed ends
    fn extremal(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: extremal(L, y, x, a, b, ya, yb) finds the path y(x) from (a, ya) to (b, yb) that makes the integral of L(x, y, y') stationary (usually smallest), numerically on 100 pieces; a last argument n sets the number of pieces";
        let (ln, qn, xn, an, bn, yan, ybn, nn) = match args {
            [l, q, x, a, b, ya, yb] => (l, q, x, a, b, ya, yb, None),
            [l, q, x, a, b, ya, yb, n] => (l, q, x, a, b, ya, yb, Some(n)),
            _ => return Err(usage.into()),
        };
        let lag = self.lagrangian(ln, qn, xn, "extremal")?;
        if lag.qs.len() != 1 {
            return Err("extremal: one unknown y(x)".into());
        }
        let num = |s: &mut Session, n: &Node, what: &str| -> R<f64> {
            let v = s.eval(n)?;
            evalf(&v).num().map(|x| x.to_f64()).filter(|x| x.is_finite()).ok_or_else(|| format!("extremal: {} must be a number, got {}", what, show(&v)))
        };
        let (a, b) = (num(self, an, "a")?, num(self, bn, "b")?);
        let (ya, yb) = (num(self, yan, "ya")?, num(self, ybn, "yb")?);
        if a >= b {
            return Err("extremal: need a < b".into());
        }
        let n = match nn {
            Some(k) => {
                let v = num(self, k, "n")?;
                if v.fract() != 0.0 || !(4.0..=5000.0).contains(&v) {
                    return Err("extremal: n is a whole number from 4 to 5000".into());
                }
                v as usize
            }
            None => 100,
        };
        let stray: Vec<String> = lag.l.symbols().into_iter().filter(|s| !lag.vars().contains(s) && s != "pi" && s != "e").collect();
        if !stray.is_empty() {
            return Err(format!("extremal: give {} a value first", stray.join(", ")));
        }
        let (y0, y1) = (ph(&lag.qs[0], 0), ph(&lag.qs[0], 1));
        let ly = lag.d(&lag.l, &y0);
        let lp = lag.d(&lag.l, &y1);
        let lyy = lag.d(&ly, &y0);
        let lyp = lag.d(&ly, &y1);
        let lpp = lag.d(&lp, &y1);
        let [cl, cly, clp, clyy, clyp, clpp] = [&lag.l, &ly, &lp, &lyy, &lyp, &lpp].map(|e| lag.code(e));
        // points crowd towards the ends (cosine spacing), where extremals often turn steep
        let xs: Vec<f64> = (0..=n).map(|i| a + (b - a) * (1.0 - (std::f64::consts::PI * i as f64 / n as f64).cos()) / 2.0).collect();
        let hs: Vec<f64> = (0..n).map(|k| xs[k + 1] - xs[k]).collect();
        let mut y: Vec<f64> = xs.iter().map(|&x| ya + (yb - ya) * (x - a) / (b - a)).collect();
        // J = sum over pieces of the 3-point Gauss rule for L(x, y, slope) on the piece
        const GX: [f64; 3] = [0.1127016653792583, 0.5, 0.8872983346207417];
        const GW: [f64; 3] = [5.0 / 18.0, 8.0 / 18.0, 5.0 / 18.0];
        // on the two end pieces put the nodes at u = w^2 (u = 1 - w^2): exact for an
        // inverse square root at the end point, as for the fastest slide from rest
        let node = |k: usize, gp: usize| -> (f64, f64) {
            if k == 0 {
                (GX[gp] * GX[gp], 2.0 * GX[gp] * GW[gp])
            } else if k == n - 1 {
                (1.0 - GX[gp] * GX[gp], 2.0 * GX[gp] * GW[gp])
            } else {
                (GX[gp], GW[gp])
            }
        };
        let at = |y: &[f64], k: usize, gp: usize| -> [f64; 3] {
            let u = node(k, gp).0;
            [y[k] * (1.0 - u) + y[k + 1] * u, (y[k + 1] - y[k]) / hs[k], xs[k] + u * hs[k]]
        };
        let total = |y: &[f64]| -> f64 {
            (0..n).map(|k| (0..3).map(|gp| node(k, gp).1 * run(&cl, &at(y, k, gp)) * hs[k]).sum::<f64>()).sum()
        };
        let mut j = total(&y);
        if !j.is_finite() {
            return Err("extremal: L is not a number along the straight line between the end points; try other end points".into());
        }
        let m = n - 1;
        let mut iters = 0;
        let mut converged = false;
        let mut still = 0;
        while iters < 200 {
            iters += 1;
            let j_before = j;
            // gradient g and tridiagonal Hessian (sub, diag, sup) for the inside points 1..n-1
            let mut g = vec![0.0; m];
            let mut dg = vec![0.0; m];
            let mut sub = vec![0.0; m];
            let mut sup = vec![0.0; m];
            for k in 0..n {
                for gp in 0..3 {
                    let v = at(&y, k, gp);
                    let h = hs[k];
                    let w = node(k, gp).1 * h;
                    let (fy, fp) = (run(&cly, &v), run(&clp, &v));
                    let (fyy, fyp, fpp) = (run(&clyy, &v), run(&clyp, &v), run(&clpp, &v));
                    // d(y)/dy_k = 1 - u, d(slope)/dy_k = -1/h; d/dy_{k+1}: u, 1/h
                    let u = node(k, gp).0;
                    let da = [1.0 - u, -1.0 / h];
                    let db = [u, 1.0 / h];
                    let grad = |d: [f64; 2]| w * (fy * d[0] + fp * d[1]);
                    let hess = |d: [f64; 2], e: [f64; 2]| w * (fyy * d[0] * e[0] + fyp * (d[0] * e[1] + d[1] * e[0]) + fpp * d[1] * e[1]);
                    if k >= 1 {
                        let i = k - 1;
                        g[i] += grad(da);
                        dg[i] += hess(da, da);
                        if k < n - 1 {
                            sup[i] += hess(da, db);
                        }
                    }
                    if k + 1 <= m {
                        let i = k;
                        g[i] += grad(db);
                        dg[i] += hess(db, db);
                        if k >= 1 {
                            sub[i] += hess(db, da);
                        }
                    }
                }
            }
            let gnorm = g.iter().fold(0.0f64, |s, v| s.max(v.abs()));
            if !gnorm.is_finite() {
                return Err("extremal: L or its derivatives stopped being numbers along the way".into());
            }
            if gnorm < 1e-12 * (1.0 + j.abs()) {
                converged = true;
                break;
            }
            // Newton step: solve H d = -g (Thomas)
            let mut r: Vec<f64> = g.iter().map(|v| -v).collect();
            let mut c = vec![0.0; m];
            let mut ok = true;
            let mut den = dg[0];
            if den.abs() < 1e-300 {
                ok = false;
            } else {
                c[0] = sup[0] / den;
                r[0] /= den;
                for i in 1..m {
                    den = dg[i] - sub[i] * c[i - 1];
                    if den.abs() < 1e-300 {
                        ok = false;
                        break;
                    }
                    c[i] = sup[i] / den;
                    r[i] = (r[i] - sub[i] * r[i - 1]) / den;
                }
                if ok {
                    for i in (0..m - 1).rev() {
                        r[i] -= c[i] * r[i + 1];
                    }
                }
            }
            if !ok || r.iter().any(|v| !v.is_finite()) {
                // fall back to a small gradient step
                r = g.iter().map(|v| -v * 1e-3).collect();
            }
            // step length: halve until J goes down (a minimum is what is usually wanted);
            // if Newton's direction does not lead down, try the steepest descent
            let mut moved = false;
            let gmax = g.iter().fold(0.0f64, |s, v| s.max(v.abs())).max(1e-300);
            let ymax = y.iter().fold(1.0f64, |s, v| s.max(v.abs()));
            for dir in [r.clone(), g.iter().map(|v| -v / gmax * ymax * 0.1).collect::<Vec<f64>>()] {
                let mut t = 1.0;
                for _ in 0..50 {
                    let mut trial = y.clone();
                    for i in 0..m {
                        trial[i + 1] += t * dir[i];
                    }
                    let jt = total(&trial);
                    if jt.is_finite() && jt <= j + 1e-14 * j.abs().max(1.0) {
                        let size = dir.iter().fold(0.0f64, |s, v| s.max(v.abs())) * t;
                        y = trial;
                        j = jt;
                        moved = true;
                        if size < 1e-13 * ymax && gnorm < 1e-6 * (1.0 + j.abs()) {
                            converged = true;
                        }
                        break;
                    }
                    t /= 2.0;
                }
                if moved {
                    break;
                }
            }
            // J no longer changes (the gradient can stay large on the tiny end pieces)
            still = if moved && (j_before - j).abs() <= 1e-12 * j.abs().max(1.0) { still + 1 } else { 0 };
            if still >= 3 {
                converged = true;
            }
            if converged {
                break;
            }
            if !moved {
                // no step lowers J: a minimum up to rounding, or a saddle (stationary but not smallest)
                converged = gnorm < 1e-6 * (1.0 + j.abs());
                break;
            }
        }
        if !converged {
            return Err("extremal: Newton's method did not settle; there may be no smooth extremal between these end points (or try fewer pieces)".into());
        }
        self.note(format!(
            "A numerical extremal of the integral of L = {} from ({}, {}) to ({}, {}): the path is cut into {} straight pieces and the integral of L along them (3-point Gauss rule per piece) is made stationary by Newton's method ({} steps). x and y hold the path, J the value of the integral along it. Compare with the exact solution of eulerlagrange where there is one.",
            show(&primes(&lag.l, &lag.qs)),
            fmt(a),
            fmt(ya),
            fmt(b),
            fmt(yb),
            n,
            iters
        ));
        Ok(record(vec![("x", floats_row(&xs)), ("y", floats_row(&y)), ("J", Expr::float(j))]))
    }

    /// motion(L, [q], t, q0, v0, T[, frames]): solve the Euler–Lagrange equations numerically
    fn motion(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: motion(L, q, t, q0, v0, T) solves the equations of motion of the Lagrangian L(q, q', t) from position q0 and velocity v0 up to time T; motion(L, [q1 q2], t, [..], [..], T) for several coordinates; a last argument sets the number of time steps (2000)";
        let (ln, qn, tn, q0n, v0n, tt, sn) = match args {
            [l, q, t, a, b, c] => (l, q, t, a, b, c, None),
            [l, q, t, a, b, c, s] => (l, q, t, a, b, c, Some(s)),
            _ => return Err(usage.into()),
        };
        let lag = self.lagrangian(ln, qn, tn, "motion")?;
        let k = lag.qs.len();
        let vals = |s: &mut Session, n: &Node, what: &str| -> R<Vec<f64>> {
            let v = s.eval(n)?;
            let items: Vec<Expr> = match &v {
                Expr::Matrix(m) => m.iter().flatten().cloned().collect(),
                o => vec![o.clone()],
            };
            items
                .iter()
                .map(|x| evalf(x).num().map(|n| n.to_f64()).filter(|x| x.is_finite()))
                .collect::<Option<Vec<f64>>>()
                .ok_or_else(|| format!("motion: {} must be numbers, got {}", what, show(&v)))
        };
        let q0 = vals(self, q0n, "the start positions")?;
        let v0 = vals(self, v0n, "the start velocities")?;
        if q0.len() != k || v0.len() != k {
            return Err(format!("motion: give {} start position{} and as many velocities", k, if k > 1 { "s" } else { "" }));
        }
        let tmax = vals(self, tt, "the time T")?;
        let tmax = match tmax[..] {
            [t] if t > 0.0 => t,
            _ => return Err("motion: the time T must be a positive number".into()),
        };
        let steps = match sn {
            Some(s) => {
                let v = vals(self, s, "the number of steps")?;
                match v[..] {
                    [n] if n.fract() == 0.0 && (10.0..=1_000_000.0).contains(&n) => n as usize,
                    _ => return Err("motion: the number of steps is a whole number from 10 to 1000000".into()),
                }
            }
            None => 2000,
        };
        let stray: Vec<String> = lag.l.symbols().into_iter().filter(|s| !lag.vars().contains(s) && s != "pi" && s != "e").collect();
        if !stray.is_empty() {
            return Err(format!("motion: give {} a value first", stray.join(", ")));
        }
        // M(q, v, t) a = f(q, v, t) with M_ij = d2L/dv_i dv_j and
        // f_i = dL/dq_i - sum_j d2L/dv_i dq_j v_j - d2L/dv_i dt
        let mut mc = vec![];
        let mut fc = vec![];
        for qi in &lag.qs {
            let p = lag.d(&lag.l, &ph(qi, 1));
            let mut row = vec![];
            for qj in &lag.qs {
                row.push(lag.code(&lag.d(&p, &ph(qj, 1))));
            }
            mc.push(row);
            let mut terms = vec![lag.d(&lag.l, &ph(qi, 0)), neg(lag.d(&p, &lag.x))];
            for qj in &lag.qs {
                terms.push(neg(mul(vec![lag.d(&p, &ph(qj, 0)), Expr::sym(&ph(qj, 1))])));
            }
            fc.push(lag.code(&simplify(&add(terms))));
        }
        let en = lag.code(&lag.energy());
        let accel = |q: &[f64], v: &[f64], t: f64| -> Option<Vec<f64>> {
            let mut args: Vec<f64> = q.to_vec();
            args.extend_from_slice(v);
            args.push(t);
            let mut m: Vec<Vec<f64>> = mc.iter().map(|r| r.iter().map(|c| run(c, &args)).collect()).collect();
            let mut f: Vec<f64> = fc.iter().map(|c| run(c, &args)).collect();
            if gauss(&mut m, &mut f) { Some(f) } else { None }
        };
        let dt = tmax / steps as f64;
        let (mut q, mut v) = (q0.clone(), v0.clone());
        let mut ts = vec![0.0];
        let mut qrows: Vec<Vec<f64>> = q.iter().map(|&x| vec![x]).collect();
        let mut vrows: Vec<Vec<f64>> = v.iter().map(|&x| vec![x]).collect();
        let energy_at = |q: &[f64], v: &[f64], t: f64| {
            let mut a = q.to_vec();
            a.extend_from_slice(v);
            a.push(t);
            run(&en, &a)
        };
        let e0 = energy_at(&q, &v, 0.0);
        let stride = steps.div_ceil(2000).max(1);
        for s in 0..steps {
            let t = s as f64 * dt;
            let fail = || format!("motion: the equations break down at t = {} (the mass matrix d2L/dq'2 is singular there)", fmt(t));
            let a1 = accel(&q, &v, t).ok_or_else(fail)?;
            let q2: Vec<f64> = (0..k).map(|i| q[i] + dt / 2.0 * v[i]).collect();
            let v2: Vec<f64> = (0..k).map(|i| v[i] + dt / 2.0 * a1[i]).collect();
            let a2 = accel(&q2, &v2, t + dt / 2.0).ok_or_else(fail)?;
            let q3: Vec<f64> = (0..k).map(|i| q[i] + dt / 2.0 * v2[i]).collect();
            let v3: Vec<f64> = (0..k).map(|i| v[i] + dt / 2.0 * a2[i]).collect();
            let a3 = accel(&q3, &v3, t + dt / 2.0).ok_or_else(fail)?;
            let q4: Vec<f64> = (0..k).map(|i| q[i] + dt * v3[i]).collect();
            let v4: Vec<f64> = (0..k).map(|i| v[i] + dt * a3[i]).collect();
            let a4 = accel(&q4, &v4, t + dt).ok_or_else(fail)?;
            for i in 0..k {
                q[i] += dt / 6.0 * (v[i] + 2.0 * v2[i] + 2.0 * v3[i] + v4[i]);
                v[i] += dt / 6.0 * (a1[i] + 2.0 * a2[i] + 2.0 * a3[i] + a4[i]);
            }
            if q.iter().chain(&v).any(|x| !x.is_finite()) {
                return Err(format!("motion: the solution blows up near t = {}", fmt(t)));
            }
            if (s + 1) % stride == 0 || s + 1 == steps {
                ts.push((s + 1) as f64 * dt);
                for i in 0..k {
                    qrows[i].push(q[i]);
                    vrows[i].push(v[i]);
                }
            }
        }
        let e1 = energy_at(&q, &v, tmax);
        let timeless = !lag.l.contains_sym(&lag.x);
        self.note(format!(
            "Equations of motion of L = {} solved by Runge–Kutta (4th order) with {} steps (dt = {}). t holds the times, q the coordinate{} and v the velocit{}{}.{}",
            show(&primes(&lag.l, &lag.qs)),
            steps,
            format!("{:.3e}", dt),
            if k > 1 { "s, one row each in the order given" } else { "" },
            if k > 1 { "ies" } else { "y" },
            if k > 1 { "" } else { " as rows" },
            if timeless {
                format!(" The energy changed from {} to {} (it should stay constant; a big change means too few steps).", fmt(e0), fmt(e1))
            } else {
                String::new()
            }
        ));
        let rows = |r: &[Vec<f64>]| -> Expr {
            if r.len() == 1 { floats_row(&r[0]) } else { Expr::Matrix(r.iter().map(|x| x.iter().map(|&v| Expr::float(v)).collect()).collect()) }
        };
        Ok(record(vec![("t", floats_row(&ts)), ("q", rows(&qrows)), ("v", rows(&vrows))]))
    }
}
