//! Commands for differential equations: assume, laplace, ilaplace, partfrac,
//! fourier, seriessolve.

use crate::assume;
use crate::commands::ident;
use crate::eval::{R, Session};
use crate::expr::Expr;
use crate::laplace;
use crate::parser::{Node, Op};
use crate::print::show;

impl Session {
    pub(crate) fn de_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "assume" => self.assume_cmd(args),
            "forget" | "unassume" => {
                for a in args {
                    match ident(a, "forget") {
                        Ok(n) => assume::forget(&n),
                        Err(e) => return Some(Err(e)),
                    }
                }
                if args.is_empty() {
                    assume::clear();
                }
                Ok(Expr::Str(assume::list().join(", ")))
            }
            "assumptions" => Ok(Expr::Str(assume::list().join(", "))),
            "laplace" => self.laplace_cmd(args),
            "ilaplace" => self.ilaplace_cmd(args),
            "partfrac" | "apart" => self.partfrac_cmd(args),
            "fourier" => self.fourier_cmd(args),
            _ => return None,
        };
        Some(r)
    }

    fn assume_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: assume(a > 0), assume(a < 0), assume(n, integer) or assume(n, integer, positive)";
        let first = args.first().ok_or(usage)?;
        match first {
            Node::Bin(op @ (Op::Gt | Op::Lt | Op::Ge | Op::Le), a, b) => {
                let (name, other, flip) = match (&**a, &**b) {
                    (Node::Ident(n), o) => (n.clone(), o, false),
                    (o, Node::Ident(n)) => (n.clone(), o, true),
                    _ => return Err(usage.into()),
                };
                let v = self.eval(other)?;
                if !v.is_zero() {
                    return Err("assume: compare with 0, as in assume(a > 0)".into());
                }
                let gt = matches!(op, Op::Gt | Op::Ge) != flip;
                assume::set_sign(&name, if gt { 1 } else { -1 });
            }
            Node::Ident(n) => {
                let flags: Vec<String> = args[1..]
                    .iter()
                    .map(|a| match a {
                        Node::Ident(s) | Node::Str(s) => Ok(s.clone()),
                        _ => Err(usage.to_string()),
                    })
                    .collect::<R<_>>()?;
                if flags.is_empty() {
                    return Err(usage.into());
                }
                for fl in flags {
                    match fl.as_str() {
                        "integer" => assume::set_integer(n),
                        "positive" => assume::set_sign(n, 1),
                        "negative" => assume::set_sign(n, -1),
                        _ => return Err(format!("assume: unknown property {}; use integer, positive or negative", fl)),
                    }
                }
            }
            _ => return Err(usage.into()),
        }
        let all = assume::list();
        self.note(format!(
            "Assumptions now: {}. They decide signs in limits and integrals (e^(-a t) → 0 as t → ∞ when a > 0, sqrt(a²) = a) and simplify sin(nπ) = 0, cos(nπ) = (−1)^n for an integer n. forget(a) removes them.",
            all.join(", ")
        ));
        Ok(Expr::Str(all.join(", ")))
    }

    fn laplace_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (f, t, s) = match args {
            [f] => (f, "t".to_string(), "s".to_string()),
            [f, t, s] => (f, ident(t, "t")?, ident(s, "s")?),
            _ => return Err("usage: laplace(f, t, s)".into()),
        };
        let fx = crate::eval::equation_lhs(&self.eval_hidden(f, std::slice::from_ref(&t))?);
        let r = laplace::laplace(&fx, &t, &s)?;
        self.note(format!(
            "L{{f}}({s}) = ∫₀^∞ f({t}) e^(−{s}{t}) d{t}, from the table: L{{1}} = 1/{s}, L{{{t}^n}} = n!/{s}^(n+1), L{{e^(a{t})}} = 1/({s} − a), L{{sin(b{t})}} = b/({s}² + b²), L{{cos(b{t})}} = {s}/({s}² + b²), and the rules e^(a{t})f ↦ F({s} − a), {t}·f ↦ −F′({s}), heaviside({t} − a)f({t} − a) ↦ e^(−a{s})F({s}), L{{y′}} = {s}Y − y(0). Valid for {s} large enough that the integral converges."
        ));
        Ok(r)
    }

    fn ilaplace_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (f, s, t) = match args {
            [f] => (f, "s".to_string(), "t".to_string()),
            [f, s, t] => (f, ident(s, "s")?, ident(t, "t")?),
            _ => return Err("usage: ilaplace(F, s, t)".into()),
        };
        let fx = self.eval_hidden(f, std::slice::from_ref(&s))?;
        let r = laplace::ilaplace(&fx, &s, &t)?;
        let pf = laplace::partfrac_expr(&fx, &s).ok();
        self.note(format!(
            "Inverse transform: split F into partial fractions{}, then read each piece backwards from the table: c/({s} − r)^n ↦ c {t}^(n−1) e^(r{t})/(n−1)!, quadratic factors ↦ e^(a{t}) cos and sin, and a factor e^(−a{s}) delays the piece to start at {t} = a (heaviside).",
            pf.map(|p| format!(" ({})", show(&p))).unwrap_or_default()
        ));
        Ok(r)
    }

    fn partfrac_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let [f, x] = args else {
            return Err("usage: partfrac(f, x)".into());
        };
        let x = ident(x, "x")?;
        let fx = self.eval_hidden(f, std::slice::from_ref(&x))?;
        let r = laplace::partfrac_expr(&fx, &x)?;
        self.note("Partial fractions: the denominator is factored over the rationals and f is written as a polynomial plus one fraction per factor (and per power of a repeated factor), with constant numerators over linear factors and linear numerators over quadratic ones.");
        Ok(r)
    }

    /// fourier(f, x, L, N[, "sine" | "cosine"]): the Fourier series of f on
    /// [-L, L] (or the half-range sine or cosine series on [0, L]) up to N
    fn fourier_cmd(&mut self, args: &[Node]) -> R<Expr> {
        use crate::build::*;
        use crate::calc::simplify;
        let usage = "usage: fourier(f, x, L, N) on [-L, L], or fourier(f, x, L, N, \"sine\") / \"cosine\" for the half-range series on [0, L]";
        if args.len() != 4 && args.len() != 5 {
            return Err(usage.into());
        }
        let x = ident(&args[1], "x")?;
        let fx = crate::eval::equation_lhs(&self.eval_hidden(&args[0], std::slice::from_ref(&x))?);
        let l = self.eval(&args[2])?;
        let lv = crate::numeric::value(&l);
        if lv.is_nan() || lv <= 0.0 {
            return Err("fourier: L must be a positive number (the interval is -L..L)".into());
        }
        let nmax = match self.eval(&args[3])?.num().and_then(|k| k.as_int()) {
            Some(k) if (1..=60).contains(&k) => k as usize,
            _ => return Err("fourier: N must be a whole number from 1 to 60".into()),
        };
        let kind = match args.get(4) {
            None => "full",
            Some(Node::Str(k) | Node::Ident(k)) if k == "sine" || k == "sin" => "sine",
            Some(Node::Str(k) | Node::Ident(k)) if k == "cosine" || k == "cos" => "cosine",
            _ => return Err(usage.into()),
        };
        let xs = Expr::sym(&x);
        let pi = Expr::Const(crate::expr::Const::Pi);
        let angle = |k: &Expr| simplify(&mul(vec![k.clone(), pi.clone(), xs.clone(), pow(l.clone(), Expr::int(-1))]));
        let (lo, scale) = if kind == "full" {
            (neg(l.clone()), pow(l.clone(), Expr::int(-1)))
        } else {
            (Expr::zero(), div(Expr::int(2), l.clone()))
        };
        let coef = |k: &Expr, f: Option<crate::expr::Func>| -> R<Expr> {
            let g = match f {
                Some(t) => mul2(fx.clone(), func(t, angle(k))),
                None => fx.clone(),
            };
            let v = crate::cas_cmd::definite(&simplify(&g), &x, &lo, &l, 0)?;
            Ok(simplify(&mul2(scale.clone(), v)))
        };
        use crate::expr::Func::{Cos, Sin};
        let (want_a, want_b) = (kind != "sine", kind != "cosine");
        // the general coefficients, with n a positive integer
        let nn = if fx.contains_sym("n") { "k" } else { "n" };
        let before = assume::facts(nn);
        assume::set_integer(nn);
        assume::set_sign(nn, 1);
        let nv = Expr::sym(nn);
        let gen_a = if want_a { coef(&nv, Some(Cos)).ok() } else { None };
        let gen_b = if want_b { coef(&nv, Some(Sin)).ok() } else { None };
        assume::forget(nn);
        if before.integer {
            assume::set_integer(nn);
        }
        if let Some(sg) = before.sign {
            assume::set_sign(nn, sg);
        }
        let mut terms = vec![];
        let a0 = if want_a { coef(&Expr::zero(), None)? } else { Expr::zero() };
        terms.push(simplify(&div(a0.clone(), Expr::int(2))));
        for k in 1..=nmax {
            let kv = Expr::int(k as i128);
            if want_a {
                let a = coef(&kv, Some(Cos))?;
                terms.push(mul2(a, func(Cos, angle(&kv))));
            }
            if want_b {
                let b = coef(&kv, Some(Sin))?;
                terms.push(mul2(b, func(Sin, angle(&kv))));
            }
        }
        let series = add(terms.into_iter().map(|t| simplify(&t)).collect());
        let w = show(&angle(&nv));
        let range = if kind == "full" { format!("[-{}, {}]", show(&l), show(&l)) } else { format!("[0, {}]", show(&l)) };
        let sv = simplify(&scale);
        let sc = if sv == Expr::one() { String::new() } else { format!("({}) ", show(&sv)) };
        let mut msg = match kind {
            "full" => format!("Fourier series on {range}: f ≈ a0/2 + Σ (a_{nn} cos({w}) + b_{nn} sin({w})), with a_{nn} = {sc}∫ f cos({w}) dx and b_{nn} = {sc}∫ f sin({w}) dx over {range}."),
            "sine" => format!("Half-range sine series on {range} (f extended as an odd function): f ≈ Σ b_{nn} sin({w}), with b_{nn} = {sc}∫ f sin({w}) dx over {range}."),
            _ => format!("Half-range cosine series on {range} (f extended as an even function): f ≈ a0/2 + Σ a_{nn} cos({w}), with a_{nn} = {sc}∫ f cos({w}) dx over {range}."),
        };
        if want_a {
            msg.push_str(&format!(" a0 = {}.", show(&a0)));
        }
        if let Some(a) = gen_a {
            msg.push_str(&format!(" a_{nn} = {}.", show(&a)));
        }
        if let Some(b) = gen_b {
            msg.push_str(&format!(" b_{nn} = {}.", show(&b)));
        }
        msg.push_str(&format!(" The answer keeps the terms up to {nn} = {nmax}."));
        self.note(msg);
        Ok(series)
    }
}
