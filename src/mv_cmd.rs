//! Commands for multivariable calculus, optimization, special functions and
//! the field plots (contour, implicitplot, fieldplot).

use crate::build::*;
use crate::calc::{deriv, subst};
use crate::commands::{ident, label, names, obj};
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::numeric::{self, run};
use crate::parser::Node;
use crate::plot::{self, Figure, Series};
use crate::print::show;
use crate::special;
use crate::vcalc;

fn row(v: Vec<Expr>) -> Expr {
    Expr::Matrix(vec![v])
}

fn rows(v: Vec<Vec<Expr>>) -> Expr {
    Expr::Matrix(v)
}

fn flat(e: Expr) -> Vec<Expr> {
    match e {
        Expr::Matrix(m) => m.into_iter().flatten().collect(),
        other => vec![other],
    }
}

fn fmt_point(p: &[Expr]) -> String {
    format!("({})", p.iter().map(show).collect::<Vec<_>>().join(", "))
}

fn assign(vars: &[String], p: &[Expr]) -> String {
    vars.iter()
        .zip(p)
        .map(|(v, x)| format!("{} = {}", v, show(x)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// 0.1234 style numbers for notes
fn paren(v: f64) -> String {
    if v < 0.0 { format!("({})", num(v)) } else { num(v) }
}

fn num(v: f64) -> String {
    if v.fract() == 0.0 && v.abs() < 1e15 {
        format!("{}", v as i64)
    } else {
        let r = (v * 1e10).round() / 1e10;
        crate::num::fmt_float(r)
    }
}

impl Session {
    /// Some(result) if f is one of the commands here
    pub(crate) fn mv_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        if special::is_special(f, args.len()) {
            return Some(self.special_fn(f, args));
        }
        let r = match f {
            "gamma" | "digamma" | "polygamma" | "erf" | "erfc" | "erfi" | "zeta" | "polylog" | "besselj"
            | "bessely" | "besseli" => Err(format!("{} takes {} argument(s); see help({})", f,
                special::NAMES.iter().find(|p| p.0 == f).unwrap().1, f)),
            "beta" => self.beta(args),
            "legendre" | "hermite" | "laguerre" | "chebyshev" | "chebyshevu" => self.orthopoly(f, args),
            "grad" | "gradient" => self.grad(args),
            "div" | "divergence" => self.div_cmd(args),
            "curl" => self.curl(args),
            "laplacian" => self.laplacian(args),
            "jacobian" => self.jacobian(args),
            "hessian" => self.hessian(args),
            "potential" => self.potential(args),
            "lineint" => self.lineint(args),
            "flux" | "surfint" => self.flux(f == "surfint", args),
            "critical" => self.critical(args),
            "classify" => self.classify(args),
            "minimize" | "maximize" => self.optimize(f == "maximize", args),
            "lagrange" => self.lagrange(args),
            "contour" => self.contour(args),
            "fieldplot" | "vectorfield" => self.fieldplot(args),
            _ => return None,
        };
        Some(r)
    }

    // ---------- special functions ----------

    fn special_fn(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let mut vals = vec![];
        for a in args {
            vals.push(self.eval(a)?);
        }
        // gamma([1 2 3]): element by element in the last argument
        if let Some(Expr::Matrix(m)) = vals.last().cloned() {
            let mut out = vec![];
            for r in m {
                let mut o = vec![];
                for e in r {
                    let mut a = vals.clone();
                    *a.last_mut().unwrap() = e;
                    special::check(f, &a).map_err(|e| format!("{}: {}", f, e))?;
                    o.push(special::apply(f, a));
                }
                out.push(o);
            }
            return Ok(Expr::Matrix(out));
        }
        if vals.iter().any(|v| !v.is_scalar()) {
            return Err(format!("{}: arguments must be expressions", f));
        }
        special::check(f, &vals).map_err(|e| format!("{}: {}", f, e))?;
        let r = special::apply(f, vals.clone());
        if self.teaching() {
            if let Some(t) = special_teach(f, &vals, &r) {
                self.note(t);
            }
        }
        Ok(r)
    }

    fn beta(&mut self, args: &[Node]) -> R<Expr> {
        let [a, b] = args else {
            return Err("usage: beta(a, b) = gamma(a) gamma(b) / gamma(a + b)".into());
        };
        let (a, b) = (self.eval(a)?, self.eval(b)?);
        let g = |x: Expr| special::apply("gamma", vec![x]);
        Ok(crate::ratfun::tidy(&div(
            mul2(g(a.clone()), g(b.clone())),
            g(add2(a, b)),
        )))
    }

    fn orthopoly(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let [n, x] = args else {
            return Err(format!("usage: {}(n, x), the polynomial of degree n in x", f));
        };
        let n = self.eval(n)?;
        let n = n
            .num()
            .filter(|v| v.is_exact() && v.is_integer())
            .and_then(|v| v.as_int())
            .filter(|v| (0..=200).contains(v))
            .ok_or_else(|| format!("{}(n, x): n must be a whole number from 0 to 200", f))?;
        let x = self.eval(x)?;
        let e = |x: &Expr| special::orthopoly(f, n as usize, x).unwrap();
        match x {
            Expr::Matrix(m) => Ok(Expr::Matrix(
                m.iter().map(|r| r.iter().map(e).collect()).collect(),
            )),
            x => Ok(e(&x)),
        }
    }

    // ---------- vector calculus ----------

    /// (vars, scale factors, system name) from [x y z] and an optional "spherical"
    fn coords(&mut self, cmd: &str, vars: &Node, system: Option<&Node>) -> R<(Vec<String>, Vec<crate::expr::Expr>, String)> {
        let vs = names(vars, &format!("{}: the variables", cmd))?;
        let sys = match system {
            None => "cartesian".to_string(),
            Some(Node::Str(s)) => s.clone(),
            Some(Node::Ident(s)) => s.clone(),
            Some(_) => return Err(format!("{}: the coordinate system is one of {}", cmd, vcalc::SYSTEMS)),
        };
        let h = vcalc::scale_factors(&sys, &vs).map_err(|e| format!("{}: {}", cmd, e))?;
        Ok((vs, h, sys))
    }

    fn field(&mut self, cmd: &str, n: &Node, vars: &[String]) -> R<Vec<Expr>> {
        let v = flat(self.eval_hidden(n, vars)?);
        if v.len() != vars.len() {
            return Err(format!(
                "{}: the field has {} components but there are {} variables",
                cmd,
                v.len(),
                vars.len()
            ));
        }
        Ok(v)
    }

    fn grad(&mut self, args: &[Node]) -> R<Expr> {
        let (f, v, s) = match args {
            [f, v] => (f, v, None),
            [f, v, s] => (f, v, Some(s)),
            _ => return Err("usage: grad(f, [x y z]) or grad(f, [r theta phi], \"spherical\")".into()),
        };
        let (vs, h, sys) = self.coords("grad", v, s)?;
        let fx = self.eval_hidden(f, &vs)?;
        if !fx.is_scalar() {
            return Err("grad needs a scalar function; for a vector field use jacobian".into());
        }
        let g = vcalc::grad(&fx, &vs, &h);
        let parts: Vec<String> = vs.iter().map(|x| format!("∂f/∂{}", x)).collect();
        let mut note = format!(
            "The gradient lists the partial derivatives [{}]. It points in the direction in which f increases fastest, its length is that fastest rate, and it is perpendicular to the level curves (or surfaces) of f.",
            parts.join(", ")
        );
        if sys != "cartesian" {
            note.push_str(&format!(
                " In {} coordinates each partial derivative is divided by its scale factor ({}), so the components are rates per unit of distance in the directions of the unit vectors.",
                sys,
                vs.iter().zip(&h).map(|(x, hh)| format!("h_{} = {}", x, show(hh))).collect::<Vec<_>>().join(", ")
            ));
        }
        self.note(note);
        Ok(row(g))
    }

    fn div_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (f, v, s) = match args {
            [f, v] => (f, v, None),
            [f, v, s] => (f, v, Some(s)),
            _ => return Err("usage: div([P Q R], [x y z]) or div(F, [r theta phi], \"spherical\")".into()),
        };
        let (vs, h, sys) = self.coords("div", v, s)?;
        let fs = self.field("div", f, &vs)?;
        let d = vcalc::divergence(&fs, &vs, &h);
        let mut note = "The divergence is the net outflow of the field per unit volume (per unit area in 2D): positive at a source, negative at a sink. In Cartesian coordinates it is the sum ∂P/∂x + ∂Q/∂y + ∂R/∂z.".to_string();
        if self.teaching() {
            let terms: Vec<String> = fs
                .iter()
                .zip(&vs)
                .map(|(c, x)| format!("∂({})/∂{} = {}", show(c), x, show(&crate::ratfun::tidy(&deriv(c, x)))))
                .collect();
            if sys == "cartesian" {
                note.push_str(&format!("\n\nHere: {}; the sum is {}.", terms.join(", "), show(&d)));
            }
        }
        if sys != "cartesian" {
            note.push_str(&format!(" In {} coordinates the formula includes the scale factors: (1/H) Σ ∂(H F_i / h_i)/∂q_i with H = {}.", sys, show(&mul(h.clone()))));
        }
        self.note(note);
        Ok(d)
    }

    fn curl(&mut self, args: &[Node]) -> R<Expr> {
        let (f, v, s) = match args {
            [f, v] => (f, v, None),
            [f, v, s] => (f, v, Some(s)),
            _ => return Err("usage: curl([P Q R], [x y z]) or curl([P Q], [x y])".into()),
        };
        let (vs, h, _) = self.coords("curl", v, s)?;
        let fs = self.field("curl", f, &vs)?;
        let c = vcalc::curl(&fs, &vs, &h).map_err(|e| format!("curl: {}", e))?;
        self.note(if vs.len() == 2 {
            "In the plane the curl is the number ∂Q/∂x − ∂P/∂y: twice the rate at which the field would spin a tiny paddle wheel (counterclockwise is positive). Green's theorem says its integral over a region equals the circulation around the boundary."
        } else {
            "The curl [∂R/∂y − ∂Q/∂z, ∂P/∂z − ∂R/∂x, ∂Q/∂x − ∂P/∂y] measures rotation: its direction is the axis a tiny paddle wheel would spin around, its length twice the spin rate. A field with zero curl (on a simply connected region) is a gradient; potential finds the function."
        });
        Ok(c)
    }

    fn laplacian(&mut self, args: &[Node]) -> R<Expr> {
        let (f, v, s) = match args {
            [f, v] => (f, v, None),
            [f, v, s] => (f, v, Some(s)),
            _ => return Err("usage: laplacian(f, [x y z]) or laplacian(f, [r theta phi], \"spherical\")".into()),
        };
        let (vs, h, _) = self.coords("laplacian", v, s)?;
        let fx = self.eval_hidden(f, &vs)?;
        let l = vcalc::laplacian(&fx, &vs, &h);
        self.note("The Laplacian is div(grad f): the sum of the second partial derivatives in Cartesian coordinates. It compares f at a point with its average nearby; functions with Laplacian 0 are harmonic (steady temperatures, electrostatic potentials in empty space).");
        Ok(l)
    }

    fn jacobian(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v] = args else {
            return Err("usage: jacobian([f g], [x y])".into());
        };
        let vs = names(v, "jacobian: the variables")?;
        let fs = flat(self.eval_hidden(f, &vs)?);
        let j = vcalc::jacobian(&fs, &vs);
        self.note("Row i of the Jacobian holds the partial derivatives of the i-th function. It is the best linear approximation of the map near a point; for a change of variables its determinant is the local area (or volume) scale factor, as in dx dy = |det J| dr dθ.");
        Ok(rows(j))
    }

    fn hessian(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v] = args else {
            return Err("usage: hessian(f, [x y])".into());
        };
        let vs = names(v, "hessian: the variables")?;
        let fx = self.eval_hidden(f, &vs)?;
        self.note("The Hessian holds the second partial derivatives ∂²f/∂x_i∂x_j; it is symmetric when they are continuous. At a critical point it decides the shape: all eigenvalues positive means a minimum, all negative a maximum, mixed signs a saddle (classify does this).");
        Ok(rows(vcalc::hessian(&fx, &vs)))
    }

    fn potential(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v] = args else {
            return Err("usage: potential([P Q R], [x y z]) finds phi with grad(phi) = F".into());
        };
        let vs = names(v, "potential: the variables")?;
        let fs = self.field("potential", f, &vs)?;
        match vcalc::potential(&fs, &vs) {
            Some(p) => {
                self.note("A potential φ has grad φ = F, so the work of F along any path is φ(end) − φ(start): the field is conservative. Found by integrating the first component in its variable and fixing what the other components still need. Any constant can be added.");
                Ok(p)
            }
            None => {
                self.note("[] means no potential: the mixed partial derivatives disagree (for example ∂P/∂y ≠ ∂Q/∂x), so the curl is not zero and the work depends on the path.");
                Ok(Expr::Matrix(vec![]))
            }
        }
    }

    /// lineint(F, [x y], [x(t) y(t)], t, a, b)
    fn lineint(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v, r, t, a, b] = args else {
            return Err("usage: lineint(F, [x y], [x(t) y(t)], t, a, b) for the work of a field F, or lineint(f, [x y], [x(t) y(t)], t, a, b) for a scalar f (arc length)".into());
        };
        let vs = names(v, "lineint: the variables")?;
        let t = ident(t, "lineint: the parameter")?;
        let mut hidden = vs.clone();
        hidden.push(t.clone());
        let fx = self.eval_hidden(f, &hidden)?;
        let path = flat(self.eval_hidden(r, std::slice::from_ref(&t))?);
        if path.len() != vs.len() {
            return Err("lineint: the path needs one expression per variable".into());
        }
        let (a, b) = (self.eval(a)?, self.eval(b)?);
        let on = |e: &Expr| {
            let mut e = e.clone();
            for (x, p) in vs.iter().zip(&path) {
                e = subst(&e, x, p);
            }
            e
        };
        let dr: Vec<Expr> = path.iter().map(|p| deriv(p, &t)).collect();
        let (integrand, kind) = match &fx {
            Expr::Matrix(_) => {
                let fs = flat(fx.clone());
                if fs.len() != vs.len() {
                    return Err("lineint: the field needs one component per variable".into());
                }
                (add(fs.iter().zip(&dr).map(|(c, d)| mul2(on(c), d.clone())).collect()), "work")
            }
            s => {
                let n2 = add(dr.iter().map(|d| pow(d.clone(), Expr::int(2))).collect());
                let speed = sqrt_pos(&n2, &[(t.clone(), numeric::value(&self.eval(&args[4])?), numeric::value(&self.eval(&args[5])?))]);
                (mul2(on(s), speed), "scalar")
            }
        };
        let integrand = crate::calc::simplify(&integrand);
        let res = integrate_exact(&integrand, &t, &a, &b)
            .map_err(|e| e.replace("integrate", "lineint"))?;
        let res = crate::ratfun::tidy(&positive_params(&res, &[]));
        self.note(if kind == "work" {
            format!("The work ∫ F · dr: along the path, F · r'(t) = {}, integrated over t from {} to {}. For a gradient field it depends only on the end points.", show(&integrand), show(&a), show(&b))
        } else {
            format!("The integral ∫ f ds of a scalar along the path, with ds = |r'(t)| dt: integrand {} over t from {} to {}. With f = 1 it is the length of the curve.", show(&integrand), show(&a), show(&b))
        });
        Ok(res)
    }

    /// flux(F, [x y z], [x(u,v) y(u,v) z(u,v)], u, a, b, v, c, d)
    fn flux(&mut self, scalar: bool, args: &[Node]) -> R<Expr> {
        let cmd = if scalar { "surfint" } else { "flux" };
        let [f, vars, r, u, a, b, v, c, d] = args else {
            return Err(format!(
                "usage: {}({}, [x y z], [x(u,v) y(u,v) z(u,v)], u, a, b, v, c, d)",
                cmd,
                if scalar { "f" } else { "F" }
            ));
        };
        let vs = names(vars, &format!("{}: the variables", cmd))?;
        if vs.len() != 3 {
            return Err(format!("{}: surfaces live in 3 variables, e.g. [x y z]", cmd));
        }
        let (u, v) = (ident(u, "u")?, ident(v, "v")?);
        let mut hidden = vs.clone();
        hidden.push(u.clone());
        hidden.push(v.clone());
        let fx = self.eval_hidden(f, &hidden)?;
        let surf = flat(self.eval_hidden(r, &[u.clone(), v.clone()])?);
        if surf.len() != 3 {
            return Err(format!("{}: the surface needs three expressions [x(u,v) y(u,v) z(u,v)]", cmd));
        }
        let ru: Vec<Expr> = surf.iter().map(|p| deriv(p, &u)).collect();
        let rv: Vec<Expr> = surf.iter().map(|p| deriv(p, &v)).collect();
        let n = [
            sub(mul2(ru[1].clone(), rv[2].clone()), mul2(ru[2].clone(), rv[1].clone())),
            sub(mul2(ru[2].clone(), rv[0].clone()), mul2(ru[0].clone(), rv[2].clone())),
            sub(mul2(ru[0].clone(), rv[1].clone()), mul2(ru[1].clone(), rv[0].clone())),
        ];
        let on = |e: &Expr| {
            let mut e = e.clone();
            for (x, p) in vs.iter().zip(&surf) {
                e = subst(&e, x, p);
            }
            e
        };
        let integrand = if scalar {
            if !fx.is_scalar() {
                return Err("surfint integrates a scalar; for a field use flux".into());
            }
            let n2 = add(n.iter().map(|c| pow(c.clone(), Expr::int(2))).collect());
            let ranges = [
                (u.clone(), numeric::value(&self.eval(&args[4])?), numeric::value(&self.eval(&args[5])?)),
                (v.clone(), numeric::value(&self.eval(&args[7])?), numeric::value(&self.eval(&args[8])?)),
            ];
            mul2(on(&fx), sqrt_pos(&n2, &ranges))
        } else {
            let fs = flat(fx);
            if fs.len() != 3 {
                return Err("flux needs a field with 3 components".into());
            }
            add(fs.iter().zip(&n).map(|(c, k)| mul2(on(c), k.clone())).collect())
        };
        let integrand = crate::ratfun::tidy(&crate::calc::simplify(&integrand));
        let (a, b, c, d) = (self.eval(a)?, self.eval(b)?, self.eval(c)?, self.eval(d)?);
        let exact = integrate_exact(&integrand, &u, &a, &b)
            .and_then(|inner| integrate_exact(&inner, &v, &c, &d))
            .map(|r| crate::ratfun::tidy(&positive_params(&r, &[])));
        let res = match exact {
            Ok(r) => r,
            Err(_) => {
                // numeric double integral
                let vals: Vec<f64> = [&a, &b, &c, &d].iter().map(|e| numeric::value(e)).collect();
                if vals.iter().any(|x| !x.is_finite()) || !numeric::free(&integrand, &[&u, &v]).is_empty() {
                    return Err(format!("{}: can't integrate {} exactly; give numbers for the limits and the constants", cmd, show(&integrand)));
                }
                let code = numeric::compile(&integrand, &[&u, &v]);
                let q = crate::cas::quad(
                    &|vv: f64| crate::cas::quad(&|uu: f64| run(&code, &[uu, vv]), vals[0], vals[1]).unwrap_or(f64::NAN),
                    vals[2],
                    vals[3],
                )
                .map_err(|e| format!("{}: {}", cmd, e))?;
                Expr::float(q)
            }
        };
        self.note(if scalar {
            "The surface integral ∫∫ f dS with dS = |r_u × r_v| du dv; with f = 1 it is the area of the surface."
        } else {
            "The flux ∫∫ F · n dS = ∫∫ F(r(u,v)) · (r_u × r_v) du dv: how much of the field passes through the surface, counted in the direction of r_u × r_v (swap u and v to flip it). The divergence theorem equates the flux out of a closed surface with the integral of div F inside."
        });
        Ok(res)
    }

    // ---------- optimization ----------

    fn critical(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v] = args else {
            return Err("usage: critical(f, [x y]): the points where grad f = 0".into());
        };
        let vs = names(v, "critical: the variables")?;
        let fx = self.eval_hidden(f, &vs)?;
        let g: Vec<Expr> = vs.iter().map(|x| deriv(&fx, x)).collect();
        let pts = vcalc::solve_system(&g, &vs).ok_or_else(|| {
            "critical: can't solve grad f = 0 exactly (it is not a polynomial system); use minimize(f, vars, start) or fsolve with a guess".to_string()
        })?;
        let mut lines = vec![];
        for p in &pts {
            let val = vcalc::at(&fx, &vs, p);
            let pf: Vec<f64> = p.iter().map(numeric::value).collect();
            let kind = vcalc::classify(&vcalc::hessian_at(&fx, &vs, &pf));
            let vs_ = if val.num().is_some_and(|n| !n.is_exact()) { num(numeric::value(&val)) } else { show(&val) };
            lines.push(format!("- {}: f = {}, {}", fmt_point(p), vs_, kind_text(kind)));
        }
        let mut note = format!(
            "Critical points are where every partial derivative is 0 (grad f = 0); Galileo solved {} and lists the real solutions, one per row.",
            g.iter().map(|e| format!("{} = 0", show(&crate::ratfun::tidy(e)))).collect::<Vec<_>>().join(", ")
        );
        note.push_str(vcalc::family_note(&pts));
        if !lines.is_empty() {
            note.push_str(&format!(" The second-derivative test (classify) says:\n\n{}", lines.join("\n")));
        } else {
            note.push_str(" There are none.");
        }
        self.note(note);
        if pts.len() == 1 {
            return Ok(row(pts[0].clone()));
        }
        Ok(rows(pts))
    }

    fn classify(&mut self, args: &[Node]) -> R<Expr> {
        let [f, v, p] = args else {
            return Err("usage: classify(f, [x y], [x0 y0]) gives \"min\", \"max\", \"saddle\" or \"inconclusive\"".into());
        };
        let vs = names(v, "classify: the variables")?;
        let fx = self.eval_hidden(f, &vs)?;
        let pt = flat(self.eval(p)?);
        if pt.len() != vs.len() {
            return Err("classify: the point needs one value per variable".into());
        }
        let pf: Vec<f64> = pt.iter().map(numeric::value).collect();
        if pf.iter().any(|x| !x.is_finite()) {
            return Err("classify: the point must be numbers".into());
        }
        let g: Vec<f64> = vs
            .iter()
            .map(|x| numeric::run(&numeric::compile(&deriv(&fx, x), &vs.iter().map(|s| s.as_str()).collect::<Vec<_>>()), &pf))
            .collect();
        let h = vcalc::hessian_at(&fx, &vs, &pf);
        let kind = vcalc::classify(&h);
        let ev = vcalc::sym_eigenvalues(&h);
        let mut note = String::new();
        if g.iter().any(|x| x.abs() > 1e-8) {
            note.push_str("Careful: this is not a critical point (grad f is not 0 here), so the test does not apply. ");
        }
        let hs = h.iter().map(|r| r.iter().map(|x| num(*x)).collect::<Vec<_>>().join(", ")).collect::<Vec<_>>().join("; ");
        note.push_str(&format!(
            "The Hessian at {} is [{}] with eigenvalues {}. All positive: the graph curves up in every direction (minimum). All negative: maximum. Both signs: a saddle. A zero eigenvalue leaves the test inconclusive.",
            fmt_point(&pt),
            hs,
            ev.iter().map(|x| num(*x)).collect::<Vec<_>>().join(", ")
        ));
        if self.teaching() && vs.len() == 2 {
            let d = h[0][0] * h[1][1] - h[0][1] * h[1][0];
            note.push_str(&format!(
                "\n\nWith two variables the same test reads: D = f_xx f_yy − f_xy² = {} · {} − {}² = {}. D > 0 and f_xx > 0: minimum; D > 0 and f_xx < 0: maximum; D < 0: saddle; D = 0: inconclusive.",
                paren(h[0][0]), paren(h[1][1]), paren(h[0][1]), num(d)
            ));
        }
        self.note(note);
        Ok(Expr::Str(kind.into()))
    }

    /// minimize(f, x, a, b), minimize(f, [x y]), minimize(f, [x y], [x0 y0])
    fn optimize(&mut self, max: bool, args: &[Node]) -> R<Expr> {
        let cmd = if max { "maximize" } else { "minimize" };
        let word = if max { "maximum" } else { "minimum" };
        match args {
            [f, x, a, b] => {
                let x = ident(x, &format!("{}: x", cmd))?;
                let fx = self.eval_hidden(f, std::slice::from_ref(&x))?;
                let (a, b) = (self.eval(a)?, self.eval(b)?);
                let (av, bv) = (numeric::value(&a), numeric::value(&b));
                if !(av.is_finite() && bv.is_finite() && av < bv) {
                    return Err(format!("{}(f, x, a, b): a and b must be numbers with a < b", cmd));
                }
                let df = deriv(&fx, &x);
                let mut cands: Vec<Expr> = vec![a.clone(), b.clone()];
                let crit = match crate::cas::solve(&df, &x) {
                    Ok(rs) => rs
                        .into_iter()
                        .filter(|r| numeric::value(r).is_finite())
                        .filter(|r| {
                            let v = numeric::value(r);
                            v > av && v < bv
                        })
                        .collect::<Vec<_>>(),
                    Err(_) => {
                        // scan for sign changes of f' and polish them
                        let c = numeric::compile(&df, &[&x]);
                        let n = 2000;
                        let mut out = vec![];
                        let mut prev = run(&c, &[av]);
                        for k in 1..=n {
                            let t = av + (bv - av) * k as f64 / n as f64;
                            let cur = run(&c, &[t]);
                            if prev.is_finite() && cur.is_finite() && prev * cur < 0.0 {
                                let t0 = t - (bv - av) / n as f64 / 2.0;
                                if let Ok(r) = crate::cas::fsolve(&df, &x, t0) {
                                    if r > av && r < bv {
                                        out.push(Expr::float(r));
                                    }
                                }
                            }
                            prev = cur;
                        }
                        out
                    }
                };
                cands.extend(crit.iter().cloned());
                let fc = numeric::compile(&fx, &[&x]);
                let mut best: Option<(Expr, f64)> = None;
                let mut table = vec![];
                for c in &cands {
                    let v = run(&fc, &[numeric::value(c)]);
                    if !v.is_finite() {
                        continue;
                    }
                    table.push(format!("- {} = {}: f = {}", x, show(c), show(&vcalc::at(&fx, std::slice::from_ref(&x), std::slice::from_ref(c)))));
                    let better = match &best {
                        None => true,
                        Some((_, bvv)) => if max { v > *bvv + 1e-12 } else { v < *bvv - 1e-12 },
                    };
                    if better {
                        best = Some((c.clone(), v));
                    }
                }
                let (bx, bv) = best.ok_or_else(|| format!("{}: f has no real value on the interval", cmd))?;
                let bval = vcalc::at(&fx, std::slice::from_ref(&x), std::slice::from_ref(&bx));
                let ties: Vec<String> = cands
                    .iter()
                    .filter(|c| **c != bx)
                    .filter(|c| (run(&fc, &[numeric::value(c)]) - bv).abs() <= 1e-12 * (1.0 + bv.abs()))
                    .map(show)
                    .collect();
                let also = if ties.is_empty() { String::new() } else { format!(" (reached again at {} = {})", x, ties.join(", ")) };
                self.note(format!(
                    "Closed interval method: a continuous f on [{}, {}] takes its {} at a critical point (f' = 0) or at an end point. Candidates:\n\n{}\n\nThe {} is f = {} at {} = {}{}.",
                    show(&a), show(&b), word, table.join("\n"), word, show(&bval), x, show(&bx), also
                ));
                Ok(bx)
            }
            [f, v] => {
                let vs = names(v, &format!("{}: the variables", cmd))?;
                let fx = self.eval_hidden(f, &vs)?;
                let g: Vec<Expr> = vs.iter().map(|x| deriv(&fx, x)).collect();
                let pts = vcalc::solve_system(&g, &vs).ok_or_else(|| {
                    format!("{}: can't find the critical points exactly; give a starting point: {}(f, vars, [x0 y0])", cmd, cmd)
                })?;
                let want = if max { "max" } else { "min" };
                let mut best: Option<(Vec<Expr>, f64)> = None;
                let fc = numeric::compile(&fx, &vs.iter().map(|s| s.as_str()).collect::<Vec<_>>());
                for p in &pts {
                    let pf: Vec<f64> = p.iter().map(numeric::value).collect();
                    if vcalc::classify(&vcalc::hessian_at(&fx, &vs, &pf)) != want {
                        continue;
                    }
                    let val = run(&fc, &pf);
                    let better = match &best {
                        None => true,
                        Some((_, b)) => if max { val > *b } else { val < *b },
                    };
                    if better {
                        best = Some((p.clone(), val));
                    }
                }
                let Some((p, _)) = best else {
                    self.note(format!("No critical point is a local {}; [] means none found. The function may have no {} (it can run off to infinity), or the {} may sit on a boundary you have in mind.", word, word, word));
                    return Ok(Expr::Matrix(vec![]));
                };
                self.note(format!(
                    "Among the critical points (grad f = 0), the local {} with the {} value: f = {} at {}. Galileo did not check what f does far away or on a boundary; a local {} need not be global.",
                    word, if max { "largest" } else { "smallest" }, show(&vcalc::at(&fx, &vs, &p)), assign(&vs, &p), word
                ));
                Ok(if p.len() == 1 { p[0].clone() } else { row(p) })
            }
            [f, v, s] => {
                let vs = names(v, &format!("{}: the variables", cmd))?;
                let fx = self.eval_hidden(f, &vs)?;
                let start = self.numbers(s, &format!("{}: the starting point", cmd))?;
                if start.len() != vs.len() {
                    return Err(format!("{}: the starting point needs one number per variable", cmd));
                }
                let target = if max { neg(fx.clone()) } else { fx.clone() };
                let (p, val, it) = vcalc::bfgs(&target, &vs, &start).map_err(|e| format!("{}: {}", cmd, e))?;
                let val = if max { -val } else { val };
                let kind = vcalc::classify(&vcalc::hessian_at(&fx, &vs, &p));
                let want = if max { "max" } else { "min" };
                let warn = if kind != want {
                    format!(" Careful: the second-derivative test at this point gives \"{}\", not a local {}: the search stopped where the gradient is 0. Try another start.", kind_text(kind), word)
                } else {
                    String::new()
                };
                self.note(format!(
                    "Numeric search (BFGS: steps downhill along the gradient, learning the curvature as it goes) from {} reached a local {} after {} steps: f = {} at {}. Another start can find another local {}.",
                    fmt_point(&start.iter().map(|x| Expr::float(*x)).collect::<Vec<_>>()),
                    word, it, num(val),
                    vs.iter().zip(&p).map(|(x, v)| format!("{} = {}", x, num(*v))).collect::<Vec<_>>().join(", "),
                    word
                ) + &warn);
                // 10 significant digits: the search is not more accurate than that
                let pe: Vec<Expr> = p
                    .iter()
                    .map(|x| {
                        if *x == 0.0 || x.abs() < 1e-9 {
                            Expr::float(0.0)
                        } else {
                            let m = 10f64.powi(9 - x.abs().log10().floor() as i32);
                            Expr::float((x * m).round() / m)
                        }
                    })
                    .collect();
                Ok(if pe.len() == 1 { pe[0].clone() } else { row(pe) })
            }
            _ => Err(format!("usage: {}(f, x, a, b) on an interval, {}(f, [x y]) via critical points, or {}(f, [x y], [x0 y0]) numerically from a start", cmd, cmd, cmd)),
        }
    }

    /// lagrange(f, g, [x y]) or lagrange(f, [g1 g2], [x y z]): extremes of f with g = 0
    fn lagrange(&mut self, args: &[Node]) -> R<Expr> {
        let [f, g, v] = args else {
            return Err("usage: lagrange(f, g, [x y]) for the extremes of f on the curve g = 0 (or [g1 g2] for two constraints)".into());
        };
        let vs = names(v, "lagrange: the variables")?;
        let fx = self.eval_hidden(f, &vs)?;
        let gs: Vec<Expr> = flat(self.eval_hidden(g, &vs)?)
            .into_iter()
            .map(|e| crate::eval::equation_lhs(&e))
            .collect();
        let lams: Vec<String> = (0..gs.len())
            .map(|k| if gs.len() == 1 { "lambda".to_string() } else { format!("lambda{}", k + 1) })
            .collect();
        let mut eqs = vec![];
        for x in &vs {
            let mut e = deriv(&fx, x);
            for (gk, l) in gs.iter().zip(&lams) {
                e = sub(e, mul2(Expr::sym(l), deriv(gk, x)));
            }
            eqs.push(e);
        }
        eqs.extend(gs.iter().cloned());
        let mut all = vs.clone();
        all.extend(lams.iter().cloned());
        let sols = vcalc::solve_system(&eqs, &all).ok_or_else(|| {
            "lagrange: can't solve the Lagrange system exactly (not polynomial); try minimize on a parametrization of the constraint".to_string()
        })?;
        let n = vs.len();
        let pts: Vec<Vec<Expr>> = sols.iter().map(|s| s[..n].to_vec()).collect();
        let vals: Vec<(Expr, f64)> = pts
            .iter()
            .map(|p| {
                let v = vcalc::at(&fx, &vs, p);
                let fv = numeric::value(&v);
                (v, fv)
            })
            .collect();
        let hi = vals.iter().map(|v| v.1).fold(f64::NEG_INFINITY, f64::max);
        let lo = vals.iter().map(|v| v.1).fold(f64::INFINITY, f64::min);
        let lines: Vec<String> = sols
            .iter()
            .zip(&vals)
            .map(|(s, (v, fv))| {
                let tag = if pts.len() > 1 && (fv - hi).abs() < 1e-12 * (1.0 + hi.abs()) {
                    " (largest)"
                } else if pts.len() > 1 && (fv - lo).abs() < 1e-12 * (1.0 + lo.abs()) {
                    " (smallest)"
                } else {
                    ""
                };
                format!(
                    "- {}, {}: f = {}{}",
                    fmt_point(&s[..n]),
                    lams.iter().zip(&s[n..]).map(|(l, x)| format!("{} = {}", l, show(x))).collect::<Vec<_>>().join(", "),
                    show(v),
                    tag
                )
            })
            .collect();
        let system = eqs
            .iter()
            .map(|e| format!("{} = 0", show(&crate::ratfun::tidy(e))))
            .collect::<Vec<_>>()
            .join(", ");
        let mut note = format!(
            "At an extreme of f on the constraint, grad f is parallel to grad g: grad f = λ grad g, together with g = 0. Galileo solved {} and lists the points, one per row:\n\n{}",
            system,
            if lines.is_empty() { "(no real solutions)".to_string() } else { lines.join("\n") }
        );
        note.push_str(vcalc::family_note(&pts));
        note.push_str("\n\nOn a closed and bounded constraint (a circle, an ellipse) the largest of these values is the maximum and the smallest the minimum. λ tells how fast the best value changes when the constraint is relaxed.");
        self.note(note);
        if pts.len() == 1 {
            return Ok(row(pts[0].clone()));
        }
        Ok(rows(pts))
    }

    // ---------- field plots ----------

    #[allow(clippy::too_many_arguments)]
    fn grid_args(&mut self, cmd: &str, f: &Node, x: &Node, a: &Node, b: &Node, y: &Node, c: &Node, d: &Node) -> R<(String, String, f64, f64, f64, f64, Expr)> {
        let (x, y) = (ident(x, "x")?, ident(y, "y")?);
        let (x0, x1) = self.range(cmd, a, b)?;
        let (y0, y1) = self.range(cmd, c, d)?;
        let e = self.eval_hidden(f, &[x.clone(), y.clone()])?;
        Ok((x, y, x0.min(x1), x0.max(x1), y0.min(y1), y0.max(y1), e))
    }

    fn contour(&mut self, args: &[Node]) -> R<Expr> {
        let (f, x, a, b, y, c, d, lv) = match args {
            [f, x, a, b, y, c, d] => (f, x, a, b, y, c, d, None),
            [f, x, a, b, y, c, d, l] => (f, x, a, b, y, c, d, Some(l)),
            _ => return Err("usage: contour(f, x, from, to, y, from, to) or with levels: a count (12) or a list ([0 1 4 9])".into()),
        };
        let (xn, yn, x0, x1, y0, y1, e) = self.grid_args("contour", f, x, a, b, y, c, d)?;
        if !e.is_scalar() {
            return Err("contour needs a scalar function of x and y".into());
        }
        let code = numeric::compile(&e, &[&xn, &yn]);
        let n = 101;
        let xs = plot::steps(x0, x1, n - 1);
        let ys = plot::steps(y0, y1, n - 1);
        let z: Vec<Vec<f64>> = xs.iter().map(|&p| ys.iter().map(|&q| run(&code, &[p, q])).collect()).collect();
        let mut fin: Vec<f64> = z.iter().flatten().copied().filter(|v| v.is_finite()).collect();
        if fin.is_empty() {
            return Err(format!("contour: {} has no real values on this range", show(&e)));
        }
        fin.sort_by(|p, q| p.total_cmp(q));
        let levels: Vec<f64> = match lv {
            None => auto_levels(&fin, 10),
            Some(l) => {
                let vals = self.numbers(l, "contour: the levels")?;
                if vals.len() == 1 && vals[0].fract() == 0.0 && vals[0] >= 1.0 && vals[0] <= 60.0 && !matches!(l, Node::Matrix(_)) {
                    auto_levels(&fin, vals[0] as usize)
                } else {
                    vals
                }
            }
        };
        let name = label(f, &e);
        let k = levels.len();
        let series: Vec<Series> = levels
            .iter()
            .enumerate()
            .map(|(i, &lvl)| Series {
                label: if name.len() <= 24 { format!("{} = {}", name, num(lvl)) } else { format!("level {}", num(lvl)) },
                points: march(&xs, &ys, &z, lvl),
                color: Some(ramp(if k > 1 { i as f64 / (k - 1) as f64 } else { 0.5 })),
                thin: true,
                ..Default::default()
            })
            .collect();
        let mut fig = Figure::new(series, &xn, &yn, true);
        fig.title = String::new();
        self.note(format!("Level curves of {}: each curve joins the points where f has one value (the legend gives it), from blue (low) to red (high). Where curves crowd together f changes fast; the gradient crosses them at right angles.", name));
        Ok(obj(Obj::Figure(fig)))
    }

    fn fieldplot(&mut self, args: &[Node]) -> R<Expr> {
        let (f, x, a, b, y, c, d, k) = match args {
            [f, x, a, b, y, c, d] => (f, x, a, b, y, c, d, None),
            [f, x, a, b, y, c, d, k] => (f, x, a, b, y, c, d, Some(k)),
            _ => return Err("usage: fieldplot([P Q], x, from, to, y, from, to) or with the arrows per side (default 15)".into()),
        };
        let (xn, yn, x0, x1, y0, y1, e) = self.grid_args("fieldplot", f, x, a, b, y, c, d)?;
        let comps = flat(e.clone());
        if comps.len() != 2 {
            return Err("fieldplot needs a field with two components [P Q]".into());
        }
        let m = match k {
            Some(k) => self.number(k, "fieldplot: the arrows per side")? as usize,
            None => 15,
        }
        .clamp(3, 60);
        let cp = numeric::compile(&comps[0], &[&xn, &yn]);
        let cq = numeric::compile(&comps[1], &[&xn, &yn]);
        let (dx, dy) = ((x1 - x0) / m as f64, (y1 - y0) / m as f64);
        let mut vecs = vec![];
        let mut big = 0.0f64;
        for i in 0..m {
            for j in 0..m {
                let (px, py) = (x0 + (i as f64 + 0.5) * dx, y0 + (j as f64 + 0.5) * dy);
                let (u, v) = (run(&cp, &[px, py]), run(&cq, &[px, py]));
                if u.is_finite() && v.is_finite() {
                    // length measured in cells, so both axes count the same
                    let len = ((u / dx).powi(2) + (v / dy).powi(2)).sqrt();
                    big = big.max(len);
                    vecs.push((px, py, u, v));
                }
            }
        }
        let scale = if big > 0.0 { 0.9 / big } else { 0.0 };
        let mut pts = vec![];
        let nan = (f64::NAN, f64::NAN);
        for (px, py, u, v) in vecs {
            let (ex, ey) = (px + u * scale, py + v * scale);
            if (u * scale / dx).hypot(v * scale / dy) < 0.02 {
                continue;
            }
            pts.push((px, py));
            pts.push((ex, ey));
            pts.push(nan);
            // arrow head: two short strokes, drawn in cell units
            let (ux, uy) = ((ex - px) / dx, (ey - py) / dy);
            let l = ux.hypot(uy);
            let h = (0.3 * l).min(0.25);
            for s in [1.0f64, -1.0] {
                let ang = s * 0.45;
                let (cs, sn) = (ang.cos(), ang.sin());
                let (rx, ry) = (-(ux * cs - uy * sn) / l * h, -(ux * sn + uy * cs) / l * h);
                pts.push((ex, ey));
                pts.push((ex + rx * dx, ey + ry * dy));
                pts.push(nan);
            }
        }
        let s = Series {
            label: label(f, &e),
            points: pts,
            thin: true,
            ..Default::default()
        };
        self.note("Each arrow shows the field at the middle of its cell: its direction, and its length relative to the longest arrow.");
        Ok(obj(Obj::Figure(Figure::new(vec![s], &xn, &yn, true))))
    }
}

/// sqrt of a sum of squares, taking square roots of squared factors that stay
/// non-negative over the parameter ranges (other symbols count as positive):
/// sqrt(sin(u)^2 R^4) = R^2 sin(u) for 0 <= u <= pi
fn sqrt_pos(n2: &Expr, ranges: &[(String, f64, f64)]) -> Expr {
    let e = crate::calc::simplify(n2);
    let e = crate::factor_cmd::factor_poly(&e).unwrap_or(e);
    let e = crate::calc::factor(&e);
    let fs = match &e {
        Expr::Mul(v) => v.clone(),
        o => vec![o.clone()],
    };
    let nonneg = |g: &Expr| -> bool {
        let mut names: Vec<String> = ranges.iter().map(|r| r.0.clone()).collect();
        for s in g.symbols() {
            if !names.contains(&s) {
                names.push(s);
            }
        }
        let nm: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        let c = numeric::compile(g, &nm);
        let k = 9;
        let mut idx = vec![0usize; ranges.len()];
        loop {
            let mut x: Vec<f64> = ranges
                .iter()
                .zip(&idx)
                .map(|(r, &i)| {
                    let (a, b) = if r.1.is_finite() && r.2.is_finite() { (r.1, r.2) } else { (0.1, 3.0) };
                    a + (b - a) * (i as f64 + 0.5) / k as f64
                })
                .collect();
            x.extend((ranges.len()..names.len()).map(|j| 0.7 + 0.31 * j as f64));
            let v = run(&c, &x);
            if !(v.is_finite() && v >= -1e-12) {
                return false;
            }
            let mut d = 0;
            loop {
                if d == idx.len() {
                    return true;
                }
                idx[d] += 1;
                if idx[d] < k {
                    break;
                }
                idx[d] = 0;
                d += 1;
            }
        }
    };
    let mut out = vec![];
    for f in fs {
        match &f {
            Expr::Pow(g, p) if p.num().is_some_and(|n| n.is_integer() && n.as_int().is_some_and(|k| k > 0 && k % 2 == 0)) && nonneg(g) => {
                let k = p.num().unwrap().as_int().unwrap();
                out.push(pow((**g).clone(), Expr::int(k / 2)));
            }
            Expr::Num(_) => out.push(crate::cas::rsqrt(&f)),
            _ => out.push(sqrt(f.clone())),
        }
    }
    crate::ratfun::tidy(&mul(out))
}

/// definite integral, retried after rewriting powers of sin and cos as
/// multiple angles when the first try only gave a number
fn integrate_exact(f: &Expr, t: &str, a: &Expr, b: &Expr) -> R<Expr> {
    let r = crate::cas_cmd::definite(f, t, a, b, 0);
    let exact = |r: &R<Expr>| r.as_ref().is_ok_and(|e| !vcalc::has_float(e));
    if exact(&r) || vcalc::has_float(f) {
        return r;
    }
    let g = crate::calc::expand(&crate::extra_cmd::trig_reduce(&crate::calc::expand(f)));
    let r2 = crate::cas_cmd::definite(&g, t, a, b, 0);
    if let (Ok(e2), true) = (&r2, exact(&r2)) {
        let agree = match &r {
            Ok(e1) => {
                let (v1, v2) = (numeric::value(e1), numeric::value(e2));
                !v1.is_finite() || (v1 - v2).abs() < 1e-8 * (1.0 + v1.abs())
            }
            Err(_) => true,
        };
        if agree {
            return Ok(e2.clone());
        }
    }
    r
}

/// constants are taken positive: (R^2)^(k/2) = R^k
fn positive_params(e: &Expr, bound: &[&str]) -> Expr {
    let inner = e.map_children(&mut |c| positive_params(c, bound));
    if let Expr::Pow(b, p) = &inner {
        if let Expr::Pow(s, two) = &**b {
            if matches!(&**s, Expr::Sym(n) if !bound.contains(&n.as_str())) && two.num().is_some_and(|n| n.is_exact() && n.as_int() == Some(2)) {
                return pow((**s).clone(), mul2(Expr::int(2), (**p).clone()));
            }
        }
    }
    inner
}

fn kind_text(k: &str) -> &'static str {
    match k {
        "min" => "local minimum",
        "max" => "local maximum",
        "saddle" => "saddle point",
        _ => "test inconclusive",
    }
}

fn special_teach(f: &str, args: &[Expr], r: &Expr) -> Option<String> {
    let a0 = show(&args[0]);
    Some(match f {
        "gamma" => format!("gamma(x) = ∫₀^∞ t^(x−1) e^(−t) dt extends the factorial: gamma(n) = (n − 1)! for whole n, and gamma(x + 1) = x·gamma(x) for all x. Here gamma({}) = {}.", a0, show(r)),
        "erf" => format!("erf(x) = (2/√π) ∫₀^x e^(−t²) dt, the area under the bell curve from 0 to x, scaled so erf(∞) = 1. e^(−t²) has no antiderivative made of elementary functions, which is why erf is a function of its own. Here erf({}) = {}.", a0, show(r)),
        "zeta" => format!("zeta(s) = 1 + 1/2^s + 1/3^s + ... for s > 1, continued to other s. Euler found zeta(2) = π²/6; every even value is a rational multiple of π^s. Here zeta({}) = {}.", a0, show(r)),
        "polylog" => format!("polylog(s, z) = Li_s(z) = z + z²/2^s + z³/3^s + ... for |z| ≤ 1, continued to the rest of the plane. Li_1(z) = −log(1 − z), Li_s(1) = zeta(s), and for whole s ≤ 0 it is a rational function of z. Here polylog({}, {}) = {}.", a0, show(&args[1]), show(r)),
        "besselj" => format!("J_n(x) solves Bessel's equation x²y'' + xy' + (x² − n²)y = 0 and stays finite at x = 0: it describes the vibrating drum and waves in a cylinder. Here besselj({}, {}) = {}.", a0, show(&args[1]), show(r)),
        _ => return None,
    })
}

/// Nice round levels strictly inside the range of values
pub(crate) fn auto_levels(sorted: &[f64], k: usize) -> Vec<f64> {
    let q = |p: f64| sorted[((p * (sorted.len() - 1) as f64).round() as usize).min(sorted.len() - 1)];
    let (lo, hi) = (q(0.02), q(0.98));
    if hi - lo <= 0.0 {
        return vec![lo];
    }
    let t = plot::ticks(lo, hi, k.max(1));
    let inside: Vec<f64> = t.into_iter().filter(|v| *v > sorted[0] && *v < sorted[sorted.len() - 1]).collect();
    if inside.is_empty() { vec![(lo + hi) / 2.0] } else { inside }
}

/// Blue to red through light grey, for t in [0, 1]
pub(crate) fn ramp(t: f64) -> String {
    let stops = [
        (59.0, 76.0, 192.0),
        (124.0, 159.0, 249.0),
        (180.0, 180.0, 190.0),
        (238.0, 132.0, 104.0),
        (180.0, 4.0, 38.0),
    ];
    let t = t.clamp(0.0, 1.0) * (stops.len() - 1) as f64;
    let i = (t.floor() as usize).min(stops.len() - 2);
    let u = t - i as f64;
    let (a, b) = (stops[i], stops[i + 1]);
    let c = |p: f64, q: f64| (p + (q - p) * u).round() as u8;
    format!("#{:02x}{:02x}{:02x}", c(a.0, b.0), c(a.1, b.1), c(a.2, b.2))
}

/// Marching squares: the level curve z = lvl as polylines separated by NaN
pub(crate) fn march(xs: &[f64], ys: &[f64], z: &[Vec<f64>], lvl: f64) -> Vec<(f64, f64)> {
    let mut segs: Vec<((f64, f64), (f64, f64))> = vec![];
    let lerp = |p: (f64, f64, f64), q: (f64, f64, f64)| {
        let t = (lvl - p.2) / (q.2 - p.2);
        (p.0 + t * (q.0 - p.0), p.1 + t * (q.1 - p.1))
    };
    for i in 0..xs.len() - 1 {
        for j in 0..ys.len() - 1 {
            let c = [
                (xs[i], ys[j], z[i][j]),
                (xs[i + 1], ys[j], z[i + 1][j]),
                (xs[i + 1], ys[j + 1], z[i + 1][j + 1]),
                (xs[i], ys[j + 1], z[i][j + 1]),
            ];
            if c.iter().any(|p| !p.2.is_finite()) {
                continue;
            }
            // crossing points on the four edges
            let mut cross = vec![];
            for k in 0..4 {
                let (p, q) = (c[k], c[(k + 1) % 4]);
                if (p.2 < lvl) != (q.2 < lvl) {
                    cross.push((k, lerp(p, q)));
                }
            }
            match cross.len() {
                2 => segs.push((cross[0].1, cross[1].1)),
                4 => {
                    // saddle cell: decide by the value in the middle
                    let mid = c.iter().map(|p| p.2).sum::<f64>() / 4.0;
                    let first_low = c[0].2 < lvl;
                    if (mid < lvl) == first_low {
                        segs.push((cross[0].1, cross[3].1));
                        segs.push((cross[1].1, cross[2].1));
                    } else {
                        segs.push((cross[0].1, cross[1].1));
                        segs.push((cross[2].1, cross[3].1));
                    }
                }
                _ => {}
            }
        }
    }
    chain(segs)
}

/// Join segments that share end points into polylines
fn chain(segs: Vec<((f64, f64), (f64, f64))>) -> Vec<(f64, f64)> {
    use std::collections::HashMap;
    let key = |p: (f64, f64)| ((p.0 * 1e9).round() as i64, (p.1 * 1e9).round() as i64);
    let mut at: HashMap<(i64, i64), Vec<usize>> = HashMap::new();
    for (k, s) in segs.iter().enumerate() {
        at.entry(key(s.0)).or_default().push(k);
        at.entry(key(s.1)).or_default().push(k);
    }
    let mut used = vec![false; segs.len()];
    let mut out = vec![];
    for start in 0..segs.len() {
        if used[start] {
            continue;
        }
        used[start] = true;
        let mut line = std::collections::VecDeque::new();
        line.push_back(segs[start].0);
        line.push_back(segs[start].1);
        // grow at both ends
        for front in [false, true] {
            loop {
                let end = if front { *line.front().unwrap() } else { *line.back().unwrap() };
                let next = at
                    .get(&key(end))
                    .and_then(|v| v.iter().copied().find(|&k| !used[k]));
                let Some(k) = next else { break };
                used[k] = true;
                let s = segs[k];
                let other = if key(s.0) == key(end) { s.1 } else { s.0 };
                if front {
                    line.push_front(other);
                } else {
                    line.push_back(other);
                }
            }
        }
        out.extend(line);
        out.push((f64::NAN, f64::NAN));
    }
    out
}
