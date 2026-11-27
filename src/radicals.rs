//! Exact roots of cubics and quartics in radicals.
//!
//! Cubics use Cardano's formula; when all three roots are real (the "casus
//! irreducibilis", where Cardano's square root is of a negative number) they use
//! the trigonometric form instead, so every root is written with real numbers.
//! Quartics use Ferrari's method: a root m of the resolvent cubic splits the
//! quartic into two quadratics.
//!
//! With real coefficients the roots come back as `re + im*i` with real `re` and
//! `im`, so `eval` gives their values. With symbolic coefficients the general
//! formulas come back (principal roots). The steps are recorded in a [`Trace`]
//! so that `solve` can explain them.

use crate::build::*;
use crate::calc::{self, simplify};
use crate::expr::{Const, Expr, Func};
use crate::numeric;
use std::cell::{Cell, RefCell};

/// How a cubic was solved
#[derive(Clone, Debug)]
pub enum CubicCase {
    /// D > 0: one real root. u^3 and v^3 are -q/2 ± sqrt(D); u, v their real cube roots.
    OneReal {
        u3: Expr,
        v3: Expr,
        u: Expr,
        v: Expr,
    },
    /// D < 0: three real roots t = r*cos((theta - 2*pi*k)/3) with cos(theta) = c
    ThreeReal { r: Expr, c: Expr },
    /// D = 0: a repeated root
    Repeated,
    /// symbolic coefficients: u = (-q/2 + sqrt(D))^(1/3), v = -p/(3u)
    Symbolic { u: Expr, v: Expr },
}

/// The steps of a cubic: a x^3 + b x^2 + c x + d with x = t - shift gives
/// t^3 + p t + q; D = (q/2)^2 + (p/3)^3
#[derive(Clone, Debug)]
pub struct Cubic {
    pub coeffs: Vec<Expr>,
    pub shift: Expr,
    pub p: Expr,
    pub q: Expr,
    pub d: Expr,
    pub case: CubicCase,
    pub roots: Vec<Expr>,
}

/// How a quartic was solved
#[derive(Clone, Debug)]
pub enum QuarticCase {
    /// q = 0: y^2 = w with w = (-p ± sqrt(p^2 - 4r))/2
    Biquadratic { disc: Expr, w: Vec<Expr> },
    /// Ferrari: m a root of the resolvent 8m^3 + 8p m^2 + (2p^2 - 8r) m - q^2,
    /// splitting into y^2 - s y + c1 and y^2 + s y + c2 with s = sqrt(2m)
    Ferrari {
        resolvent: Vec<Expr>,
        m: Expr,
        s: Expr,
        quads: Vec<(Expr, Expr)>,
    },
}

/// The steps of a quartic: a x^4 + ... with x = y - shift gives y^4 + p y^2 + q y + r
#[derive(Clone, Debug)]
pub struct Quartic {
    pub coeffs: Vec<Expr>,
    pub shift: Expr,
    pub p: Expr,
    pub q: Expr,
    pub r: Expr,
    pub case: QuarticCase,
    pub roots: Vec<Expr>,
}

/// What `solve` did with a polynomial piece, for its notes
#[derive(Clone, Debug)]
pub enum Trace {
    Cubic(Cubic),
    Quartic(Quartic),
    /// a quartic without rational roots that splits into these factors over Q
    Factored {
        poly: Vec<Expr>,
        factors: Vec<Vec<Expr>>,
    },
    /// a piece of degree 5 or more without factors: numeric roots
    Numeric {
        poly: Vec<Expr>,
    },
    /// a x + b = 0
    Linear {
        poly: Vec<Expr>,
    },
    /// a x^2 + b x + c = 0, by completing the square
    Quadratic {
        poly: Vec<Expr>,
    },
    /// rational roots (with multiplicities) found by the rational root test, and
    /// the polynomial left after dividing them out
    RationalRoots {
        poly: Vec<Expr>,
        roots: Vec<(Expr, usize)>,
        rest: Vec<Expr>,
    },
}

thread_local! {
    static RECORDING: Cell<bool> = const { Cell::new(false) };
    static QUIET: Cell<u32> = const { Cell::new(0) };
    static TRACES: RefCell<Vec<Trace>> = const { RefCell::new(vec![]) };
}

/// Start recording the steps of the next solve
pub fn begin() {
    RECORDING.with(|r| r.set(true));
    TRACES.with(|t| t.borrow_mut().clear());
}

/// Stop recording and return what was recorded
pub fn take() -> Vec<Trace> {
    RECORDING.with(|r| r.set(false));
    TRACES.with(|t| std::mem::take(&mut *t.borrow_mut()))
}

pub(crate) fn record(t: Trace) {
    if RECORDING.with(|r| r.get()) && QUIET.with(|q| q.get()) == 0 {
        TRACES.with(|v| v.borrow_mut().push(t));
    }
}

fn quietly<T>(f: impl FnOnce() -> T) -> T {
    QUIET.with(|q| q.set(q.get() + 1));
    let r = f();
    QUIET.with(|q| q.set(q.get() - 1));
    r
}

// ---------- helpers ----------

fn tidy(e: &Expr) -> Expr {
    // the parts are tidied as they are made; tidying the large assembled
    // formulas costs a lot and gains little
    if e.size() > 60 {
        return e.clone();
    }
    let t = crate::ratfun::tidy(e);
    let s = simplify(e);
    if s.size() < t.size() { s } else { t }
}

fn int(n: i128) -> Expr {
    Expr::int(n)
}

fn i() -> Expr {
    Expr::Const(Const::I)
}

fn val(e: &Expr) -> f64 {
    numeric::value(e)
}

/// true when every coefficient is a real number (no symbols, no i)
pub fn real_coeffs(c: &[Expr]) -> bool {
    c.iter()
        .all(|e| e.symbols().is_empty() && val(e).is_finite())
}

/// exactly zero; large real expressions are judged by their value, since
/// simplifying nested radicals is slow
fn zero(e: &Expr) -> bool {
    if e.size() <= 40 {
        return crate::cas::is_zero(e);
    }
    let v = val(e);
    v.is_finite() && v.abs() < 1e-13
}

/// the real cube root of a real number
fn cbrt_real(e: &Expr) -> Expr {
    if zero(e) {
        return Expr::zero();
    }
    if val(e) < 0.0 {
        neg(pow(tidy(&neg(e.clone())), rat(1, 3)))
    } else {
        pow(e.clone(), rat(1, 3))
    }
}

/// sqrt of a real number as (real part, imaginary part)
fn sqrt_parts(e: &Expr) -> (Expr, Expr) {
    if zero(e) {
        (Expr::zero(), Expr::zero())
    } else if val(e) < 0.0 {
        (Expr::zero(), tidy(&sqrt(tidy(&neg(e.clone())))))
    } else {
        (tidy(&sqrt(e.clone())), Expr::zero())
    }
}

/// a complex number from real and imaginary parts
fn complex(re: Expr, im: Expr) -> Expr {
    let re = tidy(&re);
    let im = tidy(&im);
    if im.is_zero() {
        re
    } else {
        add2(re, mul2(im, i()))
    }
}

/// real roots first, by value; then complex ones by real and imaginary part
fn sorted(mut roots: Vec<(Expr, Expr)>) -> Vec<Expr> {
    let key = |r: &(Expr, Expr)| (!zero(&r.1), val(&r.0), val(&r.1));
    #[allow(clippy::type_complexity)]
    let mut keyed: Vec<((bool, f64, f64), (Expr, Expr))> =
        roots.drain(..).map(|r| (key(&r), r)).collect();
    keyed.sort_by(|a, b| {
        a.0.0
            .cmp(&b.0.0)
            .then(
                a.0.1
                    .partial_cmp(&b.0.1)
                    .unwrap_or(std::cmp::Ordering::Equal),
            )
            .then(
                a.0.2
                    .partial_cmp(&b.0.2)
                    .unwrap_or(std::cmp::Ordering::Equal),
            )
    });
    keyed
        .into_iter()
        .map(|(_, (re, im))| complex(re, im))
        .collect()
}

/// coefficients (low to high) of the polynomial with coefficients c after
/// x = t - shift, divided by its leading coefficient
fn depressed(c: &[Expr], shift: &Expr) -> Option<Vec<Expr>> {
    let x = Expr::sym("x__");
    let t = Expr::sym("t__");
    let lead = c.last()?.clone();
    let poly = add(c
        .iter()
        .enumerate()
        .map(|(k, a)| mul2(div(a.clone(), lead.clone()), pow(x.clone(), int(k as i128))))
        .collect());
    let moved = calc::subst(&poly, "x__", &sub(t, shift.clone()));
    let d = crate::cas::poly_coeffs(&moved, "t__")?;
    Some(d.iter().map(tidy).collect())
}

// ---------- the cubic ----------

/// All three roots of c0 + c1 x + c2 x^2 + c3 x^3 (c3 not zero)
pub fn cubic(c: &[Expr]) -> Option<Cubic> {
    if c.len() != 4 || zero(&c[3]) {
        return None;
    }
    let real = real_coeffs(c);
    let shift = tidy(&div(c[2].clone(), mul2(int(3), c[3].clone())));
    let dep = depressed(c, &shift)?;
    let (p, q) = (dep[1].clone(), dep[0].clone());
    let d = tidy(&add2(
        pow(div(q.clone(), int(2)), int(2)),
        pow(div(p.clone(), int(3)), int(3)),
    ));
    let ns = neg(shift.clone());
    let half_q = tidy(&div(neg(q.clone()), int(2)));
    // the three roots from u and v: u + v and -(u + v)/2 ± sqrt(3)/2 (u - v) i
    let from_uv = |u: &Expr, v: &Expr| -> Vec<(Expr, Expr)> {
        let s = add2(u.clone(), v.clone());
        let re = add2(neg(div(s.clone(), int(2))), ns.clone());
        let im = mul2(div(sqrt(int(3)), int(2)), sub(u.clone(), v.clone()));
        vec![
            (add2(s, ns.clone()), Expr::zero()),
            (re.clone(), neg(im.clone())),
            (re, im),
        ]
    };
    let (case, roots) = if !real {
        let (u, v) = if zero(&p) {
            (tidy(&pow(neg(q.clone()), rat(1, 3))), Expr::zero())
        } else {
            let u = tidy(&pow(add2(half_q.clone(), sqrt(d.clone())), rat(1, 3)));
            let v = tidy(&div(neg(p.clone()), mul2(int(3), u.clone())));
            (u, v)
        };
        let r: Vec<Expr> = from_uv(&u, &v)
            .into_iter()
            .map(|(re, im)| complex(re, im))
            .collect();
        (CubicCase::Symbolic { u, v }, r)
    } else if zero(&p) {
        // t^3 = -q: u is the real cube root of -q
        let u = cbrt_real(&tidy(&neg(q.clone())));
        let r = sorted(from_uv(&u, &Expr::zero()));
        (
            CubicCase::OneReal {
                u3: tidy(&neg(q.clone())),
                v3: Expr::zero(),
                u,
                v: Expr::zero(),
            },
            r,
        )
    } else if zero(&d) {
        // a repeated root: t = 3q/p once and -3q/(2p) twice (all 0 when p = 0)
        let r = if zero(&p) {
            vec![(ns.clone(), Expr::zero())]
        } else {
            let a = div(mul2(int(3), q.clone()), p.clone());
            vec![
                (add2(a.clone(), ns.clone()), Expr::zero()),
                (add2(neg(div(a, int(2))), ns.clone()), Expr::zero()),
            ]
        };
        (CubicCase::Repeated, sorted(r))
    } else if val(&d) > 0.0 {
        let sd = tidy(&sqrt(d.clone()));
        let u3 = tidy(&add2(half_q.clone(), sd.clone()));
        let v3 = tidy(&sub(half_q.clone(), sd));
        let (u, v) = (cbrt_real(&u3), cbrt_real(&v3));
        let r = sorted(from_uv(&u, &v));
        (CubicCase::OneReal { u3, v3, u, v }, r)
    } else {
        // t = r cos(phi) with 4 cos^3 phi - 3 cos phi = cos(3 phi)
        let r = tidy(&mul2(int(2), sqrt(tidy(&div(neg(p.clone()), int(3))))));
        let cc = tidy(&mul2(
            div(mul2(int(3), q.clone()), mul2(int(2), p.clone())),
            sqrt(tidy(&div(int(-3), p.clone()))),
        ));
        let theta = func(Func::Acos, cc.clone());
        let roots = (0..3)
            .map(|k| {
                let phi = div(sub(theta.clone(), mul2(int(2 * k), pi())), int(3));
                (
                    add2(mul2(r.clone(), func(Func::Cos, tidy(&phi))), ns.clone()),
                    Expr::zero(),
                )
            })
            .collect();
        (CubicCase::ThreeReal { r, c: cc }, sorted(roots))
    };
    Some(Cubic {
        coeffs: c.to_vec(),
        shift,
        p,
        q,
        d,
        case,
        roots,
    })
}

// ---------- the quartic ----------

/// All four roots of c0 + c1 x + ... + c4 x^4 (c4 not zero)
pub fn quartic(c: &[Expr]) -> Option<Quartic> {
    if c.len() != 5 || zero(&c[4]) {
        return None;
    }
    let real = real_coeffs(c);
    let shift = tidy(&div(c[3].clone(), mul2(int(4), c[4].clone())));
    let dep = depressed(c, &shift)?;
    let (p, q, r) = (dep[2].clone(), dep[1].clone(), dep[0].clone());
    let ns = neg(shift.clone());
    let disc = tidy(&sub(pow(p.clone(), int(2)), mul2(int(4), r.clone())));
    let mut roots: Vec<(Expr, Expr)> = vec![];
    let case;
    if zero(&q) && (!real || val(&disc) >= 0.0) {
        // y^4 + p y^2 + r: a quadratic in w = y^2
        let sd = sqrt(disc.clone());
        let w: Vec<Expr> = [-1, 1]
            .iter()
            .map(|&s| tidy(&div(add2(neg(p.clone()), mul2(int(s), sd.clone())), int(2))))
            .collect();
        for wk in &w {
            let (re, im) = if real {
                sqrt_parts(wk)
            } else {
                (tidy(&sqrt(wk.clone())), Expr::zero())
            };
            roots.push((add2(neg(re.clone()), ns.clone()), neg(im.clone())));
            roots.push((add2(re, ns.clone()), im));
        }
        case = QuarticCase::Biquadratic { disc, w };
    } else {
        // (y^2 + p/2 + m)^2 = 2m y^2 - q y + (m^2 + m p + p^2/4 - r), a square in y
        // when 8m^3 + 8p m^2 + (2p^2 - 8r) m - q^2 = 0
        let resolvent = vec![
            tidy(&neg(pow(q.clone(), int(2)))),
            tidy(&sub(
                mul2(int(2), pow(p.clone(), int(2))),
                mul2(int(8), r.clone()),
            )),
            tidy(&mul2(int(8), p.clone())),
            int(8),
        ];
        let ms = quietly(|| crate::cas::poly_roots(&resolvent)).ok()?;
        let m = if real {
            // a positive real root exists (the resolvent is -q^2 < 0 at m = 0); take the simplest
            ms.into_iter()
                .filter(|m| val(m) > 1e-12)
                .min_by_key(|m| crate::print::show(m).len())?
        } else {
            ms.into_iter().next()?
        };
        let s = tidy(&sqrt(mul2(int(2), m.clone())));
        let mut quads = vec![];
        for sg in [1i128, -1] {
            // y^2 - sg s y + (p/2 + m + sg q/(2s))
            let c0 = tidy(&add(vec![
                div(p.clone(), int(2)),
                m.clone(),
                div(mul2(int(sg), q.clone()), mul2(int(2), s.clone())),
            ]));
            quads.push((tidy(&mul2(int(-sg), s.clone())), c0));
            // y = sg s/2 ± sqrt(-2m - 2p - 2 sg q/s)/2
            let delta = tidy(&add(vec![
                mul2(int(-2), m.clone()),
                mul2(int(-2), p.clone()),
                div(mul2(int(-2 * sg), q.clone()), s.clone()),
            ]));
            let centre = add2(div(mul2(int(sg), s.clone()), int(2)), ns.clone());
            let (re, im) = if real {
                sqrt_parts(&delta)
            } else {
                (tidy(&sqrt(delta)), Expr::zero())
            };
            let (re, im) = (div(re, int(2)), div(im, int(2)));
            roots.push((sub(centre.clone(), re.clone()), neg(im.clone())));
            roots.push((add2(centre, re), im));
        }
        case = QuarticCase::Ferrari {
            resolvent,
            m,
            s,
            quads,
        };
    }
    let roots = if real {
        sorted(roots)
    } else {
        roots.into_iter().map(|(re, im)| complex(re, im)).collect()
    };
    Some(Quartic {
        coeffs: c.to_vec(),
        shift,
        p,
        q,
        r,
        case,
        roots,
    })
}

// ---------- explanations ----------

fn sh(e: &Expr) -> String {
    format!("`{}`", crate::print::show(e))
}

fn poly_text(c: &[Expr], x: &str) -> String {
    let xv = Expr::sym(x);
    let p = add(c
        .iter()
        .enumerate()
        .map(|(k, a)| mul2(a.clone(), pow(xv.clone(), int(k as i128))))
        .collect());
    sh(&p)
}

fn g(v: f64) -> String {
    let v = if v.abs() < 1e-12 { 0.0 } else { v };
    let s = format!("{:.6}", v);
    let s = s.trim_end_matches('0').trim_end_matches('.').to_string();
    if s == "-0" { "0".into() } else { s }
}

/// the roots as decimals, when the coefficients are real numbers
fn decimals(c: &[Expr]) -> Option<String> {
    if !real_coeffs(c) {
        return None;
    }
    let f: Vec<f64> = c.iter().map(val).collect();
    let roots = crate::cas::numeric_roots(&f);
    let mut out: Vec<String> = vec![];
    let mut k = 0;
    while k < roots.len() {
        let (re, im) = roots[k];
        if im == 0.0 {
            out.push(g(re));
        } else {
            out.push(format!("{} ± {}i", g(re), g(im.abs())));
            k += 1;
        }
        k += 1;
    }
    Some(out.join(", "))
}

fn sign_word(e: &Expr) -> &'static str {
    let v = val(e);
    if v > 0.0 { "positive" } else { "negative" }
}

fn cubic_note(t: &Cubic, x: &str, teach: bool) -> String {
    let mut md = vec![];
    let head = format!(
        "**Cubic formula (Cardano)** for {} = 0. {} t^3 + p·t + q = 0 with p = {}, q = {}; D = (q/2)^2 + (p/3)^3 = {}.",
        poly_text(&t.coeffs, x),
        if t.shift.is_zero() {
            format!("With {x} = t it reads")
        } else {
            format!(
                "Putting {x} = t − {} removes the square term and leaves",
                sh(&t.shift)
            )
        },
        sh(&t.p),
        sh(&t.q),
        sh(&t.d)
    );
    let verdict = match &t.case {
        CubicCase::OneReal { .. } if t.p.is_zero() => format!(
            "Here p = 0, so t^3 = {}: one real cube root and two complex ones.",
            sh(&neg(t.q.clone()))
        ),
        CubicCase::OneReal { .. } => {
            "D > 0: one real root and two complex ones, from real cube roots u and v.".into()
        }
        CubicCase::ThreeReal { .. } => "D < 0: three real roots. Cardano's sqrt(D) would be imaginary, so they are written with cos instead.".into(),
        CubicCase::Repeated => "D = 0: two of the roots are equal.".into(),
        CubicCase::Symbolic { .. } => "With symbolic coefficients these are the general formulas, with principal square and cube roots; u = 0 (p = 0 and q = 0) needs the special case x = −b/(3a).".into(),
    };
    if !teach {
        return format!("{} {}", head, verdict);
    }
    let a = &t.coeffs[3];
    md.push(format!(
        "**Solving {} = 0 with Cardano's formula (1545).**",
        poly_text(&t.coeffs, x)
    ));
    let shift_text = if t.shift.is_zero() {
        format!("There is no square term, so no shift is needed: {x} = t")
    } else {
        format!(
            "Shift the unknown: {x} = t − b/(3a) = t − {}. Expanding, the t^2 terms cancel",
            sh(&t.shift)
        )
    };
    md.push(format!(
        "**Step 1: remove the square term.** Divide by the leading coefficient {}. {}. What is left is the depressed cubic t^3 + p·t + q = 0 with p = {} and q = {}. Every cubic can be brought to this form, so one formula for it covers them all.",
        sh(a),
        shift_text,
        sh(&t.p),
        sh(&t.q)
    ));
    md.push("**Step 2: try t = u + v.** Cubing gives t^3 = u^3 + v^3 + 3uv(u + v), so t^3 − 3uv·t − (u^3 + v^3) = 0. That is our cubic when 3uv = −p and u^3 + v^3 = −q. So u^3 and v^3 have sum −q and product −p^3/27: they are the two roots of the quadratic z^2 + q·z − p^3/27 = 0, namely z = −q/2 ± sqrt(D) with D = (q/2)^2 + (p/3)^3. A cubic has been reduced to a quadratic and two cube roots.".into());
    match &t.case {
        CubicCase::OneReal { u3, v3, u, v } => {
            md.push(format!(
                "**Step 3: this cubic.** D = {}, which is {}. So u^3 = {} and v^3 = {} are real numbers, and their real cube roots are u = {} and v = {}. The real root is t = u + v.",
                sh(&t.d),
                if t.d.is_zero() { "zero" } else { sign_word(&t.d) },
                sh(u3),
                sh(v3),
                sh(u),
                sh(v)
            ));
            md.push("**Step 4: the other two roots.** Multiplying u by a cube root of unity ω = (−1 + sqrt(3)·i)/2 and v by ω^2 keeps u^3, v^3 and uv the same, so ω·u + ω^2·v is a root too, and so is ω^2·u + ω·v. Written out: t = −(u + v)/2 ± (sqrt(3)/2)(u − v)·i.".into());
        }
        CubicCase::ThreeReal { r, c } => {
            md.push(format!(
                "**Step 3: this cubic.** D = {}, which is negative. Then sqrt(D) is imaginary even though all three roots are real: the casus irreducibilis. It is what made Bombelli calculate with square roots of negative numbers in 1572, and it cannot be avoided with real radicals. Galileo goes around it with the identity cos(3φ) = 4·cos(φ)^3 − 3·cos(φ): put t = r·cos(φ) with r = 2·sqrt(−p/3) = {}; the cubic becomes cos(3φ) = (3q/(2p))·sqrt(−3/p) = {}, a number between −1 and 1.",
                sh(&t.d),
                sh(r),
                sh(c)
            ));
            md.push(format!(
                "**Step 4: three angles.** 3φ = acos({}) − 2πk for k = 0, 1, 2 gives the three roots t = r·cos((acos(c) − 2πk)/3).",
                crate::print::show(c)
            ));
        }
        CubicCase::Repeated => {
            md.push(format!(
                "**Step 3: this cubic.** D = 0, so the quadratic for u^3 and v^3 has one double root and u = v. Then t = 2u once and t = −u twice, which works out to t = 3q/p and t = −3q/(2p) (twice){}.",
                if t.p.is_zero() { "; with p = 0 all three are t = 0" } else { "" }
            ));
        }
        CubicCase::Symbolic { u, v } => {
            md.push(format!(
                "**Step 3: in general.** u = (−q/2 + sqrt(D))^(1/3) = {} and, from 3uv = −p, v = −p/(3u) = {}. Then t = u + v, and the cube roots of unity ω = (−1 + sqrt(3)·i)/2 give the other two: t = −(u + v)/2 ± (sqrt(3)/2)(u − v)·i. With numbers for the coefficients the signs of D tell the cases apart: D > 0 one real root, D < 0 three real roots, D = 0 a repeated root.",
                sh(u),
                sh(v)
            ));
        }
    }
    let mut last = if t.shift.is_zero() {
        format!("**Last step.** There was no shift, so {x} = t.")
    } else {
        format!("**Last step: undo the shift.** {x} = t − {}.", sh(&t.shift))
    };
    if let Some(d) = decimals(&t.coeffs) {
        last.push_str(&format!(" As decimals the roots are {}.", d));
    }
    last.push_str(&format!(
        " Check: they add up to −b/a = {}, as the roots of any cubic must.",
        sh(&tidy(&neg(div(t.coeffs[2].clone(), a.clone()))))
    ));
    md.push(last);
    md.push("**What to notice.** The whole method is: shift to kill one term, guess a sum of two unknowns, and get a quadratic. The quartic has a formula of the same kind (Ferrari's); from degree 5 on there is none in general (Abel and Ruffini), which is why solve gives numeric roots there.".into());
    md.join("\n\n")
}

fn quartic_note(t: &Quartic, x: &str, teach: bool) -> String {
    let head = format!(
        "**Quartic formula (Ferrari)** for {} = 0. {} y^4 + p·y^2 + q·y + r = 0 with p = {}, q = {}, r = {}.",
        poly_text(&t.coeffs, x),
        if t.shift.is_zero() {
            format!("With {x} = y it reads")
        } else {
            format!(
                "Putting {x} = y − {} removes the cubic term and leaves",
                sh(&t.shift)
            )
        },
        sh(&t.p),
        sh(&t.q),
        sh(&t.r)
    );
    let brief = match &t.case {
        QuarticCase::Biquadratic { .. } => "q = 0, so it is a quadratic in y^2.".to_string(),
        QuarticCase::Ferrari { resolvent, m, .. } => format!(
            "The resolvent cubic {} = 0 has the root m = {}, which splits it into two quadratics in y.",
            poly_text(resolvent, "m"),
            sh(m)
        ),
    };
    if !teach {
        return format!("{} {}", head, brief);
    }
    let mut md = vec![format!(
        "**Solving {} = 0 with Ferrari's method (1540).**",
        poly_text(&t.coeffs, x)
    )];
    md.push(format!(
        "**Step 1: remove the cubic term.** Divide by the leading coefficient {} and {}, leaving y^4 + p·y^2 + q·y + r = 0 with p = {}, q = {}, r = {}.",
        sh(&t.coeffs[4]),
        if t.shift.is_zero() {
            format!("note there is no cubic term, so {x} = y")
        } else {
            format!("put {x} = y − b/(4a) = y − {}; the y^3 terms cancel", sh(&t.shift))
        },
        sh(&t.p),
        sh(&t.q),
        sh(&t.r)
    ));
    match &t.case {
        QuarticCase::Biquadratic { disc, w } => {
            md.push(format!(
                "**Step 2: a quadratic in disguise.** Here q = 0, so with w = y^2 the equation is w^2 + p·w + r = 0. The quadratic formula gives w = (−p ± sqrt(p^2 − 4r))/2, with p^2 − 4r = {}: w = {} or w = {}.",
                sh(disc),
                sh(&w[0]),
                sh(&w[1])
            ));
            md.push("**Step 3: square roots.** Each w gives y = ±sqrt(w), four roots in all (a negative w gives a pair ±sqrt(−w)·i).".into());
        }
        QuarticCase::Ferrari {
            resolvent,
            m,
            s,
            quads,
        } => {
            md.push("**Step 2: make both sides squares.** Write the equation as (y^2 + p/2)^2 = −q·y − r + p^2/4, then add 2m·(y^2 + p/2) + m^2 to both sides for a number m still to be chosen: (y^2 + p/2 + m)^2 = 2m·y^2 − q·y + (m^2 + p·m + p^2/4 − r). The left side is a square for every m. The right side is a quadratic in y, and it is a perfect square exactly when its discriminant is zero: q^2 = 8m·(m^2 + p·m + p^2/4 − r).".into());
            md.push(format!(
                "**Step 3: the resolvent cubic.** That condition is the cubic {} = 0 in m, which Cardano's formula solves. It is −q^2 < 0 at m = 0 and grows without bound, so it has a positive root; Galileo takes m = {}.",
                poly_text(resolvent, "m"),
                sh(m)
            ));
            md.push(format!(
                "**Step 4: split.** With s = sqrt(2m) = {} the right side is (s·y − q/(2s))^2, and A^2 = B^2 means A = B or A = −B. That gives two quadratics: y^2 + ({})·y + ({}) = 0 and y^2 + ({})·y + ({}) = 0.",
                sh(s),
                crate::print::show(&quads[0].0),
                crate::print::show(&quads[0].1),
                crate::print::show(&quads[1].0),
                crate::print::show(&quads[1].1)
            ));
            md.push("**Step 5: the quadratic formula** on each gives y = s/2 ± sqrt(−2m − 2p − 2q/s)/2 and y = −s/2 ± sqrt(−2m − 2p + 2q/s)/2.".into());
        }
    }
    let mut last = if t.shift.is_zero() {
        format!("**Last step.** There was no shift, so {x} = y.")
    } else {
        format!("**Last step: undo the shift.** {x} = y − {}.", sh(&t.shift))
    };
    if let Some(d) = decimals(&t.coeffs) {
        last.push_str(&format!(" As decimals the roots are {}.", d));
    }
    md.push(last);
    md.push("**What to notice.** A quartic is solved by a cubic (the resolvent) and then quadratics, just as a cubic is solved by a quadratic and then cube roots. For degree 5 that ladder breaks: Abel and Ruffini showed there is no general formula in radicals, which is why solve gives numeric roots there.".into());
    md.join("\n\n")
}

/// The notes for what solve did (Markdown), or None when it used no formula
pub fn notes(traces: &[Trace], x: &str, teach: bool) -> Option<String> {
    let mut parts = vec![];
    for t in traces {
        parts.push(match t {
            Trace::Cubic(c) => cubic_note(c, x, teach),
            Trace::Linear { poly } => linear_note(poly, x, teach),
            Trace::Quadratic { poly } => quadratic_note(poly, x, teach),
            Trace::RationalRoots { poly, roots, rest } => {
                rational_note(poly, roots, rest, x, teach)
            }
            Trace::Quartic(q) => quartic_note(q, x, teach),
            Trace::Factored { poly, factors } => format!(
                "{} = 0 has no rational roots, but it splits over the rationals as {}, so each factor is solved on its own{}.",
                poly_text(poly, x),
                factor_text(factors, x),
                if factors.iter().all(|f| f.len() <= 3) { " by the quadratic formula" } else { "" }
            ),
            Trace::Numeric { poly } => format!(
                "{} = 0 has degree {} and no factors over the rationals; its roots are numeric{}.",
                poly_text(poly, x),
                poly.len() - 1,
                if teach { ". From degree 5 on there is no general formula in radicals (Abel–Ruffini, 1824), and most such equations, like x^5 − x − 1 = 0, have roots that cannot be written with radicals at all" } else { "" }
            ),
        });
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("\n\n"))
    }
}

fn from_poly(c: &[Expr], x: &str) -> Expr {
    let xv = Expr::sym(x);
    add(c
        .iter()
        .enumerate()
        .map(|(k, a)| mul2(a.clone(), pow(xv.clone(), int(k as i128))))
        .collect())
}

/// (x^2-2)^2·(x^2+1): the factors with their multiplicities
fn factor_text(factors: &[Vec<Expr>], x: &str) -> String {
    let mut seen: Vec<(String, usize)> = vec![];
    for f in factors {
        let t = crate::print::show(&from_poly(f, x));
        match seen.iter_mut().find(|(s, _)| *s == t) {
            Some(e) => e.1 += 1,
            None => seen.push((t, 1)),
        }
    }
    seen.iter()
        .map(|(t, k)| {
            if *k > 1 {
                format!("({})^{}", t, k)
            } else {
                format!("({})", t)
            }
        })
        .collect::<Vec<_>>()
        .join("·")
}

// ---------- degree 1 and 2, and rational roots ----------

fn linear_note(c: &[Expr], x: &str, teach: bool) -> String {
    let r = tidy(&div(neg(c[0].clone()), c[1].clone()));
    if teach {
        format!(
            "**A linear equation.** {} = 0 has the form a·{x} + b = 0 with a = {} and b = {}. Subtract b and divide by a (a is not 0): {x} = −b/a = {}.",
            poly_text(c, x),
            sh(&c[1]),
            sh(&c[0]),
            sh(&r)
        )
    } else {
        format!("Linear: {x} = −b/a = {}.", sh(&r))
    }
}

/// "2*(5/2)^2-3*(5/2)-5": the polynomial with the value written in
fn plugged(c: &[Expr], v: &Expr) -> String {
    let vs = crate::print::show(v);
    let vs = if vs.chars().all(|ch| ch.is_ascii_alphanumeric()) {
        vs
    } else {
        format!("({})", vs)
    };
    let mut terms = vec![];
    for (k, a) in c.iter().enumerate().rev() {
        if a.is_zero() {
            continue;
        }
        let a_s = crate::print::show(a);
        let a_s =
            if a_s.contains(['+', '-']) && !a_s.starts_with('-') || a_s[1..].contains(['+', '-']) {
                format!("({})", a_s)
            } else {
                a_s
            };
        let t = match k {
            0 => a_s.clone(),
            1 => format!("{}*{}", a_s, vs),
            _ => format!("{}*{}^{}", a_s, vs, k),
        };
        let t = t.replacen("1*", "", if a.is_one() { 1 } else { 0 });
        terms.push(t);
    }
    terms.join(" + ").replace("+ -", "− ")
}

fn quadratic_note(c: &[Expr], x: &str, teach: bool) -> String {
    let (a, b, cc) = (&c[2], &c[1], &c[0]);
    let d = tidy(&sub(
        pow(b.clone(), int(2)),
        mul(vec![int(4), a.clone(), cc.clone()]),
    ));
    let roots = crate::cas::quadratic(a, b, cc);
    let real = real_coeffs(c);
    let sd = tidy(&sqrt(d.clone()));
    let rational_d = real && sd.num().is_some_and(|n| n.is_exact());
    let (sign, kind) = if !real {
        (
            "",
            "with letters its sign decides which case it is".to_string(),
        )
    } else if zero(&d) {
        ("D = 0: ", "one double root".to_string())
    } else if val(&d) > 0.0 {
        (
            "D > 0: ",
            format!(
                "two real roots{}",
                if rational_d {
                    ", rational because D is a perfect square"
                } else {
                    ""
                }
            ),
        )
    } else {
        ("D < 0: ", "two complex roots".to_string())
    };
    if !teach {
        return format!(
            "**Quadratic formula** {x} = (−b ± sqrt(D))/(2a) with D = b^2 − 4ac = {}; {}{}.",
            sh(&d),
            sign,
            kind
        );
    }
    let xv = Expr::sym(x);
    let h = tidy(&div(b.clone(), mul2(int(2), a.clone()))); // x + h
    let k = tidy(&div(d.clone(), mul2(int(4), pow(a.clone(), int(2))))); // (x + h)^2 = k
    let monic: Vec<Expr> = c.iter().map(|v| tidy(&div(v.clone(), a.clone()))).collect();
    let square = sh(&pow(add2(xv.clone(), h.clone()), int(2)));
    let mut md = vec![format!(
        "**Solving {} = 0, a quadratic, by completing the square.** Here a = {}, b = {}, c = {}{}.",
        poly_text(c, x),
        sh(a),
        sh(b),
        sh(cc),
        if real {
            ""
        } else {
            " (a is assumed not to be 0)"
        }
    )];
    md.push(format!(
        "**Step 1: make the x^2 coefficient 1.** Divide by a: {} = 0.",
        poly_text(&monic, x)
    ));
    md.push(format!(
        "**Step 2: complete the square.** (x + h)^2 = x^2 + 2h·x + h^2, so with h = b/(2a) = {} the first two terms are the start of the square {}. Add h^2 = {} to both sides and move c/a = {} across: {} = h^2 − c/a = {}. The right side is (b^2 − 4ac)/(4a^2) = D/(4a^2): the discriminant D = b^2 − 4ac = {} appears by itself.",
        sh(&h),
        square,
        sh(&tidy(&pow(h.clone(), int(2)))),
        sh(&monic[0]),
        square,
        sh(&k),
        sh(&d)
    ));
    let lhs = sh(&add2(xv.clone(), h.clone()));
    let ks = crate::print::show(&k);
    let step3 = if zero(&k) {
        format!("{} = 0, only one value", lhs)
    } else if real && val(&k) < 0.0 {
        let r = crate::print::show(&tidy(&sqrt(tidy(&neg(k.clone())))));
        let r = if r == "1" {
            String::new()
        } else {
            format!("{}·", r)
        };
        format!(
            "{} = ±sqrt({}) = ±{}i, since a negative number has no real square root",
            lhs, ks, r
        )
    } else {
        let r = crate::print::show(&tidy(&sqrt(k.clone())));
        if r == format!("sqrt({})", ks) {
            format!("{} = ±{}", lhs, r)
        } else {
            format!("{} = ±sqrt({}) = ±{}", lhs, ks, r)
        }
    };
    md.push(format!(
        "**Step 3: take square roots.** {}. Subtract h: {x} = {}. In letters this is the quadratic formula {x} = (−b ± sqrt(b^2 − 4ac))/(2a): completing the square once with letters is all the formula is.",
        step3,
        roots.iter().map(sh).collect::<Vec<_>>().join(if roots.len() == 2 { " or " } else { ", " })
    ));
    md.push(format!(
        "**Step 4: what D says.** D > 0: two real roots, where the parabola y = {} crosses the x-axis. D = 0: one double root, where it touches the axis. D < 0: two complex roots, the parabola misses the axis. A perfect-square D means rational roots. Here D = {}{}{}.",
        crate::print::show(&from_poly(c, x)),
        sh(&d),
        if real { ", so " } else { "; " },
        kind
    ));
    // the check: plug in, and the sum and product of the roots
    let (r1, r2) = (roots[0].clone(), roots.last().unwrap().clone());
    let sum = tidy(&calc::expand(&add2(r1.clone(), r2.clone())));
    let prod = tidy(&calc::expand(&mul2(r1.clone(), r2.clone())));
    let mut check = vec![];
    if real {
        let v = tidy(&calc::expand(&calc::subst(&from_poly(c, x), x, &r1)));
        check.push(format!(
            "Put {x} = {} back in: `{}` = {}.",
            crate::print::show(&r1),
            plugged(c, &r1),
            if zero(&v) {
                "0".to_string()
            } else {
                crate::print::show(&v)
            }
        ));
    }
    check.push(format!(
        "The roots add up to {} and multiply to {}, which should be −b/a = {} and c/a = {} (Vieta: a(x − r1)(x − r2) expands to a·x^2 − a(r1 + r2)·x + a·r1·r2){}.",
        sh(&sum),
        sh(&prod),
        sh(&tidy(&div(neg(b.clone()), a.clone()))),
        sh(&tidy(&div(cc.clone(), a.clone()))),
        if roots.len() == 1 { "; a double root counts twice" } else { "" }
    ));
    md.push(format!("**Step 5: check.** {}", check.join(" ")));
    md.join("\n\n")
}

fn divisors(n: i128) -> Vec<i128> {
    let n = n.abs();
    let mut d = vec![];
    let mut k = 1i128;
    while k * k <= n {
        if n % k == 0 {
            d.push(k);
            if k * k != n {
                d.push(n / k);
            }
        }
        k += 1;
    }
    d.sort();
    d
}

fn rational_note(
    c: &[Expr],
    roots: &[(Expr, usize)],
    rest: &[Expr],
    x: &str,
    teach: bool,
) -> String {
    let found: Vec<String> = roots
        .iter()
        .map(|(r, m)| match m {
            1 => crate::print::show(r),
            2 => format!("{} (twice)", crate::print::show(r)),
            m => format!("{} ({} times)", crate::print::show(r), m),
        })
        .collect();
    let left = if rest.len() > 1 {
        format!("; what is left is {} = 0", poly_text(rest, x))
    } else {
        String::new()
    };
    if !teach {
        return format!(
            "Rational roots (rational root test): {}{}.",
            found.join(", "),
            left
        );
    }
    let mut md = vec![format!(
        "**Solving {} = 0: rational roots first.**",
        poly_text(c, x)
    )];
    // integer coefficients
    let nums: Vec<crate::num::Number> = c.iter().filter_map(|e| e.num()).collect();
    let mut l = 1i128;
    for n in &nums {
        let d = n.denom().unwrap_or(1);
        l = l / gcd(l, d) * d;
    }
    let ints: Vec<i128> = nums
        .iter()
        .map(|n| n.mul(crate::num::Number::int(l)).as_int().unwrap_or(0))
        .collect();
    let scaled = if l != 1 {
        format!(
            " Multiplying by {} first gives integer coefficients: {} = 0.",
            l,
            poly_text(&ints.iter().map(|&v| int(v)).collect::<Vec<_>>(), x)
        )
    } else {
        String::new()
    };
    let zeros = ints.iter().take_while(|v| **v == 0).count();
    let ints = &ints[zeros..];
    md.push(format!(
        "**The rational root test.** If a fraction p/q in lowest terms is a root of a polynomial with integer coefficients, then p divides the constant term and q divides the leading coefficient. (Put {x} = p/q, multiply by q^n, and every term but the last is a multiple of p, every term but the first a multiple of q.) So there are only finitely many candidates to try.{}",
        scaled
    ));
    let mut cand = vec![];
    if zeros > 0 {
        cand.push(format!(
            "Every term has a factor {x}{}, so {x} = 0 is a root{} and we divide it out.",
            if zeros > 1 {
                format!("^{}", zeros)
            } else {
                String::new()
            },
            if zeros > 1 {
                format!(" {} times", zeros)
            } else {
                String::new()
            }
        ));
    }
    let (a0, an) = (ints[0], *ints.last().unwrap());
    let ps = divisors(a0);
    let qs = divisors(an);
    let mut fr: Vec<(i128, i128)> = vec![];
    for q in &qs {
        for p in &ps {
            if gcd(*p, *q) == 1 {
                fr.push((*p, *q));
            }
        }
    }
    fr.sort_by(|a, b| (a.0 * b.1).cmp(&(b.0 * a.1)));
    let show_fr = |(p, q): &(i128, i128)| {
        if *q == 1 {
            format!("±{}", p)
        } else {
            format!("±{}/{}", p, q)
        }
    };
    let list: Vec<String> = fr.iter().take(16).map(show_fr).collect();
    cand.push(format!(
        "Constant term {}, leading coefficient {}: p is one of {}, q one of {}, so the candidates are {}{} ({} in all, counting signs).",
        a0,
        an,
        ps.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", "),
        qs.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", "),
        list.join(", "),
        if fr.len() > 16 { ", …" } else { "" },
        2 * fr.len()
    ));
    md.push(format!("**The candidates.** {}", cand.join(" ")));
    let lin: Vec<String> = roots
        .iter()
        .filter(|(r, _)| !r.is_zero())
        .map(|(r, m)| {
            let f = crate::print::show(&from_poly(&[neg(r.clone()), Expr::one()], x));
            if *m > 1 {
                format!("({})^{}", f, m)
            } else {
                format!("({})", f)
            }
        })
        .collect();
    md.push(format!(
        "**Trying them.** Putting each candidate in, these give 0: {x} = {}. A root r means {x} − r divides the polynomial, so we divide each one out (and try the same r again on the quotient, which finds repeated roots). Dividing by {} leaves {}.",
        found.join(", "),
        if lin.is_empty() { format!("{x}") } else { lin.join("·") },
        if rest.len() > 1 {
            format!(
                "{}, which has no rational roots, so its roots are found next{}",
                poly_text(rest, x),
                match rest.len() {
                    3 => " by completing the square",
                    4 => " with Cardano's formula",
                    5 => " with Ferrari's method",
                    _ => "",
                }
            )
        } else {
            "a constant, so these are all the roots".to_string()
        }
    ));
    md.push("**What to notice.** The test only finds rational roots, but it is exact and needs no formula; each root found lowers the degree by one, so a cubic with one rational root is down to a quadratic.".into());
    md.join("\n\n")
}

fn gcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a
}
