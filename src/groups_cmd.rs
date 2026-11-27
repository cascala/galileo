//! Commands for factoring with groups: pm1, ecm, squfof, classfactor, and the
//! binary quadratic form tools classgroup, reduceform, composeforms, formpow.

use crate::discover::Report;
use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::groups::{self as g, Form};
use crate::num::Number;
use crate::parser::Node;
use crate::print::show;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive};
use std::rc::Rc;

fn report(kind: &'static str, text: String, markdown: String) -> Expr {
    Expr::Obj(Rc::new(Obj::Report(Report {
        kind,
        text,
        markdown,
        formulas: vec![],
        best: 0,
        vars: vec![],
        order: 0,
        target: 0,
        value: None,
    })))
}

fn bnum(n: BigInt) -> Expr {
    Expr::Num(Number::from_bigint(n))
}

fn form_expr(f: &Form) -> Expr {
    Expr::Matrix(vec![vec![
        bnum(f.a.clone()),
        bnum(f.b.clone()),
        bnum(f.c.clone()),
    ]])
}

impl Session {
    fn big_arg(&mut self, a: &Node, what: &str) -> R<BigInt> {
        let v = self.eval(a)?;
        v.num()
            .and_then(|n| n.as_bigint())
            .ok_or_else(|| format!("{} must be a whole number, got {}", what, show(&v)))
    }

    fn form_arg(&mut self, a: &Node, f: &str) -> R<Form> {
        let v = self.eval(a)?;
        let items: Vec<Expr> = match &v {
            Expr::Matrix(m) if m.len() == 1 => m[0].clone(),
            Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => {
                m.iter().map(|r| r[0].clone()).collect()
            }
            _ => vec![],
        };
        let nums: Option<Vec<BigInt>> = items
            .iter()
            .map(|e| e.num().and_then(|n| n.as_bigint()))
            .collect();
        match nums.as_deref() {
            Some([a, b, c]) => {
                let f2 = Form::new(a.clone(), b.clone(), c.clone());
                if !f2.disc().is_negative() || !a.is_positive() {
                    return Err(format!(
                        "{}: the form [a, b, c] needs a > 0 and b^2 - 4ac < 0 (got discriminant {})",
                        f,
                        f2.disc()
                    ));
                }
                Ok(f2)
            }
            _ => Err(format!(
                "{}: a form is written [a, b, c] for a x^2 + b x y + c y^2, got {}",
                f,
                show(&v)
            )),
        }
    }

    fn composite_arg(&mut self, a: &Node, f: &str) -> R<BigInt> {
        let n = self.big_arg(a, &format!("{}: n", f))?.abs();
        if n < BigInt::from(4) {
            return Err(format!("{} needs a composite number of 4 or more", f));
        }
        if crate::numtheory::is_prime(&n) {
            return Err(format!("{}: {} is prime", f, n));
        }
        Ok(n)
    }

    pub(crate) fn groups_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        Some(match f {
            "pm1" => (|| {
                let (n, b) = match args {
                    [a] => (self.composite_arg(a, "pm1")?, 100_000u64),
                    [a, b] => {
                        let b = self.big_arg(b, "pm1(n, B): B")?.to_u64().filter(|&b| (2..=100_000_000).contains(&b)).ok_or("pm1(n, B): B is a bound from 2 to 10^8")?;
                        (self.composite_arg(a, "pm1")?, b)
                    }
                    _ => return Err("usage: pm1(n) or pm1(n, B): Pollard's p - 1 method with bound B (default 100000)".into()),
                };
                let (f, gg) = g::pm1(&n, b);
                let value = crate::factor_cmd::split_rows(&n, f.as_ref());
                let text = match f {
                    Some(p) => format!(
                        "found the factor {}: for this prime p, p - 1 has no prime factor above {} (p - 1 = {}), so 3^E = 1 mod p for E = lcm(1..{}), and gcd(3^E - 1, n) = p",
                        p,
                        b,
                        factor_text(&(&p - 1u32)),
                        b
                    ),
                    None if gg.is_one() => format!(
                        "no factor: no prime p of n has p - 1 made only of primes up to {} (prime powers up to {}); try a larger B, or ecm(n), which does not depend on p - 1",
                        b, b
                    ),
                    None => format!(
                        "no factor: every prime p of n has a {}-smooth p - 1, so 3^E = 1 mod n; try a smaller B",
                        b
                    ),
                };
                self.note(text);
                Ok(value)
            })(),
            "ecm" => (|| {
                let (n, curves, b1) = match args {
                    [a] => (self.composite_arg(a, "ecm")?, 50u64, 11_000u64),
                    [a, c] => (self.composite_arg(a, "ecm")?, self.big_arg(c, "ecm: curves")?.to_u64().unwrap_or(0), 11_000),
                    [a, c, b] => (self.composite_arg(a, "ecm")?, self.big_arg(c, "ecm: curves")?.to_u64().unwrap_or(0), self.big_arg(b, "ecm: B1")?.to_u64().unwrap_or(0)),
                    _ => return Err("usage: ecm(n), ecm(n, curves) or ecm(n, curves, B1): Lenstra's elliptic curve method (defaults 50 curves, B1 = 11000)".into()),
                };
                if !(1..=100_000).contains(&curves) || !(100..=10_000_000).contains(&b1) {
                    return Err(
                        "ecm(n, curves, B1): curves from 1 to 100000, B1 from 100 to 10^7".into(),
                    );
                }
                let found = g::ecm(&n, curves, b1);
                let value = crate::factor_cmd::split_rows(&n, found.as_ref().map(|(p, _)| p));
                let text = match found {
                    Some((p, c)) => format!(
                        "found the factor {} on curve {} of {}: the group of that elliptic curve mod {} has an order made of primes up to {}, plus at most one prime up to {}",
                        p,
                        c,
                        curves,
                        p,
                        b1,
                        25 * b1
                    ),
                    None => format!(
                        "no factor with {} curves and B1 = {}; more curves or a larger B1 find larger factors (B1 = 11000 suits about 20 digits, 50000 about 25, 250000 about 30)",
                        curves, b1
                    ),
                };
                self.note(text);
                Ok(value)
            })(),
            "squfof" => (|| {
                let [a] = args else {
                    return Err("usage: squfof(n): Shanks' square forms factorization".into());
                };
                let n = self.composite_arg(a, "squfof")?;
                if n.bits() > 100 {
                    return Err("squfof: n has more than 100 bits; SQUFOF takes about n^(1/4) steps, which is too many".into());
                }
                let found = g::squfof(&n);
                let value = crate::factor_cmd::split_rows(&n, found.as_ref().map(|(p, _, _)| p));
                self.note(match found {
                    Some((p, k, steps)) => format!(
                        "found the factor {} after {} steps of the continued fraction of sqrt({}·n)",
                        p, steps, k
                    ),
                    None => "no factor found (the value is [])".into(),
                });
                Ok(value)
            })(),
            "classfactor" => (|| {
                let (n, b) = match args {
                    [a] => (self.composite_arg(a, "classfactor")?, 50_000u64),
                    [a, b] => (self.composite_arg(a, "classfactor")?, self.big_arg(b, "classfactor: B")?.to_u64().filter(|&b| (10..=10_000_000).contains(&b)).ok_or("classfactor(n, B): B from 10 to 10^7")?),
                    _ => return Err("usage: classfactor(n) or classfactor(n, B): factoring with the class group of discriminant -4kn".into()),
                };
                if n.bits() > 140 {
                    return Err("classfactor: n has more than 140 bits; the class group is too large for this method".into());
                }
                let found = g::class_factor(&n, b, 3);
                let value = crate::factor_cmd::split_rows(&n, found.as_ref().map(|(p, _, _)| p));
                let text = match found {
                    Some((p, k, f)) => format!(
                        "found the factor {}: the form {} of discriminant -4·{}·n has order 2 (it is ambiguous), and ambiguous forms are factorizations",
                        p,
                        f.text(),
                        k
                    ),
                    None => format!(
                        "no factor: the odd part of the class number was not {}-smooth for the multipliers tried; try a larger B",
                        b
                    ),
                };
                self.note(text);
                Ok(value)
            })(),
            "polyselect" => (|| {
                let usage = "usage: polyselect(n), polyselect(n, d) or polyselect(n, d, generations): number field sieve polynomials for n (at least 30 digits) of degree d (3 to 6)";
                let (n, d, gens) = match args {
                    [a] => (self.big_arg(a, "polyselect: n")?.abs(), 0usize, 30usize),
                    [a, d] => (
                        self.big_arg(a, "polyselect: n")?.abs(),
                        self.big_arg(d, "polyselect: d")?.to_usize().unwrap_or(0),
                        30,
                    ),
                    [a, d, g] => (
                        self.big_arg(a, "polyselect: n")?.abs(),
                        self.big_arg(d, "polyselect: d")?.to_usize().unwrap_or(0),
                        self.big_arg(g, "polyselect: generations")?
                            .to_usize()
                            .unwrap_or(0),
                    ),
                    _ => return Err(usage.into()),
                };
                let digits = n.to_string().len();
                if !(30..=620).contains(&digits) {
                    return Err("polyselect: n should have 30 to 620 digits".into());
                }
                let d = if d == 0 {
                    if digits < 60 {
                        3
                    } else if digits < 110 {
                        4
                    } else if digits < 220 {
                        5
                    } else {
                        6
                    }
                } else {
                    d
                };
                if !(3..=6).contains(&d) || !(1..=1000).contains(&gens) {
                    return Err("polyselect: d from 3 to 6, generations from 1 to 1000".into());
                }
                Ok(polyselect_report(&n, d, gens, self.teaching()))
            })(),
            "reduceform" => (|| {
                let [a] = args else {
                    return Err("usage: reduceform([a, b, c])".into());
                };
                let f = self.form_arg(a, "reduceform")?;
                Ok(form_expr(&f.reduce()))
            })(),
            "composeforms" => (|| {
                let [a, b] = args else {
                    return Err("usage: composeforms([a, b, c], [a2, b2, c2])".into());
                };
                let f1 = self.form_arg(a, "composeforms")?;
                let f2 = self.form_arg(b, "composeforms")?;
                if f1.disc() != f2.disc() {
                    return Err(format!(
                        "composeforms: the discriminants differ ({} and {})",
                        f1.disc(),
                        f2.disc()
                    ));
                }
                Ok(form_expr(&f1.compose(&f2)))
            })(),
            "formpow" => (|| {
                let [a, k] = args else {
                    return Err("usage: formpow([a, b, c], k)".into());
                };
                let f = self.form_arg(a, "formpow")?;
                let k = self.big_arg(k, "formpow: k")?;
                if k.is_negative() {
                    let inv = Form::new(f.a.clone(), -f.b.clone(), f.c.clone()).reduce();
                    return Ok(form_expr(&inv.pow(&-k)));
                }
                Ok(form_expr(&f.reduce().pow(&k)))
            })(),
            "classgroup" => (|| {
                let [a] = args else {
                    return Err(
                        "usage: classgroup(D): the reduced forms of discriminant D < 0".into(),
                    );
                };
                let d = self.big_arg(a, "classgroup: D")?;
                let d = d
                    .to_i64()
                    .filter(|&d| d < 0 && d >= -100_000_000)
                    .ok_or("classgroup(D): D is a negative whole number down to -10^8")?;
                if d.rem_euclid(4) != 0 && d.rem_euclid(4) != 1 {
                    return Err(format!(
                        "classgroup: a discriminant b^2 - 4ac is 0 or 1 mod 4, and {} is not",
                        d
                    ));
                }
                let fs = g::reduced_forms(d);
                let h = fs.len();
                let shown = 40;
                let mut md = vec![
                    format!(
                        "The class group of discriminant {} has **h = {}** classes of primitive forms. Each class has one reduced form $(a, b, c)$, meaning $a x^2 + b x y + c y^2$ with $|b| \\le a \\le c$.",
                        d, h
                    ),
                    String::new(),
                    "| form | order | ambiguous |".into(),
                    "|---|---|---|".into(),
                ];
                let mut tx = vec![format!(
                    "The class group of discriminant {} has h = {} classes (reduced forms a x^2 + b x y + c y^2):",
                    d, h
                )];
                let mut amb = vec![];
                for f in &fs {
                    if f.is_ambiguous() {
                        amb.push(f.clone());
                    }
                }
                for f in fs.iter().take(shown) {
                    let ord = g::form_order(f, h as u64)
                        .map(|o| o.to_string())
                        .unwrap_or("?".into());
                    let a = if f.is_ambiguous() { "yes" } else { "" };
                    md.push(format!("| {} | {} | {} |", f.text(), ord, a));
                    tx.push(format!(
                        "  {}  order {}{}",
                        f.text(),
                        ord,
                        if f.is_ambiguous() { "  ambiguous" } else { "" }
                    ));
                }
                if h > shown {
                    md.push(format!("| ... {} more | | |", h - shown));
                    tx.push(format!("  ... {} more", h - shown));
                }
                let nn = BigInt::from(-d);
                let n4 = if d % 4 == 0 {
                    BigInt::from(-d / 4)
                } else {
                    nn.clone()
                };
                let facts: Vec<String> = amb
                    .iter()
                    .filter(|f| !f.is_identity())
                    .filter_map(|f| {
                        g::ambiguous_factor(f, &n4)
                            .map(|p| format!("{} gives {} = {} · {}", f.text(), n4, p, &n4 / &p))
                    })
                    .collect();
                let line = format!(
                    "{} ambiguous forms (order 1 or 2). For D = -4N, the ambiguous forms other than the identity are exactly the ways to split N: {}",
                    amb.len(),
                    if facts.is_empty() {
                        "none here, so N has no such split (N is prime or a prime power, or the split needs a = c forms)".to_string()
                    } else {
                        facts.join("; ")
                    }
                );
                md.push(String::new());
                md.push(line.clone());
                tx.push(line);
                Ok(crate::discover::with_value(
                    report("Class group", tx.join("\n"), md.join("\n")),
                    Expr::int(h as i128),
                ))
            })(),
            _ => return None,
        })
    }
}

/// n written as a product of primes, for messages
fn factor_text(n: &BigInt) -> String {
    match crate::numtheory::factorize(n) {
        Ok(fs) => fs
            .iter()
            .map(|(p, e)| {
                if *e == 1 {
                    p.to_string()
                } else {
                    format!("{}^{}", p, e)
                }
            })
            .collect::<Vec<_>>()
            .join(" * "),
        Err(_) => n.to_string(),
    }
}

fn poly_text(f: &[BigInt]) -> String {
    crate::algfactor::show_zpoly(f, "x")
}

/// the polyselect report: base m, root sieve, evolution, and the best polynomial pair
fn polyselect_report(n: &BigInt, d: usize, gens: usize, teach: bool) -> Expr {
    use crate::polyselect as ps;
    let (bf, _, area) = ps::e_params(n);
    let base = ps::score(n, d, 1, 0, 0, 0).expect("base m");
    let (j, k, _) = ps::root_sieve(&base.f, &base.m, 4, 20_000);
    let sieved = ps::score(n, d, 1, 0, j, k).unwrap_or_else(|| base.clone());
    let start = if sieved.e > base.e {
        sieved.clone()
    } else {
        base.clone()
    };
    let seed = n.to_u64_digits().1.first().copied().unwrap_or(7);
    let hist = ps::evolve(n, d, &start, gens, 30, seed);
    let best = hist.last().unwrap().clone();
    let digits = n.to_string().len();
    let row = |name: &str, c: &ps::Cand| {
        format!(
            "| {} | {} | {} | {}, {} | {:.3e} | {:.2} | {:+.2} | {:.3e} |",
            name, c.ad, c.dm, c.j, c.k, c.skew, c.lognorm, c.alpha, c.e
        )
    };
    let mut md = vec![
        format!(
            "Polynomial selection for this {}-digit number, degree {}: pairs $f(x)$, $g(x) = x - m$ with $f(m) = N$. Smaller lognorm (size of $F(a, b)$ over the sieve region, at the best skew), more negative $\\alpha$ (more roots mod small primes) and larger Murphy $E$ (estimated chance that $F$ and $G$ are both smooth, with bounds $B = {:.0e}$ and area {:.0e}) are better. $E$ only compares polynomials for the same $N$.",
            digits, d, bf, area
        ),
        String::new(),
        "| step | leading coeff | m shift | rotation j, k | skew | lognorm | α | E |".into(),
        "|---|---|---|---|---|---|---|---|".into(),
        row("base m", &base),
        row("root sieve", &sieved),
    ];
    let step = (gens / 6).max(1);
    for (g, c) in hist.iter().enumerate() {
        if g % step == 0 || g + 1 == hist.len() {
            md.push(row(&format!("generation {}", g), c));
        }
    }
    let ratio = best.e / base.e;
    md.push(String::new());
    md.push(format!(
        "Best E is {:.1} times that of plain base m. The best pair:",
        ratio
    ));
    md.push(String::new());
    md.push(format!("* $f(x) = $ `{}`", poly_text(&best.f)));
    md.push(format!("* $g(x) = x - m$ with $m = $ `{}`", best.m));
    md.push(format!(
        "* check: $f(m) - N = {}$",
        ps::eval(&best.f, &best.m) - n
    ));
    md.push(String::new());
    let start1 = if sieved.e > base.e {
        sieved.clone()
    } else {
        base.clone()
    };
    let monic = ps::evolve_with(n, d, &start1, gens, 30, seed ^ 0x5bd1e995, true)
        .last()
        .unwrap()
        .clone();
    let own = crate::gnfs::choices(n, 20)
        .into_iter()
        .filter(|c| c.degree() == d)
        .map(|c| c.score())
        .fold(f64::MAX, f64::min);
    md.push(format!("Galileo's teaching sieve `gnfs` wants a monic $f$ and sieves a rectangle without skew ($|a| \\le 30000$, $b$ up to a few thousand), so the best pair above (leading coefficient {}) is not the one for it. A second search keeps $f$ monic and scores with the sieve's own estimate (bits of both norms over its rectangle plus $\\alpha$; lower is better). Best monic pair: score {:.1}, against {:.1} for the polynomial `gnfs` picks by itself at this degree:", best.ad, ps::gnfs_score(&monic.f, &monic.m), own));
    md.push(String::new());
    md.push(format!(
        "`gnfs({}, {}, {})`",
        n,
        poly_text(&monic.f),
        monic.m
    ));
    let mut tx = vec![format!(
        "Polynomial selection for this {}-digit number, degree {} (g(x) = x - m, f(m) = N). Lower lognorm, more negative alpha and larger Murphy E are better:",
        digits, d
    )];
    for l in md.iter().skip(4) {
        tx.push(l.replace("$", "").replace("`", "").replace("\\", ""));
    }
    let value = Expr::Matrix(vec![vec![
        crate::eval::value_of(&poly_text(&monic.f)),
        bnum(monic.m.clone()),
    ]]);
    md.push(String::new());
    md.push(
        "The value is the best monic pair as [f(x) m], ready for `gnfs(N, r(1, 1), r(1, 2))`."
            .into(),
    );
    if teach {
        let t = crate::teach::polyselect(
            n,
            d,
            &base,
            &sieved,
            &best,
            gens,
            &monic,
            ps::gnfs_score(&monic.f, &monic.m),
            own,
        );
        md.push(String::new());
        md.push(t.clone());
        tx.push(crate::teach::plain(&t));
    }
    crate::discover::with_value(
        report("Polynomial selection", tx.join("\n"), md.join("\n")),
        value,
    )
}
