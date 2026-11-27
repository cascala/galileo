//! Teaching explanations (explain = "teach"): the method walked through with the
//! numbers of the run that just happened. Each function returns Markdown; the
//! plain form (REPL comments) is the same text without the Markdown marks.
//!
//! Style: say what the step is for before what it did, show real numbers that a
//! reader can check by hand, and end with what to notice.

use crate::algfactor as af;
use crate::gnfs::{self, Choice, NfsRun};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

/// Markdown to plain text for `//` comments
pub fn plain(md: &str) -> String {
    md.replace("**", "").replace('`', "")
}

fn poly(f: &[BigInt]) -> String {
    af::show_zpoly(f, "x")
}

/// a number with its digit count when long
fn num(x: &BigInt) -> String {
    let s = x.to_string();
    let d = s.trim_start_matches('-').len();
    if d > 12 {
        format!("{} ({} digits)", s, d)
    } else {
        s
    }
}

/// x written as a product of primes up to `limit`, with what is left over
fn factored(x: &BigInt, limit: u64) -> (String, BigInt) {
    let (parts, rest) = gnfs::small_parts(x, limit);
    let mut t: Vec<String> = vec![];
    if x.is_negative() {
        t.push("-1".into());
    }
    for (p, e) in parts {
        t.push(if e > 1 {
            format!("{}^{}", p, e)
        } else {
            p.to_string()
        });
    }
    if !rest.is_one() {
        t.push(rest.to_string());
    }
    if t.is_empty() {
        t.push("1".into());
    }
    (t.join(" · "), rest)
}

/// "2^43" when x is a power of a small base, else x
fn power_form(x: &BigInt) -> Option<String> {
    for b in 2u32..=12 {
        let mut k = 0;
        let mut v = x.clone();
        while v > BigInt::one() && (&v % b).is_zero() {
            v /= b;
            k += 1;
        }
        if v.is_one() && k > 1 {
            return Some(format!("{}^{}", b, k));
        }
    }
    None
}

fn with_commas(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::new();
    for (i, c) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// f(m) as a multiple of N, e.g. "2 · N"
fn multiple_of_n(f: &[BigInt], m: &BigInt, n: &BigInt) -> String {
    let v = af::peval(f, m);
    if (&v % n).is_zero() {
        let k = &v / n;
        if k.is_one() {
            "N".into()
        } else {
            format!("{} · N", k)
        }
    } else {
        "not a multiple of N".into()
    }
}

/// the root of f mod p list for the first primes, to show which have 0, 1 or d roots
fn root_examples(f: &[BigInt], d: usize) -> (Option<(u64, Vec<u64>)>, Option<u64>) {
    let mut full = None;
    let mut none = None;
    for p in crate::factorlab::small_primes(200) {
        if (f.last().unwrap() % p).is_zero() {
            continue;
        }
        let r = gnfs::roots_mod(f, p);
        if r.len() == d && full.is_none() && p > d as u64 {
            full = Some((p, r));
        } else if r.is_empty() && none.is_none() {
            none = Some(p);
        }
    }
    (full, none)
}

/// The number field sieve, step by step with this run's numbers
pub fn gnfs(n: &BigInt, run: &NfsRun, work: &str, time: &str) -> String {
    let t = &run.teach;
    let digits = n.to_string().len();
    let mut md: Vec<String> = vec![];
    if let Some((g, gm, gv, f, m)) = &t.alg_split {
        let mpow = power_form(m)
            .map(|s| format!(" = {}", s))
            .unwrap_or_default();
        md.push(format!(
            "**The polynomial.** To factor N = {}, take f(x) = {} and m = {}{}: f(m) = {} = {}, so f(m) ≡ 0 (mod N) as the method needs.",
            n,
            poly(f), m, mpow, num(&af::peval(f, m)), multiple_of_n(f, m, n)
        ));
        md.push(format!(
            "**It factors.** Over the integers f(x) = {}. Then f(m) is the product of the factors' values at m, and since N divides f(m), each prime of N divides one of those values. For g(x) = {}: g(m) = {}, and gcd(g(m), N) = {}. That is a factor, found by algebra alone: no sieve, no matrix, no square root. It is the algebraic factoring of lesson 3 in another form.",
            t.f_factors.iter().map(|g| format!("({})", poly(g))).collect::<Vec<_>>().join(" · "),
            poly(g),
            num(gm),
            gv
        ));
        md.push(format!("**What to notice.** The sieve needs an irreducible f; when the f you are given factors, that is a shortcut, not a problem. Work: {} ({}).", work, time));
        return md.join("\n\n");
    }
    md.push(format!(
        "**Goal.** Factor N = {} ({} digits). Like the quadratic sieve, the number field sieve looks for two numbers X and Y with X² ≡ Y² (mod N) but X ≢ ±Y. Then N divides X² − Y² = (X − Y)(X + Y) without dividing either factor, so gcd(X − Y, N) is a proper factor. The squares are built by multiplying many small pieces whose prime factors are all small.",
        n, digits
    ));
    let Some(ch) = run.choice.clone() else {
        md.push(format!(
            "No sieve ran: {}. The step-by-step log below says why.",
            run.log.last().cloned().unwrap_or_default()
        ));
        md.push(format!("```\n{}\n```", run.log.join("\n")));
        return md.join("\n\n");
    };
    let (f, m) = (ch.f.clone(), ch.m.clone());
    let d = f.len() - 1;
    // 1. polynomial
    let mpow = power_form(&m)
        .map(|s| format!(" = {}", s))
        .unwrap_or_default();
    md.push(format!(
        "**Step 1: two worlds that agree mod N.** Galileo chose f(x) = {} and m = {}{}. Check: f(m) = {} = {}, so f(m) ≡ 0 (mod N). ({}.) Let α be a root of f, a number with f(α) = 0. Sums and products of numbers a − bα can be worked out like polynomials in α, and if you then put m in place of α the answer is still right modulo N, because f(α) = 0 and f(m) ≡ 0. So the plan: find pairs (a, b) that multiply to a square in the world of α **and** to a square among ordinary numbers a − bm. Mod N the two squares are then equal: that gives X² ≡ Y².",
        poly(&f), m, mpow, num(&af::peval(&f, &m)), multiple_of_n(&f, &m, n), how_text(&ch)
    ));
    let (rn10, an10) = (
        gnfs::norm_estimate(&f, &m).0 * std::f64::consts::LOG10_2,
        gnfs::norm_estimate(&f, &m).1 * std::f64::consts::LOG10_2,
    );
    let qs10 = (gnfs::lg(n) * std::f64::consts::LOG10_2 / 2.0).round() as usize;
    let gain = rn10.max(an10) < qs10 as f64;
    md.push(format!(
        "Why bother with two worlds? The numbers whose factors we need are a − bm (about 10^{:.0} here) and the **norm** F(a, b) = b^{}·f(a/b) of a − bα (about 10^{:.0}). The quadratic sieve would need numbers near √N ≈ 10^{}. {}",
        rn10, d, an10, qs10,
        if gain {
            "Small numbers split into small primes far more often, and that is the whole advantage."
        } else {
            "So for a number this small there is no gain yet: the two worlds pay off only for large N, because a − bm and the norm grow like N^(1/d) while √N grows like N^(1/2)."
        }
    ));
    if t.fb == 0 {
        md.push(format!(
            "The run stopped before the sieve:\n\n```\n{}\n```",
            run.log.join("\n")
        ));
        return md.join("\n\n");
    }
    // 2. factor bases
    let (full, none) = root_examples(&f, d);
    let mut fbt = format!(
        "**Step 2: factor bases.** \"Small\" means: no prime factor above B = {}. On the ordinary (rational) side that is the {} primes {}, …. On the algebraic side a prime p divides F(a, b) exactly when a ≡ b·r (mod p) for a root r of f mod p, so the factor base is the {} pairs (p, r) with f(r) ≡ 0 (mod p): {}, ….",
        t.fb,
        t.nrfb,
        t.rfb
            .iter()
            .take(8)
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", "),
        t.nafb,
        t.afb
            .iter()
            .take(6)
            .map(|(p, r)| format!("({}, {})", p, r))
            .collect::<Vec<_>>()
            .join(", ")
    );
    if let Some((p, rs)) = &t
        .afb
        .iter()
        .find(|(p, _)| *p > 3)
        .map(|&(p, r)| (p, r))
        .map(|(p, r)| (p, vec![r]))
    {
        let r = rs[0];
        let v = af::peval(&f, &BigInt::from(r));
        fbt.push_str(&format!(
            " Check one by hand: f({}) = {}, which is divisible by {}.",
            r, v, p
        ));
    }
    let mut notes = vec![];
    if let Some((p, rs)) = full {
        notes.push(format!(
            "mod {} f has {} roots ({})",
            p,
            if d == 2 {
                "both".to_string()
            } else {
                format!("all {}", d)
            },
            rs.iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if let Some(p) = none {
        notes.push(format!(
            "mod {} it has none, so {} never divides a norm",
            p, p
        ));
    }
    if !notes.is_empty() {
        fbt.push_str(&format!(
            " A prime can have several roots or none: {}.",
            notes.join("; ")
        ));
    }
    md.push(fbt);
    // 3. sieve
    let per = if t.candidates > 0 {
        t.pairs / t.candidates
    } else {
        0
    };
    md.push(format!(
        "**Step 3: the sieve.** Galileo tried {} and every a from −{} to {}: {} pairs. Factoring each would be far too slow. Instead, for each prime p it adds log p at every a with a ≡ b·m (mod p) on one side and a ≡ b·r (mod p) on the other, stepping by p exactly like the sieve of Eratosthenes. Where both totals come close to the size of the number, the pair is checked by division. Only {} pairs (one in {}) were worth checking, and {} of those were **relations**: pairs where both sides split.",
        if run.lines == 1 { "the line b = 1".to_string() } else { format!("b = 1, 2, …, {}", run.lines) },
        t.half,
        t.half,
        with_commas(t.pairs),
        with_commas(t.candidates),
        with_commas(per),
        with_commas((t.full + t.partial) as u64)
    ));
    let mut ex = vec![];
    for &(a, b) in &t.first {
        if ex.len() == 3 {
            break;
        }
        let r = BigInt::from(a) - BigInt::from(b) * &m;
        let nr = gnfs::norm_of(&f, a, b);
        let (rs, rl) = factored(&r, t.fb);
        let (ns, nl) = factored(&nr, t.fb);
        if rl.is_one() && nl.is_one() {
            ex.push(format!(
                "* (a, b) = ({}, {}): a − bm = {} = {}, and F(a, b) = {} = {}",
                a, b, r, rs, nr, ns
            ));
        }
    }
    if !ex.is_empty() {
        md.push(format!(
            "Some relations, with every prime at most {}:\n\n{}",
            t.fb,
            ex.join("\n")
        ));
    }
    let mut lpt = format!(
        "Of the relations, {} are **full** (both sides split completely) and {} have one **large prime**, between {} and {}, left over on one side. One such relation is useless, because its large prime appears only once and a square needs every prime an even number of times. Two relations with the same large prime are useful together.",
        t.full, t.partial, t.fb, t.lp
    );
    if let Some((side, p, (a1, b1), (a2, b2))) = t.lp_pair {
        lpt.push_str(&format!(
            " For example ({}, {}) and ({}, {}) both have {} on the {} side; their product has {}².",
            a1,
            b1,
            a2,
            b2,
            p,
            if side == 0 { "rational" } else { "algebraic" },
            p
        ));
    }
    md.push(lpt);
    // 4. linear algebra
    let chs = t
        .chars
        .iter()
        .take(2)
        .map(|(q, s)| format!("(q, s) = ({}, {})", q, s))
        .collect::<Vec<_>>()
        .join(" and ");
    md.push(format!(
        "**Step 4: choosing a square.** Write each relation as a row of 0s and 1s: for every prime (and every algebraic pair), is its exponent odd? Plus one entry for each sign. A set of rows that adds up to all zeros mod 2 is a set of relations whose product has only even exponents: a square. After dropping relations with a prime that no other relation has (they can never be in a square), {} relations remained, using {} primes and pairs. More rows than columns guarantees such sets exist. Merging rows that share a rare prime shrank the matrix to {} × {}, and Gaussian elimination mod 2 found {} sets. Even exponents are not quite enough on the algebraic side (units and the class group get in the way), so the matrix has {} more columns, **quadratic characters**: for primes q above the large primes, with a root s of f mod q, the Legendre symbol (a − b·s | q) must be +1 for a real square. The first two are {}.",
        t.kept,
        t.used,
        t.rows,
        t.cols,
        t.deps,
        t.nchars,
        chs
    ));
    // 5. square roots
    if t.dep_size > 0 {
        md.push(format!(
            "**Step 5: the two square roots.** Galileo used set {} ({} relations). Rational side: the product of the {} numbers a − bm has {} digits, and every prime in it has an even exponent, so halving the exponents gives its square root, kept mod N; times f'(m) that is X. (Why f'? A square root of ∏(a − bα) can have fractions in its coefficients; the extra factor f'(α)² rules that out. To keep the two sides equal mod N, the rational side gets the matching factor f'(m)².) Algebraic side: β with β² = f'(α)²·∏(a − bα) is the hard part of the method. Galileo computes it modulo q = {}, where f has {} roots ({}): at each root it is an ordinary square root mod q, made exact mod q^k by Hensel lifting (Newton's method for remainders), then put back together into a polynomial in α. {}Only Y = β(m) mod N is needed.",
            t.tried,
            t.dep_size,
            t.dep_size,
            t.product_digits,
            t.q,
            if d == 2 { "both".to_string() } else { format!("all {}", d) },
            t.q_roots.iter().map(|r| r.to_string()).collect::<Vec<_>>().join(", "),
            coef_digits(run)
                .map(|k| format!("The coefficients of β have about {} digits. ", k))
                .unwrap_or_default()
        ));
        let g1 = (&t.x - &t.y).gcd(n);
        let g2 = (&t.x + &t.y).gcd(n);
        let chk = (&t.x * &t.x - &t.y * &t.y).mod_floor(n);
        md.push(format!(
            "**Step 6: the factor.** X = {} and Y = {}. Check: X² − Y² mod N = {}. And gcd(X − Y, N) = {}, gcd(X + Y, N) = {}: the two factors.{}",
            t.x,
            t.y,
            chk,
            g1,
            g2,
            if t.tried > 1 {
                format!(" The first {} set(s) gave X ≡ ±Y, which tells nothing; each set works with chance about 1/2 when N has two prime factors, so a few spare sets are kept.", t.tried - 1)
            } else {
                " The first set worked; each one works with chance about 1/2 when N has two prime factors, so a few spare sets are kept.".into()
            }
        ));
    } else {
        md.push(format!(
            "No set gave a factor this time. The log:\n\n```\n{}\n```",
            run.log.join("\n")
        ));
    }
    let lp_share = if t.full + t.partial > 0 {
        100 * t.partial / (t.full + t.partial)
    } else {
        0
    };
    md.push(format!(
        "**What to notice.** (1) The numbers that had to split were about 10^{:.0} and 10^{:.0}, against 10^{} for the quadratic sieve. (2) The sieve threw away all but one pair in {} without dividing. (3) {}% of the relations had a large prime: allowing them is cheap and multiplies the yield. (4) The algebraic square root has huge coefficients, but X and Y are only needed mod N. Work: {} ({}).",
        rn10,
        an10,
        qs10,
        with_commas(per),
        lp_share,
        work,
        time
    ));
    md.join("\n\n")
}

/// digits of the algebraic square root's coefficients, from the run's log
fn coef_digits(run: &NfsRun) -> Option<usize> {
    run.log.iter().rev().find_map(|l| {
        let i = l.find("coefficients have about ")?;
        l[i + 24..].split_whitespace().next()?.parse().ok()
    })
}

fn how_text(c: &Choice) -> String {
    if c.how.starts_with("special form") {
        let s = c.how.trim_start_matches("special form: ");
        format!("Special form: {}", s)
    } else if c.how == "your polynomial" {
        "You gave this polynomial".into()
    } else if c.how.starts_with("base m") {
        "Base m: m is about N^(1/d), and the digits of N in base m are the coefficients of f, so f(m) = N".into()
    } else {
        c.how.clone()
    }
}

/// nfspoly: what the columns mean, with the best polynomial's numbers
pub fn nfspoly(n: &BigInt, list: &[Choice]) -> String {
    let mut md = vec![];
    let Some(best) = list.first() else {
        return "No polynomial found.".into();
    };
    md.push("**What this table is for.** The number field sieve works with any f and m with f(m) ≡ 0 (mod N); the choice decides how big the numbers are that must split into small primes, and so how long the sieve runs. Each row is one candidate pair f, m, best first.".into());
    let (rn, an) = gnfs::norm_estimate(&best.f, &best.m);
    md.push(format!(
        "**The columns, using the top row** f(x) = {}, m = {}. Check: f(m) = {}. The **norm bits** {:.1} estimate how large the two numbers are over a typical sieve region: the rational one a − bm is about 2^{:.0} and the algebraic norm F(a, b) about 2^{:.0}; together {:.1} bits, because a pair must be smooth on both sides at once. **alpha** ({:+.2}; Murphy's alpha, nothing to do with the root α of f) corrects for small primes: a polynomial with many roots mod 2, 3, 5, 7, … has values divisible by small primes more often than a random number, which acts like having fewer bits (alpha below 0); with few roots it is the other way round (alpha above 0). {} The **score** is norm bits + alpha. Lower is better: at these sizes 10 bits less makes a pair several times more likely to be smooth on both sides.",
        poly(&best.f), best.m, multiple_of_n(&best.f, &best.m, n), best.norm_bits, rn, an, best.norm_bits, best.alpha,
        if best.alpha > 0.0 {
            format!("Counting the roots mod every prime up to 200, this f's values are divisible by small primes less often than random numbers of the same size, which costs it {:.2} bits. (Even one root mod every prime is a little worse than a random number, which is divisible by p a bit more often than once in p tries, counting p², p³, …; so alpha is above 0 unless f has extra roots.)", best.alpha)
        } else {
            format!("Counting the roots mod every prime up to 200, this f's values are divisible by small primes more often than random numbers of the same size, which saves it {:.2} bits.", -best.alpha)
        }
    ));
    let d = best.f.len() - 1;
    let roots: Vec<String> = [2u64, 3, 5, 7, 11, 13]
        .iter()
        .map(|&p| format!("{} mod {}", gnfs::roots_mod(&best.f, p).len(), p))
        .collect();
    md.push(format!(
        "Roots of the top f modulo the first primes: {}. A random polynomial has on average one root mod each prime (alpha counts all primes up to 200).",
        roots.join(", ")
    ));
    let degs: Vec<String> = list
        .iter()
        .map(|c| c.degree())
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .filter_map(|k| list.iter().find(|c| c.degree() == k))
        .map(|c| {
            let (r, a) = gnfs::norm_estimate(&c.f, &c.m);
            format!(
                "degree {}: rational 2^{:.0}, algebraic 2^{:.0}",
                c.degree(),
                r,
                a
            )
        })
        .collect();
    md.push(format!(
        "**The degree is a balance.** A higher degree d makes m ≈ N^(1/d) smaller, so a − bm shrinks; but F(a, b) has terms like a^d and grows. Best by degree in this list: {}. The top row has degree {}, where the two sides balance best for a number of this size.",
        degs.join("; "), d
    ));
    let special = list.iter().any(|c| c.how.starts_with("special"));
    md.push(if special {
        "**What to notice.** Special forms (N divides b^k ± 1) give tiny coefficients, so their rows beat base m by many bits: that is the special number field sieve. The value is a table with one row [f(x) m score] per candidate; r(1, 1) and r(1, 2) are the best f and m, ready for gnfs(N, f, m).".to_string()
    } else {
        "**What to notice.** Without a special form, base m gives coefficients about as large as m itself, which is why the scores are high; polyselect searches harder. The value is a table with one row [f(x) m score] per candidate; r(1, 1) and r(1, 2) are the best f and m, ready for gnfs(N, f, m).".to_string()
    });
    md.join("\n\n")
}

/// polyselect: the three search steps with this run's numbers
#[allow(clippy::too_many_arguments)]
pub fn polyselect(
    n: &BigInt,
    d: usize,
    base: &crate::polyselect::Cand,
    sieved: &crate::polyselect::Cand,
    best: &crate::polyselect::Cand,
    gens: usize,
    monic: &crate::polyselect::Cand,
    monic_score: f64,
    own_score: f64,
) -> String {
    let mut md = vec![];
    md.push(format!(
        "**What polyselect is for.** The sieve's running time depends mostly on the polynomial pair f(x), g(x) = x − m with f(m) = N. For a {}-digit N good and bad choices can differ by a factor of ten in sieve time, so serious factorizations spend a few percent of their effort just searching. polyselect searches in three steps and judges candidates by **Murphy's E**, an estimate of the chance that F(a, b) and a − bm are both smooth at a random point of the sieve region (higher is better).",
        n.to_string().len()
    ));
    md.push(format!(
        "**Step 1: base m.** Take m ≈ N^(1/{}) and write N in base m; the digits are the coefficients: f(x) = {}, m = {}. Then f(m) = N exactly. Its E is {:.3e}, alpha {:+.2}, lognorm {:.2} (the log of the typical size of F(a, b), at the best skew {:.3e}: the sieve region is that many times wider in a than in b).",
        d, poly(&base.f), base.m, base.e, base.alpha, base.lognorm, base.skew
    ));
    md.push(format!(
        "**Step 2: root sieve.** Adding (j·x + k)·(x − m) to f does not change f(m), so it keeps f(m) = N for free, but it changes how many roots f has mod small primes. A sieve over j and k found j = {}, k = {}: alpha went from {:+.2} to {:+.2} (more negative = more roots = values divisible by small primes more often) and E from {:.3e} to {:.3e}.{}",
        sieved.j, sieved.k, base.alpha, sieved.alpha, base.e, sieved.e,
        if sieved.e <= base.e { " Here the rotation made the coefficients larger than alpha gained, so the search starts from plain base m." } else { "" }
    ));
    md.push(format!(
        "**Step 3: evolution.** A population of 30 candidates (leading coefficient, a shift of m, a rotation) is changed at random for {} generations, and the ones with the highest E survive. Best: leading coefficient {}, alpha {:+.2}, lognorm {:.2}, E = {:.3e}, which is {:.1} times plain base m. The table above shows how E rose generation by generation.",
        gens, best.ad, best.alpha, best.lognorm, best.e, best.e / base.e
    ));
    md.push(format!(
        "**For Galileo's own sieve.** gnfs needs a monic f (leading coefficient 1) and sieves a plain rectangle, so a second search keeps f monic and scores with gnfs's own estimate (bits of both norms plus alpha, lower is better): {:.1} for the pair found, against {:.1} for the polynomial gnfs picks by itself. The value of this command is that pair, [f(x) m] = [{} {}], so r = polyselect(N, {}, …) and then gnfs(N, r(1, 1), r(1, 2)) uses it.",
        monic_score, own_score, poly(&monic.f), monic.m, d
    ));
    md.push("**What to notice.** E compares polynomials for the same N and the same kind of sieve only. A better polynomial saves a constant factor; it does not change how the cost grows with N.".into());
    md.join("\n\n")
}
