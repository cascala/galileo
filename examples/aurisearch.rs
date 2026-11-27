// Search for Aurifeuillean-type identities: g irreducible, g(c x^e) reducible.
use galileo_rs::algfactor as af;
use num_bigint::BigInt;
fn sqfree(c: i64) -> bool {
    let a = c.abs();
    (2..=a).take_while(|p| p * p <= a).all(|p| a % (p * p) != 0)
}
fn subst(g: &[BigInt], c: i64, e: usize) -> Vec<BigInt> {
    let mut out = vec![BigInt::from(0); (g.len() - 1) * e + 1];
    let mut cp = BigInt::from(1);
    for (i, a) in g.iter().enumerate() {
        out[i * e] = a * &cp;
        cp *= c;
    }
    out
}
fn nfactors(f: &[BigInt]) -> u32 {
    af::factor_zpoly(f)
        .map(|(_, fs)| fs.iter().map(|(_, m)| *m).sum())
        .unwrap_or(1)
}
fn main() {
    let args: Vec<i64> = std::env::args()
        .skip(1)
        .map(|a| a.parse().unwrap())
        .collect();
    let (amax, kmax, mmax, cmax, e) = (
        args[0] as usize,
        args[1],
        args[2],
        args[3],
        args[4] as usize,
    );
    let cyclo: Vec<Vec<BigInt>> = (1..200).map(af::cyclotomic).collect();
    for a in 2..=amax {
        for b in 1..a {
            for k in -kmax..=kmax {
                if k == 0 {
                    continue;
                }
                for m in -mmax..=mmax {
                    if m == 0 {
                        continue;
                    }
                    let mut g = vec![BigInt::from(0); a + 1];
                    g[a] = 1.into();
                    g[b] = k.into();
                    g[0] = m.into();
                    if nfactors(&g) > 1 {
                        continue;
                    }
                    // skip g(y) = q(y^j): those reduce to a smaller family
                    if (2..=a).any(|j| a % j == 0 && b % j == 0) {
                        continue;
                    }
                    let known = cyclo.contains(&g);
                    for c in -cmax..=cmax {
                        if c == 0 || c == 1 || !sqfree(c) {
                            continue;
                        }
                        let h = subst(&g, c, e);
                        if nfactors(&h) > 1 {
                            let fs = af::factor_zpoly(&h).unwrap().1;
                            let parts: Vec<String> = fs
                                .iter()
                                .map(|(p, _)| format!("({})", af::show_zpoly(p, "x")))
                                .collect();
                            if std::env::var("RUST_OUT").is_ok() {
                                let cs: Vec<String> = h.iter().map(|v| v.to_string()).collect();
                                println!(
                                    "    (\"{}{} with y = {}x^{}\", &[{}]),",
                                    if known { "cyclotomic " } else { "" },
                                    af::show_zpoly(&g, "y"),
                                    c,
                                    e,
                                    cs.join(", ")
                                );
                            } else {
                                println!(
                                    "{}g = {}, c = {}: g({}x^{}) = {}",
                                    if known { "[cyclotomic] " } else { "" },
                                    af::show_zpoly(&g, "y"),
                                    c,
                                    c,
                                    e,
                                    parts.join(" * ")
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}
