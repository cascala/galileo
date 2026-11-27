use galileo_rs::polyselect::*;
use num_bigint::BigInt;
use std::time::Instant;
fn main() {
    let n: BigInt = galileo_rs::rsa_data::RSA_NUMBERS
        .iter()
        .find(|r| r.0 == "RSA100")
        .unwrap()
        .1
        .parse()
        .unwrap();
    for d in [4usize, 5] {
        let t = Instant::now();
        let c = score(&n, d, 1, 0, 0, 0).unwrap();
        eprintln!(
            "d={} base-m: f={:?}\n  skew {:.3e} lognorm {:.2} alpha {:.2} E {:.3e} ({:?})",
            d,
            c.f.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
            c.skew,
            c.lognorm,
            c.alpha,
            c.e,
            t.elapsed()
        );
        let t = Instant::now();
        let (j, k, a) = root_sieve(&c.f, &c.m, 3, 20000);
        let r = score(&n, d, 1, 0, j, k).unwrap();
        eprintln!(
            "  root sieve j={} k={} est alpha {:.2}: alpha {:.2} lognorm {:.2} E {:.3e} ({:?})",
            j,
            k,
            a,
            r.alpha,
            r.lognorm,
            r.e,
            t.elapsed()
        );
        let t = Instant::now();
        let h = evolve(&n, d, &r, 30, 30, 12345);
        for (g, c) in h.iter().enumerate().step_by(5) {
            eprintln!(
                "  gen {} ad {} dm {} j {} k {}: alpha {:.2} lognorm {:.2} E {:.3e}",
                g, c.ad, c.dm, c.j, c.k, c.alpha, c.lognorm, c.e
            );
        }
        eprintln!("  evolve {:?}", t.elapsed());
    }
}
