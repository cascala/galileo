use galileo_rs::labsearch::*;
fn main() {
    let a: Vec<String> = std::env::args().collect();
    let r = search(
        &a[1],
        a[2].parse().unwrap(),
        a.get(3).map_or(1, |s| s.parse().unwrap()),
    );
    println!(
        "evaluated {} distinct {} secs {:?}",
        r.evaluated, r.distinct, r.seconds
    );
    for (n, e) in &r.classics {
        println!(
            "CLASSIC {:22} {:8.1} {}/{} a={:?} {}",
            n,
            e.score.score,
            e.score.solved,
            e.score.total,
            e.score.exponent(),
            e.prog
        );
    }
    println!(
        "rho test {:.1} {}/{} a={:?}",
        r.rho_test.score,
        r.rho_test.solved,
        r.rho_test.total,
        r.rho_test.exponent()
    );
    for e in &r.best {
        println!(
            "FOUND {:8.1} {}/{} a={:?} test={:?} {}",
            e.score.score,
            e.score.solved,
            e.score.total,
            e.score.exponent(),
            e.test
                .as_ref()
                .map(|t| (t.score, t.solved, speedup(t, &r.rho_test))),
            e.prog
        );
    }
}
