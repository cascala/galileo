use galileo_rs::factorlab::*;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let m = &args[1];
    let d: u32 = args[2].parse().unwrap();
    for seed in 0..3 {
        let (n, p, _q) = semiprime(d, seed);
        let c = Clock::start();
        start_budget(1e12, 120.0);
        let f = run_method(m, &n, seed);
        let u = end_budget();
        println!(
            "{} {} d={} ok={} units={:.0} t={:.3}",
            m,
            n,
            d,
            f.as_ref().map_or(false, |f| f == &p || f * &p == n),
            u,
            c.seconds().unwrap()
        );
    }
}
