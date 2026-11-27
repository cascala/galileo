use galileo_rs::algfactor as af;
use num_bigint::BigInt;
use std::time::Instant;
fn main() {
    let p: BigInt = "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001".parse().unwrap();
    let n = &p * BigInt::from(1234567891u64)
        + BigInt::from(
            std::env::args()
                .nth(1)
                .map(|a| a.parse::<i64>().unwrap())
                .unwrap_or(2),
        );
    for effort in [1u64, 10, 40] {
        let t = Instant::now();
        let r = af::catalogue_match2(&n, effort);
        println!(
            "effort {:>3}: {:?} in {:.3}s",
            effort,
            r.map(|x| x.3),
            t.elapsed().as_secs_f64()
        );
    }
    let x: BigInt = "12345678901234567890123".parse().unwrap();
    for y in [37u64, 1999] {
        let yb = BigInt::from(y);
        let m = x.pow(6) + BigInt::from(27) * yb.pow(6);
        let t = Instant::now();
        let r = af::catalogue_match2(&m, 40);
        println!(
            "x^6+27y^6, y={}: found y={:?} in {:.3}s",
            y,
            r.map(|x| x.3),
            t.elapsed().as_secs_f64()
        );
    }
    let t = Instant::now();
    let r = af::catalogue_match(&n);
    println!(
        "one variable: {:?} in {:.4}s",
        r.is_some(),
        t.elapsed().as_secs_f64()
    );
}
