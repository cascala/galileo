use galileo_rs::groups::*;
use galileo_rs::numtheory as nt;
use num_bigint::BigInt;
use std::time::Instant;
fn main() {
    // class group sanity: h(-23) = 3, h(-47) = 5, h(-4*5) = 2, h(-71)=7
    for d in [-23i64, -47, -20, -71, -84] {
        let fs = reduced_forms(d);
        // closure: composition of any two forms is in the list
        let ok = fs
            .iter()
            .take(30)
            .all(|f| fs.iter().take(30).all(|g| fs.contains(&f.compose(g))));
        let order_sum: Vec<u64> = fs
            .iter()
            .take(8)
            .map(|f| form_order(f, 10000).unwrap())
            .collect();
        eprintln!(
            "D={} h={} closed={} orders={:?}",
            d,
            fs.len(),
            ok,
            order_sum
        );
    }
    let p = nt::next_prime(&BigInt::from(10u64).pow(12));
    let q = nt::next_prime(&BigInt::from(10u64).pow(13));
    let n = &p * &q;
    let t = Instant::now();
    eprintln!("squfof {:?} {:?}", squfof(&n), t.elapsed());
    let t = Instant::now();
    eprintln!(
        "class {:?} {:?}",
        class_factor(&n, 50000, 3).map(|x| (x.0, x.1, x.2.text())),
        t.elapsed()
    );
    let t = Instant::now();
    eprintln!("pm1 {:?} {:?}", pm1(&n, 100000).0, t.elapsed());
    for digits in [8u32, 10, 12] {
        let p = nt::next_prime(&BigInt::from(10u64).pow(digits));
        let q = nt::next_prime(&(BigInt::from(10u64).pow(digits) * 3u32));
        let t = Instant::now();
        eprintln!(
            "class {} digits {:?} {:?}",
            2 * digits,
            class_factor(&(&p * &q), 50000, 3).map(|x| (x.0, x.1, x.2.text())),
            t.elapsed()
        );
    }
    let p = nt::next_prime(&BigInt::from(10u64).pow(22));
    let q = nt::next_prime(&BigInt::from(10u64).pow(40));
    let n = &p * &q;
    let t = Instant::now();
    eprintln!(
        "ecm 23-digit factor {:?} {:?}",
        ecm(&n, 300, 11000),
        t.elapsed()
    );
}
