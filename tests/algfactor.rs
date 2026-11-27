use galileo_rs::algfactor::*;
use num_bigint::BigInt;

fn zp(v: &[i64]) -> ZPoly {
    v.iter().map(|&c| BigInt::from(c)).collect()
}

fn s(p: &ZPoly) -> String {
    format!("{:?}", p.iter().map(|c| c.to_string()).collect::<Vec<_>>())
}

#[test]
fn cyclotomics() {
    assert_eq!(cyclotomic(12), zp(&[1, 0, -1, 0, 1]));
    assert_eq!(cyclotomic(1), zp(&[-1, 1]));
    assert_eq!(cyclotomic(105)[7], BigInt::from(-2));
    assert_eq!(
        cyclotomic_value(8, &BigInt::from(10), &BigInt::from(1)),
        BigInt::from(10001)
    );
}

#[test]
fn polys() {
    let (c, f) = factor_zpoly(&zp(&[4, 0, 0, 0, 1])).unwrap();
    assert_eq!(c, BigInt::from(1));
    assert_eq!(f.len(), 2, "{:?}", f);
    let a = zp(&[5, 2, 0, 1]);
    let b = zp(&[7, -1, 0, 0, 1]);
    let q = zp(&[3, 0, 2]);
    let prod = pmul(&pmul(&a, &b), &pmul(&q, &q));
    let prod: ZPoly = prod.iter().map(|x| x * 6).collect();
    let (c, f) = factor_zpoly(&prod).unwrap();
    assert_eq!(c, BigInt::from(6));
    let mut got: Vec<(String, u32)> = f.iter().map(|(p, m)| (s(p), *m)).collect();
    got.sort();
    let mut want = vec![(s(&a), 1), (s(&b), 1), (s(&q), 2)];
    want.sort();
    assert_eq!(got, want);
    // irreducible but splits mod every prime
    assert_eq!(factor_zpoly(&zp(&[1, 0, -10, 0, 1])).unwrap().1.len(), 1);
    assert_eq!(
        factor_zpoly(&zp(&[-1, 0, 0, 0, 0, 0, 0, 0, 1]))
            .unwrap()
            .1
            .len(),
        4
    );
    // x^n + x + 1 has x^2 + x + 1 when n = 2 mod 3
    let mut v = vec![0i64; 21];
    v[0] = 1;
    v[1] = 1;
    v[20] = 1;
    let f = factor_zpoly(&zp(&v)).unwrap().1;
    assert!(f.iter().any(|(p, _)| *p == zp(&[1, 1, 1])), "{:?}", f);
    // a product of many small factors
    let mut p = zp(&[1]);
    for k in 1..9 {
        p = pmul(&p, &zp(&[k, 1, 0, 1]));
    }
    assert_eq!(factor_zpoly(&p).unwrap().1.len(), 9); // x^3 + x + 2 = (x + 1)(x^2 - x + 2)
}

#[test]
fn aurifeuille() {
    assert_eq!(
        aurifeuille_poly(2, 4).map(|p| s(&p)),
        Some(s(&zp(&[1, -2, 2]))).or(Some(s(&zp(&[1, 2, 2]))))
    );
    for (s_, n) in [
        (2u64, 4u64),
        (2, 12),
        (2, 20),
        (3, 6),
        (3, 18),
        (5, 5),
        (5, 15),
        (6, 12),
        (7, 14),
        (10, 20),
        (13, 13),
        (11, 22),
    ] {
        assert!(aurifeuille_poly(s_, n).is_some(), "s = {} n = {}", s_, n);
    }
    assert!(aurifeuille_poly(5, 10).is_none());
}

#[test]
fn numbers() {
    // 2^58 + 1 = 5 * 107367629 * 536903681 (Landry, Aurifeuille)
    let p = algebraic_pieces(&BigInt::from(2), &BigInt::from(1), 58, true).unwrap();
    let vals: Vec<String> = p.iter().map(|x| x.value.to_string()).collect();
    assert!(
        vals.contains(&"107367629".to_string()) && vals.contains(&"536903681".to_string()),
        "{:?}",
        vals
    );
    let p = algebraic_pieces(&BigInt::from(10), &BigInt::from(1), 300, true).unwrap();
    let prod: BigInt = p.iter().map(|x| x.value.clone()).product();
    assert_eq!(prod, num_traits::pow(BigInt::from(10), 300) + 1);
    assert_eq!(
        detect_binomial(&(num_traits::pow(BigInt::from(10), 300) + 1)),
        Some((BigInt::from(10), 300, true))
    );
    assert_eq!(
        detect_binomial(&(num_traits::pow(BigInt::from(4), 30) - 1)),
        Some((BigInt::from(2), 60, false))
    );
}

#[test]
fn prime_exponent() {
    let p = algebraic_pieces(&BigInt::from(3), &BigInt::from(1), 67, true);
    assert!(p.is_some());
    assert_eq!(divisors_u64(134), vec![1, 2, 67, 134]);
}

#[test]
fn small_primes_equal_to_a_base() {
    for p in [53u128, 59, 61, 67, 71] {
        assert!(galileo_rs::numtheory::small::is_prime(p), "{}", p);
    }
}

#[test]
fn aurifeuillean_type_families_and_fermat() {
    let mut s = galileo_rs::eval::Session::new();
    let run = |s: &mut galileo_rs::eval::Session, src: &str| -> String {
        s.run(src)
            .into_iter()
            .map(|r| format!("{:?}", r))
            .collect::<Vec<_>>()
            .join("\n")
    };
    let out = run(&mut s, "x = 98765432123456789; algfactor(x^6 + 8*x^2 - 1)");
    assert!(out.contains("Aurifeuillean-type"), "{}", out);
    let out = run(&mut s, "x = 10^20 + 7; algfactor(100*x^4 - 120*x^2 + 1)");
    assert!(out.contains("splits into 2 pieces"), "{}", out);
    let out = run(
        &mut s,
        "p = nextprime(10^60); q = nextprime(p + 10^31); algfactor(p*q)",
    );
    assert!(out.contains("Fermat"), "{}", out);
    assert!(out.contains("All 2 prime factors found"), "{}", out);
}
