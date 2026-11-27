//! Stochastic processes: Markov chains, walks, arrivals, Brownian motion, SDEs, Ising, Black-Scholes.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show;

fn one(code: &str) -> String {
    let mut s = Session::new();
    let mut last = String::new();
    for r in s.run(code) {
        last = match r {
            Ok(Output::Value(v)) => show(&v),
            Ok(o) => format!("{:?}", o),
            Err(e) => format!("ERROR {}", e),
        };
    }
    last.replace('\n', "")
}

fn num(code: &str) -> f64 {
    one(code).parse().unwrap_or_else(|_| panic!("{} gave {}", code, one(code)))
}

const P: &str = "P = [1/2 1/2 0; 1/4 1/2 1/4; 0 1/3 2/3];";
const G: &str = "G = [1 0 0 0 0; 1/2 0 1/2 0 0; 0 1/2 0 1/2 0; 0 0 1/2 0 1/2; 0 0 0 0 1];";

#[test]
fn markov_chains_exact() {
    for (code, want) in [
        ("stationary(P)", "[2/9, 4/9, 1/3]"),
        ("stationary([0 1; 1 0])", "[1/2, 1/2]"),
        ("stationary([1 0; 0 1])", "[1, 0; 0, 1]"),
        // a transient state gets 0
        ("stationary([1/2 1/2; 0 1])", "[0, 1]"),
        // gambler's ruin with 4 dollars total
        ("absorption(G)", "[  1,   0; 3/4, 1/4; 1/2, 1/2; 1/4, 3/4;   0,   1]"),
        ("hitting(G, [1 5])", "[0, 3, 4, 3, 0]"),
        ("hitting(G, 1)", "[0, inf, inf, inf, inf]"),
        ("hitting(P, 3)", "[8, 6, 0]"),
        ("stationary([1/2 1/3; 1/2 1/2])", "ERROR stationary: row 1 adds up to 5/6, not 1"),
        ("absorption(P)", "ERROR absorption: no state is absorbing (an absorbing state i has P(i, i) = 1)"),
    ] {
        let code = format!("{} {} {}", P, G, code);
        assert_eq!(one(&code), want, "{}", code);
    }
    let d = one("stationary([0.9 0.1; 0.5 0.5])");
    assert!(d.starts_with("[0.833333"), "{}", d);
}

#[test]
fn simulations_have_the_right_shape_and_law() {
    assert_eq!(one("seed(1); size(randchain([1/2 1/2; 1/2 1/2], 1, 50))"), "[1, 51]");
    assert_eq!(one("seed(1); size(randwalk(100))"), "[1, 101]");
    assert_eq!(one("seed(1); size(randwalk(100, 3))"), "[101, 3]");
    assert_eq!(one("seed(1); size(brownian(1, 100, 5))"), "[5, 101]");
    assert_eq!(one("seed(1); size(sde(-x, 1, 0, 1, 50, 3))"), "[3, 51]");
    // the chain spends about 4/9 of its time in state 2
    let share = num(&format!("{} seed(3); c = randchain(P, 1, 40000); s = 0; for x in c, if x == 2, s = s + 1; end; end; s / 40001.0", P));
    assert!((share - 4.0 / 9.0).abs() < 0.02, "{}", share);
    // Poisson arrivals: about rate * T of them
    let k = num("seed(2); length(arrivals(50, 100))");
    assert!((k - 5000.0).abs() < 300.0, "{}", k);
    // Brownian motion: var W(1) = 1
    let v = num("seed(4); B = brownian(1, 10, 4000); var(column(B, 11))");
    assert!((v - 1.0).abs() < 0.1, "{}", v);
    // Ornstein-Uhlenbeck dx = -x dt + dW settles to variance 1/2
    let v = num("seed(5); X = sde(-x, 1, 0, 5, 200, 3000); var(column(X, 201))");
    assert!((v - 0.5).abs() < 0.06, "{}", v);
}

#[test]
fn ising_and_options() {
    let cold = num("seed(1); r = ising(16, 1.5, 300); r.m");
    let hot = num("seed(1); r = ising(16, 4, 300); r.m");
    assert!(cold > 0.9 && hot < 0.3, "{} {}", cold, hot);
    let c = num("bscall(100, 100, 0.05, 0.2, 1)");
    let p = num("bsput(100, 100, 0.05, 0.2, 1)");
    assert!((c - 10.4506).abs() < 1e-3 && (p - 5.5735).abs() < 1e-3, "{} {}", c, p);
    // put-call parity
    assert!((c - p - (100.0 - 100.0 * (-0.05f64).exp())).abs() < 1e-9);
}
