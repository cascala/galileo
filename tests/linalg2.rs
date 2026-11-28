//! Wave 2: QR, Gram-Schmidt, Cholesky, SVD, Jordan form, matrix exponential,
//! Kronecker product and least squares.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show;

fn one(code: &str) -> String {
    let mut s = Session::tidy();
    let mut last = String::new();
    for r in s.run(code) {
        last = match r {
            Ok(Output::Value(v)) => show(&v),
            Ok(o) => format!("{:?}", o),
            Err(e) => format!("ERROR {}", e),
        };
    }
    last
}

#[test]
fn qr_and_gram_schmidt() {
    assert_eq!(one("A = [1 1; 1 0; 0 1]; [Q, R] = qr(A); simplify(Q*R - A)"), "[0, 0;\n 0, 0;\n 0, 0]");
    assert_eq!(one("[Q, R] = qr([1 1; 1 0; 0 1]); simplify(transpose(Q)*Q)"), "[1, 0;\n 0, 1]");
    assert_eq!(one("[Q, R] = qr([3 1; 4 2]); R"), "[5, 11/5;\n 0,  2/5]");
    assert!(one("[Q, R] = qr([1 2; 2 4])").starts_with("ERROR"));
    assert_eq!(one("size(gramschmidt([1 2 3; 1 0 1; 0 1 1]))"), "[3, 2]");
}

#[test]
fn cholesky() {
    assert_eq!(one("chol([4 2; 2 3])"), "[2,       0;\n 1, sqrt(2)]");
    assert_eq!(one("L = chol([4 2 2; 2 5 3; 2 3 6]); L*transpose(L)"), "[4, 2, 2;\n 2, 5, 3;\n 2, 3, 6]");
    assert!(one("chol([1 2; 2 1])").contains("not positive definite"));
}

#[test]
fn singular_values() {
    assert_eq!(one("svd([3 0; 4 5])"), "[3*sqrt(5), sqrt(5)]");
    assert_eq!(one("svd([1 1; 1 1; 0 0])"), "[2, 0]");
    assert_eq!(one("A = [3 0; 4 5]; [U, S, V] = svd(A); simplify(U*S*transpose(V) - A)"), "[0, 0;\n 0, 0]");
    // numeric: compare with the 2-norm and the product of singular values = |det|
    let s = one("s = svd([1 2; 3 4]); s[0]*s[1]");
    assert!((s.parse::<f64>().unwrap() - 2.0).abs() < 1e-9, "{}", s);
    let r = one("A = [1 2; 3 4; 5 6]; [U, S, V] = svd(A); norm(U*S*transpose(V) - A, \"fro\")");
    assert!(r.parse::<f64>().unwrap() < 1e-9, "{}", r);
}

#[test]
fn jordan_form() {
    assert_eq!(one("jordan([2 1; 0 2])"), "[2, 1;\n 0, 2]");
    assert_eq!(one("jordan([1 2; 2 1])"), "[-1, 0;\n  0, 3]");
    assert_eq!(
        one("A = [5 4 2 1; 0 1 -1 -1; -1 -1 3 0; 1 1 -1 2]; [P, J] = jordan(A); simplify(P*J*inv(P) - A)"),
        "[0, 0, 0, 0;\n 0, 0, 0, 0;\n 0, 0, 0, 0;\n 0, 0, 0, 0]"
    );
}

#[test]
fn matrix_exponential() {
    assert_eq!(one("expm([0 -1; 1 0], t)"), "[cos(t), -sin(t);\n sin(t),  cos(t)]");
    assert_eq!(one("expm([1 1; 0 1], t)"), "[exp(t), exp(t)*t;\n      0,   exp(t)]");
    assert_eq!(one("expm([2 0; 0 3])"), "[exp(2),      0;\n      0, exp(3)]");
    assert_eq!(
        one("expm([0 1; -2 -3], t)"),
        "[  2*exp(-t)-exp(-2*t),   exp(-t)-exp(-2*t);\n 2*exp(-2*t)-2*exp(-t), 2*exp(-2*t)-exp(-t)]"
    );
    // derivative at 0 gives A back
    assert_eq!(one("E = expm([1 -2; 1 3], t); subs(deriv(E, t), t, 0)"), "[1, -2;\n 1,  3]");
    assert_eq!(one("expm([0.1 1; 0 0.2])"), "[exp(1/10), 10*(exp(1/5)-exp(1/10));\n         0,                exp(1/5)]");
    let r = one("eval(expm([0.1 1; 0 0.2]))");
    assert!(r.contains("1.1051709180"), "{}", r);
}

#[test]
fn kron_diag_least_squares() {
    assert_eq!(one("kron([1 2], [1; 1])"), "[1, 2;\n 1, 2]");
    assert_eq!(one("diag([1 2 3])"), "[1, 0, 0;\n 0, 2, 0;\n 0, 0, 3]");
    assert_eq!(one("diag([1 2; 3 4])"), "[1, 4]");
    assert_eq!(one("[1 1; 1 2; 1 3]\\[1; 2; 2]"), "[2/3;\n 1/2]");
}

#[test]
fn posdef_cond_and_systems() {
    assert_eq!(one("posdef([2 -1; -1 2])"), "true");
    assert_eq!(one("posdef([1 2; 2 1])"), "false");
    assert_eq!(one("cond([3 0; 4 5])"), "3");
    assert_eq!(one("cond([1 2; 2 4])"), "inf");
    // singular linear systems: free unknowns stay as symbols
    assert_eq!(one("solve([x + y + z == 6, x - y == 0, 2*x + 2*y + 2*z == 12], [x, y, z])"), "[3-z/2, 3-z/2, z]");
    assert!(one("solve([x + y == 1, 2*x + 2*y == 3], [x, y])").contains("no solution"));
    // decimals: no rounding noise
    assert_eq!(one("[1 1; 1 2; 1 3; 1 4; 1 5]\\[2.1; 3.9; 6.2; 7.8; 10.1]"), "[0.05;\n 1.99]");
}

#[test]
fn numeric_symmetric_eigenvalues() {
    // a chain of 8 springs: 2 - 2 cos(k pi/9), found by Jacobi rotations, not by exact roots
    let chain = "N = 8; M = zeros(N, N);\nfor k in 1:N\n M(k, k) = 2;\n if k < N\n  M(k, k+1) = -1; M(k+1, k) = -1;\n end\nend\n";
    assert_eq!(
        one(&format!("{} eig(M)", chain)),
        "[0.120614758428, 0.467911113762, 1.0, 1.65270364467, 2.34729635533, 3.0, 3.53208888624, 3.87938524157]"
    );
    assert_eq!(one(&format!("{} [V, D] = eig(M); eval([V(1, 1), sin(pi/9)*sqrt(2/9)], 6)", chain)), "[0.161230, 0.161230]");
    assert_eq!(one("eig([2.0, 1; 1, 2])"), "[1, 3]");
    // decimals are exact numbers, so their eigenvectors are exact too
    assert_eq!(one("[V, D] = eig([2.0, 1; 1, 2]); V"), "[-1.0, 1.0;\n    1,   1]");
    // exact matrices keep exact eigenvalues
    assert_eq!(one("eig([2, -1; -1, 2])"), "[1, 3]");
}
