//! Linear algebra over finite fields, matrix groups, and Lie algebra
//! representations (Gell-Mann, structure constants, roots, Gelfand–Tsetlin).

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

fn check(cases: &[(&str, &str)]) {
    for (code, want) in cases {
        let got: String = one(code).chars().filter(|c| !c.is_whitespace()).collect();
        let want: String = want.chars().filter(|c| !c.is_whitespace()).collect();
        let want = want.as_str();
        if want == "ERROR" {
            assert!(got.starts_with("ERROR"), "{} gave {}", code, got);
        } else {
            assert_eq!(got, want, "{}", code);
        }
    }
}

#[test]
fn matrices_over_finite_fields() {
    check(&[
        ("F = gf(17); A = F([1, 2; 3, 4]); inv(A)", "gf(17, [15, 1; 10, 8])"),
        ("F = gf(17); A = F([1, 2; 3, 4]); A * inv(A)", "gf(17, [1, 0; 0, 1])"),
        ("F = gf(17); A = F([1, 2; 3, 4]); A^-1 == inv(A)", "true"),
        ("F = gf(17); det(F([1, 2; 3, 4]))", "gf(17, 15)"),
        ("F = gf(17); gf(17, [1, 2; 3, 4]) == F([1, 2; 3, 4])", "true"),
        // over Q this matrix has rank 3; mod 2 its rows add up to 0
        ("rank(gf(2, [1, 1, 0; 0, 1, 1; 1, 0, 1]))", "2"),
        ("rank([1, 1, 0; 0, 1, 1; 1, 0, 1])", "3"),
        ("null(gf(2, [1, 1, 0; 0, 1, 1; 1, 0, 1]))", "gf(2, [1; 1; 1])"),
        ("rref(gf(2, [1, 1, 0; 0, 1, 1; 1, 0, 1]))", "gf(2, [1, 0, 1; 0, 1, 1; 0, 0, 0])"),
        ("F = gf(17); F([1, 2; 3, 4]) \\ F([1; 0])", "gf(17, [15; 10])"),
        ("inv(gf(2, [1, 1; 1, 1]))", "ERROR"),
        ("eig(gf(17, [1, 2; 3, 4]))", "gf(17, [9, 13])"),
        ("eig(gf(3, [0, 1; -1, 0]))", "[]"),
        ("eigvec(gf(17, [2, 1; 0, 3]))", "gf(17, [1, 1; 0, 1])"),
        ("trace(gf(5, [4, 1; 2, 3]))", "gf(5, 2)"),
        ("charpoly(gf(2, [1, 1, 0; 0, 1, 1; 1, 0, 1]), x)", "x^3+x^2+x"),
        // Cayley–Hamilton in gf(7)
        ("A = gf(7, [1, 2; 4, 5]); A^2 - trace(A)*A + det(A)*eye(2) == gf(7, [0, 0; 0, 0])", "true"),
        ("G = gf(4); A = G([a, 1; 0, a + 1]); det(A)", "gf(4, 1)"),
        ("G = gf(9); A = G([a, 1; 0, a + 1]); A * inv(A)", "gf(9, [1, 0; 0, 1])"),
        // zmod(n), n composite: det and inv work, rank needs a field
        ("Z = zmod(12); C = Z([5, 2; 2, 1]); inv(C)", "zmod(12, [1, 10; 10, 5])"),
        ("Z = zmod(12); C = Z([5, 2; 2, 1]); C * inv(C)", "zmod(12, [1, 0; 0, 1])"),
        ("Z = zmod(12); det(Z([5, 2; 1, 1]))", "zmod(12, 3)"),
        ("Z = zmod(12); inv(Z([5, 2; 1, 1]))", "ERROR"),
        ("rank(zmod(12, [5, 2; 1, 1]))", "ERROR"),
        ("A = gf(17, [1, 2; 3, 4]); A^16", "gf(17, [1, 0; 0, 1])"),
        ("A = gf(17, [1, 2; 3, 4]); 2*A + 1", "gf(17, [3, 5; 7, 9])"),
        ("gf(17, [1, 2; 3, 4]) + gf(5, [1, 0; 0, 1])", "ERROR"),
        ("latex(gf(17, [1, 2; 3, 4]))", "\\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix}_{\\mathbb{F}_{17}}"),
    ]);
}

#[test]
fn solving_over_finite_fields() {
    check(&[
        ("solve([x + y == 1, x == 1], [x, y], gf(2))", "gf(2, [1, 0])"),
        ("solve([x + y == 1, x + z == 0, y + z == 1], [x, y, z], gf(2))", "[z, z+1, z]"),
        ("solve([x + y + z == 1, x + z == 0], [x, y, z], 2)", "[z, 1, z]"),
        ("solve([3*x + y == 1, x - y == 5], [x, y], gf(17))", "gf(17, [10, 5])"),
        ("solve([x + y == 1, x + y == 0], [x, y], gf(2))", "ERROR"),
        ("solve(x^2 + x + 1, x, gf(4))", "gf(4, [a, a+1])"),
        ("solve(x^2 + 1, x, gf(17))", "gf(17, [4, 13])"),
        ("solve(x^2 + 1, x, gf(3))", "[]"),
        ("solve(x^2 + 1, x, gf(9))", "gf(9, [a+2, 2*a+1])"),
        ("size(solve([x^2 + y^2 == 1], [x, y], gf(7)))", "[8, 2]"),
        ("solve([x*y == 1, x + y == 0], [x, y], gf(5))", "gf(5, [2, 3; 3, 2])"),
        ("solve([2*x == 1], [x], zmod(9))", "zmod(9, 5)"),
    ]);
}

#[test]
fn matrix_groups() {
    check(&[
        ("order(gl(2, 3))", "48"),
        ("order(sl(2, 3))", "24"),
        ("order(sl(2, 7))", "336"),
        ("S = sl(2, 7); isomorphic(quotient(S, center(S)), gl(3, 2))", "true"),
        ("order(gl(2, 4))", "180"),
        ("order(gl(3, 2))", "168"),
        ("order(sl(2, 9))", "720"),
        ("issimple(sl(3, 2))", "true"),
        ("issimple(sl(2, 7))", "false"),
        ("isomorphic(gl(3, 2), sl(2, 7))", "false"),
        ("isomorphic(gl(2, 2), sym(3))", "true"),
        ("order(center(sl(2, 5)))", "2"),
        ("elements(center(sl(2, 3)))", "{gf(3, [1, 0; 0, 1]), gf(3, [2, 0; 0, 2])}"),
        ("order(group(gf(3, [1, 1; 0, 1]), gf(3, [1, 0; 1, 1])))", "24"),
        ("order(gf(3, [1, 1; 0, 1]))", "3"),
        ("order(gf(7, [3, 0; 0, 1]))", "6"),
        ("abelianinv(gl(2, 3))", "[2]"),
        ("order(subgroup(gl(2, 3), gf(3, [2, 0; 0, 1])))", "2"),
        ("length(classes(gl(2, 3)))", "8"),
        ("group(gf(2, [1, 1; 1, 1]))", "ERROR"),
        ("gl(4, 4)", "ERROR"),
    ]);
}

#[test]
fn lie_algebras() {
    check(&[
        ("L = gellmann(); L(2)", "[0, -i, 0; i, 0, 0; 0, 0, 0]"),
        ("L = gellmann(); L(8)", "[sqrt(3)/3, 0, 0; 0, sqrt(3)/3, 0; 0, 0, -2*sqrt(3)/3]"),
        ("length(gellmann(4))", "15"),
        ("structure(gellmann(2))", "[1, 2, 3, 1]"),
        (
            "structure(gellmann())",
            "[1, 2, 3, 1; 1, 4, 7, 1/2; 1, 5, 6, -1/2; 2, 4, 6, 1/2; 2, 5, 7, 1/2; 3, 4, 5, 1/2; 3, 6, 7, -1/2; 4, 5, 8, sqrt(3)/2; 6, 7, 8, sqrt(3)/2]",
        ),
        ("d = structure(gellmann(), \"d\"); d(1, 4)", "sqrt(3)/3"),
        ("structure(su(2))", "[1, 2, 3, 1; 1, 3, 2, -1; 2, 3, 1, 1]"),
        ("cartan(\"G2\")", "[2, -1; -3, 2]"),
        ("cartan(\"B2\")", "[2, -2; -1, 2]"),
        ("cartan(so(5))", "[2, -2; -1, 2]"),
        ("cartan(\"C3\")", "[2, -1, 0; -1, 2, -1; 0, -2, 2]"),
        ("roots(su(3))", "[1, 0; 0, 1; 1, 1]"),
        ("roots(\"G2\")", "[1, 0; 0, 1; 1, 1; 2, 1; 3, 1; 3, 2]"),
        ("size(roots(\"E8\"))", "[120, 8]"),
        ("size(roots(\"F4\"))", "[24, 4]"),
        ("size(roots(\"E6\"))", "[36, 6]"),
        ("size(roots(\"D4\"))", "[12, 4]"),
        ("dim(su(3), [1, 1])", "8"),
        ("dim(su(3), [2, 2])", "27"),
        ("dim(\"G2\", [1, 0])", "7"),
        ("dim(\"E8\", [0, 0, 0, 0, 0, 0, 0, 1])", "248"),
        ("dim(\"E6\", [1, 0, 0, 0, 0, 0])", "27"),
        ("dim(\"B3\", [0, 0, 1])", "8"),
        ("casimir(su(3))", "4/3"),
        ("casimir(su(3), [1, 1])", "3"),
        ("casimir(su(2), [2])", "2"),
        ("casimir(\"A1\", 3)", "15/4"),
        ("rep(gellmann(), [1, 0]) == gellmann()", "true"),
        ("R = rep(gellmann(), [1, 1]); size(R(1))", "[8, 8]"),
        ("R = rep(gellmann(), [2, 0]); R(5) == R(5)'", "true"),
        ("R = rep(su(2), [2]); R(3)", "[-i, 0, 0; 0, 0, 0; 0, 0, i]"),
        ("decompose(su(3), [1, 0], [0, 1])", "{[1, 1], [0, 0]}"),
        ("decompose(su(3), [1, 0], [1, 0], [1, 0])", "{[3, 0], [1, 1], [1, 1], [0, 0]}"),
        ("decompose(su(3), [1, 1], [1, 1])", "{[2, 2], [3, 0], [0, 3], [1, 1], [1, 1], [0, 0]}"),
        ("decompose(su(2), 1, 1, 1)", "{[3], [1], [1]}"),
        ("weights(su(3), [1, 0])", "[1, 0; -1, 1; 0, -1]"),
        ("rep(su(3), [1])", "ERROR"),
        ("cartan(\"E9\")", "ERROR"),
    ]);
}

/// The representation matrices keep the brackets, and sum Ta Ta is the Casimir
#[test]
fn representations_are_representations() {
    let code = r#"
function ok = keeps(L, w)
  R = rep(L, w);
  ok = true;
  for a = 1:length(L)
    for b = a+1:length(L)
      c = coords(bracket(L(a), L(b)), L);
      rhs = 0*R(1);
      for k = 1:length(L)
        rhs = rhs + c(k)*R(k);
      end
      if !(simplify(bracket(R(a), R(b)) - rhs) == 0*R(1))
        ok = false;
      end
    end
  end
end
function ok = cas(w, d)
  L = gellmann();
  R = rep(L, w);
  S = 0*R(1);
  for a = 1:8
    S = S + R(a)*R(a)/4;
  end
  ok = simplify(S) == casimir(su(3), w)*eye(d);
end
[keeps(gellmann(), [1, 1]), keeps(su(3), [2, 1]), keeps(sl(4), [1, 0, 1]), keeps(su(2), [3]), cas([2, 1], 15), cas([0, 3], 10)]
"#;
    assert_eq!(one(code), "[true, true, true, true, true, true]");
}
