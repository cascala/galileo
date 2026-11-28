//! Groups as values: orders, classes, quotients, Sylow subgroups.

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
fn orders_and_properties() {
    for (code, want) in [
        ("order(sym(5))", "120"),
        ("order(alt(5))", "60"),
        ("order(dihedral(6))", "12"),
        ("order(units(20))", "8"),
        ("order(group(\"Q8\"))", "8"),
        ("order(dprod(cyclic(2), cyclic(3)))", "6"),
        ("order(center(dihedral(4)))", "2"),
        ("order(center(group(\"Q8\")))", "2"),
        ("order(derived(sym(4)))", "12"),
        ("order(sylow(sym(4), 2))", "8"),
        ("order(sylow(alt(5), 5))", "5"),
        ("issimple(alt(5))", "true"),
        ("issimple(alt(4))", "false"),
        ("issolvable(sym(4))", "true"),
        ("issolvable(sym(5))", "false"),
        ("isabelian(units(20))", "true"),
        ("isabelian(group(\"Q8\"))", "false"),
        ("size(classes(sym(5)))", "[7, 5]"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
}

#[test]
fn quotients_and_names() {
    let k = "G = sym(4); V = subgroup(G, [2, 1, 4, 3], [3, 4, 1, 2]);";
    assert_eq!(one(&format!("{} isnormal(G, V)", k)), "true");
    assert_eq!(one(&format!("{} order(quotient(G, V))", k)), "6");
    assert_eq!(one(&format!("{} isabelian(quotient(G, V))", k)), "false");
    assert!(one("quotient(sym(3), subgroup(sym(3), [2, 1, 3]))").starts_with("ERROR"));
    // a group prints as the call that rebuilds it
    let shown = one("sylow(sym(4), 2)");
    assert_eq!(one(&format!("order({})", shown)), "8");
    assert_eq!(
        one("dprod(cyclic(2), cyclic(3))"),
        "dprod(cyclic(2), cyclic(3))"
    );
}

#[test]
fn presentations_homs_invariants() {
    for (p, want) in [
        ("<a | a^5>", "5"),
        ("<a, b | a^4, b^2, (a*b)^2>", "8"),
        ("<a, b | a^2, b^3, (a*b)^5>", "60"),
        ("<a, b | a^4, a^2*b^-2, b^-1*a*b*a>", "8"),
        ("<a, b | a*b*a^-1*b^-2, b*a*b^-1*a^-2>", "1"),
        ("<a, b, c | a^2, b^2, c^2, (a*b)^3, (b*c)^3, (a*c)^2>", "24"),
    ] {
        assert_eq!(one(&format!("order(group(\"{}\"))", p)), want, "{}", p);
    }
    assert_eq!(
        one("isomorphic(group(\"<a, b | a^4, a^2*b^-2, b^-1*a*b*a>\"), group(\"Q8\"))"),
        "true"
    );
    assert_eq!(
        one("isomorphic(group(\"<a, b | a^4, b^2, (a*b)^2>\"), dihedral(4))"),
        "true"
    );
    assert_eq!(one("isomorphic(group(\"Q8\"), dihedral(4))"), "false");
    assert_eq!(one("isomorphic(units(5), cyclic(4))"), "true");
    assert_eq!(one("isomorphic(sym(3), dihedral(3))"), "true");
    assert_eq!(one("abelianinv(units(20))"), "[2, 4]");
    assert_eq!(one("abelianinv(dprod(cyclic(4), cyclic(6)))"), "[2, 12]");
    assert_eq!(one("abelianinv(sym(4))"), "[2]");
    // sign: sym(3) -> cyclic(2); the kernel is alt(3)
    assert_eq!(one("order(kernel(sym(3), cyclic(2), [2, 1], [1, 2]))"), "3");
    assert_eq!(one("ishom(cyclic(4), cyclic(2), [1, 2])"), "true");
    assert_eq!(one("ishom(cyclic(3), cyclic(2), [2, 1])"), "false");
}
