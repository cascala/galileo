//! Phase 1 of the language: blocks (for, while, if, function), strings,
//! lists, records, assignment to parts, elementwise operators and
//! many-digit decimals.

use galileo_rs::Output;
use galileo_rs::eval::Session;
use galileo_rs::print::show_value;

/// everything a program shows, one line per result ("ERROR ..." for an error)
fn shown(code: &str) -> Vec<String> {
    let mut s = Session::new();
    s.run_quiet(code)
        .into_iter()
        .filter(|(_, quiet)| !quiet)
        .filter_map(|(r, _)| match r {
            Ok(Output::Value(v)) => Some(show_value(&v)),
            Ok(Output::Nothing) => None,
            Ok(o) => Some(o.render()),
            Err(e) => Some(format!("ERROR {}", e)),
        })
        .collect()
}

/// the last thing a program shows
fn last(code: &str) -> String {
    shown(code).pop().unwrap_or_default()
}

#[test]
fn for_loops() {
    assert_eq!(last("s = 0;\nfor k = 1:10\n  s = s + k;\nend\ns"), "55");
    assert_eq!(last("s = 0; for k = 1:2:9, s = s + k; end; s"), "25");
    assert_eq!(last("s = 0; for k = 10:-3:1, s = s + k; end; s"), "22");
    assert_eq!(last("p = 1; for x in [2 3 5], p = p*x; end; p"), "30");
    assert_eq!(
        last("t = \"\"; for c = \"abc\", t = c + t; end; t"),
        "\"cba\""
    );
    assert_eq!(
        last("n = 0; for x in {1, \"a\", [1 2]}, n = n + 1; end; n"),
        "3"
    );
    // a big range is walked, not built
    assert_eq!(last("s = 0; for k = 1:200000, s = s + 1; end; s"), "200000");
    // exact numbers stay exact
    assert_eq!(last("s = 0; for k = 1:4, s = s + 1/k; end; s"), "25/12");
    // statements without ; show their results
    assert_eq!(shown("for k = 1:3\n  k^2\nend"), vec!["1", "4", "9"]);
}

#[test]
fn break_and_continue() {
    assert_eq!(
        last("s = 0; for k = 1:100\n if k > 4, break, end\n s = s + k;\nend\ns"),
        "10"
    );
    assert_eq!(
        last("s = 0; for k = 1:6\n if mod(k, 2) == 0\n continue\n end\n s = s + k;\nend\ns"),
        "9"
    );
    assert!(last("break").starts_with("ERROR"));
}

#[test]
fn while_and_if() {
    let collatz = "n = 27; steps = 0;\nwhile n ~= 1\n  if mod(n, 2) == 0\n    n = n/2;\n  else\n    n = 3*n + 1;\n  end\n  steps = steps + 1;\nend\nsteps";
    assert_eq!(last(collatz), "111");
    let sign = |x: &str| {
        last(&format!(
            "x = {};\nif x > 0\n  s = 1;\nelseif x < 0\n  s = -1;\nelse\n  s = 0;\nend\ns",
            x
        ))
    };
    assert_eq!(sign("5"), "1");
    assert_eq!(sign("-2"), "-1");
    assert_eq!(sign("0"), "0");
    // the function form of if still works
    assert_eq!(last("if(2 > 1, \"yes\", \"no\")"), "\"yes\"");
    assert_eq!(last("f(n) := if(n == 0, 1, n*f(n - 1)); f(5)"), "120");
    assert_eq!(last("x = 1; while x < 10, x = 2*x; end; x"), "16");
    assert!(last("if x > 0\n 1").contains("missing end"));
}

#[test]
fn functions() {
    assert_eq!(last("function y = sq(x)\n  y = x^2;\nend\nsq(7)"), "49");
    assert_eq!(
        last(
            "function fib(n)\n  a = 0; b = 1;\n  for k = 1:n, t = a + b; a = b; b = t; end\n  return a\nend\nfib(90)"
        ),
        "2880067194370816120"
    );
    // its own variables: nothing leaks in or out
    assert_eq!(
        last("a = 5;\nfunction r = g(x)\n  a = 100;\n  r = x + 1;\nend\ng(1); a"),
        "5"
    );
    // a name it never set is a symbol, as everywhere in Galileo
    assert_eq!(
        last("function r = h(x)\n  r = a + x;\nend\na = 1; h(2)"),
        "a+2"
    );
    // recursion
    assert_eq!(
        last(
            "function r = fact(n)\n  if n == 0\n    r = 1;\n  else\n    r = n*fact(n - 1);\n  end\nend\nfact(20)"
        ),
        "2432902008176640000"
    );
    // a function with no value, called for what it prints
    assert_eq!(
        shown("function hello(name)\n  print(\"hello\", name)\nend\nhello(\"world\")"),
        vec!["hello world"]
    );
    // one line
    assert_eq!(
        last("function r = hyp(a, b), r = sqrt(a^2 + b^2); end; hyp(3, 4)"),
        "5"
    );
}

#[test]
fn print_and_strings() {
    assert_eq!(
        shown("for k = 1:2, print(\"k =\", k), end"),
        vec!["k = 1", "k = 2"]
    );
    assert_eq!(last("\"x = \" + 3"), "\"x = 3\"");
    assert_eq!(last("upper(\"abc\") + lower(\"DEF\")"), "\"ABCdef\"");
    assert_eq!(last("split(\"a,b,c\", \",\")"), "{\"a\", \"b\", \"c\"}");
    assert_eq!(last("split(\"to be or\")"), "{\"to\", \"be\", \"or\"}");
    assert_eq!(last("join({\"a\", \"b\"}, \"-\")"), "\"a-b\"");
    assert_eq!(last("replace(\"banana\", \"an\", \"AN\")"), "\"bANANa\"");
    assert_eq!(last("contains(\"galileo\", \"lil\")"), "true");
    assert_eq!(last("format(\"{} + {} = {}\", 1, 2, 3)"), "\"1 + 2 = 3\"");
    assert_eq!(last("num(\"3/4\") + 1"), "7/4");
    assert_eq!(last("str(1/3)"), "\"1/3\"");
    assert_eq!(last("length(\"hello\")"), "5");
    assert_eq!(last("s = \"hello\"; s(1) + s[4] + s(end)"), "\"hoo\"");
    assert_eq!(last("trim(\"  x  \")"), "\"x\"");
}

#[test]
fn lists() {
    assert_eq!(last("L = {1, \"two\", x^2}"), "{1, \"two\", x^2}");
    assert_eq!(last("L = {10, 20, 30}; L(2) + L[0]"), "30");
    assert_eq!(last("L = {10, 20, 30}; L(end)"), "30");
    assert_eq!(last("L = {1, 2}; L(2) = \"b\"; L"), "{1, \"b\"}");
    assert_eq!(last("L = {1}; L(3) = 5; L"), "{1, 0, 5}");
    assert_eq!(last("append({1, 2}, 3, 4)"), "{1, 2, 3, 4}");
    assert_eq!(last("concat({1}, {2, 3})"), "{1, 2, 3}");
    assert_eq!(last("length({1, {2, 3}, [1 2 3]})"), "3");
    assert_eq!(last("sq(x) := x^2; map(sq, {1, 2, 3})"), "{1, 4, 9}");
    assert_eq!(last("sq(x) := x^2; map(sq, [1 2 3])"), "[1, 4, 9]");
    assert_eq!(last("big(x) := x > 2; filter(big, {1, 5, 2, 7})"), "{5, 7}");
    assert_eq!(last("map(sin, {0, pi/2})"), "{0, 1}");
    assert_eq!(last("reverse({1, 2, 3})"), "{3, 2, 1}");
    assert_eq!(last("list([1 2 3])"), "{1, 2, 3}");
    assert_eq!(last("contains({1, 2, 3}, 2)"), "true");
    assert!(last("L = {1, 2}; L(5)").contains("outside 1..2"));
}

#[test]
fn records() {
    assert_eq!(
        last("p.mass = 3; p.name = \"ball\"; p"),
        "{mass: 3, name: \"ball\"}"
    );
    assert_eq!(last("p = {mass: 2, v: 3}; p.mass*p.v^2/2"), "9");
    assert_eq!(last("q = {x: 1, y: {z: 2}}; q.y.z"), "2");
    assert_eq!(last("q = {x: 1}; q.y.z = 5; q"), "{x: 1, y: {z: 5}}");
    assert_eq!(last("p = {a: 1}; p.a = 7; p.a"), "7");
    assert_eq!(last("fields({a: 1, b: 2})"), "{\"a\", \"b\"}");
    assert_eq!(last("has({a: 1}, \"b\")"), "false");
    assert!(last("p = {a: 1}; p.b").contains("no field b"));
    assert!(last("n = 3; n.x = 1").contains("not a record"));
}

#[test]
fn assignment_to_parts() {
    assert_eq!(
        last("v = []; for k = 1:5, v(k) = k^2; end; v"),
        "[1, 4, 9, 16, 25]"
    );
    assert_eq!(last("v = [1 2 3]; v(end + 1) = 4; v"), "[1, 2, 3, 4]");
    assert_eq!(last("v = [1 2 3]; v[0] = 9; v"), "[9, 2, 3]");
    assert_eq!(last("v = [1 2 3]; v[end]"), "3");
    assert_eq!(last("A = [1 2; 3 4]; A(2, 1) = 7; A"), "[1, 2;\n 7, 4]");
    assert_eq!(last("A = [1 2; 3 4]; A[1, 1] = 0; A(end, end)"), "0");
    assert_eq!(last("A = [1 2; 3 4]; A(1) = [5 6]; A"), "[5, 6;\n 3, 4]");
    assert_eq!(last("A = [1 2]; A(2, 3) = 1; A"), "[1, 2, 0;\n 0, 0, 1]");
    // outside a block, f(x) = ... still defines a function
    assert_eq!(last("f(x) = x^2 + 1; f(2)"), "5");
    // and on an existing vector it changes an entry
    assert_eq!(last("w = [1 2 3]; k = 2; w(k) = 0; w"), "[1, 0, 3]");
}

#[test]
fn elementwise() {
    assert_eq!(last("[1 2 3] .* [4 5 6]"), "[4, 10, 18]");
    assert_eq!(last("[1 2 3] .^ 2"), "[1, 4, 9]");
    assert_eq!(last("2 .^ [1 2 3]"), "[2, 4, 8]");
    assert_eq!(last("1 ./ [1 2 4]"), "[1, 1/2, 1/4]");
    assert_eq!(last("[2 4] ./ [2 8]"), "[1, 1/2]");
    assert_eq!(last("x = [1 2]; x.^2 + 1"), "[2, 5]");
    assert!(last("[1 2] .* [1 2 3]").contains("sizes differ"));
    // a number before .* keeps working: 2.*x is 2 .* x
    assert_eq!(last("x = [1 2]; 2.*x"), "[2, 4]");
}

#[test]
fn many_digits() {
    assert_eq!(
        last("eval(pi, 50)"),
        "3.1415926535897932384626433832795028841971693993751"
    );
    assert_eq!(last("eval(sqrt(2), 30)"), "1.41421356237309504880168872421");
    assert_eq!(last("eval(e, 30)"), "2.71828182845904523536028747135");
    assert_eq!(last("eval(log(10), 25)"), "2.302585092994045684017991");
    assert_eq!(last("eval(sin(1), 25)"), "0.8414709848078965066525023");
    assert_eq!(
        last("eval(atan(1)*4, 30)"),
        "3.14159265358979323846264338328"
    );
    assert_eq!(last("eval(exp(100), 20)"), "2.6881171418161354484e43");
    assert_eq!(last("eval(1/7, 20)"), "0.14285714285714285714");
    // the digits carry through arithmetic with exact numbers
    assert_eq!(
        last("x = eval(pi, 30); 2*x"),
        "6.28318530717958647692528676656"
    );
    // a long literal keeps its digits
    assert_eq!(
        last("3.14159265358979323846264338327950288"),
        "3.14159265358979323846264338327950288"
    );
    assert_eq!(last("eval(cos(pi/3), 10)"), "0.5000000000");
}

#[test]
fn blocks_over_lines_in_scripts() {
    use galileo_rs::parser::{needs_more, open_blocks};
    assert!(needs_more("for k = 1:3"));
    assert!(needs_more("for k = 1:3\n  if k > 1"));
    assert!(!needs_more("for k = 1:3\n  if k > 1\n  k\n  end\nend"));
    assert!(!needs_more("y = if(x > 0, 1, 2)"));
    assert!(needs_more("if (x > 0)"));
    assert_eq!(
        open_blocks("function f(x)\n  for k = 1:x, print(k), end"),
        1
    );
    assert_eq!(open_blocks("v(end) = 3"), 0);
    assert_eq!(open_blocks("s = \"for while\""), 0);
}

#[test]
fn small_coefficients_are_kept() {
    // a coefficient is never dropped for being small next to the others
    assert_eq!(last("x^3 == x^3 + 1e-12*x^5"), "x^3 == 1e-12*x^5+x^3");
    assert_eq!(last("1e20*x == 1e20*x + 1"), "false");
    assert_eq!(last("simplify(x^3 - (x^3 + 1e-12*x^5))"), "-1e-12*x^5");
    assert_eq!(last("simplify(1e20*x + 1 - 1e20*x)"), "1");
    // float cancellation still gives exact zeros
    assert_eq!(last("expand((x+0.1)^2) == x^2+0.2*x+0.01"), "true");
    assert_eq!(last("simplify((x^2-0.01)/(x-0.1))"), "x+0.1");
}

#[test]
fn exact_decimals() {
    // a decimal is an exact number written in decimal notation
    assert_eq!(last("0.1*x + 0.2*x == 0.3*x"), "true");
    assert_eq!(last("0.1 + 0.2"), "0.3");
    assert_eq!(last("0.1 + 0.2 - 0.3"), "0.0");
    assert_eq!(last("0.1/3"), "1/30");
    assert_eq!(last("1.5/7"), "3/14");
    assert_eq!(last("simplify(sqrt(0.1/3))"), "sqrt(30)/30");
    assert_eq!(last("(0.1/3)*3"), "1/10");
    assert_eq!(last("solve(3*x == 0.1, x)"), "1/30");
    assert_eq!(last("0.1/4"), "0.025");
    assert_eq!(last("1e20 + 1"), "100000000000000000001.0");
    assert_eq!(last("sqrt(0.25)"), "0.5");
    assert_eq!(last("simplify(sqrt(0.5))"), "0.5*sqrt(2)");
    assert_eq!(last("sin(0.5)"), "sin(0.5)");
    // eval gives digits
    assert_eq!(last("eval(exp(1.0))"), "2.718281828459045");
    assert_eq!(last("eval(0.1/3)"), "0.03333333333333333");
    assert_eq!(last("x^2.0"), "x^2");
    // small coefficients are still kept
    assert_eq!(last("x^3 == x^3 + 1e-12*x^5"), "x^3 == 1e-12*x^5+x^3");
    // exactdigits: a limit after which decimals become floats
    assert_eq!(last("exactdigits"), "38");
    assert_eq!(last("exactdigits = 0; 0.1 + 0.2"), "0.30000000000000004");
    assert_eq!(last("exactdigits = 3; 0.001*0.1"), "0.0001");
    assert!(shown("exactdigits = -1").iter().any(|l| l.contains("whole number of digits")));
}

#[test]
fn held_input() {
    let h = |code: &str| last(&format!("simplify = false; {}", code));
    assert_eq!(h("1/2"), "1/2");
    assert_eq!(h("0.5"), "0.5");
    assert_eq!(h("1/3 + 2/6"), "1/3+2/6");
    assert_eq!(h("simplify(1/3 + 2/6)"), "2/3");
    assert_eq!(h("eval(1/3 + 2/6)"), "0.6666666666666666");
    assert_eq!(h("2*3"), "2*3");
    assert_eq!(h("x + x"), "x+x");
    assert_eq!(h("simplify(x + x)"), "2*x");
    assert_eq!(h("0.1/3"), "0.1/3");
    assert_eq!(h("simplify(0.1/3)"), "1/30");
    assert_eq!(h("0.1*x + 0.2*x"), "0.1*x+0.2*x");
    assert_eq!(h("simplify(0.1*x + 0.2*x)"), "0.3*x");
    assert_eq!(h("0.1*x + 0.2*x == 0.3*x"), "true");
    assert_eq!(h("sqrt(0.5)"), "sqrt(0.5)");
    assert_eq!(h("simplify(sqrt(0.5))"), "0.5*sqrt(2)");
    assert_eq!(h("sin(0)"), "sin(0)");
    assert_eq!(h("(x+1)^2"), "(x+1)^2");
    assert_eq!(h("-x^2"), "-x^2");
    assert_eq!(h("2^3^2"), "2^3^2");
    assert_eq!(h("[1/2 2/4; 1 2]"), "[1/2, 2/4;\n   1,   2]");
    // variables are replaced by their values, and stay as typed
    assert_eq!(h("a = 1/3 + 2/6; a*3"), "(1/3+2/6)*3");
    assert_eq!(h("a = 1/3 + 2/6; simplify(a*3)"), "2");
    // commands simplify their input
    assert_eq!(h("inv([0.1 0.2; 0.3 0.5])"), "[-50,  20;\n  30, -10]");
    assert_eq!(h("diff(x^3 + 1/3, x)"), "3*x^2");
    // float arithmetic runs straight away
    assert_eq!(h("eval(pi)*2"), "6.283185307179586");
    // loops and functions compute
    assert_eq!(h("s = 0; for k = 1:10; s = s + 0.1*k; end; s"), "5.5");
    assert_eq!(h("f(x) := x^2 + 1/3; f(2)"), "13/3");
    assert!(shown("simplify = 1").iter().any(|l| l.contains("simplify is true")));
}

#[test]
fn simplify_is_idempotent() {
    for e in [
        "1/3 + 2/6",
        "(x^2-1)/(x-1)",
        "1/(2*x) - 1/(2*(x-1))",
        "(x+1)^3",
        "(x^2-0.01)/(x-0.1)",
        "x/2 + 1/3",
        "1/(1-x)",
        "sqrt(8)",
        "1/sqrt(2)",
        "exp(x)*exp(y)",
        "sin(x)^2",
        "1 - cos(x)^2",
        "sin(x)^4 - cos(x)^4",
        "cos(x)^2 + 2*sin(x)^2 - 1",
        "(a+b)/(c+d) - (b+a)/(d+c)",
        "x^3 + 1e-12*x^5",
        "0.1*x + 0.2*x",
    ] {
        let once = last(&format!("simplify({})", e));
        let twice = last(&format!("simplify(simplify({}))", e));
        assert_eq!(once, twice, "simplify({}) is not stable", e);
    }
    // one form for one value, whichever way it was written
    assert_eq!(last("simplify(sin(x)^2)"), last("simplify(1 - cos(x)^2)"));
    assert_eq!(last("simplify((x+1)^2)"), last("simplify(x^2 + 2*x + 1)"));
}
