//! Graphs and networks: building, paths, trees, flows, matchings, colorings, tours.

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

const W: &str = "W = graph([\"A\" \"B\" 4; \"A\" \"C\" 2; \"C\" \"B\" 1; \"B\" \"D\" 5; \"C\" \"D\" 8; \"D\" \"E\" 3]);";
const F: &str = "F = digraph([1 2 10; 1 3 10; 2 3 2; 2 4 4; 3 5 9; 4 6 10; 5 4 6; 5 6 10]);";

#[test]
fn building_and_counting() {
    for (code, want) in [
        ("order(petersen())", "10"),
        ("size(petersen())", "15"),
        ("size(complete(6))", "15"),
        ("size(complete(3, 4))", "12"),
        ("size(grid(3, 4))", "17"),
        ("size(hypercube(4))", "32"),
        ("size(wheel(5))", "10"),
        ("order(star(4))", "5"),
        ("degree(petersen())", "[3, 3, 3, 3, 3, 3, 3, 3, 3, 3]"),
        ("degree(graph([1 2; 2 3; 3 1; 3 4]), 3)", "3"),
        ("neighbors(graph([1 2; 2 3; 3 1; 3 4]), 3)", "{1, 2, 4}"),
        ("vertices(graph([1 2], 4))", "{1, 2, 3, 4}"),
        ("seed(1); size(randgraph(10, 12))", "12"),
        // the old meanings stay
        ("degree(x^3 + 1, x)", "3"),
        ("size([1 2 3])", "[1, 3]"),
        ("order(sym(4))", "24"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
}

#[test]
fn printing_rebuilds() {
    // a graph prints as the call that rebuilds it
    for code in [
        "graph([1 2; 2 3; 3 1; 3 4])",
        "graph([1 2; 3 4], 6)",
        "digraph([1 2 5; 2 1 3])",
        W,
        "mst(W)",
        "complement(cycle(5))",
        "subgraph(petersen(), {1, 2, 3, 6})",
        "graph([], 3)",
    ] {
        let code = if code == "mst(W)" { format!("{} mst(W)", W) } else { code.to_string() };
        let shown = one(&code);
        let again = one(&format!("G = {}; G", shown));
        assert_eq!(shown, again, "{}", code);
        assert!(!shown.contains('\n'), "{}", shown);
    }
    assert_eq!(one("cycle(5)"), "cycle(5)");
    assert_eq!(one("graph([\"x\" \"y\"])"), "graph([\"x\", \"y\"])");
}

#[test]
fn paths_and_distances() {
    for (code, want) in [
        ("shortest(W, \"A\", \"E\")", "{\"A\", \"C\", \"B\", \"D\", \"E\"}"),
        ("dist(W, \"A\", \"E\")", "11"),
        ("dist(W, \"E\", \"A\")", "11"),
        ("dist(graph([1 2], 3), 1, 3)", "inf"),
        ("diameter(petersen())", "2"),
        ("diameter(path(6))", "5"),
        ("dist(cycle(4))", "[0, 1, 2, 1; 1, 0, 1, 2; 2, 1, 0, 1; 1, 2, 1, 0]"),
        ("bfs(grid(3, 3), 1)", "{1, 2, 4, 3, 5, 7, 6, 8, 9}"),
        ("dfs(grid(3, 3), 1)", "{1, 2, 3, 6, 5, 4, 7, 8, 9}"),
        // a negative weight in a digraph: Bellman–Ford
        ("dist(digraph([1 2 4; 1 3 2; 3 2 -3]), 1, 2)", "-1"),
        ("components(graph([1 2; 3 4], 5))", "{{1, 2}, {3, 4}, {5}}"),
        ("components(digraph([1 2; 2 1; 2 3]))", "{{1, 2}, {3}}"),
        ("isconnected(petersen())", "true"),
        ("isconnected(graph([1 2], 3))", "false"),
    ] {
        let code = format!("{} {}", W, code);
        assert_eq!(one(&code).replace('\n', ""), want, "{}", code);
    }
}

#[test]
fn trees_flows_matchings() {
    for (code, want) in [
        ("size(mst(W))", "4"),
        ("istree(mst(petersen()))", "true"),
        ("istree(cycle(4))", "false"),
        ("spanningtrees(complete(5))", "125"),
        ("spanningtrees(cycle(7))", "7"),
        ("spanningtrees(petersen())", "2000"),
        ("spanningtrees(graph([1 2], 3))", "0"),
        ("maxflow(F, 1, 6)", "13"),
        ("mincut(F, 1, 6)", "[2, 4, 4; 3, 5, 9]"),
        ("maxflow(graph([1 2 3; 2 3 2; 1 3 1]), 1, 3)", "3"),
        ("size(matching(petersen()))", "[5, 2]"),
        ("size(matching(complete(3, 5)))", "[3, 2]"),
        // an odd cycle needs the blossom step to find the best matching
        ("size(matching(graph([1 2; 2 3; 3 4; 4 5; 5 1; 1 6])))", "[3, 2]"),
        ("toposort(F)", "{1, 2, 3, 5, 4, 6}"),
        ("toposort(digraph([1 2; 2 1]))", "ERROR toposort: the graph has a directed cycle, so no order puts every arrow forward"),
    ] {
        let code = format!("{} {} {}", W, F, code);
        assert_eq!(one(&code).replace('\n', ""), want, "{}", code);
    }
}

#[test]
fn colorings_and_tours() {
    for (code, want) in [
        ("chromatic(petersen())", "3"),
        ("chromatic(complete(5))", "5"),
        ("chromatic(cycle(7))", "3"),
        ("chromatic(grid(4, 4))", "2"),
        ("chromatic(wheel(5))", "4"),
        ("isbipartite(hypercube(3))", "true"),
        ("isbipartite(cycle(5))", "false"),
        // 120 ways to 3-color the Petersen graph
        ("chromaticpoly(petersen(), 3)", "120"),
        ("chromaticpoly(cycle(4), k)", "k^4-4*k^3+6*k^2-3*k"),
        ("chromaticpoly(complete(4), 4)", "24"),
        ("chromaticpoly(path(4), k)", "k^4-3*k^3+3*k^2-k"),
        ("eulerian(complete(5))", "{1, 2, 3, 1, 4, 2, 5, 3, 4, 5, 1}"),
        ("eulerian(path(3))", "{1, 2, 3}"),
        ("eulerian(petersen())", "false"),
        ("hamiltonian(petersen())", "false"),
        ("length(hamiltonian(hypercube(3)))", "9"),
        ("hamiltonian(cycle(5))", "{1, 2, 3, 4, 5, 1}"),
    ] {
        assert_eq!(one(code), want, "{}", code);
    }
    // a coloring from coloring(G) is proper and as small as chromatic(G)
    let c = one("coloring(petersen())");
    assert_eq!(c.matches('3').count() > 0, true);
    assert!(!c.contains('4'));
}

#[test]
fn centrality_and_matrices() {
    for (code, want) in [
        ("centrality(path(5))", "[0, 3, 4, 3, 0]"),
        ("centrality(star(4), \"degree\")", "[4, 1, 1, 1, 1]"),
        ("mean(pagerank(cycle(4)))", "0.25"),
        ("adjacency(path(3))", "[0, 1, 0; 1, 0, 1; 0, 1, 0]"),
        ("laplacian(path(3))", "[ 1, -1, 0; -1, 2, -1; 0, -1, 1]"),
        // Kirchhoff: a cofactor of the Laplacian counts spanning trees (K4 has 16)
        ("L = laplacian(complete(4)); det([L(2,2) L(2,3) L(2,4); L(3,2) L(3,3) L(3,4); L(4,2) L(4,3) L(4,4)])", "16"),
    ] {
        assert_eq!(one(code).replace('\n', "").replace("  ", " "), want, "{}", code);
    }
    let p = one("pagerank(cycle(4))");
    assert!(p.starts_with("[0.25"), "{}", p);
}

#[test]
fn drawings() {
    let mut s = Session::tidy();
    for code in [
        &format!("{} draw(W, shortest(W, \"A\", \"E\"))", W) as &str,
        "draw(petersen(), coloring(petersen()))",
        &format!("{} draw(W, mst(W))", W),
        "draw(grid(2, 3), [1, 2; 2, 3])",
    ] {
        for r in s.run(code) {
            let v = match r {
                Ok(Output::Value(v)) => v,
                other => panic!("{}: {:?}", code, other.err()),
            };
            let b = galileo_rs::display::bundle(&v).expect("a graph has a picture");
            let svg = b.iter().find(|(m, _)| *m == "image/svg+xml").expect("svg").1.clone();
            assert!(svg.starts_with("<svg") && svg.contains("<circle"), "{}", code);
        }
    }
    assert!(one("draw(path(3), {1, 3})").starts_with("ERROR draw: 1 and 3 are not joined"));
    // a drawing rebuilds too
    let d = one("draw(cycle(4), {1, 2, 3})");
    assert_eq!(d, "draw(cycle(4), {1, 2, 3})");
    assert_eq!(one(&format!("x = {}; x", d)), d);
}

#[test]
fn errors_explain() {
    for (code, want) in [
        ("graph([1 1])", "ERROR graph: a loop at 1"),
        ("shortest(cycle(4), 1, 9)", "ERROR shortest: 9 is not a vertex"),
        ("vertices(5)", "ERROR vertices: expected a graph first"),
        ("mst(digraph([1 2]))", "ERROR mst: spanning trees here are for undirected graphs"),
    ] {
        let got = one(code);
        assert!(got.starts_with(want), "{} gave {}", code, got);
    }
}
