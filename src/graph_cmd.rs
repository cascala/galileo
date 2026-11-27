//! Commands for graphs and networks ([`crate::graph`]): building graphs,
//! looking at them, paths and distances, trees, flows, matchings, colorings,
//! tours, orderings, centrality and drawings.

use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::graph::{self as gr, Edge, Graph};
use crate::num::Number;
use crate::parser::Node;
use crate::print::show;
use std::rc::Rc;

/// Commands that build a graph
const BUILD: &[&str] = &[
    "graph", "digraph", "complete", "cycle", "path", "star", "wheel", "grid", "hypercube", "petersen", "randgraph",
];

/// Commands that only take graphs
const ONLY: &[&str] = &[
    "vertices",
    "edges",
    "neighbors",
    "adjacency",
    "complement",
    "isconnected",
    "components",
    "isbipartite",
    "istree",
    "bfs",
    "dfs",
    "shortest",
    "dist",
    "diameter",
    "spanningtrees",
    "mst",
    "maxflow",
    "mincut",
    "matching",
    "coloring",
    "chromatic",
    "chromaticpoly",
    "eulerian",
    "hamiltonian",
    "toposort",
    "pagerank",
    "centrality",
    "draw",
];

/// Commands of other kinds that also take a graph first
const SHARED: &[&str] = &["order", "size", "degree", "laplacian", "subgraph"];

pub fn gval(g: Graph) -> Expr {
    Expr::Obj(Rc::new(Obj::Graph(g)))
}

pub fn as_graph(e: &Expr) -> Option<&Graph> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Graph(g) => Some(g),
            _ => None,
        },
        _ => None,
    }
}

fn list(v: Vec<Expr>) -> Expr {
    Expr::Obj(Rc::new(Obj::List(v)))
}

/// numbers in a row, so sum, mean, max and bar work on them
fn row(v: Vec<Expr>) -> Expr {
    Expr::Matrix(vec![v])
}

fn labels(g: &Graph, vs: &[usize]) -> Expr {
    list(vs.iter().map(|&i| g.labels[i].clone()).collect())
}

fn number(n: &Option<Number>) -> Expr {
    match n {
        Some(x) => Expr::Num(x.clone()),
        None => Expr::sym("inf"),
    }
}

/// The items of a row, a column or a list
fn items(e: &Expr) -> Option<Vec<Expr>> {
    match e {
        Expr::Matrix(m) if m.len() == 1 => Some(m[0].clone()),
        Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => Some(m.iter().map(|r| r[0].clone()).collect()),
        Expr::Obj(o) => match &**o {
            Obj::List(l) => Some(l.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn whole(e: &Expr, f: &str, lo: i128, hi: i128) -> R<usize> {
    let n = crate::extra_cmd::int_of(e, f)?;
    if n < lo || n > hi {
        return Err(format!("{}: needs a whole number from {} to {}", f, lo, hi));
    }
    Ok(n as usize)
}

fn vertex(g: &Graph, e: &Expr, f: &str) -> R<usize> {
    g.index(e).ok_or_else(|| {
        format!(
            "{}: {} is not a vertex of the graph (its vertices are {})",
            f,
            crate::print::show_value(e),
            short_list(g)
        )
    })
}

fn short_list(g: &Graph) -> String {
    if g.n() <= 12 {
        crate::lang::list_text(&g.labels)
    } else {
        format!(
            "{}, {}, ..., {}",
            g.label(0),
            g.label(1),
            g.label(g.n() - 1)
        )
    }
}

fn weight_of(e: &Expr, f: &str) -> R<Number> {
    if let Some(n) = e.num() {
        return Ok(n);
    }
    match crate::calc::evalf(e).num() {
        Some(n) => Ok(n),
        None => Err(format!("{}: an edge weight must be a number, got {}", f, show(e))),
    }
}

/// graph(E), graph(E, n), graph(E, {names}): E has rows [u, v] or [u, v, w]
fn build(f: &str, v: &[Expr]) -> R<Graph> {
    let usage = || {
        format!(
            "usage: {f}([1, 2; 2, 3; 3, 1]) with one edge per row (add a third column for weights), {f}(edges, n) for vertices 1..n, or {f}(edges, {{\"A\", \"B\", \"C\"}}) for named vertices"
        )
    };
    let directed = f == "digraph";
    let (rows, given): (Vec<Vec<Expr>>, Option<&Expr>) = match v {
        [e] | [e, _] => {
            let rows = match e {
                Expr::Matrix(m) => m.clone(),
                Expr::Obj(o) => match &**o {
                    Obj::List(l) => l
                        .iter()
                        .map(|r| items(r).ok_or_else(usage))
                        .collect::<R<Vec<_>>>()?,
                    _ => return Err(usage()),
                },
                _ => return Err(usage()),
            };
            (rows, v.get(1))
        }
        _ => return Err(usage()),
    };
    let rows: Vec<Vec<Expr>> = rows.into_iter().filter(|r| !r.is_empty()).collect();
    if rows.iter().any(|r| r.len() != 2 && r.len() != 3) {
        return Err(format!(
            "{}: each edge is a row [u, v] or [u, v, weight]; {}",
            f,
            usage()
        ));
    }
    let weighted = rows.iter().any(|r| r.len() == 3);
    let mut g = match given {
        None => {
            let mut ls: Vec<Expr> = vec![];
            for r in &rows {
                for x in &r[..2] {
                    if !ls.iter().any(|l| crate::print::show_value(l) == crate::print::show_value(x)) {
                        ls.push(x.clone());
                    }
                }
            }
            // numbered vertices: 1..max, so a missing number is an isolated vertex
            let ints: Option<Vec<i128>> = ls.iter().map(|l| l.num().filter(|n| n.is_exact()).and_then(|n| n.as_int())).collect();
            match ints {
                Some(is) if !is.is_empty() && is.iter().all(|&i| i >= 1) => {
                    let n = *is.iter().max().unwrap() as usize;
                    if n > 100_000 {
                        return Err(format!("{}: vertex {} is too large (at most 100000 vertices)", f, n));
                    }
                    Graph::numbered(n, directed)
                }
                _ => Graph::new(ls, directed),
            }
        }
        Some(Expr::Num(_)) => Graph::numbered(whole(given.unwrap(), f, 0, 100_000)?, directed),
        Some(e) => {
            let ls = items(e).ok_or_else(usage)?;
            for (i, l) in ls.iter().enumerate() {
                if ls[..i].iter().any(|m| crate::print::show_value(m) == crate::print::show_value(l)) {
                    return Err(format!("{}: vertex {} is listed twice", f, crate::print::show_value(l)));
                }
            }
            Graph::new(ls, directed)
        }
    };
    g.weighted = weighted;
    for r in &rows {
        let u = vertex(&g, &r[0], f)?;
        let w = vertex(&g, &r[1], f)?;
        let wt = match r.get(2) {
            Some(x) => weight_of(x, f)?,
            None => Number::one(),
        };
        g.add(u, w, wt).map_err(|e| e.replacen("graph", f, 1))?;
    }
    g.name = g.edge_call();
    Ok(g)
}

fn circle(n: usize, r: f64, turn: f64) -> Vec<(f64, f64)> {
    (0..n)
        .map(|k| {
            let t = std::f64::consts::TAU * k as f64 / n as f64 + std::f64::consts::FRAC_PI_2 + turn;
            (r * t.cos(), r * t.sin())
        })
        .collect()
}

fn named(mut g: Graph, name: String, pos: Vec<(f64, f64)>) -> Expr {
    g.name = name;
    g.pos = Some(pos);
    gval(g)
}

impl Session {
    /// Some(result) if f is a graph command and the arguments fit it
    pub(crate) fn graph_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let build = BUILD.contains(&f);
        let only = ONLY.contains(&f);
        if !build && !only && !SHARED.contains(&f) {
            return None;
        }
        // a shared name (order, size, degree, ...) is ours only when its first
        // argument is plainly a graph: no evaluating other commands' arguments twice
        if !build && !only {
            let graphy = match args.first() {
                Some(Node::Ident(x)) => self.get(x).is_some_and(|v| as_graph(v).is_some()),
                Some(Node::Call(h, _)) => {
                    BUILD.contains(&h.as_str()) || matches!(h.as_str(), "mst" | "complement" | "subgraph" | "draw")
                }
                _ => false,
            };
            if !graphy {
                return None;
            }
        }
        let v = match args.iter().map(|a| self.eval(a)).collect::<R<Vec<Expr>>>() {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        if build {
            return Some(self.graph_build(f, &v));
        }
        let Some(g) = v.first().and_then(as_graph).cloned() else {
            if only {
                return Some(Err(format!(
                    "{}: expected a graph first, such as graph([1, 2; 2, 3; 3, 1]) or cycle(5)",
                    f
                )));
            }
            return None;
        };
        Some(self.graph_cmd(f, &g, &v[1..]))
    }

    fn graph_build(&mut self, f: &str, v: &[Expr]) -> R<Expr> {
        let arg = |k: usize, lo: i128, hi: i128| -> R<usize> {
            match v.get(k) {
                Some(e) => whole(e, f, lo, hi),
                None => Err(format!("{}: needs a whole number; see help(\"{}\")", f, f)),
            }
        };
        let one = Number::one;
        match f {
            "graph" | "digraph" => {
                let g = build(f, v)?;
                self.note(format!(
                    "{} vertices, {} {}edges{}",
                    g.n(),
                    g.edges.len(),
                    if g.directed { "directed " } else { "" },
                    if g.weighted { " with weights" } else { "" }
                ));
                Ok(gval(g))
            }
            "complete" if v.len() == 2 => {
                let (a, b) = (arg(0, 1, 500)?, arg(1, 1, 500)?);
                let mut g = Graph::numbered(a + b, false);
                for i in 0..a {
                    for j in 0..b {
                        g.add(i, a + j, one())?;
                    }
                }
                let span = |k: usize, m: usize| if m > 1 { k as f64 / (m - 1) as f64 * (a.max(b) - 1) as f64 } else { (a.max(b) - 1) as f64 / 2.0 };
                let pos = (0..a).map(|i| (span(i, a), 1.0)).chain((0..b).map(|j| (span(j, b), 0.0))).collect();
                self.note(format!("the complete bipartite graph K({a}, {b}): every one of the first {a} vertices joined to every one of the last {b}"));
                Ok(named(g, format!("complete({a}, {b})"), pos))
            }
            "complete" => {
                let n = arg(0, 1, 1000)?;
                let mut g = Graph::numbered(n, false);
                for i in 0..n {
                    for j in i + 1..n {
                        g.add(i, j, one())?;
                    }
                }
                self.note(format!("the complete graph K{n}: all {} pairs of vertices joined", n * (n - 1) / 2));
                Ok(named(g, format!("complete({n})"), circle(n, 1.0, 0.0)))
            }
            "cycle" => {
                let n = arg(0, 3, 100_000)?;
                let mut g = Graph::numbered(n, false);
                for i in 0..n {
                    g.add(i, (i + 1) % n, one())?;
                }
                Ok(named(g, format!("cycle({n})"), circle(n, 1.0, 0.0)))
            }
            "path" => {
                let n = arg(0, 1, 100_000)?;
                let mut g = Graph::numbered(n, false);
                for i in 0..n.saturating_sub(1) {
                    g.add(i, i + 1, one())?;
                }
                Ok(named(g, format!("path({n})"), (0..n).map(|i| (i as f64, 0.0)).collect()))
            }
            "star" => {
                let n = arg(0, 1, 100_000)?;
                let mut g = Graph::numbered(n + 1, false);
                for i in 1..=n {
                    g.add(0, i, one())?;
                }
                let mut pos = vec![(0.0, 0.0)];
                pos.extend(circle(n, 1.0, 0.0));
                self.note(format!("vertex 1 joined to {} outer vertices", n));
                Ok(named(g, format!("star({n})"), pos))
            }
            "wheel" => {
                let n = arg(0, 3, 100_000)?;
                let mut g = Graph::numbered(n + 1, false);
                for i in 1..=n {
                    g.add(0, i, one())?;
                    g.add(i, i % n + 1, one())?;
                }
                let mut pos = vec![(0.0, 0.0)];
                pos.extend(circle(n, 1.0, 0.0));
                self.note(format!("a hub (vertex 1) joined to a cycle of {} vertices", n));
                Ok(named(g, format!("wheel({n})"), pos))
            }
            "grid" => {
                let (r, c) = (arg(0, 1, 1000)?, if v.len() > 1 { arg(1, 1, 1000)? } else { arg(0, 1, 1000)? });
                let mut g = Graph::numbered(r * c, false);
                let at = |i: usize, j: usize| i * c + j;
                let mut pos = vec![];
                for i in 0..r {
                    for j in 0..c {
                        pos.push((j as f64, -(i as f64)));
                        if j + 1 < c {
                            g.add(at(i, j), at(i, j + 1), one())?;
                        }
                        if i + 1 < r {
                            g.add(at(i, j), at(i + 1, j), one())?;
                        }
                    }
                }
                self.note(format!("{r} rows of {c} vertices, numbered along the rows; each joined to its neighbours left, right, up and down"));
                Ok(named(g, format!("grid({r}, {c})"), pos))
            }
            "hypercube" => {
                let d = arg(0, 0, 14)?;
                let n = 1usize << d;
                let mut g = Graph::numbered(n, false);
                for i in 0..n {
                    for k in 0..d {
                        let j = i ^ (1 << k);
                        if i < j {
                            g.add(i, j, one())?;
                        }
                    }
                }
                // project: each bit is a direction
                let pos = (0..n)
                    .map(|i| {
                        let (mut x, mut y) = (0.0, 0.0);
                        for k in 0..d {
                            if i >> k & 1 == 1 {
                                let t = std::f64::consts::PI * k as f64 / d.max(1) as f64 + 0.3;
                                let len = 1.0 / (1.0 + 0.35 * k as f64);
                                x += len * t.cos();
                                y += len * t.sin();
                            }
                        }
                        (x, y)
                    })
                    .collect();
                self.note(format!("the {d}-dimensional cube: vertex i is the {d}-bit number i - 1, joined to the vertices that differ from it in one bit"));
                Ok(named(g, format!("hypercube({d})"), pos))
            }
            "petersen" => {
                if !v.is_empty() {
                    return Err("petersen() takes no arguments".into());
                }
                let mut g = Graph::numbered(10, false);
                for i in 0..5 {
                    g.add(i, (i + 1) % 5, one())?;
                    g.add(i, i + 5, one())?;
                    g.add(5 + i, 5 + (i + 2) % 5, one())?;
                }
                let mut pos = circle(5, 1.0, 0.0);
                pos.extend(circle(5, 0.5, 0.0));
                self.note("the Petersen graph: 10 vertices, 15 edges, every vertex of degree 3; a five-pointed star inside a pentagon");
                Ok(named(g, "petersen()".into(), pos))
            }
            "randgraph" => {
                let n = arg(0, 1, 5000)?;
                let p = v.get(1).ok_or("usage: randgraph(n, p) joins each pair with probability p, randgraph(n, m) picks m edges")?;
                let pf = crate::calc::evalf(p).num().map(|x| x.to_f64()).ok_or("randgraph: p must be a number")?;
                let mut g = Graph::numbered(n, false);
                let pairs = n * (n - 1) / 2;
                if pf >= 1.0 && p.num().is_some_and(|x| x.is_integer()) {
                    let m = pf as usize;
                    if m > pairs {
                        return Err(format!("randgraph: {} vertices have only {} pairs", n, pairs));
                    }
                    while g.edges.len() < m {
                        let a = (crate::eval::rand_f64() * n as f64) as usize;
                        let b = (crate::eval::rand_f64() * n as f64) as usize;
                        if a != b && !g.has_edge(a, b) {
                            g.add(a.min(n - 1), b.min(n - 1), one())?;
                        }
                    }
                } else {
                    if !(0.0..=1.0).contains(&pf) {
                        return Err("randgraph: p is a probability from 0 to 1, or a whole number of edges".into());
                    }
                    for i in 0..n {
                        for j in i + 1..n {
                            if crate::eval::rand_f64() < pf {
                                g.add(i, j, one())?;
                            }
                        }
                    }
                }
                g.edges.sort_by_key(|e| (e.u, e.v));
                g.name = g.edge_call();
                self.note(format!(
                    "a random graph (Erdős–Rényi) with {} vertices and {} edges; seed(k) first makes it repeatable",
                    n,
                    g.edges.len()
                ));
                Ok(gval(g))
            }
            _ => unreachable!(),
        }
    }

    fn graph_cmd(&mut self, f: &str, g: &Graph, v: &[Expr]) -> R<Expr> {
        let n = g.n();
        let need = |k: usize, what: &str| -> R<()> {
            if v.len() < k {
                Err(format!("usage: {}(G, {})", f, what))
            } else {
                Ok(())
            }
        };
        match f {
            "order" => {
                self.note("the number of vertices");
                Ok(Expr::int(n as i128))
            }
            "size" => {
                self.note("the number of edges");
                Ok(Expr::int(g.edges.len() as i128))
            }
            "vertices" => {
                self.note(format!("{} vertices", n));
                Ok(list(g.labels.clone()))
            }
            "edges" => {
                self.note(format!(
                    "{} edges, one per row{}",
                    g.edges.len(),
                    if g.weighted { ": from, to, weight" } else if g.directed { ": from, to" } else { "" }
                ));
                Ok(g.edge_rows())
            }
            "degree" => {
                let d = g.degrees();
                if let Some(x) = v.first() {
                    let i = vertex(g, x, f)?;
                    if g.directed {
                        self.note(format!(
                            "{} edges out, {} in",
                            g.out_degrees()[i],
                            g.in_degrees()[i]
                        ));
                    }
                    return Ok(Expr::int(d[i] as i128));
                }
                let sum: usize = d.iter().sum();
                if !g.directed {
                    self.note(format!(
                        "the degrees add up to {} = 2 × {} edges, since each edge has two ends",
                        sum,
                        g.edges.len()
                    ));
                }
                Ok(row(d.iter().map(|&x| Expr::int(x as i128)).collect()))
            }
            "neighbors" => {
                need(1, "v")?;
                let i = vertex(g, &v[0], f)?;
                let mut nb: Vec<usize> = g.adj()[i].iter().map(|x| x.0).collect();
                nb.sort();
                self.note(format!(
                    "the {} vertices {} {}",
                    nb.len(),
                    if g.directed { "that arrows lead to from" } else { "joined to" },
                    g.label(i)
                ));
                Ok(labels(g, &nb))
            }
            "adjacency" | "laplacian" => {
                let mut a = vec![vec![Expr::int(0); n]; n];
                for e in &g.edges {
                    let w = if g.weighted { Expr::Num(e.w.clone()) } else { Expr::int(1) };
                    a[e.u][e.v] = w.clone();
                    if !g.directed {
                        a[e.v][e.u] = w;
                    }
                }
                if f == "adjacency" {
                    self.note(if g.directed {
                        "entry (i, j) is the weight of the edge from vertex i to vertex j (0 for none); entry (i, j) of A^k counts the walks of length k from i to j"
                    } else {
                        "entry (i, j) is 1 when vertices i and j are joined (the weight for a weighted graph); entry (i, j) of A^k counts the walks of length k from i to j"
                    });
                    return Ok(Expr::Matrix(a));
                }
                // L = D - A
                let mut l = vec![vec![Expr::int(0); n]; n];
                for i in 0..n {
                    let mut d = Number::zero();
                    for j in 0..n {
                        if i != j {
                            let x = a[i][j].num().unwrap();
                            d = d.add(&x);
                            l[i][j] = Expr::Num(x.neg());
                        }
                    }
                    l[i][i] = Expr::Num(d);
                }
                self.note("L = D - A (degrees on the diagonal, minus the adjacency matrix). The number of times 0 is an eigenvalue is the number of components; any cofactor of L counts the spanning trees (Kirchhoff).");
                Ok(Expr::Matrix(l))
            }
            "subgraph" => {
                need(1, "{vertices}")?;
                let vs = items(&v[0]).ok_or("usage: subgraph(G, {1, 2, 3}) keeps those vertices and the edges between them")?;
                let keep = vs.iter().map(|x| vertex(g, x, f)).collect::<R<Vec<_>>>()?;
                Ok(gval(g.induced(&keep)))
            }
            "complement" => {
                self.note("the same vertices, joined exactly where G has no edge");
                Ok(gval(g.complement()))
            }
            "isconnected" => {
                let c = gr::components(g);
                if c.len() > 1 {
                    self.note(format!(
                        "{} {}components",
                        c.len(),
                        if g.directed { "strongly connected " } else { "" }
                    ));
                }
                Ok(Expr::Bool(gr::is_connected(g)))
            }
            "components" => {
                let c = gr::components(g);
                self.note(if g.directed {
                    format!("{} strongly connected components: in each, every vertex can reach every other along the arrows", c.len())
                } else {
                    format!("{} connected component{}", c.len(), if c.len() == 1 { "" } else { "s" })
                });
                Ok(list(c.iter().map(|c| labels(g, c)).collect()))
            }
            "isbipartite" => match gr::bipartition(g) {
                Some(side) => {
                    let a: Vec<usize> = (0..n).filter(|&i| side[i] == 0).collect();
                    let b: Vec<usize> = (0..n).filter(|&i| side[i] == 1).collect();
                    self.note(format!(
                        "every edge joins {} to {}",
                        show(&labels(g, &a)),
                        show(&labels(g, &b))
                    ));
                    Ok(Expr::Bool(true))
                }
                None => {
                    self.note("not bipartite: the graph has a cycle of odd length");
                    Ok(Expr::Bool(false))
                }
            },
            "istree" => {
                let t = !g.directed && n > 0 && g.edges.len() + 1 == n && gr::is_connected(g);
                if !t && !g.directed {
                    self.note(format!(
                        "a tree is connected with one edge fewer than vertices; this graph has {} vertices and {} edges{}",
                        n,
                        g.edges.len(),
                        if gr::is_connected(g) { "" } else { " and is not connected" }
                    ));
                }
                Ok(Expr::Bool(t))
            }
            "bfs" | "dfs" => {
                need(1, "start")?;
                let s = vertex(g, &v[0], f)?;
                let order = if f == "bfs" { gr::bfs(g, s) } else { gr::dfs(g, s) };
                self.note(if f == "bfs" {
                    "breadth-first: the start, then all its neighbours, then their new neighbours, and so on (smaller vertices first); the order is by distance from the start"
                } else {
                    "depth-first: go to the smallest new neighbour, as deep as possible, and back up only when stuck"
                });
                Ok(labels(g, &order))
            }
            "shortest" => {
                need(2, "from, to")?;
                let (s, t) = (vertex(g, &v[0], f)?, vertex(g, &v[1], f)?);
                let (d, prev) = gr::shortest_from(g, s).map_err(|e| format!("shortest: {}", e))?;
                if d[t].is_none() {
                    self.note(format!("there is no path from {} to {}", g.label(s), g.label(t)));
                    return Ok(list(vec![]));
                }
                let p = gr::path_to(&prev, s, t);
                self.note(format!(
                    "{} {} along {} edges{}",
                    if g.weighted { "total weight" } else { "length" },
                    show(&number(&d[t])),
                    p.len() - 1,
                    if g.edges.iter().any(|e| e.w.is_negative()) {
                        " (Bellman–Ford, since some weights are negative)"
                    } else if g.weighted {
                        " (Dijkstra's algorithm)"
                    } else {
                        " (breadth-first search)"
                    }
                ));
                Ok(labels(g, &p))
            }
            "dist" => match v {
                [] => {
                    let d = gr::all_pairs(g).map_err(|e| format!("dist: {}", e))?;
                    self.note("entry (i, j) is the distance from vertex i to vertex j; inf means no path (Floyd–Warshall)");
                    Ok(Expr::Matrix(d.iter().map(|r| r.iter().map(number).collect()).collect()))
                }
                [a, b] => {
                    let (s, t) = (vertex(g, a, f)?, vertex(g, b, f)?);
                    let (d, _) = gr::shortest_from(g, s).map_err(|e| format!("dist: {}", e))?;
                    self.note(format!(
                        "the {} of a shortest path from {} to {}",
                        if g.weighted { "total weight" } else { "number of edges" },
                        g.label(s),
                        g.label(t)
                    ));
                    Ok(number(&d[t]))
                }
                _ => Err("usage: dist(G, from, to) or dist(G) for all distances".into()),
            },
            "diameter" => {
                let d = gr::all_pairs(g).map_err(|e| format!("diameter: {}", e))?;
                let mut best: Option<(Number, usize, usize)> = None;
                for i in 0..n {
                    for j in 0..n {
                        match &d[i][j] {
                            None => {
                                self.note(format!("no path from {} to {}, so the diameter is infinite", g.label(i), g.label(j)));
                                return Ok(Expr::sym("inf"));
                            }
                            Some(x) => {
                                if best.as_ref().is_none_or(|b| x.cmp_value(&b.0) == std::cmp::Ordering::Greater) {
                                    best = Some((x.clone(), i, j));
                                }
                            }
                        }
                    }
                }
                let Some((x, i, j)) = best else { return Ok(Expr::int(0)) };
                self.note(format!("the largest distance between two vertices, e.g. from {} to {}", g.label(i), g.label(j)));
                Ok(Expr::Num(x))
            }
            "spanningtrees" => {
                if g.directed {
                    return Err("spanningtrees: needs an undirected graph".into());
                }
                if n > 300 {
                    return Err("spanningtrees: at most 300 vertices".into());
                }
                // Kirchhoff: the determinant of the Laplacian without its first row and column
                let mut m: Vec<Vec<Number>> = vec![vec![Number::zero(); n]; n];
                for e in &g.edges {
                    m[e.u][e.u] = m[e.u][e.u].add(Number::one());
                    m[e.v][e.v] = m[e.v][e.v].add(Number::one());
                    m[e.u][e.v] = m[e.u][e.v].sub(Number::one());
                    m[e.v][e.u] = m[e.v][e.u].sub(Number::one());
                }
                let mut a: Vec<Vec<Number>> = m[1..].iter().map(|r| r[1..].to_vec()).collect();
                let k = a.len();
                let mut det = Number::one();
                for c in 0..k {
                    let Some(p) = (c..k).find(|&r| !a[r][c].is_zero()) else {
                        det = Number::zero();
                        break;
                    };
                    if p != c {
                        a.swap(p, c);
                        det = det.neg();
                    }
                    det = det.mul(&a[c][c]);
                    for r in c + 1..k {
                        if a[r][c].is_zero() {
                            continue;
                        }
                        let f = a[r][c].div(&a[c][c]).unwrap();
                        for j in c..k {
                            let x = a[c][j].mul(&f);
                            a[r][j] = a[r][j].sub(&x);
                        }
                    }
                }
                self.note("the number of different spanning trees (Kirchhoff's matrix-tree theorem: any cofactor of the Laplacian)");
                Ok(Expr::Num(if n == 0 { Number::zero() } else { det }))
            }
            "mst" => {
                if g.directed {
                    return Err("mst: spanning trees here are for undirected graphs".into());
                }
                let ks = gr::mst(g);
                let es: Vec<Edge> = ks.iter().map(|&k| g.edges[k].clone()).collect();
                let total = es.iter().fold(Number::zero(), |a, e| a.add(&e.w));
                let c = gr::components(g).len();
                self.note(format!(
                    "{} edges{}{}; Kruskal's algorithm: take the lightest edges first, skipping any that would close a cycle",
                    es.len(),
                    if g.weighted { format!(", total weight {}", show(&Expr::Num(total))) } else { String::new() },
                    if c > 1 { format!(" (a forest: the graph has {} components)", c) } else { String::new() }
                ));
                Ok(gval(g.with_edges(es, g.weighted)))
            }
            "maxflow" | "mincut" => {
                need(2, "source, sink")?;
                let (s, t) = (vertex(g, &v[0], f)?, vertex(g, &v[1], f)?);
                if s == t {
                    return Err(format!("{}: the source and the sink must differ", f));
                }
                let fl = gr::max_flow(g, s, t).map_err(|e| format!("{}: {}", f, e))?;
                let cut: Vec<&Edge> = g
                    .edges
                    .iter()
                    .filter(|e| {
                        (fl.source_side[e.u] && !fl.source_side[e.v])
                            || (!g.directed && fl.source_side[e.v] && !fl.source_side[e.u])
                    })
                    .collect();
                let cut_rows = Expr::Matrix(
                    cut.iter()
                        .map(|e| {
                            let (a, b) = if fl.source_side[e.u] { (e.u, e.v) } else { (e.v, e.u) };
                            let mut r = vec![g.labels[a].clone(), g.labels[b].clone()];
                            if g.weighted {
                                r.push(Expr::Num(e.w.clone()));
                            }
                            r
                        })
                        .collect(),
                );
                if f == "mincut" {
                    self.note(format!(
                        "removing these {} edges separates {} from {}; their capacities add up to {}, the maximum flow (max-flow min-cut theorem)",
                        cut.len(),
                        g.label(s),
                        g.label(t),
                        show(&Expr::Num(fl.value.clone()))
                    ));
                    return Ok(cut_rows);
                }
                let mut lines = vec![];
                for (k, e) in g.edges.iter().enumerate() {
                    let x = &fl.on[k];
                    if !x.is_zero() {
                        let (a, b, x) = if x.is_negative() { (e.v, e.u, x.neg()) } else { (e.u, e.v, x.clone()) };
                        lines.push(format!("{} → {}: {}", g.label(a), g.label(b), show(&Expr::Num(x))));
                    }
                }
                self.note(format!(
                    "flow on the edges: {}. A minimum cut, {}, has the same total capacity (max-flow min-cut theorem). Edmonds–Karp: push flow along shortest paths with room left until none is left.",
                    lines.join(", "),
                    gr::one_line(&cut_rows)
                ));
                Ok(Expr::Num(fl.value))
            }
            "matching" => {
                let mate = gr::matching(g);
                // a bipartite graph: each pair starts on the side of the first vertex
                let side = gr::bipartition(g);
                let pairs: Vec<Vec<Expr>> = (0..n)
                    .filter_map(|i| mate[i].filter(|&j| i < j).map(|j| (i, j)))
                    .map(|(i, j)| match &side {
                        Some(sd) if sd[i] != sd[0] => (j, i),
                        _ => (i, j),
                    })
                    .map(|(i, j)| vec![g.labels[i].clone(), g.labels[j].clone()])
                    .collect();
                let k = pairs.len();
                self.note(format!(
                    "{} pairs, the most possible (Edmonds' blossom algorithm){}",
                    k,
                    if 2 * k == n { "; a perfect matching: every vertex is paired" } else { "" }
                ));
                Ok(Expr::Matrix(pairs))
            }
            "coloring" | "chromatic" => {
                if n > 500 {
                    return Err(format!("{}: {} vertices is too many (at most 500)", f, n));
                }
                let (c, exact) = gr::coloring(g);
                let k = c.iter().max().map_or(0, |m| m + 1);
                if f == "chromatic" {
                    if !exact {
                        return Err(format!(
                            "chromatic: the search ran out of time; the graph can be colored with {} colors, maybe fewer",
                            k
                        ));
                    }
                    self.note(format!("{} colors are enough for neighbours to differ, and fewer are not", k));
                    return Ok(Expr::int(k as i128));
                }
                self.note(if exact {
                    format!("{} colors, the fewest possible; joined vertices get different colors", k)
                } else {
                    format!("{} colors; joined vertices get different colors. The exact search ran out of time, so fewer might do", k)
                });
                Ok(row(c.iter().map(|&x| Expr::int(x as i128 + 1)).collect()))
            }
            "chromaticpoly" => {
                need(1, "k")?;
                let p = gr::chromatic_poly(g)?;
                let k = v[0].clone();
                let mut terms = vec![];
                for (i, &c) in p.iter().enumerate() {
                    if c != 0 {
                        terms.push(crate::build::mul(vec![
                            Expr::int(c),
                            crate::build::pow(k.clone(), Expr::int(i as i128)),
                        ]));
                    }
                }
                let poly = crate::calc::simplify(&crate::build::add(terms));
                self.note("the number of ways to color the vertices with k colors so that joined vertices differ (deletion–contraction: P(G) = P(G - e) - P(G / e))");
                Ok(poly)
            }
            "eulerian" => match gr::euler(g) {
                Some(t) => {
                    let closed = t.first() == t.last();
                    self.note(if closed {
                        "an Euler circuit: every edge once, back to the start (every vertex has even degree)"
                    } else {
                        "an Euler trail: every edge once, from one odd-degree vertex to the other"
                    });
                    Ok(labels(g, &t))
                }
                None => {
                    let d = if g.directed { vec![] } else { g.degrees() };
                    let odd: Vec<usize> = (0..n).filter(|&i| !d.is_empty() && d[i] % 2 == 1).collect();
                    self.note(if !odd.is_empty() && odd.len() > 2 {
                        format!(
                            "no walk uses every edge once: {} vertices have odd degree ({}), and at most 2 may",
                            odd.len(),
                            show(&labels(g, &odd))
                        )
                    } else {
                        "no walk uses every edge exactly once".to_string()
                    });
                    Ok(Expr::Bool(false))
                }
            },
            "hamiltonian" => match gr::hamiltonian(g)? {
                Some(c) => {
                    self.note("a cycle through every vertex exactly once, back to the start");
                    Ok(labels(g, &c))
                }
                None => {
                    self.note("no cycle passes through every vertex exactly once (checked over all subsets of vertices)");
                    Ok(Expr::Bool(false))
                }
            },
            "toposort" => {
                if !g.directed {
                    return Err("toposort: needs a directed graph, digraph([...])".into());
                }
                match gr::toposort(g) {
                    Some(o) => {
                        self.note("every arrow points forward in this order (Kahn's algorithm, smaller vertices first)");
                        Ok(labels(g, &o))
                    }
                    None => Err("toposort: the graph has a directed cycle, so no order puts every arrow forward".into()),
                }
            }
            "pagerank" => {
                let d = match v.first() {
                    Some(x) => crate::calc::evalf(x).num().map(|n| n.to_f64()).filter(|d| (0.0..1.0).contains(d)).ok_or("pagerank: the damping d is a number from 0 to 1 (0.85 is usual)")?,
                    None => 0.85,
                };
                let r = gr::pagerank(g, d);
                self.note(format!("the share of time a random surfer spends at each vertex, following a random edge with probability {} and jumping anywhere otherwise; the shares add up to 1", d));
                Ok(row(r.iter().map(|&x| Expr::float(x)).collect()))
            }
            "centrality" => {
                let kind = match v.first() {
                    Some(Expr::Str(s)) => s.as_str(),
                    None => "betweenness",
                    Some(e) => return Err(format!("centrality: the kind is \"betweenness\", \"closeness\" or \"degree\", got {}", show(e))),
                };
                let xs: Vec<Expr> = match kind {
                    "betweenness" => {
                        self.note("betweenness: for each vertex, how many shortest paths between other pairs pass through it (split evenly when there are several)");
                        gr::betweenness(g)
                            .iter()
                            .map(|&x| if (x - x.round()).abs() < 1e-9 { Expr::int(x.round() as i128) } else { Expr::float(x) })
                            .collect()
                    }
                    "closeness" => {
                        self.note("closeness: 1 over the average distance to the vertices it can reach");
                        gr::closeness(g)?.iter().map(|&x| Expr::float(x)).collect()
                    }
                    "degree" => g.degrees().iter().map(|&d| Expr::int(d as i128)).collect(),
                    _ => return Err(format!("centrality: the kind is \"betweenness\", \"closeness\" or \"degree\", not \"{}\"", kind)),
                };
                Ok(row(xs))
            }
            "draw" => self.graph_draw(g, v),
            _ => unreachable!(),
        }
    }

    fn graph_draw(&mut self, g: &Graph, v: &[Expr]) -> R<Expr> {
        let mut d = g.clone();
        d.marks = Default::default();
        let n = g.n();
        let mut extra = vec![];
        for x in v {
            extra.push(gr::one_line(x));
            if let Some(h) = as_graph(x) {
                for e in &h.edges {
                    let a = vertex(g, &h.labels[e.u], "draw")?;
                    let b = vertex(g, &h.labels[e.v], "draw")?;
                    d.marks.edges.push((a, b));
                }
                continue;
            }
            if let Expr::Matrix(m) = x {
                if m.len() > 1 && m.iter().all(|r| r.len() == 2 || r.len() == 3) {
                    for r in m {
                        d.marks.edges.push((vertex(g, &r[0], "draw")?, vertex(g, &r[1], "draw")?));
                    }
                    continue;
                }
            }
            let Some(it) = items(x) else {
                return Err("draw(G, x): x is a path or cycle {1, 2, 3}, a coloring from coloring(G), edges [1, 2; 2, 3], or a graph such as mst(G)".into());
            };
            // a proper coloring of all the vertices?
            let colors: Option<Vec<usize>> = (it.len() == n)
                .then(|| it.iter().map(|c| c.num().and_then(|c| c.as_int()).filter(|&c| c >= 1).map(|c| c as usize - 1)).collect::<Option<Vec<_>>>())
                .flatten()
                .filter(|cs| g.edges.iter().all(|e| cs[e.u] != cs[e.v]));
            if let Some(cs) = colors {
                d.marks.colors = cs;
                continue;
            }
            let vs = it.iter().map(|x| vertex(g, x, "draw")).collect::<R<Vec<_>>>()?;
            for w in vs.windows(2) {
                if !g.has_edge(w[0], w[1]) {
                    return Err(format!("draw: {} and {} are not joined, so {} is not a path in the graph", g.label(w[0]), g.label(w[1]), show(x)));
                }
                d.marks.edges.push((w[0], w[1]));
            }
        }
        let what = match (d.marks.edges.is_empty(), d.marks.colors.is_empty()) {
            (false, _) => format!("{} edges marked in orange", d.marks.edges.len()),
            (true, false) => "vertices filled with their colors".to_string(),
            _ => "the graph".to_string(),
        };
        self.note(what);
        d.name = if extra.is_empty() {
            g.name.clone()
        } else {
            format!("draw({}, {})", g.name, extra.join(", "))
        };
        Ok(gval(d))
    }
}
