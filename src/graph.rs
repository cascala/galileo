//! Graphs and networks as values: vertices with names, edges with optional
//! weights, directed or not. Algorithms work on vertex indices; weights are
//! exact numbers where they can be (integer weights give integer distances
//! and flows), so results stay exact.

use crate::expr::Expr;
use crate::num::Number;
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::fmt::Write;

#[derive(Clone, Debug, PartialEq)]
pub struct Edge {
    pub u: usize,
    pub v: usize,
    pub w: Number,
}

/// What a drawing highlights: edges (a path, a tree, a matching) and vertex colors
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Marks {
    pub edges: Vec<(usize, usize)>,
    pub colors: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Graph {
    /// the call that rebuilds it
    pub name: String,
    /// vertex names: integers or strings
    pub labels: Vec<Expr>,
    pub directed: bool,
    pub weighted: bool,
    /// for an undirected graph u < v; no loops, no parallel edges
    pub edges: Vec<Edge>,
    /// a layout that comes with the graph (cycle, grid, ...)
    pub pos: Option<Vec<(f64, f64)>>,
    pub marks: Marks,
}

pub type R<T> = Result<T, String>;

impl Graph {
    pub fn new(labels: Vec<Expr>, directed: bool) -> Graph {
        Graph {
            name: String::new(),
            labels,
            directed,
            weighted: false,
            edges: vec![],
            pos: None,
            marks: Marks::default(),
        }
    }

    /// Vertices 1..n
    pub fn numbered(n: usize, directed: bool) -> Graph {
        Graph::new((1..=n).map(|k| Expr::int(k as i128)).collect(), directed)
    }

    pub fn n(&self) -> usize {
        self.labels.len()
    }

    /// Adds an edge; an edge that is already there gets the new weight
    pub fn add(&mut self, u: usize, v: usize, w: Number) -> R<()> {
        if u == v {
            return Err(format!(
                "graph: a loop at {} (an edge from a vertex to itself) is not supported",
                self.label(u)
            ));
        }
        let (a, b) = if self.directed || u < v { (u, v) } else { (v, u) };
        if let Some(e) = self.edges.iter_mut().find(|e| e.u == a && e.v == b) {
            e.w = w;
        } else {
            self.edges.push(Edge { u: a, v: b, w });
        }
        Ok(())
    }

    /// A vertex as Galileo writes it: 3 or "A"
    pub fn label(&self, i: usize) -> String {
        crate::print::show_value(&self.labels[i])
    }

    pub fn index(&self, e: &Expr) -> Option<usize> {
        let s = crate::print::show_value(e);
        self.labels.iter().position(|l| crate::print::show_value(l) == s)
    }

    /// Whether the vertices are 1, 2, ..., n
    pub fn plain_numbers(&self) -> bool {
        self.labels
            .iter()
            .enumerate()
            .all(|(i, l)| matches!(l, Expr::Num(n) if n.as_int() == Some(i as i128 + 1)))
    }

    /// The edges as rows [u, v] or [u, v, w]
    pub fn edge_rows(&self) -> Expr {
        Expr::Matrix(
            self.edges
                .iter()
                .map(|e| {
                    let mut r = vec![self.labels[e.u].clone(), self.labels[e.v].clone()];
                    if self.weighted {
                        r.push(Expr::Num(e.w.clone()));
                    }
                    r
                })
                .collect(),
        )
    }

    /// The call that rebuilds the graph from its edge list
    pub fn edge_call(&self) -> String {
        let f = if self.directed { "digraph" } else { "graph" };
        let rows = one_line(&self.edge_rows());
        let mut used = vec![false; self.n()];
        for e in &self.edges {
            used[e.u] = true;
            used[e.v] = true;
        }
        // the edges fix the vertices when they use each of 1..n in order of first use
        let mut first = vec![];
        for e in &self.edges {
            for x in [e.u, e.v] {
                if !first.contains(&x) {
                    first.push(x);
                }
            }
        }
        let numbers_in_order = self.plain_numbers() && used.iter().all(|&u| u);
        let names_in_order = !self.plain_numbers()
            && used.iter().all(|&u| u)
            && first.iter().enumerate().all(|(k, &x)| k == x);
        if self.edges.is_empty() || !(numbers_in_order || names_in_order) {
            let vs = if self.plain_numbers() {
                self.n().to_string()
            } else {
                crate::lang::list_text(&self.labels)
            };
            format!("{}({}, {})", f, rows, vs)
        } else {
            format!("{}({})", f, rows)
        }
    }

    /// Neighbours as (vertex, weight, edge index): out-edges for a digraph
    pub fn adj(&self) -> Vec<Vec<(usize, Number, usize)>> {
        let mut a = vec![vec![]; self.n()];
        for (k, e) in self.edges.iter().enumerate() {
            a[e.u].push((e.v, e.w.clone(), k));
            if !self.directed {
                a[e.v].push((e.u, e.w.clone(), k));
            }
        }
        a
    }

    /// Neighbours ignoring direction
    pub fn adj_undirected(&self) -> Vec<Vec<usize>> {
        let mut a = vec![vec![]; self.n()];
        for e in &self.edges {
            a[e.u].push(e.v);
            a[e.v].push(e.u);
        }
        for l in a.iter_mut() {
            l.sort();
            l.dedup();
        }
        a
    }

    pub fn has_edge(&self, u: usize, v: usize) -> bool {
        self.edges
            .iter()
            .any(|e| (e.u == u && e.v == v) || (!self.directed && e.u == v && e.v == u))
    }

    pub fn weight(&self, u: usize, v: usize) -> Option<Number> {
        self.edges
            .iter()
            .find(|e| (e.u == u && e.v == v) || (!self.directed && e.u == v && e.v == u))
            .map(|e| e.w.clone())
    }

    pub fn degrees(&self) -> Vec<usize> {
        let mut d = vec![0; self.n()];
        for e in &self.edges {
            d[e.u] += 1;
            d[e.v] += 1;
        }
        d
    }

    pub fn out_degrees(&self) -> Vec<usize> {
        let mut d = vec![0; self.n()];
        for e in &self.edges {
            d[e.u] += 1;
        }
        d
    }

    pub fn in_degrees(&self) -> Vec<usize> {
        let mut d = vec![0; self.n()];
        for e in &self.edges {
            d[e.v] += 1;
        }
        d
    }

    /// The graph on some of the vertices with the edges between them
    pub fn induced(&self, keep: &[usize]) -> Graph {
        let mut g = Graph::new(keep.iter().map(|&i| self.labels[i].clone()).collect(), self.directed);
        g.weighted = self.weighted;
        let at = |i: usize| keep.iter().position(|&k| k == i);
        for e in &self.edges {
            if let (Some(a), Some(b)) = (at(e.u), at(e.v)) {
                let _ = g.add(a, b, e.w.clone());
            }
        }
        g.pos = self.pos.as_ref().map(|p| keep.iter().map(|&i| p[i]).collect());
        g.name = g.edge_call();
        g
    }

    /// Same vertices (and layout), other edges
    pub fn with_edges(&self, edges: Vec<Edge>, weighted: bool) -> Graph {
        let mut g = Graph::new(self.labels.clone(), self.directed);
        g.weighted = weighted;
        g.edges = edges;
        g.pos = self.pos.clone();
        g.name = g.edge_call();
        g
    }

    pub fn complement(&self) -> Graph {
        let n = self.n();
        let mut es = vec![];
        for u in 0..n {
            for v in 0..n {
                if u != v && (self.directed || u < v) && !self.has_edge(u, v) {
                    es.push(Edge { u, v, w: Number::one() });
                }
            }
        }
        self.with_edges(es, false)
    }
}

/// A matrix on one line, strings quoted: [1, 2; "A", "B"]
pub fn one_line(e: &Expr) -> String {
    match e {
        Expr::Matrix(m) => format!(
            "[{}]",
            m.iter()
                .map(|r| r.iter().map(crate::print::show_value).collect::<Vec<_>>().join(", "))
                .collect::<Vec<_>>()
                .join("; ")
        ),
        e => crate::print::show_value(e),
    }
}

fn lt(a: &Number, b: &Number) -> bool {
    a.cmp_value(b) == Ordering::Less
}

/// Breadth-first order from s
pub fn bfs(g: &Graph, s: usize) -> Vec<usize> {
    let a = g.adj();
    let mut seen = vec![false; g.n()];
    let mut out = vec![s];
    seen[s] = true;
    let mut k = 0;
    while k < out.len() {
        let u = out[k];
        let mut nb: Vec<usize> = a[u].iter().map(|x| x.0).collect();
        nb.sort();
        for v in nb {
            if !seen[v] {
                seen[v] = true;
                out.push(v);
            }
        }
        k += 1;
    }
    out
}

/// Depth-first (preorder) from s, smaller neighbours first
pub fn dfs(g: &Graph, s: usize) -> Vec<usize> {
    let a = g.adj();
    let mut seen = vec![false; g.n()];
    let mut out = vec![];
    let mut stack = vec![s];
    while let Some(u) = stack.pop() {
        if seen[u] {
            continue;
        }
        seen[u] = true;
        out.push(u);
        let mut nb: Vec<usize> = a[u].iter().map(|x| x.0).filter(|&v| !seen[v]).collect();
        nb.sort();
        nb.reverse();
        stack.extend(nb);
    }
    out
}

/// Connected components (strongly connected for a digraph), each sorted,
/// in order of their smallest vertex
pub fn components(g: &Graph) -> Vec<Vec<usize>> {
    let mut comps = if g.directed {
        tarjan(g)
    } else {
        let a = g.adj_undirected();
        let mut seen = vec![false; g.n()];
        let mut cs = vec![];
        for s in 0..g.n() {
            if seen[s] {
                continue;
            }
            let mut c = vec![s];
            seen[s] = true;
            let mut k = 0;
            while k < c.len() {
                for &v in &a[c[k]] {
                    if !seen[v] {
                        seen[v] = true;
                        c.push(v);
                    }
                }
                k += 1;
            }
            cs.push(c);
        }
        cs
    };
    for c in comps.iter_mut() {
        c.sort();
    }
    comps.sort();
    comps
}

fn tarjan(g: &Graph) -> Vec<Vec<usize>> {
    let a = g.adj();
    let n = g.n();
    let mut index = vec![usize::MAX; n];
    let mut low = vec![0; n];
    let mut on = vec![false; n];
    let mut st = vec![];
    let mut out = vec![];
    let mut next = 0;
    for s in 0..n {
        if index[s] != usize::MAX {
            continue;
        }
        // iterative DFS: (vertex, next neighbour position)
        let mut call = vec![(s, 0usize)];
        index[s] = next;
        low[s] = next;
        next += 1;
        st.push(s);
        on[s] = true;
        while let Some(&mut (u, ref mut k)) = call.last_mut() {
            if *k < a[u].len() {
                let v = a[u][*k].0;
                *k += 1;
                if index[v] == usize::MAX {
                    index[v] = next;
                    low[v] = next;
                    next += 1;
                    st.push(v);
                    on[v] = true;
                    call.push((v, 0));
                } else if on[v] {
                    low[u] = low[u].min(index[v]);
                }
            } else {
                call.pop();
                if let Some(&(p, _)) = call.last() {
                    low[p] = low[p].min(low[u]);
                }
                if low[u] == index[u] {
                    let mut c = vec![];
                    while let Some(w) = st.pop() {
                        on[w] = false;
                        c.push(w);
                        if w == u {
                            break;
                        }
                    }
                    out.push(c);
                }
            }
        }
    }
    out
}

pub fn is_connected(g: &Graph) -> bool {
    if g.n() == 0 {
        return true;
    }
    if g.directed {
        components(g).len() == 1
    } else {
        bfs(g, 0).len() == g.n()
    }
}

/// Two-coloring if there is one
pub fn bipartition(g: &Graph) -> Option<Vec<usize>> {
    let a = g.adj_undirected();
    let mut side = vec![usize::MAX; g.n()];
    for s in 0..g.n() {
        if side[s] != usize::MAX {
            continue;
        }
        side[s] = 0;
        let mut q = VecDeque::from([s]);
        while let Some(u) = q.pop_front() {
            for &v in &a[u] {
                if side[v] == usize::MAX {
                    side[v] = 1 - side[u];
                    q.push_back(v);
                } else if side[v] == side[u] {
                    return None;
                }
            }
        }
    }
    Some(side)
}

/// Shortest distances from s and the previous vertex on a shortest path.
/// Dijkstra for nonnegative weights, Bellman–Ford otherwise (Err on a negative cycle).
pub fn shortest_from(g: &Graph, s: usize) -> R<(Vec<Option<Number>>, Vec<Option<usize>>)> {
    let n = g.n();
    let a = g.adj();
    let mut d: Vec<Option<Number>> = vec![None; n];
    let mut prev = vec![None; n];
    d[s] = Some(Number::zero());
    let negative = g.edges.iter().any(|e| e.w.is_negative());
    if negative && !g.directed {
        return Err("an undirected edge with a negative weight makes a negative cycle (go back and forth along it)".into());
    }
    if !negative {
        let mut done = vec![false; n];
        loop {
            let mut best: Option<usize> = None;
            for u in 0..n {
                if done[u] {
                    continue;
                }
                if let Some(du) = &d[u] {
                    if best.is_none_or(|b| lt(du, d[b].as_ref().unwrap())) {
                        best = Some(u);
                    }
                }
            }
            let Some(u) = best else { break };
            done[u] = true;
            let du = d[u].clone().unwrap();
            for (v, w, _) in &a[u] {
                let nd = du.add(w);
                if d[*v].as_ref().is_none_or(|dv| lt(&nd, dv)) {
                    d[*v] = Some(nd);
                    prev[*v] = Some(u);
                }
            }
        }
        return Ok((d, prev));
    }
    for round in 0..n {
        let mut changed = false;
        for e in &g.edges {
            if let Some(du) = d[e.u].clone() {
                let nd = du.add(&e.w);
                if d[e.v].as_ref().is_none_or(|dv| lt(&nd, dv)) {
                    d[e.v] = Some(nd);
                    prev[e.v] = Some(e.u);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
        if round == n - 1 {
            return Err("the graph has a cycle of negative total weight, so there is no shortest path".into());
        }
    }
    Ok((d, prev))
}

pub fn path_to(prev: &[Option<usize>], s: usize, t: usize) -> Vec<usize> {
    let mut p = vec![t];
    let mut x = t;
    while x != s {
        match prev[x] {
            Some(y) => {
                p.push(y);
                x = y;
            }
            None => return vec![],
        }
    }
    p.reverse();
    p
}

/// All distances (Floyd–Warshall); None = unreachable
pub fn all_pairs(g: &Graph) -> R<Vec<Vec<Option<Number>>>> {
    let n = g.n();
    let mut d = vec![vec![None; n]; n];
    for i in 0..n {
        d[i][i] = Some(Number::zero());
    }
    for e in &g.edges {
        let better = |old: &Option<Number>| old.as_ref().is_none_or(|o| lt(&e.w, o));
        if better(&d[e.u][e.v]) {
            d[e.u][e.v] = Some(e.w.clone());
        }
        if !g.directed && better(&d[e.v][e.u]) {
            d[e.v][e.u] = Some(e.w.clone());
        }
    }
    for k in 0..n {
        for i in 0..n {
            let Some(dik) = d[i][k].clone() else { continue };
            for j in 0..n {
                if let Some(dkj) = &d[k][j] {
                    let nd = dik.add(dkj);
                    if d[i][j].as_ref().is_none_or(|o| lt(&nd, o)) {
                        d[i][j] = Some(nd);
                    }
                }
            }
        }
    }
    if (0..n).any(|i| d[i][i].as_ref().is_some_and(|x| x.is_negative())) {
        return Err("the graph has a cycle of negative total weight".into());
    }
    Ok(d)
}

/// Minimum spanning tree (forest) by Kruskal: the edge indices
pub fn mst(g: &Graph) -> Vec<usize> {
    let mut order: Vec<usize> = (0..g.edges.len()).collect();
    order.sort_by(|&a, &b| g.edges[a].w.cmp_value(&g.edges[b].w).then(a.cmp(&b)));
    let mut parent: Vec<usize> = (0..g.n()).collect();
    fn find(p: &mut [usize], x: usize) -> usize {
        let mut r = x;
        while p[r] != r {
            r = p[r];
        }
        let mut y = x;
        while p[y] != r {
            let z = p[y];
            p[y] = r;
            y = z;
        }
        r
    }
    let mut out = vec![];
    for k in order {
        let (a, b) = (find(&mut parent, g.edges[k].u), find(&mut parent, g.edges[k].v));
        if a != b {
            parent[a] = b;
            out.push(k);
        }
    }
    out.sort();
    out
}

pub struct Flow {
    pub value: Number,
    /// flow on each edge of the graph (for an undirected edge, from u to v; negative = v to u)
    pub on: Vec<Number>,
    /// the vertices on the source side of a minimum cut
    pub source_side: Vec<bool>,
}

/// Maximum flow by shortest augmenting paths (Edmonds–Karp). Undirected edges
/// carry flow either way up to their weight.
pub fn max_flow(g: &Graph, s: usize, t: usize) -> R<Flow> {
    if g.edges.iter().any(|e| e.w.is_negative()) {
        return Err("capacities (edge weights) must not be negative".into());
    }
    let n = g.n();
    // residual arcs: to, capacity, reverse arc, (edge, sign)
    struct Arc {
        to: usize,
        cap: Number,
        rev: usize,
        edge: Option<(usize, bool)>,
    }
    let mut arcs: Vec<Vec<Arc>> = (0..n).map(|_| vec![]).collect();
    for (k, e) in g.edges.iter().enumerate() {
        let (iu, iv) = (arcs[e.u].len(), arcs[e.v].len());
        let back = if g.directed { Number::zero() } else { e.w.clone() };
        arcs[e.u].push(Arc { to: e.v, cap: e.w.clone(), rev: iv, edge: Some((k, true)) });
        arcs[e.v].push(Arc { to: e.u, cap: back, rev: iu, edge: Some((k, false)) });
    }
    let mut value = Number::zero();
    let mut on = vec![Number::zero(); g.edges.len()];
    let mut rounds = 0;
    loop {
        rounds += 1;
        if rounds > 100_000 {
            return Err("maxflow: too many augmenting paths".into());
        }
        let mut prev: Vec<Option<(usize, usize)>> = vec![None; n];
        let mut seen = vec![false; n];
        seen[s] = true;
        let mut q = VecDeque::from([s]);
        while let Some(u) = q.pop_front() {
            for (i, a) in arcs[u].iter().enumerate() {
                if !seen[a.to] && !a.cap.is_zero() && !a.cap.is_negative() {
                    seen[a.to] = true;
                    prev[a.to] = Some((u, i));
                    q.push_back(a.to);
                }
            }
        }
        if !seen[t] {
            return Ok(Flow { value, on, source_side: seen });
        }
        let mut b: Option<Number> = None;
        let mut x = t;
        while let Some((u, i)) = prev[x] {
            let c = &arcs[u][i].cap;
            if b.as_ref().is_none_or(|bb| lt(c, bb)) {
                b = Some(c.clone());
            }
            x = u;
        }
        let b = b.unwrap();
        let mut x = t;
        while let Some((u, i)) = prev[x] {
            let (to, rev, edge) = (arcs[u][i].to, arcs[u][i].rev, arcs[u][i].edge);
            arcs[u][i].cap = arcs[u][i].cap.sub(&b);
            arcs[to][rev].cap = arcs[to][rev].cap.add(&b);
            if let Some((k, fwd)) = edge {
                on[k] = if fwd { on[k].add(&b) } else { on[k].sub(&b) };
            }
            x = u;
        }
        value = value.add(&b);
    }
}

/// Maximum matching in a general graph (Edmonds' blossom algorithm): mate of each vertex
pub fn matching(g: &Graph) -> Vec<Option<usize>> {
    let n = g.n();
    let a = g.adj_undirected();
    let mut mate: Vec<Option<usize>> = vec![None; n];
    // greedy start
    for u in 0..n {
        if mate[u].is_none() {
            if let Some(&v) = a[u].iter().find(|&&v| mate[v].is_none()) {
                mate[u] = Some(v);
                mate[v] = Some(u);
            }
        }
    }
    for root in 0..n {
        if mate[root].is_some() {
            continue;
        }
        let mut used = vec![false; n];
        let mut p: Vec<Option<usize>> = vec![None; n];
        let mut base: Vec<usize> = (0..n).collect();
        used[root] = true;
        let mut q = VecDeque::from([root]);
        let mut found = None;
        'bfs: while let Some(v) = q.pop_front() {
            for &to in &a[v] {
                if base[v] == base[to] || mate[v] == Some(to) {
                    continue;
                }
                if to == root || mate[to].is_some_and(|m| p[m].is_some()) {
                    // a blossom
                    let cur = lca(&mate, &base, &p, v, to);
                    let mut blossom = vec![false; n];
                    mark_path(&mate, &mut base, &mut p, &mut blossom, v, cur, to);
                    mark_path(&mate, &mut base, &mut p, &mut blossom, to, cur, v);
                    for i in 0..n {
                        if blossom[base[i]] {
                            base[i] = cur;
                            if !used[i] {
                                used[i] = true;
                                q.push_back(i);
                            }
                        }
                    }
                } else if p[to].is_none() {
                    p[to] = Some(v);
                    match mate[to] {
                        None => {
                            found = Some(to);
                            break 'bfs;
                        }
                        Some(m) => {
                            used[m] = true;
                            q.push_back(m);
                        }
                    }
                }
            }
        }
        // augment
        let mut v = found;
        while let Some(x) = v {
            let pv = p[x].unwrap();
            let ppv = mate[pv];
            mate[x] = Some(pv);
            mate[pv] = Some(x);
            v = ppv;
        }
    }
    mate
}

fn lca(mate: &[Option<usize>], base: &[usize], p: &[Option<usize>], a: usize, b: usize) -> usize {
    let mut used = vec![false; mate.len()];
    let mut a = a;
    loop {
        a = base[a];
        used[a] = true;
        match mate[a] {
            None => break,
            Some(m) => match p[m] {
                Some(x) => a = x,
                None => break,
            },
        }
    }
    let mut b = b;
    loop {
        b = base[b];
        if used[b] {
            return b;
        }
        b = p[mate[b].unwrap()].unwrap();
    }
}

fn mark_path(
    mate: &[Option<usize>],
    base: &mut [usize],
    p: &mut [Option<usize>],
    blossom: &mut [bool],
    v: usize,
    b: usize,
    child: usize,
) {
    let mut v = v;
    let mut child = child;
    while base[v] != b {
        let m = mate[v].unwrap();
        blossom[base[v]] = true;
        blossom[base[m]] = true;
        p[v] = Some(child);
        child = m;
        v = p[m].unwrap();
    }
}

/// Greedy DSATUR coloring
fn dsatur(a: &[Vec<usize>]) -> Vec<usize> {
    let n = a.len();
    let mut col = vec![usize::MAX; n];
    for _ in 0..n {
        let mut best = None;
        let mut key = (0, 0);
        for u in 0..n {
            if col[u] != usize::MAX {
                continue;
            }
            let mut sat: Vec<usize> = a[u].iter().map(|&v| col[v]).filter(|&c| c != usize::MAX).collect();
            sat.sort();
            sat.dedup();
            let k = (sat.len(), a[u].len());
            if best.is_none() || k > key {
                best = Some(u);
                key = k;
            }
        }
        let u = best.unwrap();
        let mut c = 0;
        while a[u].iter().any(|&v| col[v] == c) {
            c += 1;
        }
        col[u] = c;
    }
    col
}

/// A coloring with the fewest colors (colors 0, 1, ...), and whether it is
/// certainly minimal (the exact search has a budget)
pub fn coloring(g: &Graph) -> (Vec<usize>, bool) {
    let a = g.adj_undirected();
    let n = a.len();
    if n == 0 {
        return (vec![], true);
    }
    let greedy = dsatur(&a);
    let k_greedy = greedy.iter().max().unwrap() + 1;
    // lower bound: a greedy clique
    let mut lower = 1;
    for s in 0..n {
        let mut clique = vec![s];
        let mut cand: Vec<usize> = a[s].clone();
        cand.sort_by_key(|&v| std::cmp::Reverse(a[v].len()));
        for v in cand {
            if clique.iter().all(|&c| a[v].contains(&c)) {
                clique.push(v);
            }
        }
        lower = lower.max(clique.len());
    }
    if lower >= k_greedy {
        return (greedy, true);
    }
    // order: DSATUR order of the greedy run is good; use degree order
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by_key(|&v| std::cmp::Reverse(a[v].len()));
    let mut budget: u64 = 2_000_000;
    let mut best = greedy;
    let mut exact = true;
    for k in lower..k_greedy {
        let mut col = vec![usize::MAX; n];
        match color_k(&a, &order, 0, k, &mut col, &mut budget) {
            Some(true) => {
                best = col;
                break;
            }
            Some(false) => {}
            None => {
                exact = false;
                break;
            }
        }
    }
    (best, exact)
}

fn color_k(a: &[Vec<usize>], order: &[usize], i: usize, k: usize, col: &mut [usize], budget: &mut u64) -> Option<bool> {
    if i == order.len() {
        return Some(true);
    }
    if *budget == 0 {
        return None;
    }
    *budget -= 1;
    let u = order[i];
    // symmetry: never use a color more than one above the largest used so far
    let maxc = order[..i].iter().map(|&v| col[v]).max().map_or(0, |m| m + 1);
    for c in 0..k.min(maxc + 1) {
        if a[u].iter().all(|&v| col[v] != c) {
            col[u] = c;
            match color_k(a, order, i + 1, k, col, budget) {
                Some(true) => return Some(true),
                Some(false) => {}
                None => return None,
            }
            col[u] = usize::MAX;
        }
    }
    Some(false)
}

/// Coefficients (constant first) of the chromatic polynomial, by deletion–contraction
pub fn chromatic_poly(g: &Graph) -> R<Vec<i128>> {
    let n = g.n();
    let mut es: Vec<(usize, usize)> = g
        .edges
        .iter()
        .map(|e| if e.u < e.v { (e.u, e.v) } else { (e.v, e.u) })
        .collect();
    es.sort();
    es.dedup();
    if es.len() > 45 {
        return Err(format!(
            "chromaticpoly: {} edges is too many for deletion–contraction (at most 45)",
            es.len()
        ));
    }
    let mut memo = HashMap::new();
    let mut budget: u64 = 3_000_000;
    let p = cp(n, es, &mut memo, &mut budget).ok_or("chromaticpoly: the graph is too large to finish")?;
    Ok(p)
}

fn cp(n: usize, es: Vec<(usize, usize)>, memo: &mut HashMap<(usize, Vec<(usize, usize)>), Vec<i128>>, budget: &mut u64) -> Option<Vec<i128>> {
    if *budget == 0 {
        return None;
    }
    *budget -= 1;
    if es.is_empty() {
        let mut p = vec![0; n + 1];
        p[n] = 1;
        return Some(p);
    }
    // relabel the vertices that have edges to 0..m, isolated ones multiply by k
    let mut used: Vec<usize> = es.iter().flat_map(|&(a, b)| [a, b]).collect();
    used.sort();
    used.dedup();
    let iso = n - used.len();
    let at = |x: usize| used.binary_search(&x).unwrap();
    let mut es2: Vec<(usize, usize)> = es.iter().map(|&(a, b)| (at(a), at(b))).collect();
    es2.sort();
    let m = used.len();
    let key = (m, es2.clone());
    let base = if let Some(p) = memo.get(&key) {
        p.clone()
    } else {
        // a tree on m vertices: k (k - 1)^(m - 1)
        let p = if es2.len() == m - 1 && connected_edges(m, &es2) {
            // k (k-1)^(m-1)
            let mut p = vec![0i128, 1];
            for _ in 0..m - 1 {
                let mut q = vec![0i128; p.len() + 1];
                for (i, &c) in p.iter().enumerate() {
                    q[i + 1] += c;
                    q[i] -= c;
                }
                p = q;
            }
            p
        } else {
            let (u, v) = *es2.last().unwrap();
            let del: Vec<(usize, usize)> = es2[..es2.len() - 1].to_vec();
            // contract v into u
            let mut con: Vec<(usize, usize)> = del
                .iter()
                .map(|&(a, b)| {
                    let a = if a == v { u } else { a };
                    let b = if b == v { u } else { b };
                    if a < b { (a, b) } else { (b, a) }
                })
                .filter(|&(a, b)| a != b)
                .collect();
            con.sort();
            con.dedup();
            // v is now isolated in con: drop it by counting m - 1 vertices
            let con: Vec<(usize, usize)> = con
                .into_iter()
                .map(|(a, b)| (if a > v { a - 1 } else { a }, if b > v { b - 1 } else { b }))
                .collect();
            let pd = cp(m, del, memo, budget)?;
            let pc = cp(m - 1, con, memo, budget)?;
            let mut p = pd;
            for (i, c) in pc.iter().enumerate() {
                p[i] -= c;
            }
            p
        };
        memo.insert(key, p.clone());
        p
    };
    // times k^iso
    let mut p = vec![0; iso];
    p.extend(base);
    p.resize(n + 1, 0);
    Some(p)
}

fn connected_edges(m: usize, es: &[(usize, usize)]) -> bool {
    let mut parent: Vec<usize> = (0..m).collect();
    fn find(p: &mut Vec<usize>, x: usize) -> usize {
        if p[x] != x {
            let r = find(p, p[x]);
            p[x] = r;
        }
        p[x]
    }
    let mut c = m;
    for &(a, b) in es {
        let (x, y) = (find(&mut parent, a), find(&mut parent, b));
        if x != y {
            parent[x] = y;
            c -= 1;
        }
    }
    c == 1
}

/// An Euler circuit (or trail) using every edge once, if there is one
pub fn euler(g: &Graph) -> Option<Vec<usize>> {
    let n = g.n();
    if g.edges.is_empty() {
        return Some(if n > 0 { vec![0] } else { vec![] });
    }
    // the vertices with edges must be connected (ignoring direction)
    let a = g.adj_undirected();
    let start_any = g.edges[0].u;
    let mut seen = vec![false; n];
    let mut st = vec![start_any];
    seen[start_any] = true;
    while let Some(u) = st.pop() {
        for &v in &a[u] {
            if !seen[v] {
                seen[v] = true;
                st.push(v);
            }
        }
    }
    if g.edges.iter().any(|e| !seen[e.u] || !seen[e.v]) {
        return None;
    }
    let start = if g.directed {
        let (o, i) = (g.out_degrees(), g.in_degrees());
        let plus: Vec<usize> = (0..n).filter(|&v| o[v] == i[v] + 1).collect();
        let minus: Vec<usize> = (0..n).filter(|&v| i[v] == o[v] + 1).collect();
        let bad = (0..n).any(|v| o[v].abs_diff(i[v]) > 1);
        if bad || plus.len() > 1 || minus.len() != plus.len() {
            return None;
        }
        plus.first().copied().unwrap_or(start_any)
    } else {
        let d = g.degrees();
        let odd: Vec<usize> = (0..n).filter(|&v| d[v] % 2 == 1).collect();
        if odd.len() > 2 {
            return None;
        }
        odd.first().copied().unwrap_or(start_any)
    };
    // Hierholzer, smaller neighbours first
    let mut adj: Vec<Vec<(usize, usize)>> = g.adj().into_iter().map(|l| l.into_iter().map(|(v, _, k)| (v, k)).collect()).collect();
    for l in adj.iter_mut() {
        l.sort();
        l.reverse();
    }
    let mut used = vec![false; g.edges.len()];
    let mut stack = vec![start];
    let mut out = vec![];
    while let Some(&u) = stack.last() {
        while adj[u].last().is_some_and(|&(_, k)| used[k]) {
            adj[u].pop();
        }
        match adj[u].pop() {
            Some((v, k)) => {
                used[k] = true;
                stack.push(v);
            }
            None => {
                out.push(stack.pop().unwrap());
            }
        }
    }
    out.reverse();
    Some(out)
}

/// A Hamiltonian cycle through every vertex once (back to the start), if there is one.
/// Err when the graph is too large to decide.
pub fn hamiltonian(g: &Graph) -> R<Option<Vec<usize>>> {
    let n = g.n();
    if n == 0 {
        return Ok(None);
    }
    if n == 1 {
        return Ok(Some(vec![0, 0]));
    }
    if n > 22 {
        return Err(format!("hamiltonian: {} vertices is too many to decide (at most 22)", n));
    }
    let adj = |u: usize, v: usize| {
        g.edges.iter().any(|e| (e.u == u && e.v == v) || (!g.directed && e.u == v && e.v == u))
    };
    let mut m = vec![0u32; n];
    for u in 0..n {
        for v in 0..n {
            if u != v && adj(u, v) {
                m[u] |= 1 << v;
            }
        }
    }
    // dp[mask][v]: a path from 0 through mask ending at v (bitset of v per mask)
    let full = 1usize << n;
    let mut dp = vec![0u32; full];
    dp[1] = 1;
    for mask in 1..full {
        if mask & 1 == 0 {
            continue;
        }
        let ends = dp[mask];
        if ends == 0 {
            continue;
        }
        for v in 0..n {
            if ends >> v & 1 == 1 {
                let mut nxt = m[v] & !(mask as u32);
                while nxt != 0 {
                    let w = nxt.trailing_zeros() as usize;
                    nxt &= nxt - 1;
                    dp[mask | 1 << w] |= 1 << w;
                }
            }
        }
    }
    let last = full - 1;
    let Some(end) = (0..n).find(|&v| dp[last] >> v & 1 == 1 && m[v] & 1 == 1) else {
        return Ok(None);
    };
    // walk back
    let mut path = vec![end];
    let mut mask = last;
    let mut v = end;
    while v != 0 {
        let prev_mask = mask & !(1 << v);
        let u = (0..n)
            .find(|&u| dp[prev_mask] >> u & 1 == 1 && m[u] >> v & 1 == 1)
            .unwrap();
        path.push(u);
        mask = prev_mask;
        v = u;
    }
    path.reverse();
    path.push(0);
    // go round the way that starts with the smaller neighbour
    if path.len() > 3 && path[1] > path[path.len() - 2] {
        path.reverse();
    }
    Ok(Some(path))
}

/// Topological order of a digraph, or None if it has a cycle
pub fn toposort(g: &Graph) -> Option<Vec<usize>> {
    let mut indeg = g.in_degrees();
    let a = g.adj();
    let mut ready: Vec<usize> = (0..g.n()).filter(|&v| indeg[v] == 0).collect();
    let mut out = vec![];
    while !ready.is_empty() {
        ready.sort();
        let u = ready.remove(0);
        out.push(u);
        for (v, _, _) in &a[u] {
            indeg[*v] -= 1;
            if indeg[*v] == 0 {
                ready.push(*v);
            }
        }
    }
    (out.len() == g.n()).then_some(out)
}

/// PageRank with damping d (power iteration)
pub fn pagerank(g: &Graph, d: f64) -> Vec<f64> {
    let n = g.n();
    if n == 0 {
        return vec![];
    }
    let a = g.adj();
    let mut r = vec![1.0 / n as f64; n];
    for _ in 0..1000 {
        let mut nr = vec![(1.0 - d) / n as f64; n];
        let mut dangling = 0.0;
        for u in 0..n {
            if a[u].is_empty() {
                dangling += r[u];
            } else {
                let share = r[u] / a[u].len() as f64;
                for (v, _, _) in &a[u] {
                    nr[*v] += d * share;
                }
            }
        }
        for x in nr.iter_mut() {
            *x += d * dangling / n as f64;
        }
        let diff: f64 = nr.iter().zip(&r).map(|(a, b)| (a - b).abs()).sum();
        r = nr;
        if diff < 1e-13 {
            break;
        }
    }
    r
}

/// Betweenness centrality (Brandes), with weights when the graph has them.
/// For an undirected graph each pair counts once.
pub fn betweenness(g: &Graph) -> Vec<f64> {
    let n = g.n();
    let a = g.adj();
    let mut cb = vec![0.0; n];
    for s in 0..n {
        let mut stack = vec![];
        let mut preds: Vec<Vec<usize>> = vec![vec![]; n];
        let mut sigma = vec![0.0f64; n];
        sigma[s] = 1.0;
        let mut dist = vec![f64::INFINITY; n];
        dist[s] = 0.0;
        let mut done = vec![false; n];
        loop {
            let mut best = None;
            for u in 0..n {
                if !done[u] && dist[u].is_finite() && best.is_none_or(|b: usize| dist[u] < dist[b]) {
                    best = Some(u);
                }
            }
            let Some(u) = best else { break };
            done[u] = true;
            stack.push(u);
            for (v, w, _) in &a[u] {
                let w = if g.weighted { w.to_f64() } else { 1.0 };
                let nd = dist[u] + w;
                if nd < dist[*v] - 1e-12 {
                    dist[*v] = nd;
                    sigma[*v] = sigma[u];
                    preds[*v] = vec![u];
                } else if (nd - dist[*v]).abs() <= 1e-12 {
                    sigma[*v] += sigma[u];
                    preds[*v].push(u);
                }
            }
        }
        let mut delta = vec![0.0; n];
        while let Some(w) = stack.pop() {
            for &v in &preds[w] {
                delta[v] += sigma[v] / sigma[w] * (1.0 + delta[w]);
            }
            if w != s {
                cb[w] += delta[w];
            }
        }
    }
    if !g.directed {
        for x in cb.iter_mut() {
            *x /= 2.0;
        }
    }
    cb
}

/// Closeness: (number of others reached) / (sum of distances to them)
pub fn closeness(g: &Graph) -> R<Vec<f64>> {
    let d = all_pairs(g)?;
    Ok((0..g.n())
        .map(|i| {
            let (mut sum, mut k) = (0.0, 0);
            for j in 0..g.n() {
                if j != i {
                    if let Some(x) = &d[i][j] {
                        sum += x.to_f64();
                        k += 1;
                    }
                }
            }
            if sum > 0.0 { k as f64 / sum } else { 0.0 }
        })
        .collect())
}

/// Positions for drawing: the graph's own layout, else layered for a tree,
/// a circle for a cycle-like or small dense graph, else a spring layout
pub fn layout(g: &Graph) -> Vec<(f64, f64)> {
    if let Some(p) = &g.pos {
        return p.clone();
    }
    let n = g.n();
    let circle = |n: usize| -> Vec<(f64, f64)> {
        (0..n)
            .map(|k| {
                let t = std::f64::consts::TAU * k as f64 / n.max(1) as f64 + std::f64::consts::FRAC_PI_2;
                (t.cos(), t.sin())
            })
            .collect()
    };
    if n <= 2 {
        return (0..n).map(|k| (k as f64, 0.0)).collect();
    }
    let a = g.adj_undirected();
    let m = a.iter().map(|l| l.len()).sum::<usize>() / 2;
    // a directed graph without cycles: layers by longest path from a source, arrows pointing down
    if g.directed && n <= 200 {
        if let Some(order) = toposort(g) {
            let mut level = vec![0usize; n];
            let a = g.adj();
            for &u in &order {
                for (v, _, _) in &a[u] {
                    level[*v] = level[*v].max(level[u] + 1);
                }
            }
            let depth = *level.iter().max().unwrap();
            if depth > 0 {
                // within a layer, order by the average position of the parents (one pass)
                let mut pos = vec![(0.0, 0.0); n];
                let width = (0..=depth).map(|l| level.iter().filter(|&&x| x == l).count()).max().unwrap() as f64;
                let mut parents: Vec<Vec<usize>> = vec![vec![]; n];
                for e in &g.edges {
                    parents[e.v].push(e.u);
                }
                for l in 0..=depth {
                    let mut layer: Vec<usize> = order.iter().copied().filter(|&v| level[v] == l).collect();
                    let key = |v: usize, pos: &Vec<(f64, f64)>| {
                        if parents[v].is_empty() {
                            v as f64 / n as f64
                        } else {
                            parents[v].iter().map(|&p| pos[p].0).sum::<f64>() / parents[v].len() as f64
                        }
                    };
                    layer.sort_by(|&a, &b| key(a, &pos).partial_cmp(&key(b, &pos)).unwrap_or(std::cmp::Ordering::Equal));
                    let k = layer.len() as f64;
                    for (j, &v) in layer.iter().enumerate() {
                        pos[v] = ((j as f64 + 0.5) * width / k, -(l as f64) * 0.8);
                    }
                }
                return pos;
            }
        }
    }
    let comps = components(&Graph { directed: false, ..g.clone() });
    // forest: a tidy tree drawing (leaves side by side, parents above the middle of their children)
    if m + comps.len() == n {
        let mut pos = vec![(0.0, 0.0); n];
        let mut seen = vec![false; n];
        let mut next_x = 0.0;
        for c in &comps {
            let root = c[0];
            // children lists by BFS from the root
            let mut kids: Vec<Vec<usize>> = vec![vec![]; n];
            let mut order = vec![root];
            seen[root] = true;
            let mut k = 0;
            while k < order.len() {
                let u = order[k];
                for &v in &a[u] {
                    if !seen[v] {
                        seen[v] = true;
                        kids[u].push(v);
                        order.push(v);
                    }
                }
                k += 1;
            }
            fn place(u: usize, depth: f64, kids: &[Vec<usize>], pos: &mut [(f64, f64)], next_x: &mut f64) {
                if kids[u].is_empty() {
                    pos[u] = (*next_x, -depth);
                    *next_x += 1.0;
                    return;
                }
                for &v in &kids[u] {
                    place(v, depth + 1.0, kids, pos, next_x);
                }
                let xs: Vec<f64> = kids[u].iter().map(|&v| pos[v].0).collect();
                pos[u] = ((xs[0] + xs[xs.len() - 1]) / 2.0, -depth);
            }
            place(root, 0.0, &kids, &mut pos, &mut next_x);
            next_x += 0.5;
        }
        return pos;
    }
    if comps.len() == 1 {
        // every vertex of degree 2: a cycle; nearly complete: a circle shows it best
        if a.iter().all(|l| l.len() == 2) || m * 3 > n * (n - 1) {
            return circle(n);
        }
        return spring(&a, circle(n));
    }
    // several components: lay out each, largest first, side by side
    let mut cs = comps.clone();
    cs.sort_by_key(|c| std::cmp::Reverse(c.len()));
    let mut pos = vec![(0.0, 0.0); n];
    let mut x0 = 0.0;
    let mut singles = vec![];
    for c in &cs {
        if c.len() == 1 {
            singles.push(c[0]);
            continue;
        }
        let sub = g.induced(c);
        let mut sub = Graph { directed: false, ..sub };
        sub.pos = None;
        let p = layout(&sub);
        let (mut lo, mut hi, mut ylo, mut yhi) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
        for &(x, y) in &p {
            lo = lo.min(x);
            hi = hi.max(x);
            ylo = ylo.min(y);
            yhi = yhi.max(y);
        }
        // scale each part so its size grows with the square root of its vertex count
        let size = (hi - lo).max(yhi - ylo).max(1e-9);
        let want = (c.len() as f64).sqrt();
        for (k, &v) in c.iter().enumerate() {
            pos[v] = (x0 + (p[k].0 - lo) / size * want, (p[k].1 - (ylo + yhi) / 2.0) / size * want);
        }
        x0 += (hi - lo) / size * want + 1.0;
    }
    // isolated vertices in a column on the right
    let rows = (singles.len() as f64).sqrt().ceil().max(1.0) as usize;
    for (k, &v) in singles.iter().enumerate() {
        pos[v] = (x0 + (k / rows) as f64 * 0.8, -((k % rows) as f64) * 0.8);
    }
    pos
}

/// Fruchterman–Reingold from a circle, deterministic
fn spring(a: &[Vec<usize>], start: Vec<(f64, f64)>) -> Vec<(f64, f64)> {
    let n = a.len();
    let mut p = start;
    // break the symmetry of the circle a little, the same way every time
    for (k, q) in p.iter_mut().enumerate() {
        let j = ((k * 7919) % 101) as f64 / 101.0 - 0.5;
        q.0 += 0.05 * j;
        q.1 -= 0.05 * j;
    }
    let area = 4.0;
    let kk = (area / n as f64).sqrt();
    let mut temp = 0.3;
    for _ in 0..400 {
        let mut disp = vec![(0.0, 0.0); n];
        for i in 0..n {
            for j in 0..n {
                if i == j {
                    continue;
                }
                let (dx, dy) = (p[i].0 - p[j].0, p[i].1 - p[j].1);
                let d = (dx * dx + dy * dy).sqrt().max(1e-6);
                let f = kk * kk / d;
                disp[i].0 += dx / d * f;
                disp[i].1 += dy / d * f;
            }
            for &j in &a[i] {
                let (dx, dy) = (p[i].0 - p[j].0, p[i].1 - p[j].1);
                let d = (dx * dx + dy * dy).sqrt().max(1e-6);
                let f = d * d / kk;
                disp[i].0 -= dx / d * f;
                disp[i].1 -= dy / d * f;
            }
        }
        for i in 0..n {
            let (dx, dy) = disp[i];
            let d = (dx * dx + dy * dy).sqrt().max(1e-9);
            let s = d.min(temp);
            p[i].0 += dx / d * s;
            p[i].1 += dy / d * s;
        }
        temp *= 0.985;
    }
    p
}

const COLORS: &[&str] = &[
    "#2563eb", "#dc2626", "#16a34a", "#d97706", "#7c3aed", "#0891b2", "#db2777", "#65a30d", "#475569", "#ea580c",
];

fn esc(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;").replace('"', "&quot;")
}

impl Graph {
    /// A node-link drawing
    pub fn svg(&self) -> String {
        let n = self.n();
        let pos = layout(self);
        let (mut x0, mut x1, mut y0, mut y1) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
        for &(x, y) in &pos {
            x0 = x0.min(x);
            x1 = x1.max(x);
            y0 = y0.min(y);
            y1 = y1.max(y);
        }
        if n == 0 {
            (x0, x1, y0, y1) = (0.0, 1.0, 0.0, 1.0);
        }
        let (w, h) = ((x1 - x0).max(1e-9), (y1 - y0).max(1e-9));
        let text_of = |i: usize| match &self.labels[i] {
            Expr::Str(st) => st.clone(),
            _ => self.label(i),
        };
        let longest = (0..n).map(|i| text_of(i).chars().count()).max().unwrap_or(1);
        let big = if longest > 3 { 560.0 } else { 420.0 };
        let scale = if w >= h { big / w } else { big / h };
        let scale = scale.min(if n <= 2 { 120.0 } else { f64::MAX });
        let pad = if longest > 3 { 12.0 + 3.4 * longest.min(16) as f64 } else { 28.0 };
        let (pw, ph) = (w * scale + 2.0 * pad, h * scale + 2.0 * pad);
        let (pw, ph) = (pw.max(2.0 * pad + 40.0), ph.max(2.0 * pad + 40.0));
        let cx = |x: f64| pad + (x - x0) * scale + (pw - 2.0 * pad - w * scale).max(0.0) / 2.0;
        let cy = |y: f64| pad + (y1 - y) * scale + (ph - 2.0 * pad - h * scale).max(0.0) / 2.0;
        let labels = n <= 60;
        let r = if labels { 11.0 } else { 3.5 };
        let mut s = String::new();
        let _ = write!(
            s,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="{pw:.0}" height="{ph:.0}" viewBox="0 0 {pw:.0} {ph:.0}" font-family="sans-serif" font-size="11">"#
        );
        s.push_str(r#"<rect width="100%" height="100%" fill="white"/>"#);
        if self.directed {
            s.push_str(r##"<defs><marker id="arr" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0L10,5L0,10z" fill="#4b5563"/></marker><marker id="arrm" viewBox="0 0 10 10" refX="10" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0,0L10,5L0,10z" fill="#d97706"/></marker></defs>"##);
        }
        let marked = |u: usize, v: usize| {
            self.marks
                .edges
                .iter()
                .any(|&(a, b)| (a == u && b == v) || (!self.directed && a == v && b == u))
        };
        // how far from its centre an edge leaves a vertex: a circle, or the box around a name
        let reach = |i: usize, ux: f64, uy: f64| -> f64 {
            let len = text_of(i).chars().count();
            if labels && len > 2 {
                let hw = (6.4 * len as f64 + 12.0) / 2.0;
                let (tx, ty) = (hw / ux.abs().max(1e-9), 10.0 / uy.abs().max(1e-9));
                tx.min(ty)
            } else {
                r
            }
        };
        for e in &self.edges {
            let (ax, ay, bx, by) = (cx(pos[e.u].0), cy(pos[e.u].1), cx(pos[e.v].0), cy(pos[e.v].1));
            let (dx, dy) = (bx - ax, by - ay);
            let l = (dx * dx + dy * dy).sqrt().max(1e-9);
            let (ux, uy) = (dx / l, dy / l);
            // stop at the circles; a digraph edge both ways bends apart
            let both = self.directed && self.has_edge(e.v, e.u);
            let off = if both { 4.0 } else { 0.0 };
            let (ox, oy) = (-uy * off, ux * off);
            let (ra, rb) = (reach(e.u, ux, uy), reach(e.v, ux, uy));
            let (sx, sy) = (ax + ux * ra + ox, ay + uy * ra + oy);
            let (tx, ty) = (bx - ux * (rb + 1.0) + ox, by - uy * (rb + 1.0) + oy);
            let m = marked(e.u, e.v);
            let (col, width) = if m { ("#d97706", "3.5") } else { ("#9ca3af", "1.6") };
            let marker = if self.directed {
                if m { r#" marker-end="url(#arrm)""# } else { r#" marker-end="url(#arr)""# }
            } else {
                ""
            };
            let _ = write!(
                s,
                r#"<line x1="{sx:.1}" y1="{sy:.1}" x2="{tx:.1}" y2="{ty:.1}" stroke="{col}" stroke-width="{width}"{marker}/>"#
            );
            if self.weighted && self.edges.len() <= 80 {
                let (mx, my) = ((sx + tx) / 2.0 + ox * 2.0, (sy + ty) / 2.0 + oy * 2.0);
                let _ = write!(
                    s,
                    r##"<text x="{mx:.1}" y="{my:.1}" dy="4" text-anchor="middle" fill="#374151" paint-order="stroke" stroke="white" stroke-width="4">{}</text>"##,
                    esc(&crate::print::show(&Expr::Num(e.w.clone())))
                );
            }
        }
        for (i, &(x, y)) in pos.iter().enumerate() {
            let fill = match self.marks.colors.get(i) {
                Some(&c) => COLORS[c % COLORS.len()],
                None => "white",
            };
            let text = if self.marks.colors.get(i).is_some() { "white" } else { "#111827" };
            let t = text_of(i);
            let len = t.chars().count();
            if labels && len > 2 {
                // a name: a rounded box that fits it
                let bw = 6.4 * len as f64 + 12.0;
                let _ = write!(
                    s,
                    r##"<rect x="{:.1}" y="{:.1}" width="{bw:.1}" height="20" rx="10" fill="{fill}" stroke="#374151" stroke-width="1.3"/>"##,
                    cx(x) - bw / 2.0,
                    cy(y) - 10.0
                );
            } else {
                let _ = write!(
                    s,
                    r##"<circle cx="{:.1}" cy="{:.1}" r="{r}" fill="{fill}" stroke="#374151" stroke-width="1.3"/>"##,
                    cx(x),
                    cy(y)
                );
            }
            if labels {
                let fs = 11;
                let _ = write!(
                    s,
                    r#"<text x="{:.1}" y="{:.1}" dy="4" text-anchor="middle" font-size="{fs}" fill="{text}">{}</text>"#,
                    cx(x),
                    cy(y),
                    esc(&t)
                );
            }
        }
        s.push_str("</svg>");
        s
    }
}
