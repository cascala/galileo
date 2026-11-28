//! The language around the math: blocks (for, while, if, function), lists,
//! records, strings, and assignments to parts of values (`x(k) = v`,
//! `A[i, j] = v`, `p.mass = v`).
//!
//! Statements inside a block show their results as they run, like typed
//! statements: each result is queued in [`Session::emitted`] and the caller
//! (`run_all`, `load_file`) hands them out in order before the block's own
//! (empty) result. `print` queues text the same way.

use crate::eval::{Note, Output, Session};
use crate::expr::{Expr, Obj};
use crate::parser::{Block, Node, Op, Stmt};
use crate::print::{show, show_value};
use std::rc::Rc;

type R<T> = Result<T, String>;

/// How a statement finished
pub(crate) enum Flow {
    Normal,
    Break,
    Continue,
    Return(Option<Expr>),
}

/// A function defined with `function ... end`
#[derive(Clone, Debug)]
pub struct Proc {
    pub params: Vec<String>,
    pub out: Option<String>,
    pub body: Rc<Block>,
}

/// Loops stop after this many rounds (an endless while loop ends with a message)
const MAX_ROUNDS: u64 = 100_000_000;

pub fn list(v: Vec<Expr>) -> Expr {
    Expr::Obj(Rc::new(Obj::List(v)))
}

pub fn record(v: Vec<(String, Expr)>) -> Expr {
    Expr::Obj(Rc::new(Obj::Record(v)))
}

pub fn as_list(e: &Expr) -> Option<&Vec<Expr>> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::List(v) => Some(v),
            _ => None,
        },
        _ => None,
    }
}

pub fn as_record(e: &Expr) -> Option<&Vec<(String, Expr)>> {
    match e {
        Expr::Obj(o) => match &**o {
            Obj::Record(v) => Some(v),
            _ => None,
        },
        _ => None,
    }
}

/// {1, "a", x}
pub fn list_text(v: &[Expr]) -> String {
    let items: Vec<String> = v.iter().map(show_value).collect();
    if items.iter().any(|s| s.contains('\n')) {
        // matrices with several rows: one item per line, columns aligned
        let items: Vec<String> = items.iter().map(|s| s.replace('\n', "\n ")).collect();
        return format!("{{{}}}", items.join(",\n "));
    }
    format!("{{{}}}", items.join(", "))
}

/// {mass: 3, name: "ball"}
pub fn record_text(v: &[(String, Expr)]) -> String {
    format!(
        "{{{}}}",
        v.iter()
            .map(|(k, x)| format!("{}: {}", k, show_value(x)))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn whole(v: &Expr, what: &str) -> R<i128> {
    let n = v
        .num()
        .ok_or_else(|| format!("{}: positions are whole numbers, got {}", what, show(v)))?;
    if n.is_integer() {
        if let Some(k) = n.as_int() {
            return Ok(k);
        }
        if !n.is_exact() {
            return Ok(n.to_f64() as i128);
        }
    }
    Err(format!(
        "{}: positions are whole numbers, got {}",
        what,
        show(v)
    ))
}

/// (rows, columns) of a value that can be indexed; a list or string is one row
fn dims_of(v: &Expr) -> (usize, usize) {
    match v {
        Expr::Matrix(m) => (m.len(), m.first().map_or(0, |r| r.len())),
        Expr::Str(s) => (1, s.chars().count()),
        _ => match as_list(v) {
            Some(l) => (1, l.len()),
            None => (1, 1),
        },
    }
}

fn element_ok(v: &Expr) -> bool {
    v.is_scalar() || matches!(v, Expr::Bool(_) | Expr::Str(_))
}

impl Session {
    /// for, while, if, function, break, continue, return at the top level
    pub(crate) fn exec_block_stmt(&mut self, s: &Stmt) -> R<Output> {
        match self.run_stmt(s, true)? {
            Flow::Normal => Ok(Output::Nothing),
            Flow::Break => Err("break is only allowed inside a loop".into()),
            Flow::Continue => Err("continue is only allowed inside a loop".into()),
            Flow::Return(_) => Err("return is only allowed inside a function".into()),
        }
    }

    fn emit(&mut self, o: Output) {
        if matches!(o, Output::Nothing) {
            return;
        }
        let notes = if self.in_proc > 0 || self.in_loop > 0 {
            vec![]
        } else {
            std::mem::take(&mut self.notes)
        };
        self.notes.clear();
        self.emitted.push((o, notes));
    }

    /// queue text to show (print)
    pub(crate) fn emit_text(&mut self, t: String) {
        self.emitted.push((Output::Text(t), vec![]));
    }

    pub(crate) fn run_block(&mut self, b: &Block) -> R<Flow> {
        for (s, quiet) in b {
            match self.run_stmt(s, *quiet)? {
                Flow::Normal => {}
                f => return Ok(f),
            }
        }
        Ok(Flow::Normal)
    }

    fn run_stmt(&mut self, s: &Stmt, quiet: bool) -> R<Flow> {
        match s {
            Stmt::For(var, vals, body) => {
                self.in_loop += 1;
                let r = self.run_for(var, vals, body);
                self.in_loop -= 1;
                r
            }
            Stmt::While(c, body) => {
                self.in_loop += 1;
                let r = self.run_while(c, body);
                self.in_loop -= 1;
                r
            }
            Stmt::If(branches, other) => {
                for (c, body) in branches {
                    let v = self.eval(c)?;
                    if crate::eval::truthy(&v).map_err(|_| {
                        format!("if: can't tell whether {} is true or false", show(&v))
                    })? {
                        return self.inner(body);
                    }
                }
                match other {
                    Some(b) => self.inner(b),
                    None => Ok(Flow::Normal),
                }
            }
            Stmt::Function(name, params, out, body) => {
                if crate::expr::Func::from_name(name).is_some()
                    || ["exp", "sqrt", "if", "print"].contains(&name.as_str())
                {
                    return Err(format!(
                        "{} is a built-in function; pick another name",
                        name
                    ));
                }
                self.funcs.remove(name);
                self.procs.insert(
                    name.clone(),
                    Proc {
                        params: params.clone(),
                        out: out.clone(),
                        body: Rc::new(body.clone()),
                    },
                );
                Ok(Flow::Normal)
            }
            Stmt::Break => Ok(Flow::Break),
            Stmt::Continue => Ok(Flow::Continue),
            Stmt::Return(v) => Ok(Flow::Return(match v {
                Some(n) => Some(self.eval(n)?),
                None => None,
            })),
            other => {
                self.notes.clear();
                let o = self.exec(other)?;
                if !quiet {
                    self.emit(o);
                } else {
                    self.notes.clear();
                }
                Ok(Flow::Normal)
            }
        }
    }

    fn inner(&mut self, b: &Block) -> R<Flow> {
        self.in_block += 1;
        let r = self.run_block(b);
        self.in_block -= 1;
        r
    }

    fn run_for(&mut self, var: &str, vals: &Node, body: &Block) -> R<Flow> {
        // a range is walked without building it: for k = 1:10^7
        if let Node::Range(a, step, b) = vals {
            let a = self.eval(a)?;
            let s = match step {
                Some(s) => self.eval(s)?,
                None => Expr::one(),
            };
            let b = self.eval(b)?;
            if let (Some(x), Some(st), Some(y)) = (a.num(), s.num(), b.num()) {
                let (x, st, y) = (x.to_f64(), st.to_f64(), y.to_f64());
                if st == 0.0 {
                    return Err("for: the step of a range can't be 0".into());
                }
                let n = ((y - x) / st + 1e-9).floor();
                let n = if n.is_finite() && n >= 0.0 {
                    n as u64 + 1
                } else {
                    0
                };
                if n > MAX_ROUNDS {
                    return Err(format!("for: {} rounds is too many", n));
                }
                for k in 0..n {
                    let v = crate::eval::arith(
                        Op::Add,
                        a.clone(),
                        crate::eval::arith(Op::Mul, Expr::int(k as i128), s.clone())?,
                    )?;
                    self.set(var, v);
                    match self.inner(body)? {
                        Flow::Break => break,
                        Flow::Return(v) => return Ok(Flow::Return(v)),
                        _ => {}
                    }
                }
                return Ok(Flow::Normal);
            }
        }
        let v = self.eval(vals)?;
        let items: Vec<Expr> = match &v {
            Expr::Matrix(m) if m.len() == 1 => m[0].clone(),
            Expr::Matrix(m) if m.first().is_some_and(|r| r.len() == 1) => {
                m.iter().map(|r| r[0].clone()).collect()
            }
            // a matrix: one row at a time
            Expr::Matrix(m) => m.iter().map(|r| Expr::Matrix(vec![r.clone()])).collect(),
            Expr::Str(s) => s.chars().map(|c| Expr::Str(c.to_string())).collect(),
            _ => match (as_list(&v), as_record(&v)) {
                (Some(l), _) => l.clone(),
                (_, Some(r)) => r.iter().map(|(k, _)| Expr::Str(k.clone())).collect(),
                _ => vec![v.clone()],
            },
        };
        for x in items {
            self.set(var, x);
            match self.inner(body)? {
                Flow::Break => break,
                Flow::Return(v) => return Ok(Flow::Return(v)),
                _ => {}
            }
        }
        Ok(Flow::Normal)
    }

    fn run_while(&mut self, c: &Node, body: &Block) -> R<Flow> {
        let mut rounds = 0u64;
        loop {
            let v = self.eval(c)?;
            let go = crate::eval::truthy(&v)
                .map_err(|_| format!("while: can't tell whether {} is true or false", show(&v)))?;
            if !go {
                return Ok(Flow::Normal);
            }
            rounds += 1;
            if rounds > MAX_ROUNDS {
                return Err("while: still running after 100 million rounds (does the condition ever become false?)".into());
            }
            match self.inner(body)? {
                Flow::Break => return Ok(Flow::Normal),
                Flow::Return(v) => return Ok(Flow::Return(v)),
                _ => {}
            }
        }
    }

    /// Calls a `function ... end`: its own variables, the arguments bound to
    /// its parameters. None when it gives no value.
    pub(crate) fn call_proc(&mut self, f: &str, args: &[Node]) -> R<Option<Expr>> {
        let p = self.procs.get(f).cloned().unwrap();
        if args.len() != p.params.len() {
            return Err(format!(
                "{} takes {} argument{}",
                f,
                p.params.len(),
                if p.params.len() == 1 { "" } else { "s" }
            ));
        }
        let mut vals = vec![];
        for a in args {
            vals.push(self.eval(a)?);
        }
        if self.depth >= 2000 {
            return Err(format!(
                "{}: too many nested calls (does the recursion stop?)",
                f
            ));
        }
        let mut scope = crate::eval::builtins();
        if let Some(x) = self.vars.get("explain") {
            scope.insert("explain".into(), x.clone());
        }
        for (k, v) in p.params.iter().zip(vals) {
            scope.insert(k.clone(), v);
        }
        let outer = std::mem::replace(&mut self.vars, scope);
        let hidden = std::mem::take(&mut self.hidden);
        let (lp, bl) = (self.in_loop, self.in_block);
        self.in_loop = 0;
        self.in_block = 1;
        self.depth += 1;
        self.in_proc += 1;
        let r = self.run_block(&p.body);
        self.in_proc -= 1;
        self.depth -= 1;
        self.in_loop = lp;
        self.in_block = bl;
        let local = std::mem::replace(&mut self.vars, outer);
        self.hidden = hidden;
        match r.map_err(|e| format!("{}: {}", f, e))? {
            Flow::Return(Some(v)) => Ok(Some(v)),
            Flow::Break | Flow::Continue => Err(format!("{}: break or continue outside a loop", f)),
            _ => match &p.out {
                Some(o) => match local.get(o) {
                    Some(v) => Ok(Some(v.clone())),
                    None => Err(format!("{}: the result {} was never set", f, o)),
                },
                None => Ok(None),
            },
        }
    }

    /// Evaluate index arguments where `end` stands for the last position
    /// (`dims` per position; counting from 0 when `zero`)
    fn positions(&mut self, args: &[Node], dims: &[usize], zero: bool, what: &str) -> R<Vec<i128>> {
        let saved = self.vars.get("end").cloned();
        let mut out = vec![];
        let mut err = None;
        for (k, a) in args.iter().enumerate() {
            let n = *dims.get(k).unwrap_or(&0) as i128;
            self.vars
                .insert("end".into(), Expr::int(if zero { n - 1 } else { n }));
            match self.eval(a).and_then(|v| whole(&v, what)) {
                Ok(p) => out.push(if zero { p } else { p - 1 }),
                Err(e) => {
                    err = Some(e);
                    break;
                }
            }
        }
        match saved {
            Some(v) => self.vars.insert("end".into(), v),
            None => self.vars.remove("end"),
        };
        match err {
            Some(e) => Err(e),
            None => Ok(out),
        }
    }

    /// dims used for `end`: one position counts all elements of a vector
    fn end_dims(v: &Expr, n: usize) -> Vec<usize> {
        let (r, c) = dims_of(v);
        if n == 1 {
            vec![if r == 1 || c == 1 { r * c } else { r }]
        } else {
            vec![r, c]
        }
    }

    /// x(k), x[k] for lists and strings (and `end` in matrix positions);
    /// None when the value is not one this handles
    pub(crate) fn index_value(
        &mut self,
        v: &Expr,
        args: &[Node],
        zero: bool,
        what: &str,
    ) -> R<Expr> {
        let dims = Self::end_dims(v, args.len());
        let ks = self.positions(args, &dims, zero, what)?;
        let first = if zero { 0 } else { 1 };
        let bad = |k: i128, n: usize| -> R<usize> {
            if k < 0 || k >= n as i128 {
                Err(format!(
                    "{}: position {} is outside {}..{}",
                    what,
                    k + 1 - first,
                    first,
                    n as i128 - 1 + first
                ))
            } else {
                Ok(k as usize)
            }
        };
        match v {
            Expr::Str(s) => {
                let c: Vec<char> = s.chars().collect();
                match ks[..] {
                    [k] => Ok(Expr::Str(c[bad(k, c.len())?].to_string())),
                    _ => Err(format!("{}: a string takes one position", what)),
                }
            }
            Expr::Matrix(m) => {
                let (r, c) = dims_of(v);
                match ks[..] {
                    [k] if r == 1 => Ok(m[0][bad(k, c)?].clone()),
                    [k] if c == 1 => Ok(m[bad(k, r)?][0].clone()),
                    [k] => Ok(Expr::Matrix(vec![m[bad(k, r)?].clone()])),
                    [i, j] => Ok(m[bad(i, r)?][bad(j, c)?].clone()),
                    _ => Err(format!("{}: use one or two positions", what)),
                }
            }
            _ => match as_list(v) {
                Some(l) => match ks[..] {
                    [k] => Ok(l[bad(k, l.len())?].clone()),
                    _ => Err(format!("{}: a list takes one position", what)),
                },
                None => Err(format!("{} can't be indexed", show(v))),
            },
        }
    }

    /// name(i, j) = value (from 1) or name[i, j] = value (from 0)
    pub(crate) fn set_index(
        &mut self,
        name: &str,
        idx: &[Node],
        zero: bool,
        value: &Node,
    ) -> R<Output> {
        let v = self.eval(value)?;
        let cur = self
            .vars
            .get(name)
            .filter(|_| !self.hidden.iter().any(|h| h == name))
            .cloned()
            .filter(|c| !matches!(c, Expr::Sym(s) if s == name));
        let what = if zero {
            format!("{}[...]", name)
        } else {
            format!("{}(...)", name)
        };
        let cur = cur.unwrap_or(Expr::Matrix(vec![]));
        let dims = Self::end_dims(&cur, idx.len());
        let ks = self.positions(idx, &dims, zero, &what)?;
        if ks.iter().any(|&k| k < 0) {
            return Err(format!(
                "{}: positions start at {}",
                what,
                if zero { 0 } else { 1 }
            ));
        }
        let ks: Vec<usize> = ks.into_iter().map(|k| k as usize).collect();
        if ks.iter().any(|&k| k > 10_000_000) {
            return Err(format!("{}: position too large", what));
        }
        let new = if let Some(l) = as_list(&cur) {
            let mut l = l.clone();
            match ks[..] {
                [k] => {
                    while l.len() <= k {
                        l.push(Expr::zero());
                    }
                    l[k] = v;
                }
                _ => return Err(format!("{}: a list takes one position", what)),
            }
            list(l)
        } else {
            match cur {
                Expr::Matrix(mut m) => {
                    let (r, c) = (m.len(), m.first().map_or(0, |x| x.len()));
                    match ks[..] {
                        [k] if r <= 1 || c == 1 && r > 1 => {
                            if !element_ok(&v) {
                                return Err(format!("{}: an entry must be a single value", what));
                            }
                            if r > 1 {
                                // a column
                                while m.len() <= k {
                                    m.push(vec![Expr::zero()]);
                                }
                                m[k][0] = v;
                            } else {
                                if m.is_empty() {
                                    m.push(vec![]);
                                }
                                while m[0].len() <= k {
                                    m[0].push(Expr::zero());
                                }
                                m[0][k] = v;
                            }
                        }
                        [k] => {
                            // a whole row
                            let row = match v {
                                Expr::Matrix(x) if x.len() == 1 && x[0].len() == c => x[0].clone(),
                                _ => {
                                    return Err(format!(
                                        "{}: a row of {} needs {} values",
                                        what, name, c
                                    ));
                                }
                            };
                            while m.len() <= k {
                                m.push(vec![Expr::zero(); c]);
                            }
                            m[k] = row;
                        }
                        [i, j] => {
                            if !element_ok(&v) {
                                return Err(format!("{}: an entry must be a single value", what));
                            }
                            let cols = c.max(j + 1);
                            for row in m.iter_mut() {
                                row.resize(cols, Expr::zero());
                            }
                            while m.len() <= i {
                                m.push(vec![Expr::zero(); cols]);
                            }
                            m[i][j] = v;
                        }
                        _ => return Err(format!("{}: use one or two positions", what)),
                    }
                    Expr::Matrix(m)
                }
                other if other.is_scalar() && ks.iter().all(|&k| k == 0) => v,
                other => {
                    return Err(format!(
                        "{} can't be changed by position ({})",
                        name,
                        show(&other)
                    ));
                }
            }
        };
        self.set(name, new.clone());
        Ok(Output::Value(new))
    }

    /// p.mass = value (and a.b.c = value): makes or updates a record
    pub(crate) fn set_field(&mut self, path: &str, v: Expr) -> R<Output> {
        let parts: Vec<&str> = path.split('.').collect();
        let base = parts[0];
        let cur = self
            .vars
            .get(base)
            .cloned()
            .filter(|c| !matches!(c, Expr::Sym(s) if s == base));
        fn put(cur: Option<&Expr>, keys: &[&str], v: Expr, at: &str) -> R<Expr> {
            let mut fields = match cur {
                None => vec![],
                Some(e) => match as_record(e) {
                    Some(r) => r.clone(),
                    None => return Err(format!("{} is not a record", at)),
                },
            };
            let k = keys[0];
            let pos = fields.iter().position(|(n, _)| n == k);
            let inner = if keys.len() == 1 {
                v
            } else {
                put(
                    pos.map(|p| &fields[p].1),
                    &keys[1..],
                    v,
                    &format!("{}.{}", at, k),
                )?
            };
            match pos {
                Some(p) => fields[p].1 = inner,
                None => fields.push((k.to_string(), inner)),
            }
            Ok(record(fields))
        }
        let new = put(cur.as_ref(), &parts[1..], v, base)?;
        self.set(base, new.clone());
        Ok(Output::Value(new))
    }

    /// p.mass for a record p (None when the name is not a field path)
    pub(crate) fn get_field(&self, path: &str) -> Option<R<Expr>> {
        let mut parts = path.split('.');
        let base = parts.next()?;
        let mut cur = self.vars.get(base)?.clone();
        as_record(&cur)?;
        let mut at = base.to_string();
        for k in parts {
            let next = match as_record(&cur) {
                Some(r) => r.iter().find(|(n, _)| n == k).map(|(_, x)| x.clone()),
                None => return Some(Err(format!("{} is not a record", at))),
            };
            cur = match next {
                Some(x) => x,
                None => return Some(Err(format!("{} has no field {}", at, k))),
            };
            at = format!("{}.{}", at, k);
        }
        Some(Ok(cur))
    }

    /// apply a function (by name) to one value
    fn apply_named(&mut self, f: &Node, x: Expr) -> R<Expr> {
        let name = match f {
            Node::Ident(n) | Node::Str(n) => n.clone(),
            _ => return Err("map and filter take a function name, such as map(f, L)".into()),
        };
        let tmp = "__item";
        let saved = self.vars.insert(tmp.into(), x);
        let r = self.eval(&Node::Call(name, vec![Node::Ident(tmp.into())]));
        match saved {
            Some(v) => self.vars.insert(tmp.into(), v),
            None => self.vars.remove(tmp),
        };
        r
    }

    fn items_of(&self, v: &Expr, f: &str) -> R<Vec<Expr>> {
        if let Some(l) = as_list(v) {
            return Ok(l.clone());
        }
        match v {
            Expr::Matrix(m) => Ok(m.iter().flatten().cloned().collect()),
            Expr::Str(s) => Ok(s.chars().map(|c| Expr::Str(c.to_string())).collect()),
            _ => Err(format!("{} needs a list or a vector, got {}", f, show(v))),
        }
    }

    /// Language functions: print, strings, lists, records. None when `f` is not one of them.
    pub(crate) fn lang_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let names = [
            "print", "disp", "str", "num", "upper", "lower", "split", "join", "replace",
            "contains", "trim", "format", "list", "append", "concat", "map", "filter", "fields",
            "has", "reverse", "length",
        ];
        if !names.contains(&f) {
            return None;
        }
        Some(self.lang_call_inner(f, args))
    }

    fn lang_call_inner(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        let mut vals = vec![];
        let lazy = matches!(f, "map" | "filter");
        for (k, a) in args.iter().enumerate() {
            if lazy && k == 0 {
                vals.push(Expr::Bool(true));
                continue;
            }
            vals.push(self.eval(a)?);
        }
        let n = |k: usize| -> R<()> {
            if vals.len() == k {
                Ok(())
            } else {
                Err(format!(
                    "{} takes {} argument{}",
                    f,
                    k,
                    if k == 1 { "" } else { "s" }
                ))
            }
        };
        let text = |v: &Expr, what: &str| -> R<String> {
            match v {
                Expr::Str(s) => Ok(s.clone()),
                other => Err(format!("{}: {} is not a string", what, show(other))),
            }
        };
        match f {
            "print" | "disp" => {
                let t = vals.iter().map(show).collect::<Vec<_>>().join(" ");
                self.emit_text(t);
                Ok(Expr::Obj(Rc::new(Obj::List(vec![]))))
            }
            "str" => {
                n(1)?;
                Ok(Expr::Str(show(&vals[0])))
            }
            "num" => {
                n(1)?;
                let s = text(&vals[0], "num")?;
                let stmts =
                    crate::parser::parse(&s).map_err(|e| format!("num(\"{}\"): {}", s, e))?;
                match &stmts[..] {
                    [Stmt::Expr(node)] => {
                        let v = self.eval(node)?;
                        if v.is_number() {
                            Ok(v)
                        } else {
                            Err(format!("num(\"{}\"): not a number", s))
                        }
                    }
                    _ => Err(format!("num(\"{}\"): not a number", s)),
                }
            }
            "upper" | "lower" | "trim" => {
                n(1)?;
                let s = text(&vals[0], f)?;
                Ok(Expr::Str(match f {
                    "upper" => s.to_uppercase(),
                    "lower" => s.to_lowercase(),
                    _ => s.trim().to_string(),
                }))
            }
            "split" => {
                if vals.is_empty() || vals.len() > 2 {
                    return Err("split(s) or split(s, separator)".into());
                }
                let s = text(&vals[0], "split")?;
                let parts: Vec<Expr> = match vals.get(1) {
                    Some(sep) => {
                        let sep = text(sep, "split")?;
                        if sep.is_empty() {
                            s.chars().map(|c| Expr::Str(c.to_string())).collect()
                        } else {
                            s.split(sep.as_str()).map(|p| Expr::Str(p.into())).collect()
                        }
                    }
                    None => s.split_whitespace().map(|p| Expr::Str(p.into())).collect(),
                };
                Ok(list(parts))
            }
            "join" => {
                if vals.is_empty() || vals.len() > 2 {
                    return Err("join(list) or join(list, separator)".into());
                }
                let sep = match vals.get(1) {
                    Some(s) => text(s, "join")?,
                    None => String::new(),
                };
                let items = self.items_of(&vals[0], "join")?;
                Ok(Expr::Str(
                    items.iter().map(show).collect::<Vec<_>>().join(&sep),
                ))
            }
            "replace" => {
                n(3)?;
                let (s, a, b) = (text(&vals[0], f)?, text(&vals[1], f)?, text(&vals[2], f)?);
                if a.is_empty() {
                    return Err("replace: the text to replace is empty".into());
                }
                Ok(Expr::Str(s.replace(&a, &b)))
            }
            "contains" => {
                n(2)?;
                if let Expr::Str(s) = &vals[0] {
                    return Ok(Expr::Bool(s.contains(&text(&vals[1], f)?)));
                }
                let items = self.items_of(&vals[0], f)?;
                Ok(Expr::Bool(items.contains(&vals[1])))
            }
            "format" => {
                let t = text(vals.first().ok_or("format(text, values...)")?, f)?;
                let mut out = String::new();
                let mut rest = vals[1..].iter();
                let mut parts = t.split("{}");
                out.push_str(parts.next().unwrap_or(""));
                for p in parts {
                    let v = rest.next().ok_or("format: more {} than values")?;
                    out.push_str(&show(v));
                    out.push_str(p);
                }
                if rest.next().is_some() {
                    return Err("format: more values than {}".into());
                }
                Ok(Expr::Str(out))
            }
            "list" => {
                if vals.len() == 1 {
                    if let Expr::Matrix(_) | Expr::Str(_) = &vals[0] {
                        return Ok(list(self.items_of(&vals[0], f)?));
                    }
                }
                Ok(list(vals))
            }
            "append" => {
                if vals.len() < 2 {
                    return Err("append(list, value, ...)".into());
                }
                let mut l = match as_list(&vals[0]) {
                    Some(l) => l.clone(),
                    None => {
                        if let Expr::Matrix(m) = &vals[0] {
                            if m.len() <= 1 {
                                let mut row = m.first().cloned().unwrap_or_default();
                                for v in &vals[1..] {
                                    if !element_ok(v) {
                                        return Err("append: a vector takes single values".into());
                                    }
                                    row.push(v.clone());
                                }
                                return Ok(Expr::Matrix(vec![row]));
                            }
                        }
                        return Err(format!("append needs a list, got {}", show(&vals[0])));
                    }
                };
                l.extend(vals[1..].iter().cloned());
                Ok(list(l))
            }
            "concat" => {
                if vals.iter().all(|v| matches!(v, Expr::Str(_))) {
                    return Ok(Expr::Str(vals.iter().map(show).collect()));
                }
                let mut l = vec![];
                for v in &vals {
                    l.extend(self.items_of(v, f)?);
                }
                Ok(list(l))
            }
            "reverse" => {
                n(1)?;
                match &vals[0] {
                    Expr::Str(s) => Ok(Expr::Str(s.chars().rev().collect())),
                    Expr::Matrix(m) if m.len() == 1 => {
                        Ok(Expr::Matrix(vec![m[0].iter().rev().cloned().collect()]))
                    }
                    Expr::Matrix(m) => Ok(Expr::Matrix(m.iter().rev().cloned().collect())),
                    v => {
                        let mut l = self.items_of(v, f)?;
                        l.reverse();
                        Ok(list(l))
                    }
                }
            }
            "map" | "filter" => {
                n(2)?;
                let items = self.items_of(&vals[1], f)?;
                let mut out = vec![];
                for x in items {
                    let y = self.apply_named(&args[0], x.clone())?;
                    if f == "map" {
                        out.push(y);
                    } else if crate::eval::truthy(&y)
                        .map_err(|_| format!("filter: {} is not true or false", show(&y)))?
                    {
                        out.push(x);
                    }
                }
                // a vector stays a vector when every result is a single value
                if matches!(vals[1], Expr::Matrix(_)) && out.iter().all(element_ok) {
                    return Ok(Expr::Matrix(vec![out]));
                }
                Ok(list(out))
            }
            "fields" => {
                n(1)?;
                let r = as_record(&vals[0]).ok_or("fields needs a record")?;
                Ok(list(r.iter().map(|(k, _)| Expr::Str(k.clone())).collect()))
            }
            "has" => {
                n(2)?;
                let r = as_record(&vals[0]).ok_or("has needs a record")?;
                let k = text(&vals[1], f)?;
                Ok(Expr::Bool(r.iter().any(|(n, _)| *n == k)))
            }
            "length" => {
                n(1)?;
                let v = &vals[0];
                Ok(Expr::int(match v {
                    Expr::Str(s) => s.chars().count(),
                    Expr::Matrix(m) => {
                        let (r, c) = (m.len(), m.first().map_or(0, |x| x.len()));
                        if r == 0 || c == 0 { 0 } else { r.max(c) }
                    }
                    _ => match (as_list(v), as_record(v)) {
                        (Some(l), _) => l.len(),
                        (_, Some(r)) => r.len(),
                        _ => 1,
                    },
                } as i128))
            }
            _ => unreachable!(),
        }
    }

    /// queue the results of statements run inside blocks; see the module docs
    pub(crate) fn take_emitted(&mut self) -> Vec<(Output, Vec<Note>)> {
        std::mem::take(&mut self.emitted)
    }
}
