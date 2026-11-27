//! Lexer and Pratt parser for the Galileo language.
//!
//! ```text
//! statement := name = expr | [a, b, ...] = expr | command | expr
//! expr      := range ; precedence from low to high:
//!              a:b:c   ||   &&   + -   * / \   unary -   ^ (right assoc)
//!              postfix: f(args)  A[i,j]  A'
//! matrix    := [a b c; d e f]   (elements separated by spaces or commas)
//! ```
//! Statements are separated by `;` or newlines; `//` starts a comment.
//!
//! ```text
//! block statements, each closed by `end`:
//!   for k = 1:10 ... end        for x in list ... end
//!   while cond ... end
//!   if cond ... elseif cond ... else ... end
//!   function name(a, b) ... end      function out = name(a, b) ... end
//!   break   continue   return [value]
//! assignments to parts:  x(k) = v (from 1)   A[i, j] = v (from 0)   p.mass = v
//! lists {1, "a", x} and records {mass: 3, name: "ball"}
//! ```

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Num(String),
    Ident(String),
    Str(String),
    Neg(Box<Node>),
    Bin(Op, Box<Node>, Box<Node>),
    Range(Box<Node>, Option<Box<Node>>, Box<Node>),
    Call(String, Vec<Node>),
    Index(Box<Node>, Vec<Node>),
    Matrix(Vec<Vec<Node>>),
    Transpose(Box<Node>),
    /// {a, b, c}
    List(Vec<Node>),
    /// {name: value, ...}
    Record(Vec<(String, Node)>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Solve,
    /// elementwise .* ./ .^
    EMul,
    EDiv,
    EPow,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Node),
    Assign(String, Node),
    MultiAssign(Vec<String>, Node),
    /// f(x, y) := body (or f(x) = body): a user-defined function
    Define(String, Vec<String>, Node),
    /// who, whos, clear, exit, ls, pwd, cd <dir>, help [name], load(<file>)
    Command(String, Option<String>),
    /// for k = values ... end
    For(String, Node, Block),
    /// while condition ... end
    While(Node, Block),
    /// if c1 ... elseif c2 ... else ... end: the branches, then the else part
    If(Vec<(Node, Block)>, Option<Block>),
    /// function name(params) ... end, or function out = name(params) ... end
    Function(String, Vec<String>, Option<String>, Block),
    Break,
    Continue,
    Return(Option<Node>),
    /// name(i, j) = value (positions from 1) or name[i, j] = value (from 0, the bool)
    SetIndex(String, Vec<Node>, bool, Node),
}

/// the statements of a block, each with whether it ends in `;`
pub type Block = Vec<(Stmt, bool)>;

/// words that open a block closed by `end`
const OPENERS: [&str; 4] = ["for", "while", "if", "function"];

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Num(String),
    Ident(String),
    Str(String),
    /// raw text argument of load(...), cd ..., help(...)
    Raw(String),
    Sym(&'static str),
    Sep, // ; or newline at top level
    Newline,
}

#[derive(Clone, Debug)]
struct Token {
    tok: Tok,
    space_before: bool,
}

const SYMS: [&str; 32] = [
    ".'", "&&", "||", "==", "~=", "!=", "<=", ">=", ":=", ".*", "./", ".^", "<", ">", "+", "-", "*", "/",
    "^", "\\", "'", "(", ")", "[", "]", "{", "}", ",", ";", ":", "=", "!",
];

/// Commands whose argument is taken as raw text
const RAW: [&str; 5] = ["load", "cd", "help", "metric.generate", "rm"];

fn lex(src: &str) -> Result<Vec<Token>, String> {
    let c: Vec<char> = src.chars().collect();
    let mut i = 0;
    let mut out: Vec<Token> = vec![];
    let mut space = false;
    let mut depth = 0i32; // bracket/paren depth: newlines inside are row separators
    while i < c.len() {
        let ch = c[i];
        if ch == '/' && c.get(i + 1) == Some(&'/') {
            while i < c.len() && c[i] != '\n' {
                i += 1;
            }
            continue;
        }
        // a backslash ending a line continues the statement on the next line
        if ch == '\\' {
            let mut j = i + 1;
            while j < c.len() && c[j] != '\n' && c[j].is_whitespace() {
                j += 1;
            }
            let comment = c.get(j) == Some(&'/') && c.get(j + 1) == Some(&'/');
            if j >= c.len() || c[j] == '\n' || comment {
                while j < c.len() && c[j] != '\n' {
                    j += 1;
                }
                i = j + 1;
                space = true;
                continue;
            }
        }
        if ch == '\n' {
            out.push(Token {
                tok: Tok::Newline,
                space_before: space,
            });
            space = true;
            i += 1;
            continue;
        }
        if ch.is_whitespace() {
            space = true;
            i += 1;
            continue;
        }
        let start = i;
        let tok = if ch.is_ascii_digit()
            || (ch == '.' && c.get(i + 1).is_some_and(|d| d.is_ascii_digit()))
        {
            while i < c.len()
                && (c[i].is_ascii_digit()
                    || (c[i] == '.' && !matches!(c.get(i + 1), Some('*' | '/' | '^'))))
            {
                i += 1;
            }
            if i < c.len() && (c[i] == 'e' || c[i] == 'E') {
                let mut j = i + 1;
                if j < c.len() && (c[j] == '+' || c[j] == '-') {
                    j += 1;
                }
                if j < c.len() && c[j].is_ascii_digit() {
                    i = j;
                    while i < c.len() && c[i].is_ascii_digit() {
                        i += 1;
                    }
                }
            }
            Tok::Num(c[start..i].iter().collect())
        } else if ch.is_alphabetic() || ch == '_' {
            while i < c.len()
                && (c[i].is_alphanumeric()
                    || c[i] == '_'
                    || (c[i] == '.' && c.get(i + 1).is_some_and(|d| d.is_alphabetic())))
            {
                i += 1;
            }
            // y'(x) was once a derivative; ' is now always the transpose
            let mut j = i;
            while j < c.len() && c[j] == '\'' {
                j += 1;
            }
            let name: String = c[start..i].iter().collect();
            if j > i && c.get(j) == Some(&'(') {
                let k = j - i;
                let arg: String = c[j + 1..].iter().take_while(|&&x| x != ')').collect();
                return Err(format!(
                    "{}{}(...): ' is the transpose; write the derivative as d({}, {}{})",
                    name,
                    "'".repeat(k),
                    name,
                    arg.trim(),
                    if k > 1 { format!(", {}", k) } else { String::new() }
                ));
            }
            if RAW.contains(&name.as_str()) {
                out.push(Token {
                    tok: Tok::Ident(name.clone()),
                    space_before: space,
                });
                space = false;
                // raw argument: (text) or a space-separated word
                let mut j = i;
                while j < c.len() && c[j] == ' ' {
                    j += 1;
                }
                if j < c.len() && c[j] == '(' {
                    let close = c[j..].iter().position(|&x| x == ')').ok_or("missing )")?;
                    let raw: String = c[j + 1..j + close].iter().collect();
                    out.push(Token {
                        tok: Tok::Raw(raw.trim().to_string()),
                        space_before: false,
                    });
                    i = j + close + 1;
                } else if name == "cd" && j < c.len() && c[j] != ';' && c[j] != '\n' {
                    let end = c[j..]
                        .iter()
                        .position(|&x| x == ';' || x == '\n')
                        .map_or(c.len(), |p| j + p);
                    out.push(Token {
                        tok: Tok::Raw(c[j..end].iter().collect::<String>().trim().to_string()),
                        space_before: true,
                    });
                    i = end;
                }
                continue;
            }
            Tok::Ident(name)
        } else if ch == '"' {
            i += 1;
            while i < c.len() && c[i] != '"' {
                i += 1;
            }
            if i >= c.len() {
                return Err("unterminated string".into());
            }
            i += 1;
            Tok::Str(c[start + 1..i - 1].iter().collect())
        } else {
            let rest: String = c[i..(i + 2).min(c.len())].iter().collect();
            let s = SYMS
                .iter()
                .find(|s| rest.starts_with(**s))
                .ok_or_else(|| format!("unexpected character '{}'", ch))?;
            i += s.len();
            match *s {
                "(" | "[" | "{" => depth += 1,
                ")" | "]" | "}" => depth -= 1,
                _ => {}
            }
            Tok::Sym(s)
        };
        out.push(Token {
            tok,
            space_before: space,
        });
        space = false;
    }
    let _ = depth;
    Ok(out)
}

struct P {
    t: Vec<Token>,
    /// for each token, whether it is a `;` that ends a statement
    semi: Vec<bool>,
    i: usize,
    /// inside [ ]: whitespace separates elements and newlines separate rows
    in_matrix: usize,
}

type R<T> = Result<T, String>;

impl P {
    fn peek(&self) -> Option<&Tok> {
        self.t.get(self.i).map(|t| &t.tok)
    }
    fn space_before(&self) -> bool {
        self.t.get(self.i).is_some_and(|t| t.space_before)
    }
    fn is(&self, s: &str) -> bool {
        matches!(self.peek(), Some(Tok::Sym(x)) if *x == s)
    }
    fn eat(&mut self, s: &str) -> bool {
        if self.is(s) {
            self.i += 1;
            true
        } else {
            false
        }
    }
    fn expect(&mut self, s: &str) -> R<()> {
        if self.eat(s) {
            Ok(())
        } else {
            Err(format!("expected '{}' {}", s, self.here()))
        }
    }
    fn here(&self) -> String {
        match self.peek() {
            None => "at end of input".into(),
            Some(t) => format!("near {}", describe(t)),
        }
    }
    fn skip_newlines(&mut self) {
        while matches!(self.peek(), Some(Tok::Newline)) {
            self.i += 1;
        }
    }

    fn statement(&mut self) -> R<Stmt> {
        // commands
        if let Some(Tok::Ident(name)) = self.peek().cloned() {
            let next = self.t.get(self.i + 1).map(|t| t.tok.clone());
            let ends = matches!(
                next,
                None | Some(Tok::Sep) | Some(Tok::Newline) | Some(Tok::Sym(";"))
            ) || matches!(&next, Some(Tok::Sym(",")))
                || matches!(&next, Some(Tok::Ident(w)) if w == "end");
            if let Some(st) = self.block_statement(&name, ends)? {
                return Ok(st);
            }
            match name.as_str() {
                "who" | "whos" | "clear" | "exit" | "quit" | "ls" | "pwd" if ends => {
                    self.i += 1;
                    return Ok(Stmt::Command(name, None));
                }
                "help" | "cd" | "load" | "rm"
                    if name != "rm" || ends || matches!(next, Some(Tok::Raw(_))) =>
                {
                    self.i += 1;
                    let arg = match self.peek() {
                        Some(Tok::Raw(r)) => {
                            let r = r.clone();
                            self.i += 1;
                            Some(r)
                        }
                        _ => None,
                    };
                    return Ok(Stmt::Command(name, arg));
                }
                _ => {}
            }
            if matches!(next, Some(Tok::Sym("=")) | Some(Tok::Sym(":="))) {
                self.i += 2;
                return Ok(Stmt::Assign(name, self.expr()?));
            }
            if let Some((params, after)) = self.definition_head() {
                self.i = after;
                return Ok(Stmt::Define(name, params, self.expr()?));
            }
            // x(k) = v, A[i, j] = v
            if let Some(st) = self.try_set_index(&name)? {
                return Ok(st);
            }
        }
        // [a, b] = ...
        if self.is("[") {
            let save = self.i;
            if let Some(names) = self.try_name_list() {
                if self.eat("=") {
                    return Ok(Stmt::MultiAssign(names, self.expr()?));
                }
            }
            self.i = save;
        }
        Ok(Stmt::Expr(self.expr()?))
    }

    /// for, while, if, function, break, continue, return; None for other statements
    fn block_statement(&mut self, word: &str, ends: bool) -> R<Option<Stmt>> {
        let start = self.i;
        match word {
            "for" => {
                self.i += 1;
                let paren = self.eat("(");
                let var = match self.peek().cloned() {
                    Some(Tok::Ident(v)) => v,
                    _ => return Err(format!("for: expected a variable name {}", self.here())),
                };
                self.i += 1;
                if !self.eat("=") && !matches!(self.peek(), Some(Tok::Ident(w)) if w == "in") {
                    return Err(format!("for {}: expected = or in {}", var, self.here()));
                }
                if matches!(self.peek(), Some(Tok::Ident(w)) if w == "in") {
                    self.i += 1;
                }
                let vals = self.expr()?;
                if paren {
                    self.expect(")")?;
                }
                let (body, _) = self.block(&["end"], "for")?;
                Ok(Some(Stmt::For(var, vals, body)))
            }
            "while" => {
                self.i += 1;
                let c = self.expr()?;
                let (body, _) = self.block(&["end"], "while")?;
                Ok(Some(Stmt::While(c, body)))
            }
            "if" => {
                // if(c, a, b) is the function; if c ... end is a block
                self.i += 1;
                let c = match self.expr() {
                    Ok(c) if self.at_separator() => c,
                    _ => {
                        self.i = start;
                        return Ok(None);
                    }
                };
                let mut branches = vec![];
                let mut cond = c;
                loop {
                    let (body, stop) = self.block(&["end", "else", "elseif"], "if")?;
                    branches.push((cond, body));
                    match stop.as_str() {
                        "elseif" => cond = self.expr()?,
                        "else" => {
                            let (body, _) = self.block(&["end"], "if")?;
                            return Ok(Some(Stmt::If(branches, Some(body))));
                        }
                        _ => return Ok(Some(Stmt::If(branches, None))),
                    }
                }
            }
            "function" => {
                self.i += 1;
                let mut name = match self.peek().cloned() {
                    Some(Tok::Ident(n)) => n,
                    _ => return Err(format!("function: expected a name {}", self.here())),
                };
                self.i += 1;
                let mut out = None;
                if self.eat("=") {
                    out = Some(name);
                    name = match self.peek().cloned() {
                        Some(Tok::Ident(n)) => n,
                        _ => return Err(format!("function: expected a name {}", self.here())),
                    };
                    self.i += 1;
                }
                let mut params = vec![];
                if self.eat("(") {
                    while !self.eat(")") {
                        match self.peek().cloned() {
                            Some(Tok::Ident(p)) => params.push(p),
                            _ => {
                                return Err(format!(
                                    "function {}: parameters are names {}",
                                    name,
                                    self.here()
                                ))
                            }
                        }
                        self.i += 1;
                        if !self.eat(",") && !self.is(")") {
                            return Err(format!("function {}: expected , or ) {}", name, self.here()));
                        }
                    }
                }
                let (body, _) = self.block(&["end"], "function")?;
                Ok(Some(Stmt::Function(name, params, out, body)))
            }
            "break" if ends => {
                self.i += 1;
                Ok(Some(Stmt::Break))
            }
            "continue" if ends => {
                self.i += 1;
                Ok(Some(Stmt::Continue))
            }
            "return" => {
                self.i += 1;
                if self.at_separator()
                    || self.peek().is_none()
                    || matches!(self.peek(), Some(Tok::Ident(w)) if w == "end")
                {
                    return Ok(Some(Stmt::Return(None)));
                }
                Ok(Some(Stmt::Return(Some(self.expr()?))))
            }
            "end" | "else" | "elseif" if ends || word != "end" => {
                Err(format!("'{}' without a block to close", word))
            }
            _ => Ok(None),
        }
    }

    fn at_separator(&self) -> bool {
        matches!(
            self.peek(),
            None | Some(Tok::Sep) | Some(Tok::Newline) | Some(Tok::Sym(","))
        )
    }

    /// statements up to one of the stop words; returns them and the stop word
    fn block(&mut self, stops: &[&str], what: &str) -> R<(Block, String)> {
        let mut out = vec![];
        loop {
            while matches!(self.peek(), Some(Tok::Sep) | Some(Tok::Newline)) || self.is(",") {
                self.i += 1;
            }
            match self.peek() {
                None => return Err(format!("{}: missing end", what)),
                Some(Tok::Ident(w)) if stops.contains(&w.as_str()) => {
                    let w = w.clone();
                    self.i += 1;
                    return Ok((out, w));
                }
                _ => {}
            }
            let st = self.statement()?;
            let quiet = self.semi.get(self.i).copied().unwrap_or(false);
            out.push((st, quiet));
            match self.peek() {
                None | Some(Tok::Sep) | Some(Tok::Newline) | Some(Tok::Sym(",")) => {}
                Some(Tok::Ident(w)) if stops.contains(&w.as_str()) => {}
                Some(_) => return Err(format!("unexpected input {}", self.here())),
            }
        }
    }

    /// name(i, j) = value or name[i, j] = value
    fn try_set_index(&mut self, name: &str) -> R<Option<Stmt>> {
        let open = match self.t.get(self.i + 1) {
            Some(t) if !t.space_before => match t.tok {
                Tok::Sym("(") => ")",
                Tok::Sym("[") => "]",
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };
        let save = self.i;
        self.i += 2;
        let save_m = self.in_matrix;
        self.in_matrix = 0;
        let mut idx = vec![];
        let ok = (|| -> R<bool> {
            if !self.is(open) {
                idx.push(self.expr()?);
                while self.eat(",") {
                    idx.push(self.expr()?);
                }
            }
            Ok(self.eat(open) && self.eat("="))
        })();
        self.in_matrix = save_m;
        match ok {
            Ok(true) => Ok(Some(Stmt::SetIndex(
                name.to_string(),
                idx,
                open == "]",
                self.expr()?,
            ))),
            _ => {
                self.i = save;
                Ok(None)
            }
        }
    }

    /// f(x, y) followed by := or = : the parameter names and the position after the =
    fn definition_head(&self) -> Option<(Vec<String>, usize)> {
        let t = |k: usize| self.t.get(self.i + k).map(|t| &t.tok);
        if !matches!(t(1), Some(Tok::Sym("(")))
            || self.t.get(self.i + 1).is_some_and(|t| t.space_before)
        {
            return None;
        }
        let mut k = 2;
        let mut params = vec![];
        loop {
            match t(k) {
                Some(Tok::Ident(p)) => params.push(p.clone()),
                _ => return None,
            }
            k += 1;
            match t(k) {
                Some(Tok::Sym(",")) => k += 1,
                Some(Tok::Sym(")")) => break,
                _ => return None,
            }
        }
        match t(k + 1) {
            Some(Tok::Sym("=")) | Some(Tok::Sym(":=")) => Some((params, self.i + k + 2)),
            _ => None,
        }
    }

    fn try_name_list(&mut self) -> Option<Vec<String>> {
        self.eat("[");
        let mut names = vec![];
        loop {
            match self.peek() {
                Some(Tok::Ident(n)) => {
                    names.push(n.clone());
                    self.i += 1;
                }
                _ => return None,
            }
            if self.eat(",") {
                continue;
            }
            if self.eat("]") {
                return Some(names);
            }
            return None;
        }
    }

    fn expr(&mut self) -> R<Node> {
        let a = self.binary(0)?;
        if self.is(":") {
            self.i += 1;
            let b = self.binary(0)?;
            if self.eat(":") {
                let c = self.binary(0)?;
                return Ok(Node::Range(Box::new(a), Some(Box::new(b)), Box::new(c)));
            }
            return Ok(Node::Range(Box::new(a), None, Box::new(b)));
        }
        Ok(a)
    }

    fn infix(&self) -> Option<(Op, u8, bool)> {
        // (op, precedence, right associative)
        let s = match self.peek() {
            Some(Tok::Sym(s)) => *s,
            _ => return None,
        };
        // inside [ ], "a -b" is two elements (as in Matlab); "a - b" and "a-b" subtract
        if self.in_matrix > 0 && (s == "-" || s == "+") && self.space_before() {
            let next_space = self.t.get(self.i + 1).is_some_and(|t| t.space_before);
            if !next_space {
                return None;
            }
        }
        Some(match s {
            "||" => (Op::Or, 1, false),
            "&&" => (Op::And, 2, false),
            "==" => (Op::Eq, 3, false),
            "~=" | "!=" => (Op::Ne, 3, false),
            "<" => (Op::Lt, 3, false),
            ">" => (Op::Gt, 3, false),
            "<=" => (Op::Le, 3, false),
            ">=" => (Op::Ge, 3, false),
            "+" => (Op::Add, 4, false),
            "-" => (Op::Sub, 4, false),
            "*" => (Op::Mul, 5, false),
            "/" => (Op::Div, 5, false),
            "\\" => (Op::Solve, 5, false),
            ".*" => (Op::EMul, 5, false),
            "./" => (Op::EDiv, 5, false),
            "^" => (Op::Pow, 7, true),
            ".^" => (Op::EPow, 7, true),
            _ => return None,
        })
    }

    fn binary(&mut self, min: u8) -> R<Node> {
        let mut lhs = self.unary()?;
        while let Some((op, prec, right)) = self.infix() {
            if prec < min {
                break;
            }
            self.i += 1;
            let rhs = if op == Op::Pow || op == Op::EPow {
                self.unary_pow()?
            } else {
                self.binary(if right { prec } else { prec + 1 })?
            };
            lhs = Node::Bin(op, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    /// exponent: allows a sign (2^-1) and is right associative (2^3^2 = 2^9)
    fn unary_pow(&mut self) -> R<Node> {
        if self.eat("-") {
            return Ok(Node::Neg(Box::new(self.unary_pow()?)));
        }
        let base = self.postfix()?;
        for (s, op) in [("^", Op::Pow), (".^", Op::EPow)] {
            if self.eat(s) {
                return Ok(Node::Bin(op, Box::new(base), Box::new(self.unary_pow()?)));
            }
        }
        Ok(base)
    }

    fn unary(&mut self) -> R<Node> {
        if self.eat("-") {
            // binds looser than ^ : -x^2 = -(x^2)
            let operand = self.binary(6)?;
            return Ok(Node::Neg(Box::new(operand)));
        }
        if self.eat("+") {
            return self.binary(6);
        }
        if self.eat("!") {
            let operand = self.binary(6)?;
            return Ok(Node::Call("not".into(), vec![operand]));
        }
        self.postfix()
    }

    fn postfix(&mut self) -> R<Node> {
        let mut e = self.primary()?;
        loop {
            if self.is("'") && !self.space_before() {
                self.i += 1;
                e = Node::Transpose(Box::new(e));
            } else if self.is(".'") && !self.space_before() {
                self.i += 1;
                e = Node::Call("transpose".into(), vec![e]);
            } else if self.is("[") && !self.space_before() {
                self.i += 1;
                let save = self.in_matrix;
                self.in_matrix = 0;
                let mut idx = vec![self.expr()?];
                while self.eat(",") {
                    idx.push(self.expr()?);
                }
                self.in_matrix = save;
                self.expect("]")?;
                e = Node::Index(Box::new(e), idx);
            } else {
                break;
            }
        }
        // 3 m/s, [1 2 3] m, (a + b) kg: a unit after a number, a matrix or
        // parentheses (outside [ ], where whitespace separates elements)
        if self.in_matrix == 0
            && self.i > 0
            && matches!(self.t[self.i - 1].tok, Tok::Num(_) | Tok::Sym(")") | Tok::Sym("]"))
            && self.unit_ahead()
        {
            let u = self.unit_text()?;
            e = Node::Call("unit".into(), vec![e, Node::Str(u)]);
        }
        Ok(e)
    }

    /// the next token is a unit name used as a unit (not assigned or called)
    fn unit_ahead(&self) -> bool {
        let Some(Tok::Ident(u)) = self.peek() else { return false };
        if !crate::units::is_unit(u) {
            return false;
        }
        match self.t.get(self.i + 1) {
            Some(t) if matches!(t.tok, Tok::Sym("=") | Tok::Sym(":=")) => false,
            Some(t) if matches!(t.tok, Tok::Sym("(") | Tok::Sym("[")) && !t.space_before => false,
            _ => true,
        }
    }

    /// a unit expression such as kg*m/s^2 or J/(kg*K), as text; compound
    /// units are written without spaces around * and /
    fn unit_text(&mut self) -> R<String> {
        let mut s = self.unit_factor()?;
        loop {
            let op = match self.peek() {
                Some(Tok::Sym(o)) if (*o == "*" || *o == "/") && !self.space_before() => *o,
                _ => break,
            };
            let next = self.t.get(self.i + 1);
            let ok = next.is_some_and(|t| {
                !t.space_before
                    && match &t.tok {
                        Tok::Ident(u) => crate::units::is_unit(u),
                        Tok::Sym("(") => matches!(self.t.get(self.i + 2).map(|t| &t.tok), Some(Tok::Ident(u)) if crate::units::is_unit(u)),
                        _ => false,
                    }
            });
            if !ok {
                break;
            }
            self.i += 1;
            s.push_str(op);
            s.push_str(&self.unit_factor()?);
        }
        Ok(s)
    }

    fn unit_factor(&mut self) -> R<String> {
        let mut s = match self.peek().cloned() {
            Some(Tok::Ident(u)) if crate::units::is_unit(&u) => {
                self.i += 1;
                u
            }
            Some(Tok::Sym("(")) => {
                self.i += 1;
                let inner = self.unit_text()?;
                self.expect(")")?;
                format!("({})", inner)
            }
            _ => return Err(format!("expected a unit {}", self.here())),
        };
        if self.is("^") && !self.space_before() {
            let save = self.i;
            self.i += 1;
            let neg = self.eat("-");
            match self.peek().cloned() {
                Some(Tok::Num(n)) if n.chars().all(|c| c.is_ascii_digit()) => {
                    self.i += 1;
                    s = format!("{}^{}{}", s, if neg { "-" } else { "" }, n);
                }
                _ => self.i = save,
            }
        }
        Ok(s)
    }

    /// the second argument of convert(q, km/h) when it is a unit expression
    fn unit_target(&mut self) -> Option<Node> {
        let save = self.i;
        if !self.unit_ahead() && !self.is("(") {
            return None;
        }
        match self.unit_text() {
            Ok(u) if self.is(")") || self.is(",") => {
                Some(Node::Call("unit".into(), vec![Node::Num("1".into()), Node::Str(u)]))
            }
            _ => {
                self.i = save;
                None
            }
        }
    }

    fn primary(&mut self) -> R<Node> {
        let tok = self.peek().cloned().ok_or("unexpected end of input")?;
        self.i += 1;
        match tok {
            Tok::Num(s) => Ok(Node::Num(s)),
            Tok::Str(s) => Ok(Node::Str(s)),
            Tok::Ident(name) => {
                if let Some(Tok::Raw(r)) = self.peek().cloned() {
                    self.i += 1;
                    return Ok(Node::Call(name, vec![Node::Str(r)]));
                }
                if self.is("(") && !self.space_before() {
                    self.i += 1;
                    let save = self.in_matrix;
                    self.in_matrix = 0;
                    let mut args = vec![];
                    if !self.is(")") {
                        args.push(self.expr()?);
                        while self.eat(",") {
                            // convert(q, km/h), value(q, km/h): the target is read as units
                            if (name == "convert" || name == "value") && args.len() == 1 {
                                if let Some(u) = self.unit_target() {
                                    args.push(u);
                                    continue;
                                }
                            }
                            args.push(self.expr()?);
                        }
                    }
                    self.in_matrix = save;
                    self.expect(")")?;
                    return Ok(Node::Call(name, args));
                }
                Ok(Node::Ident(name))
            }
            Tok::Sym("(") => {
                let save = self.in_matrix;
                self.in_matrix = 0;
                let e = self.expr()?;
                self.in_matrix = save;
                self.expect(")")?;
                Ok(e)
            }
            Tok::Sym("[") => self.matrix(),
            Tok::Sym("{") => self.list(),
            t => Err(format!("unexpected {}", describe(&t))),
        }
    }

    /// {a, b, c} or {name: value, ...}
    fn list(&mut self) -> R<Node> {
        let save = self.in_matrix;
        self.in_matrix = 0;
        let skip = |p: &mut P| {
            while matches!(p.peek(), Some(Tok::Newline)) {
                p.i += 1;
            }
        };
        skip(self);
        let record = matches!(self.peek(), Some(Tok::Ident(_)))
            && matches!(self.t.get(self.i + 1).map(|t| &t.tok), Some(Tok::Sym(":")));
        let mut items = vec![];
        let mut fields = vec![];
        while !self.eat("}") {
            if self.peek().is_none() {
                return Err("missing }".into());
            }
            if record {
                let name = match self.peek().cloned() {
                    Some(Tok::Ident(n)) => n,
                    _ => return Err(format!("expected a field name {}", self.here())),
                };
                self.i += 1;
                self.expect(":")?;
                fields.push((name, self.binary(0)?));
            } else {
                items.push(self.expr()?);
            }
            skip(self);
            if !self.eat(",") && !self.is("}") {
                return Err(format!("expected , or }} {}", self.here()));
            }
            skip(self);
        }
        self.in_matrix = save;
        Ok(if record {
            Node::Record(fields)
        } else {
            Node::List(items)
        })
    }

    fn matrix(&mut self) -> R<Node> {
        self.in_matrix += 1;
        let mut rows: Vec<Vec<Node>> = vec![];
        let mut row: Vec<Node> = vec![];
        loop {
            match self.peek() {
                None => return Err("missing ]".into()),
                Some(Tok::Sym("]")) => {
                    self.i += 1;
                    break;
                }
                Some(Tok::Sym(";")) | Some(Tok::Newline) => {
                    self.i += 1;
                    if !row.is_empty() {
                        rows.push(std::mem::take(&mut row));
                    }
                }
                Some(Tok::Sym(",")) => {
                    self.i += 1;
                }
                _ => row.push(self.expr()?),
            }
        }
        if !row.is_empty() {
            rows.push(row);
        }
        self.in_matrix -= 1;
        if rows.iter().any(|r| r.len() != rows[0].len()) {
            return Err("not all rows have the same size".into());
        }
        Ok(Node::Matrix(rows))
    }
}

fn describe(t: &Tok) -> String {
    match t {
        Tok::Num(s) | Tok::Ident(s) => format!("'{}'", s),
        Tok::Str(s) | Tok::Raw(s) => format!("\"{}\"", s),
        Tok::Sym(s) => format!("'{}'", s),
        Tok::Sep | Tok::Newline => "end of statement".into(),
    }
}

/// Whether input is unfinished: a `(` or `[` is still open, or the last line
/// ends in `\` (outside strings and `//` comments). The prompt keeps reading
/// lines while this holds.
pub fn needs_more(src: &str) -> bool {
    let (depth, cont) = scan(src);
    depth > 0 || cont || open_blocks(src) > 0
}

/// how many for/while/if/function blocks are still open (an `if(c, a, b)`
/// with commas is the function, not a block)
pub fn open_blocks(src: &str) -> i32 {
    let c: Vec<char> = src.chars().collect();
    let (mut i, mut depth, mut blocks) = (0, 0i32, 0i32);
    while i < c.len() {
        let ch = c[i];
        if ch == '"' {
            i += 1;
            while i < c.len() && c[i] != '"' && c[i] != '\n' {
                i += 1;
            }
            i += 1;
            continue;
        }
        if ch == '/' && c.get(i + 1) == Some(&'/') {
            while i < c.len() && c[i] != '\n' {
                i += 1;
            }
            continue;
        }
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ => {}
        }
        if ch.is_alphabetic() || ch == '_' {
            let start = i;
            while i < c.len() && (c[i].is_alphanumeric() || c[i] == '_' || c[i] == '.') {
                i += 1;
            }
            let prev_dot = start > 0 && c[start - 1] == '.';
            let w: String = c[start..i].iter().collect();
            if depth == 0 && !prev_dot {
                if w == "end" {
                    blocks -= 1;
                } else if OPENERS.contains(&w.as_str()) {
                    let is_call = w == "if" && {
                        // if( ... , ... ): the function form
                        let mut j = i;
                        let mut d = 0;
                        let mut comma = false;
                        if c.get(j) == Some(&'(') {
                            while j < c.len() {
                                match c[j] {
                                    '(' | '[' | '{' => d += 1,
                                    ')' | ']' | '}' => {
                                        d -= 1;
                                        if d == 0 {
                                            break;
                                        }
                                    }
                                    ',' if d == 1 => comma = true,
                                    _ => {}
                                }
                                j += 1;
                            }
                        }
                        comma
                    };
                    if !is_call {
                        blocks += 1;
                    }
                }
            }
            continue;
        }
        i += 1;
    }
    blocks
}

/// (open brackets, whether the text ends in a continuation `\`)
fn scan(src: &str) -> (i32, bool) {
    let mut depth = 0;
    let mut in_str = false;
    let mut cont = false;
    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        if !c.is_whitespace() {
            cont = false;
        }
        match c {
            '"' => in_str = !in_str,
            '\n' => in_str = false,
            _ if in_str => {}
            '/' if chars.peek() == Some(&'/') => {
                while chars.peek().is_some_and(|&c| c != '\n') {
                    chars.next();
                }
            }
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            '\\' => cont = true,
            _ => {}
        }
    }
    (depth, cont)
}

/// Split a script into pieces that each hold whole statements, with the line
/// number (from 1) each piece starts on; a piece runs on while brackets are
/// open or a line ends in `\`, so errors can name the line they are on
pub fn statement_chunks(src: &str) -> Vec<(usize, String)> {
    let mut out = vec![];
    let mut buf = String::new();
    let mut start = 1;
    for (n, line) in src.lines().enumerate() {
        if buf.is_empty() {
            start = n + 1;
        }
        buf.push_str(line);
        buf.push('\n');
        if !needs_more(&buf) {
            let piece = std::mem::take(&mut buf);
            if !piece.trim().is_empty() {
                out.push((start, piece));
            }
        }
    }
    if !buf.trim().is_empty() {
        out.push((start, buf));
    }
    out
}

/// Parse a program: statements separated by `;` or newlines.
pub fn parse(src: &str) -> Result<Vec<Stmt>, String> {
    Ok(parse_quiet(src)?.into_iter().map(|(s, _)| s).collect())
}

/// Parse a program; each statement comes with whether it ends in `;`
/// (which hides its result in notebooks and scripts)
pub fn parse_quiet(src: &str) -> Result<Vec<(Stmt, bool)>, String> {
    let toks = lex(src)?;
    let mut semi = vec![];
    // newlines outside brackets separate statements
    let mut t: Vec<Token> = vec![];
    // inside ( ) a newline is just space, so a long call can span lines;
    // inside [ ] it separates matrix rows
    let mut open: Vec<&str> = vec![];
    for tok in toks {
        match &tok.tok {
            Tok::Sym(b @ ("(" | "[" | "{")) => open.push(b),
            Tok::Sym(")") | Tok::Sym("]") | Tok::Sym("}") => {
                open.pop();
            }
            Tok::Newline if open.last() == Some(&"(") => continue,
            _ => {}
        }
        let depth = open.len();
        let sep = depth == 0 && matches!(tok.tok, Tok::Newline | Tok::Sym(";"));
        semi.push(sep && matches!(tok.tok, Tok::Sym(";")));
        t.push(if sep {
            Token {
                tok: Tok::Sep,
                space_before: tok.space_before,
            }
        } else {
            tok
        });
    }
    let mut p = P {
        t,
        semi: semi.clone(),
        i: 0,
        in_matrix: 0,
    };
    let mut out = vec![];
    loop {
        while matches!(p.peek(), Some(Tok::Sep)) {
            p.i += 1;
        }
        if p.peek().is_none() {
            break;
        }
        let st = p.statement()?;
        let quiet = semi.get(p.i).copied().unwrap_or(false);
        out.push((st, quiet));
        p.skip_newlines();
        match p.peek() {
            None => break,
            Some(Tok::Sep) => continue,
            Some(_) => return Err(format!("unexpected input {}", p.here())),
        }
    }
    Ok(out)
}

/// A node written back as source text (for plot labels): (text, precedence)
fn src_prec(n: &Node) -> (String, u8) {
    let wrap = |s: (String, u8), min: u8| if s.1 < min { format!("({})", s.0) } else { s.0 };
    match n {
        Node::Num(s) | Node::Ident(s) => (s.clone(), 9),
        Node::Str(s) => (format!("\"{}\"", s), 9),
        Node::Neg(a) => (format!("-{}", wrap(src_prec(a), 7)), 6),
        Node::Bin(op, a, b) => {
            let (sym, p, right) = match op {
                Op::Or => ("||", 1, false),
                Op::And => ("&&", 2, false),
                Op::Eq => (" == ", 3, false),
                Op::Ne => (" ~= ", 3, false),
                Op::Lt => (" < ", 3, false),
                Op::Gt => (" > ", 3, false),
                Op::Le => (" <= ", 3, false),
                Op::Ge => (" >= ", 3, false),
                Op::Add => ("+", 4, false),
                Op::Sub => ("-", 4, false),
                Op::Mul => ("*", 5, false),
                Op::Div => ("/", 5, false),
                Op::Solve => ("\\", 5, false),
                Op::EMul => (".*", 5, false),
                Op::EDiv => ("./", 5, false),
                Op::Pow => ("^", 7, true),
                Op::EPow => (".^", 7, true),
            };
            let (l, mut r) = if right { (p + 1, p) } else { (p, p + 1) };
            // a*(-b) reads better than a*-b
            if matches!(**b, Node::Neg(_)) {
                r = r.max(7);
            }
            (
                format!("{}{}{}", wrap(src_prec(a), l), sym, wrap(src_prec(b), r)),
                p,
            )
        }
        Node::Range(a, s, b) => {
            let mut t = source(a);
            if let Some(s) = s {
                t = format!("{}:{}", t, source(s));
            }
            (format!("{}:{}", t, source(b)), 0)
        }
        Node::Call(f, args) if f == "unit" && args.len() == 2 && matches!(args[1], Node::Str(_)) => {
            let Node::Str(u) = &args[1] else { unreachable!() };
            (format!("{} {}", wrap(src_prec(&args[0]), 9), u), 6)
        }
        Node::Call(f, args) => (
            format!(
                "{}({})",
                f,
                args.iter().map(source).collect::<Vec<_>>().join(", ")
            ),
            9,
        ),
        Node::Index(a, ix) => (
            format!(
                "{}[{}]",
                wrap(src_prec(a), 9),
                ix.iter().map(source).collect::<Vec<_>>().join(", ")
            ),
            9,
        ),
        Node::Matrix(rows) => (
            format!(
                "[{}]",
                rows.iter()
                    .map(|r| r.iter().map(source).collect::<Vec<_>>().join(" "))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
            9,
        ),
        Node::Transpose(a) => (format!("{}'", wrap(src_prec(a), 9)), 9),
        Node::List(v) => (
            format!("{{{}}}", v.iter().map(source).collect::<Vec<_>>().join(", ")),
            9,
        ),
        Node::Record(v) => (
            format!(
                "{{{}}}",
                v.iter()
                    .map(|(k, x)| format!("{}: {}", k, source(x)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            9,
        ),
    }
}

pub fn source(n: &Node) -> String {
    src_prec(n).0
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn precedence() {
        let s = parse("-x^2+3*y").unwrap();
        assert_eq!(
            s[0],
            Stmt::Expr(Node::Bin(
                Op::Add,
                Box::new(Node::Neg(Box::new(Node::Bin(
                    Op::Pow,
                    Box::new(Node::Ident("x".into())),
                    Box::new(Node::Num("2".into()))
                )))),
                Box::new(Node::Bin(
                    Op::Mul,
                    Box::new(Node::Num("3".into())),
                    Box::new(Node::Ident("y".into()))
                ))
            ))
        );
    }
    #[test]
    fn statements_and_matrices() {
        let s = parse("A=[4 5 6;6 5 4];norm(A-A,1)\n[L,U]=lu(A)").unwrap();
        assert_eq!(s.len(), 3);
        assert!(
            matches!(&s[0], Stmt::Assign(n, Node::Matrix(rows)) if n == "A" && rows.len() == 2)
        );
        assert!(matches!(&s[2], Stmt::MultiAssign(n, _) if n.len() == 2));
        assert!(
            matches!(&parse("[a -b]").unwrap()[0], Stmt::Expr(Node::Matrix(r)) if r[0].len() == 2)
        );
        assert!(
            matches!(&parse("[a - b]").unwrap()[0], Stmt::Expr(Node::Matrix(r)) if r[0].len() == 1)
        );
        assert!(
            matches!(&parse("[a-b]").unwrap()[0], Stmt::Expr(Node::Matrix(r)) if r[0].len() == 1)
        );
        assert!(
            matches!(&parse("load(examples/a.gg)").unwrap()[0], Stmt::Command(c, Some(p)) if c == "load" && p == "examples/a.gg")
        );
    }
}
