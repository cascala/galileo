//! Rich output: MIME bundles for notebooks (text, LaTeX, SVG, Plotly JSON).

use crate::eval::Output;
use crate::expr::{Expr, Obj};
use crate::latex::latex;
use crate::print::{show, show_value};

/// (MIME type, content) pairs; JSON types hold JSON text
pub type Bundle = Vec<(&'static str, String)>;

fn typeset(e: &Expr) -> bool {
    match e {
        Expr::Add(_)
        | Expr::Mul(_)
        | Expr::Pow(..)
        | Expr::Sym(_)
        | Expr::Func(..)
        | Expr::Apply(..) => true,
        Expr::Num(n) => !n.is_integer() && n.is_exact(),
        Expr::Matrix(m) => m.iter().flatten().any(|x| !x.is_number()),
        _ => false,
    }
}

/// The bundle for a value; `None` when plain text is all there is
pub fn bundle(e: &Expr) -> Option<Bundle> {
    match e {
        Expr::Obj(o) => Some(match &**o {
            Obj::Figure(f) => vec![
                ("text/plain", f.ascii()),
                ("image/svg+xml", f.svg()),
                ("application/vnd.plotly.v1+json", f.plotly()),
            ],
            Obj::Figure3D(f) => vec![
                ("text/plain", f.text()),
                ("image/svg+xml", f.svg()),
                ("application/vnd.plotly.v1+json", f.plotly()),
            ],
            Obj::Metric(m) => vec![
                ("text/plain", m.text()),
                ("text/latex", m.latex()),
                ("text/markdown", m.latex()),
            ],
            Obj::Components(c) => vec![
                ("text/plain", c.text()),
                ("text/latex", c.latex()),
                ("text/markdown", c.markdown()),
            ],
            Obj::Equations(q) => vec![
                ("text/plain", q.text()),
                ("text/latex", q.latex()),
                ("text/markdown", q.markdown()),
            ],
            Obj::Group(g) => vec![("text/plain", g.name.clone())],
            Obj::Graph(g) => vec![("text/plain", g.name.clone()), ("image/svg+xml", g.svg())],
            Obj::List(_) | Obj::Record(_) => vec![("text/plain", o.text())],
            Obj::Ring(_) | Obj::Elem(_) | Obj::QRing(_) | Obj::QElem(_) | Obj::Quat(_) => vec![("text/plain", o.text())],
            // the text of a report is its explanation (a note); it shows its value
            Obj::Report(r) => {
                let v = r.shown_value()?;
                return bundle(&v).or_else(|| Some(vec![("text/plain", show_value(&v))]));
            }
        }),
        _ if typeset(e) => Some(vec![
            ("text/plain", show(e)),
            ("text/latex", format!("$${}$$", latex(e))),
        ]),
        _ => None,
    }
}

/// What a statement shows: rich data, plain text, or nothing
pub enum Shown {
    Rich(Bundle),
    Text(String),
    Nothing,
}

pub fn shown(o: &Output) -> Shown {
    if let Output::Value(Expr::Obj(ob)) = o {
        if let Obj::Report(r) = &**ob {
            if r.shown_value().is_none() {
                return Shown::Nothing;
            }
        }
    }
    match o {
        Output::Value(e) => match bundle(e) {
            Some(b) => Shown::Rich(b),
            None => Shown::Text(show_value(e)),
        },
        Output::Nothing => Shown::Nothing,
        Output::Exit => Shown::Text(
            "exit is not available in a notebook; use Kernel > Shut Down instead".into(),
        ),
        other => match other.render() {
            s if s.is_empty() => Shown::Nothing,
            s => Shown::Text(s),
        },
    }
}
