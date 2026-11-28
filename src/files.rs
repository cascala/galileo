//! Data in and out: read(file), save(x, file), ls, latex(x).
//!
//! `read` turns a CSV table into a record of columns (or a matrix when there is
//! no header row); `save` picks the format from the file's extension: .csv/.tsv
//! tables, .png/.svg/.eps pictures of plots, .tex LaTeX, .gg Galileo that `load`
//! reads back, .txt text. Files go through [`crate::vfs`], so this works the same
//! on disk and in the browser.

use crate::eval::{R, Session};
use crate::expr::{Expr, Obj};
use crate::lang::{as_list, as_record, list, record};
use crate::num::Number;
use crate::parser::Node;
use crate::print::{show, show_value};
use crate::vfs;

/// The extension of a file name, lowercase ("" when there is none)
fn ext(name: &str) -> String {
    let base = name.rsplit('/').next().unwrap_or(name);
    match base.rfind('.') {
        Some(i) if i > 0 => base[i + 1..].to_ascii_lowercase(),
        _ => String::new(),
    }
}

/// The names in a folder as a list of strings; folders end in "/"
pub fn ls(dir: &str) -> R<Expr> {
    let dir = if dir.is_empty() { "." } else { dir };
    let names = vfs::list(dir).map_err(|e| format!("ls {}", e))?;
    Ok(list(names.into_iter().map(Expr::Str).collect()))
}

// ---- CSV ----

/// The fields of one CSV text: rows of cells. Quotes "..." may hold the
/// separator, new lines and doubled quotes "".
fn csv_rows(text: &str, sep: char) -> Vec<Vec<String>> {
    let mut rows = vec![];
    let mut row = vec![];
    let mut cell = String::new();
    let mut quoted = false;
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        if quoted {
            if c == '"' {
                if chars.peek() == Some(&'"') {
                    chars.next();
                    cell.push('"');
                } else {
                    quoted = false;
                }
            } else {
                cell.push(c);
            }
        } else if c == '"' && cell.trim().is_empty() {
            cell.clear();
            quoted = true;
        } else if c == sep {
            row.push(std::mem::take(&mut cell));
        } else if c == '\n' || c == '\r' {
            if c == '\r' && chars.peek() == Some(&'\n') {
                chars.next();
            }
            row.push(std::mem::take(&mut cell));
            if !(row.len() == 1 && row[0].trim().is_empty()) {
                rows.push(std::mem::take(&mut row));
            }
            row.clear();
        } else {
            cell.push(c);
        }
    }
    if !cell.is_empty() || !row.is_empty() {
        row.push(cell);
        if !(row.len() == 1 && row[0].trim().is_empty()) {
            rows.push(row);
        }
    }
    rows
}

/// The separator of a table: tab for .tsv, else whichever of , ; tab the first line has most of
fn guess_sep(text: &str, extension: &str) -> char {
    if extension == "tsv" {
        return '\t';
    }
    let first = text.lines().next().unwrap_or("");
    let count = |c: char| first.chars().filter(|&x| x == c).count();
    let (comma, semi, tab) = (count(','), count(';'), count('\t'));
    if tab > comma && tab >= semi {
        '\t'
    } else if semi > comma {
        ';'
    } else {
        ','
    }
}

/// A cell as a number: 3, -2, 0.5, 1e-3, 1.5E+6 (not "nan", "inf" or words)
fn number(cell: &str) -> Option<Expr> {
    let t = cell.trim();
    let t = t.strip_prefix('+').unwrap_or(t);
    let body = t.strip_prefix('-').unwrap_or(t);
    let (mant, exp) = match body.find(['e', 'E']) {
        Some(i) => (&body[..i], Some(&body[i + 1..])),
        None => (body, None),
    };
    let digits = mant.chars().filter(|c| c.is_ascii_digit()).count();
    let mant_ok = digits > 0
        && mant.chars().all(|c| c.is_ascii_digit() || c == '.')
        && mant.matches('.').count() <= 1;
    let exp_ok = exp.is_none_or(|e| {
        let e = e.strip_prefix(['+', '-']).unwrap_or(e);
        !e.is_empty() && e.chars().all(|c| c.is_ascii_digit())
    });
    if !(mant_ok && exp_ok) {
        return None;
    }
    if exp.is_none() && !mant.contains('.') {
        return Number::parse(t).map(Expr::Num);
    }
    t.parse::<f64>().ok().map(Expr::float)
}

/// A header cell as a field name: letters, digits and _, starting with a letter
fn field_name(h: &str, k: usize, taken: &[String]) -> String {
    let mut s: String = h
        .trim()
        .chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect();
    while s.contains("__") {
        s = s.replace("__", "_");
    }
    let s = s.trim_matches('_').to_string();
    let mut s = if s.is_empty() {
        format!("col{}", k + 1)
    } else if !s.chars().next().unwrap().is_alphabetic() {
        format!("c{}", s)
    } else {
        s
    };
    if taken.contains(&s) {
        let base = s.clone();
        let mut n = 2;
        while taken.contains(&s) {
            s = format!("{}_{}", base, n);
            n += 1;
        }
    }
    s
}

/// A table read from text: a record of columns when the first row is a header
/// (has a cell that is not a number), else a matrix
pub fn parse_table(text: &str, extension: &str) -> R<Expr> {
    let sep = guess_sep(text, extension);
    let text = text.strip_prefix('\u{feff}').unwrap_or(text);
    let rows = csv_rows(text, sep);
    if rows.is_empty() {
        return Ok(Expr::Matrix(vec![]));
    }
    let cell = |c: &str| number(c).unwrap_or_else(|| Expr::Str(c.trim().to_string()));
    let header = rows[0].iter().any(|c| number(c).is_none() && !c.trim().is_empty())
        && rows.len() > 1;
    if !header {
        let width = rows.iter().map(|r| r.len()).max().unwrap_or(0);
        let m: Vec<Vec<Expr>> = rows
            .iter()
            .map(|r| {
                (0..width)
                    .map(|j| r.get(j).map_or(Expr::float(f64::NAN), |c| missing_or(c, &cell)))
                    .collect()
            })
            .collect();
        return Ok(Expr::Matrix(m));
    }
    let names = &rows[0];
    let body = &rows[1..];
    let width = rows.iter().map(|r| r.len()).max().unwrap_or(0);
    let mut fields: Vec<(String, Expr)> = vec![];
    for j in 0..width {
        let taken: Vec<String> = fields.iter().map(|f| f.0.clone()).collect();
        let name = field_name(names.get(j).map_or("", |s| s), j, &taken);
        let cells: Vec<&str> = body.iter().map(|r| r.get(j).map_or("", |s| s)).collect();
        let numeric = cells.iter().all(|c| c.trim().is_empty() || number(c).is_some())
            && cells.iter().any(|c| !c.trim().is_empty());
        let col = if numeric {
            Expr::Matrix(vec![
                cells.iter().map(|c| missing_or(c, &cell)).collect(),
            ])
        } else {
            list(cells.iter().map(|c| Expr::Str(c.trim().to_string())).collect())
        };
        fields.push((name, col));
    }
    Ok(record(fields))
}

/// an empty cell is NaN (a missing value)
fn missing_or(c: &str, cell: &impl Fn(&str) -> Expr) -> Expr {
    if c.trim().is_empty() { Expr::float(f64::NAN) } else { cell(c) }
}

/// A value as one CSV cell: strings as they are (quoted when needed), numbers as Galileo shows them
fn csv_cell(e: &Expr, sep: char) -> String {
    let s = match e {
        Expr::Str(s) => s.clone(),
        Expr::Num(n) if n.to_f64().is_nan() && !n.is_exact() => String::new(),
        _ => show(e),
    };
    if s.contains(sep) || s.contains('"') || s.contains('\n') || s.starts_with(' ') || s.ends_with(' ') {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s
    }
}

/// The entries of a column: a vector (row or column), a list, or one value
fn column(e: &Expr) -> Vec<Expr> {
    if let Some(l) = as_list(e) {
        return l.clone();
    }
    match e {
        Expr::Matrix(m) if m.len() == 1 => m[0].clone(),
        Expr::Matrix(m) if m.iter().all(|r| r.len() == 1) => m.iter().map(|r| r[0].clone()).collect(),
        Expr::Matrix(m) => m.iter().flatten().cloned().collect(),
        _ => vec![e.clone()],
    }
}

/// A value as a CSV table: a matrix row by row, a record as named columns,
/// a list of lists or vectors as rows, a list or a single value as one column
pub fn table_text(e: &Expr, sep: char) -> R<(String, usize)> {
    let line = |cells: &[Expr]| {
        cells.iter().map(|c| csv_cell(c, sep)).collect::<Vec<_>>().join(&sep.to_string())
    };
    let rows: Vec<String> = if let Some(fields) = as_record(e) {
        let cols: Vec<Vec<Expr>> = fields.iter().map(|(_, v)| column(v)).collect();
        let n = cols.iter().map(|c| c.len()).max().unwrap_or(0);
        let mut rows = vec![fields
            .iter()
            .map(|(k, _)| csv_cell(&Expr::Str(k.clone()), sep))
            .collect::<Vec<_>>()
            .join(&sep.to_string())];
        for i in 0..n {
            let cells: Vec<Expr> = cols
                .iter()
                .map(|c| c.get(i).cloned().unwrap_or(Expr::Str(String::new())))
                .collect();
            rows.push(line(&cells));
        }
        rows
    } else if let Some(items) = as_list(e) {
        if items.iter().all(|x| as_list(x).is_some() || matches!(x, Expr::Matrix(_))) && !items.is_empty() {
            items.iter().map(|x| line(&column(x))).collect()
        } else {
            items.iter().map(|x| line(std::slice::from_ref(x))).collect()
        }
    } else {
        match e {
            Expr::Matrix(m) => m.iter().map(|r| line(r)).collect(),
            Expr::Obj(o) => {
                return Err(format!("a {} cannot be saved as a table", o.kind()));
            }
            _ => vec![line(std::slice::from_ref(e))],
        }
    };
    let n = rows.len() - usize::from(as_record(e).is_some());
    let mut text = rows.join("\n");
    text.push('\n');
    Ok((text, n))
}

// ---- pictures ----

/// A plot as a picture file: (bytes, what it is)
pub(crate) fn picture(e: &Expr, extension: &str) -> R<Vec<u8>> {
    let (svg, raster) = match e {
        Expr::Obj(o) => match &**o {
            Obj::Figure(f) => (f.svg(), f.image.clone()),
            Obj::Figure3D(f) => (f.svg(), None),
            Obj::Graph(g) => (g.svg(), None),
            other => {
                return Err(format!(
                    "only plots and graphs can be saved as .{}, not a {}",
                    extension,
                    other.kind()
                ));
            }
        },
        other => {
            return Err(format!(
                "only plots and graphs can be saved as .{}; {} is not one",
                extension,
                show(other)
            ));
        }
    };
    Ok(match extension {
        "svg" => svg.into_bytes(),
        "eps" => crate::export::eps(&svg, raster.as_ref())?.into_bytes(),
        _ => crate::export::png(&svg, raster.as_ref(), 2.0)?,
    })
}

impl Session {
    /// read, save, ls(dir), latex. None when `f` is not one of them.
    pub(crate) fn files_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        match f {
            "read" | "save" | "ls" | "latex" => {}
            _ => return None,
        }
        Some(self.files_call_inner(f, args))
    }

    fn file_arg(&mut self, n: &Node, f: &str) -> R<String> {
        match self.eval(n)? {
            Expr::Str(s) if !s.trim().is_empty() => Ok(s),
            other => Err(format!(
                "{}: the file name must be a string in quotes, like \"data.csv\" (got {})",
                f,
                show(&other)
            )),
        }
    }

    fn files_call_inner(&mut self, f: &str, args: &[Node]) -> R<Expr> {
        match f {
            "ls" => match args {
                [] => ls("."),
                [d] => {
                    let d = self.file_arg(d, "ls")?;
                    ls(&d)
                }
                _ => Err("usage: ls or ls(\"folder\")".into()),
            },
            "latex" => match args {
                [x] => {
                    let v = self.eval(x)?;
                    Ok(Expr::Str(latex_of(&v)?))
                }
                _ => Err("usage: latex(x)".into()),
            },
            "read" => {
                let [n] = args else {
                    return Err("usage: read(\"file\"), for example read(\"data.csv\")".into());
                };
                let name = self.file_arg(n, "read")?;
                let bytes = vfs::read(&name).map_err(|e| format!("read {}", e))?;
                let text = String::from_utf8(bytes).map_err(|_| {
                    format!("read {}: not a text file (read takes .csv, .tsv and text files)", name)
                })?;
                let e = ext(&name);
                if matches!(e.as_str(), "csv" | "tsv") {
                    let t = parse_table(&text, &e)?;
                    self.note(table_note(&t, &name));
                    Ok(t)
                } else {
                    Ok(Expr::Str(text))
                }
            }
            "save" => self.save(args),
            _ => unreachable!(),
        }
    }

    fn save(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: save(x, \"file\") with .csv, .tsv, .png, .svg, .eps, .tex, .gg or .txt; save(\"file.gg\") saves all your variables";
        let (value, name) = match args {
            [file] => (None, self.file_arg(file, "save")?),
            [x, file] => {
                let label = match x {
                    Node::Ident(n) => Some(n.clone()),
                    _ => None,
                };
                (Some((label, self.eval(x)?)), self.file_arg(file, "save")?)
            }
            _ => return Err(usage.into()),
        };
        let e = ext(&name);
        let (bytes, what) = match (e.as_str(), &value) {
            ("gg", None) => {
                let (text, n) = self.session_text();
                (text.into_bytes(), format!("{} definitions", n))
            }
            (_, None) => {
                return Err(format!(
                    "save(\"{}\"): give the value to save, as in save(x, \"{}\"); only .gg saves everything",
                    name, name
                ));
            }
            ("csv" | "tsv", Some((_, v))) => {
                let (t, n) = table_text(v, if e == "tsv" { '\t' } else { ',' })
                    .map_err(|m| format!("save {}: {}", name, m))?;
                (t.into_bytes(), format!("{} rows", n))
            }
            ("png" | "svg" | "eps", Some((_, v))) => (
                picture(v, &e).map_err(|m| format!("save {}: {}", name, m))?,
                "a picture".to_string(),
            ),
            ("tex", Some((_, v))) => (
                format!("\\[\n{}\n\\]\n", latex_of(v)?).into_bytes(),
                "LaTeX".to_string(),
            ),
            ("gg", Some((label, v))) => {
                if let Expr::Obj(o) = v {
                    if matches!(&**o, Obj::Figure(_) | Obj::Figure3D(_) | Obj::Graph(_)) {
                        return Err(format!(
                            "save {}: a {} has no Galileo text; save it as .png, .svg or .eps",
                            name,
                            o.kind()
                        ));
                    }
                }
                let text = match label {
                    Some(l) => format!("{} = {}\n", l, show_value(v)),
                    None => format!("{}\n", show_value(v)),
                };
                (text.into_bytes(), "Galileo".to_string())
            }
            ("txt" | "md" | "", Some((_, v))) => {
                let mut t = match v {
                    Expr::Str(s) => s.clone(),
                    other => show_value(other),
                };
                if !t.ends_with('\n') {
                    t.push('\n');
                }
                (t.into_bytes(), "text".to_string())
            }
            _ => return Err(format!("save {}: unknown kind of file .{}; {}", name, e, usage)),
        };
        vfs::write(&name, &bytes).map_err(|m| format!("save {}", m))?;
        self.note(format!("Saved {} to {} ({} bytes).", what, name, bytes.len()));
        Ok(Expr::Str(name))
    }

    /// Every variable you defined and every f(x) := ... function, as Galileo
    /// statements: (text, how many)
    fn session_text(&self) -> (String, usize) {
        let b = crate::eval::builtins();
        let mut lines = vec![];
        for (k, v) in &self.vars {
            if b.get(k) == Some(v) || k == "ans" {
                continue;
            }
            if let Expr::Obj(o) = v {
                if matches!(&**o, Obj::Figure(_) | Obj::Figure3D(_) | Obj::Graph(_)) {
                    lines.push(format!("// {}: a {}, not saved (save it as .png, .svg or .eps)", k, o.kind()));
                    continue;
                }
            }
            lines.push(format!("{} = {};", k, show_value(v)));
        }
        let mut n = lines.iter().filter(|l| !l.starts_with("//")).count();
        for (k, (p, body)) in &self.funcs {
            lines.push(format!("{}({}) := {};", k, p.join(", "), crate::parser::source(body)));
            n += 1;
        }
        if !self.procs.is_empty() {
            lines.push(format!(
                "// not saved: functions written with function ... end ({}); keep them in a script",
                self.procs.keys().cloned().collect::<Vec<_>>().join(", ")
            ));
        }
        (lines.join("\n") + "\n", n)
    }
}

/// LaTeX for a value (not for plots and other objects)
fn latex_of(v: &Expr) -> R<String> {
    match v {
        Expr::Obj(o) if as_list(v).is_none() && as_record(v).is_none() => {
            Err(format!("latex: a {} has no LaTeX form", o.kind()))
        }
        Expr::Str(s) => Ok(format!("\\text{{{}}}", s.replace('\\', "\\textbackslash{}").replace(['{', '}'], ""))),
        _ => {
            if let Some(l) = as_list(v) {
                let parts: R<Vec<String>> = l.iter().map(latex_of).collect();
                return Ok(format!("\\left\\{{ {} \\right\\}}", parts?.join(", ")));
            }
            if let Some(r) = as_record(v) {
                let parts: R<Vec<String>> = r
                    .iter()
                    .map(|(k, x)| latex_of(x).map(|t| format!("\\mathrm{{{}}}: {}", k.replace('_', "\\_"), t)))
                    .collect();
                return Ok(format!("\\left\\{{ {} \\right\\}}", parts?.join(", ")));
            }
            Ok(crate::latex::latex(v))
        }
    }
}

/// "Read 3 columns (time, temp, site) with 12 rows from data.csv."
fn table_note(t: &Expr, name: &str) -> String {
    if let Some(r) = as_record(t) {
        let n = r.first().map_or(0, |(_, c)| column(c).len());
        let names: Vec<&str> = r.iter().map(|(k, _)| k.as_str()).collect();
        return format!(
            "Read {} column{} ({}) with {} row{} from {}; a column is d.{}.",
            r.len(),
            if r.len() == 1 { "" } else { "s" },
            names.join(", "),
            n,
            if n == 1 { "" } else { "s" },
            name,
            names.first().copied().unwrap_or("name")
        );
    }
    if let Expr::Matrix(m) = t {
        return format!(
            "Read a {} x {} matrix from {} (no header row).",
            m.len(),
            m.first().map_or(0, |r| r.len()),
            name
        );
    }
    String::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables() {
        let t = parse_table("time,temp\n0,20.5\n1,21\n", "csv").unwrap();
        assert_eq!(crate::print::show_value(&t), "{time: [0, 1], temp: [20.5, 21]}");
        let m = parse_table("1;2\n3;4\n", "csv").unwrap();
        assert_eq!(show(&m), "[1, 2;\n 3, 4]");
        let q = parse_table("name,note\n\"Smith, J\",\"said \"\"hi\"\"\"\n", "csv").unwrap();
        let cols = as_record(&q).unwrap();
        assert_eq!(as_list(&cols[0].1).unwrap()[0], Expr::Str("Smith, J".into()));
        assert_eq!(as_list(&cols[1].1).unwrap()[0], Expr::Str("said \"hi\"".into()));
        let (text, n) = table_text(&t, ',').unwrap();
        assert_eq!((text.as_str(), n), ("time,temp\n0,20.5\n1,21\n", 2));
        assert_eq!(field_name("Temp (°C)", 0, &[]), "Temp_C");
        assert_eq!(field_name("2nd", 1, &[]), "c2nd");
    }
}
