//! Files: on disk in the terminal and in Jupyter, in memory in the browser.
//!
//! Every command that touches a file (`load`, `read`, `save`, `ls`, `cd`, `pwd`,
//! `rm`) goes through here, so a script that uses relative names runs the same
//! everywhere. In the browser the host (the playground or JupyterLite) puts the
//! user's files in with [`put`], and after each run takes the engine's own
//! writes and removals with [`take_changes`] to store them where the user can
//! see and download them.

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

type R<T> = Result<T, String>;

/// A change the engine made to the in-memory files, for the host to store
#[derive(Clone, Debug, PartialEq)]
pub enum Change {
    Write(String),
    Remove(String),
}

/// The browser's files: absolute paths ("/data.csv") to their bytes
#[derive(Default)]
struct MemFs {
    files: BTreeMap<String, Vec<u8>>,
    /// folders, "/" included
    dirs: BTreeSet<String>,
    cwd: String,
    /// files the host knows about but cannot hand over (notebooks), with why
    stubs: BTreeMap<String, String>,
    changes: Vec<Change>,
}

impl MemFs {
    fn new() -> MemFs {
        let mut m = MemFs { cwd: "/".into(), ..Default::default() };
        m.dirs.insert("/".into());
        m
    }

    fn abs(&self, p: &str) -> String {
        let joined = if p.starts_with('/') {
            p.to_string()
        } else {
            format!("{}/{}", self.cwd, p)
        };
        normalize(&joined)
    }

    fn add_parents(&mut self, p: &str) {
        let mut d = parent(p);
        while self.dirs.insert(d.clone()) {
            d = parent(&d);
        }
    }

    fn not_found(&self, p: &str, shown: &str) -> String {
        match self.stubs.get(p) {
            Some(why) => format!("{}: {}", shown, why),
            None if self.dirs.contains(p) => format!("{}: is a folder", shown),
            None => format!("{}: no such file", shown),
        }
    }
}

/// "/a/./b/../c" -> "/a/c"; always absolute, no trailing slash (except "/")
fn normalize(p: &str) -> String {
    let mut parts: Vec<&str> = vec![];
    for s in p.split('/') {
        match s {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            s => parts.push(s),
        }
    }
    format!("/{}", parts.join("/"))
}

fn parent(p: &str) -> String {
    match p.rfind('/') {
        Some(0) | None => "/".into(),
        Some(i) => p[..i].to_string(),
    }
}

thread_local! {
    static MEM: RefCell<Option<MemFs>> = const { RefCell::new(None) };
}

fn in_memory() -> bool {
    cfg!(target_arch = "wasm32") || MEM.with(|m| m.borrow().is_some())
}

fn mem<T>(f: impl FnOnce(&mut MemFs) -> T) -> T {
    MEM.with(|m| f(m.borrow_mut().get_or_insert_with(MemFs::new)))
}

/// Use in-memory files from now on (always so in the browser; tests use it natively)
pub fn use_memory() {
    mem(|_| ());
}

/// Read a whole file
pub fn read(p: &str) -> R<Vec<u8>> {
    if in_memory() {
        return mem(|m| {
            let a = m.abs(p);
            m.files.get(&a).cloned().ok_or_else(|| m.not_found(&a, p))
        });
    }
    std::fs::read(p).map_err(|e| format!("{}: {}", p, io_msg(&e)))
}

/// Write a whole file, replacing it if it exists
pub fn write(p: &str, data: &[u8]) -> R<()> {
    if in_memory() {
        return mem(|m| {
            let a = m.abs(p);
            if m.dirs.contains(&a) {
                return Err(format!("{}: is a folder", p));
            }
            if !m.dirs.contains(&parent(&a)) {
                return Err(format!("{}: no such folder {}", p, parent(&a)));
            }
            m.files.insert(a.clone(), data.to_vec());
            m.stubs.remove(&a);
            m.changes.retain(|c| !matches!(c, Change::Write(x) | Change::Remove(x) if *x == a));
            m.changes.push(Change::Write(a));
            Ok(())
        });
    }
    std::fs::write(p, data).map_err(|e| format!("{}: {}", p, io_msg(&e)))
}

/// Remove a file (not a folder)
pub fn remove(p: &str) -> R<()> {
    if in_memory() {
        return mem(|m| {
            let a = m.abs(p);
            if m.files.remove(&a).is_none() && m.stubs.remove(&a).is_none() {
                return Err(m.not_found(&a, p));
            }
            m.changes.retain(|c| !matches!(c, Change::Write(x) | Change::Remove(x) if *x == a));
            m.changes.push(Change::Remove(a));
            Ok(())
        });
    }
    if std::path::Path::new(p).is_dir() {
        return Err(format!("{}: is a folder", p));
    }
    std::fs::remove_file(p).map_err(|e| format!("{}: {}", p, io_msg(&e)))
}

/// Whether a file (not a folder) exists
pub fn is_file(p: &str) -> bool {
    if in_memory() {
        return mem(|m| {
            let a = m.abs(p);
            m.files.contains_key(&a) || m.stubs.contains_key(&a)
        });
    }
    std::path::Path::new(p).is_file()
}

/// The names in a folder, sorted; folders end in "/"
pub fn list(dir: &str) -> R<Vec<String>> {
    if in_memory() {
        return mem(|m| {
            let d = m.abs(dir);
            if !m.dirs.contains(&d) {
                return Err(m.not_found(&d, dir));
            }
            let pre = if d == "/" { "/".to_string() } else { format!("{}/", d) };
            let child = |p: &String| -> Option<String> {
                let rest = p.strip_prefix(&pre)?;
                (!rest.is_empty() && !rest.contains('/')).then(|| rest.to_string())
            };
            let mut out: Vec<String> = m.dirs.iter().filter_map(child).map(|n| n + "/").collect();
            out.extend(m.files.keys().chain(m.stubs.keys()).filter_map(child));
            out.sort();
            out.dedup();
            Ok(out)
        });
    }
    let mut out = vec![];
    for e in std::fs::read_dir(dir).map_err(|e| format!("{}: {}", dir, io_msg(&e)))? {
        let e = e.map_err(|e| e.to_string())?;
        let mut name = e.file_name().to_string_lossy().into_owned();
        if e.path().is_dir() {
            name.push('/');
        }
        out.push(name);
    }
    out.sort();
    Ok(out)
}

/// The current folder
pub fn cwd() -> R<String> {
    if in_memory() {
        return Ok(mem(|m| m.cwd.clone()));
    }
    std::env::current_dir()
        .map(|p| p.display().to_string())
        .map_err(|e| io_msg(&e))
}

/// Change the current folder
pub fn cd(dir: &str) -> R<()> {
    if in_memory() {
        return mem(|m| {
            let d = m.abs(dir);
            if !m.dirs.contains(&d) {
                return Err(format!("cd {}: no such folder", dir));
            }
            m.cwd = d;
            Ok(())
        });
    }
    std::env::set_current_dir(dir).map_err(|e| format!("cd {}: {}", dir, io_msg(&e)))
}

/// The full path of a name, for telling files apart (a script loading itself)
pub fn full(p: &str) -> String {
    if in_memory() {
        return mem(|m| m.abs(p));
    }
    std::fs::canonicalize(p)
        .map(|c| c.display().to_string())
        .unwrap_or_else(|_| p.to_string())
}

/// The folder a path is in ("" for a bare name)
pub fn dir_of(p: &str) -> String {
    match p.rfind('/') {
        Some(0) => "/".into(),
        Some(i) => p[..i].to_string(),
        None => String::new(),
    }
}

/// A name relative to a folder
pub fn join(dir: &str, name: &str) -> String {
    if dir.is_empty() || name.starts_with('/') {
        name.to_string()
    } else if dir.ends_with('/') {
        format!("{}{}", dir, name)
    } else {
        format!("{}/{}", dir, name)
    }
}

fn io_msg(e: &std::io::Error) -> String {
    match e.kind() {
        std::io::ErrorKind::NotFound => "no such file or folder".into(),
        std::io::ErrorKind::PermissionDenied => "permission denied".into(),
        _ => {
            let s = e.to_string();
            s.split(" (os error").next().unwrap_or(&s).to_string()
        }
    }
}

// ---- for the host (browser) ----

/// Put a file in, from the host (an upload); makes its folders. Not reported back.
pub fn put(p: &str, data: Vec<u8>) {
    mem(|m| {
        let a = m.abs(p);
        m.add_parents(&a);
        m.stubs.remove(&a);
        m.files.insert(a, data);
    })
}

/// A file the host lists but does not hand over, and why it cannot be read
pub fn put_stub(p: &str, why: &str) {
    mem(|m| {
        let a = m.abs(p);
        m.add_parents(&a);
        m.files.remove(&a);
        m.stubs.insert(a, why.to_string());
    })
}

/// Make a folder (and its parents), from the host
pub fn mkdir(p: &str) {
    mem(|m| {
        let a = m.abs(p);
        m.add_parents(&a);
        m.dirs.insert(a);
    })
}

/// Take a file or folder away, from the host (deleted in the file browser). Not reported back.
pub fn unput(p: &str) {
    mem(|m| {
        let a = m.abs(p);
        let pre = format!("{}/", a);
        m.files.retain(|k, _| *k != a && !k.starts_with(&pre));
        m.stubs.retain(|k, _| *k != a && !k.starts_with(&pre));
        if a != "/" {
            m.dirs.retain(|k| *k != a && !k.starts_with(&pre));
        }
        if m.cwd == a || m.cwd.starts_with(&pre) {
            m.cwd = parent(&a);
            while !m.dirs.contains(&m.cwd) {
                m.cwd = parent(&m.cwd);
            }
        }
    })
}

/// Forget every file and go back to "/", from the host
pub fn clear() {
    MEM.with(|m| *m.borrow_mut() = Some(MemFs::new()));
}

/// The engine's own writes and removals since the last call, with each written file's bytes
pub fn take_changes() -> Vec<(Change, Option<Vec<u8>>)> {
    mem(|m| {
        let ch = std::mem::take(&mut m.changes);
        ch.into_iter()
            .map(|c| {
                let data = match &c {
                    Change::Write(p) => m.files.get(p).cloned(),
                    Change::Remove(_) => None,
                };
                (c, data)
            })
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn memory_files() {
        use_memory();
        clear();
        put("data/a.csv", b"x\n1\n".to_vec());
        assert_eq!(list(".").unwrap(), vec!["data/"]);
        assert_eq!(list("data").unwrap(), vec!["a.csv"]);
        cd("data").unwrap();
        assert_eq!(cwd().unwrap(), "/data");
        assert_eq!(read("a.csv").unwrap(), b"x\n1\n");
        assert_eq!(read("../data/a.csv").unwrap(), b"x\n1\n");
        write("b.txt", b"hi").unwrap();
        assert!(write("nope/b.txt", b"hi").is_err());
        remove("a.csv").unwrap();
        assert!(read("a.csv").is_err());
        let ch: Vec<Change> = take_changes().into_iter().map(|c| c.0).collect();
        assert_eq!(ch, vec![Change::Write("/data/b.txt".into()), Change::Remove("/data/a.csv".into())]);
        assert!(take_changes().is_empty());
        put_stub("/n.ipynb", "notebooks cannot be read from Galileo");
        assert!(read("/n.ipynb").unwrap_err().contains("notebooks"));
        unput("/data");
        assert_eq!(cwd().unwrap(), "/");
        assert_eq!(list("/").unwrap(), vec!["n.ipynb"]);
        clear();
    }
}
