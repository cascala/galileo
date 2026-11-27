//! WebAssembly entry points, for running Galileo in a browser (JupyterLite and
//! the playground). A plain C ABI with no bindings crate:
//!
//! 1. `galileo_alloc(len)` returns a buffer; the host writes UTF-8 code into it.
//! 2. `galileo_execute(ptr, len)`, `galileo_complete(ptr, len, cursor)` or
//!    `galileo_inspect(ptr, len, cursor)` consume that buffer and run the request.
//! 3. The reply is the same JSON as the `--kernel` protocol (see `kernel.rs`);
//!    read it at `galileo_reply_ptr()` / `galileo_reply_len()`.
//!
//! `galileo_reset()` starts a fresh session; `galileo_seed(n)` seeds `rand`.
//!
//! Files live in memory (see `vfs.rs`). The host puts the user's files in with
//! `galileo_fs_put(path, data)` (each a buffer from `galileo_alloc`), lists files
//! it cannot hand over with `galileo_fs_stub(path, why)`, makes folders with
//! `galileo_fs_mkdir`, takes files away with `galileo_fs_unput`, and sets the
//! starting folder with `galileo_fs_cd`. After a run, `galileo_fs_changes()`
//! replies with what the engine wrote or removed, as JSON:
//! `[{"op": "write", "path": "/out.csv", "data": "<base64>"}, {"op": "remove", "path": "/old.csv"}]`.

use crate::eval::{Session, seed_random};
use crate::kernel;
use std::cell::RefCell;

thread_local! {
    static SESSION: RefCell<Option<Session>> = const { RefCell::new(None) };
    static REPLY: RefCell<String> = const { RefCell::new(String::new()) };
}

fn with_session<T>(f: impl FnOnce(&mut Session) -> T) -> T {
    SESSION.with(|s| f(s.borrow_mut().get_or_insert_with(Session::new)))
}

/// Take ownership of a buffer from `galileo_alloc` and read it as text
unsafe fn take(ptr: *mut u8, len: usize) -> String {
    let bytes = unsafe { Vec::from_raw_parts(ptr, len, len) };
    String::from_utf8_lossy(&bytes).into_owned()
}

fn reply(s: String) -> usize {
    REPLY.with(|r| *r.borrow_mut() = s);
    galileo_reply_len()
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_alloc(len: usize) -> *mut u8 {
    let mut v = vec![0u8; len];
    let p = v.as_mut_ptr();
    std::mem::forget(v);
    p
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_reply_ptr() -> *const u8 {
    REPLY.with(|r| r.borrow().as_ptr())
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_reply_len() -> usize {
    REPLY.with(|r| r.borrow().len())
}

/// # Safety
/// `ptr`/`len` must come from `galileo_alloc`; the buffer is freed here.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_execute(ptr: *mut u8, len: usize) -> usize {
    let code = unsafe { take(ptr, len) };
    reply(with_session(|s| kernel::execute(s, &code)))
}

/// # Safety
/// As for `galileo_execute`; `cursor` counts characters.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_complete(ptr: *mut u8, len: usize, cursor: usize) -> usize {
    let code = unsafe { take(ptr, len) };
    reply(with_session(|s| kernel::complete(s, &code, cursor)))
}

/// # Safety
/// As for `galileo_execute`; `cursor` counts characters.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_inspect(ptr: *mut u8, len: usize, cursor: usize) -> usize {
    let code = unsafe { take(ptr, len) };
    reply(kernel::inspect(&code, cursor))
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_reset() {
    SESSION.with(|s| *s.borrow_mut() = Some(Session::new()));
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_seed(seed: u64) {
    seed_random(seed);
}

/// Version string, as a reply
#[unsafe(no_mangle)]
pub extern "C" fn galileo_version() -> usize {
    reply(env!("CARGO_PKG_VERSION").to_string())
}

/// # Safety
/// Both buffers must come from `galileo_alloc`; they are freed here.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_fs_put(p: *mut u8, plen: usize, d: *mut u8, dlen: usize) {
    let path = unsafe { take(p, plen) };
    let data = unsafe { Vec::from_raw_parts(d, dlen, dlen) };
    crate::vfs::put(&path, data);
}

/// # Safety
/// Both buffers must come from `galileo_alloc`; they are freed here.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_fs_stub(p: *mut u8, plen: usize, w: *mut u8, wlen: usize) {
    let path = unsafe { take(p, plen) };
    let why = unsafe { take(w, wlen) };
    crate::vfs::put_stub(&path, &why);
}

/// # Safety
/// As for `galileo_execute`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_fs_mkdir(p: *mut u8, plen: usize) {
    let path = unsafe { take(p, plen) };
    crate::vfs::mkdir(&path);
}

/// # Safety
/// As for `galileo_execute`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_fs_unput(p: *mut u8, plen: usize) {
    let path = unsafe { take(p, plen) };
    crate::vfs::unput(&path);
}

/// # Safety
/// As for `galileo_execute`. Replies "" or the error.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_fs_cd(p: *mut u8, plen: usize) -> usize {
    let path = unsafe { take(p, plen) };
    reply(crate::vfs::cd(&path).err().unwrap_or_default())
}

#[unsafe(no_mangle)]
pub extern "C" fn galileo_fs_clear() {
    crate::vfs::clear();
}

/// What the engine wrote or removed since the last call, as JSON
#[unsafe(no_mangle)]
pub extern "C" fn galileo_fs_changes() -> usize {
    reply(changes_json())
}

pub(crate) fn changes_json() -> String {
    use crate::plot::jstr;
    use crate::vfs::Change;
    let items: Vec<String> = crate::vfs::take_changes()
        .into_iter()
        .map(|(c, data)| match c {
            Change::Write(p) => format!(
                r#"{{"op":"write","path":{},"data":"{}"}}"#,
                jstr(&p),
                crate::plot::base64(&data.unwrap_or_default())
            ),
            Change::Remove(p) => format!(r#"{{"op":"remove","path":{}}}"#, jstr(&p)),
        })
        .collect();
    format!("[{}]", items.join(","))
}

/// A picture (SVG text in the buffer) as a file: `kind` 0 for PNG (twice the
/// size), 1 for EPS. Replies with the file's bytes in base64, or "" when the
/// SVG cannot be read.
///
/// # Safety
/// As for `galileo_execute`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn galileo_export(ptr: *mut u8, len: usize, kind: u32) -> usize {
    let svg = unsafe { take(ptr, len) };
    let bytes = match kind {
        0 => crate::export::png(&svg, None, 2.0).ok(),
        _ => crate::export::eps(&svg, None).ok().map(String::into_bytes),
    };
    reply(bytes.map(|b| crate::plot::base64(&b)).unwrap_or_default())
}
