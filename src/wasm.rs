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
