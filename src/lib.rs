//! Galileo: a small computer algebra system.
#![allow(
    clippy::should_implement_trait,
    clippy::needless_range_loop,
    clippy::collapsible_if
)]

pub mod algebra_cmd;
pub mod units;
pub mod units_cmd;
pub mod signal;
pub mod algext;
pub mod algfactor;
pub mod auri_families;
pub mod build;
pub mod calc;
pub mod cas;
pub mod cas_cmd;
pub mod commands;
pub mod discover;
pub mod display;
pub mod eval;
pub mod expr;
pub mod extra_cmd;
pub mod factor_cmd;
pub mod factorlab;
pub mod ffield;
pub mod fgroup;
pub mod geometry;
pub mod gnfs;
pub mod galois;
pub mod graph;
pub mod graph_cmd;
pub mod groebner;
pub mod groups;
pub mod groups_cmd;
pub mod help;
pub mod kernel;
pub mod lab_cmd;
pub mod labgroups;
pub mod lang;
pub mod lableak;
pub mod labsearch;
pub mod latex;
pub mod lie;
pub mod linalg;
pub mod modsolve;
pub mod num;
pub mod numeric;
pub mod numtheory;
pub mod ode;
pub mod parser;
pub mod plot;
pub mod polyselect;
pub mod prec;
pub mod print;
pub mod qring;
pub mod quat;
pub mod radicals;
pub mod ratfun;
pub mod rsa_data;
pub mod special;
pub mod vcalc;
pub mod mv_cmd;
pub mod decomp;
pub mod assume;
pub mod laplace;
pub mod de_cmd;
pub mod ode2;
pub mod stats;
pub mod stoch_cmd;
pub mod pde_cmd;
pub mod cov_cmd;
pub mod stats_cmd;
pub mod cnum;
pub mod laurent;
pub mod la_cmd;
pub mod teach;
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use eval::{Output, Session};
pub use expr::Expr;
