//! Writes the SVG of every figure a script makes: cargo run --release --example svgdump -- out_dir < script
use galileo_rs::eval::Session;
use std::io::Read;
fn main() {
    let dir = std::env::args().nth(1).unwrap_or_else(|| ".".into());
    let mut src = String::new();
    std::io::stdin().read_to_string(&mut src).unwrap();
    let mut s = Session::new();
    let mut k = 0;
    for line in src.lines() {
        let out = galileo_rs::kernel::execute(&mut s, line);
        if let Some(i) = out.find("<svg") {
            let j = out.rfind("</svg>").unwrap() + 6;
            let svg = out[i..j].replace("\\\"", "\"").replace("\\n", "\n");
            k += 1;
            std::fs::write(format!("{}/fig{}.svg", dir, k), svg).unwrap();
        } else {
            println!("{}", &out[..out.len().min(300)]);
        }
    }
}
