//! cargo run --release --example nfsbench -- 30 34 38   (or "special" for 2^128+1 and friends)
use galileo_rs::factorlab as lab;
use galileo_rs::gnfs;
use num_bigint::BigInt;
use std::time::Instant;

fn go(n: &BigInt, label: &str) {
    lab::start_budget(1e13, env_secs());
    let t = Instant::now();
    let env = |k: &str| std::env::var(k).ok().and_then(|v| v.parse::<f64>().ok());
    let choice = env("NFS_D").map(|d| gnfs::base_m_search(n, d as usize, 40).remove(0));
    let params = env("NFS_FB").map(|fb| gnfs::Params {
        fb: fb as u64,
        half: env("NFS_A").unwrap_or(20000.0) as i64,
        lp_mult: env("NFS_LP").unwrap_or(40.0) as u64,
        chars: 24,
    });
    let r = gnfs::nfs(n, choice, params);
    let units = lab::end_budget();
    eprintln!(
        "units {:.3e} in {:.2}s = {:.3e}/s",
        units,
        t.elapsed().as_secs_f64(),
        units / t.elapsed().as_secs_f64()
    );
    let tn = t.elapsed().as_secs_f64();
    lab::start_budget(1e13, env_secs());
    let t = Instant::now();
    let q = if std::env::var("NFS_NOQS").is_ok() {
        None
    } else {
        lab::qs(n)
    };
    lab::end_budget();
    let tq = t.elapsed().as_secs_f64();
    println!(
        "== {} ({} digits): nfs {:.2}s -> {:?}; qs {:.2}s -> {:?}",
        label,
        n.to_string().len(),
        tn,
        r.factor.map(|x| x.to_string()),
        tq,
        q.map(|x| x.to_string())
    );
    for l in &r.log {
        println!("{}", l);
    }
}

fn main() {
    for a in std::env::args().skip(1) {
        if a == "special" {
            let two = BigInt::from(2);
            go(&(num_traits::pow(two.clone(), 128) + 1), "2^128+1");
            continue;
        }
        if let Some(s) = a.strip_prefix("n=") {
            go(&s.parse().unwrap(), s);
            continue;
        }
        let d: u32 = a.parse().unwrap();
        let (n, _, _) = lab::semiprime(d, 7);
        go(&n, &format!("semiprime({}, 7)", d));
    }
}

fn env_secs() -> f64 {
    std::env::var("NFS_SECS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(120.0)
}
