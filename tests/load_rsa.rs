use galileo_rs::Session;
use galileo_rs::kernel::execute;

/// load(RSA) defines the numbers, prints no value and no error, only a note
#[test]
fn load_rsa_is_clean() {
    let mut s = Session::new();
    let r = execute(&mut s, "load(RSA)");
    assert!(r.contains("\"error\":null"), "{}", r);
    assert!(r.contains("\"notes\":true"), "{}", r);
    assert!(
        r.starts_with("{\"outputs\":[{\"data\""),
        "a value was printed: {}",
        r
    );
    let r = execute(&mut s, "RSA100p * RSA100q - RSA100");
    assert!(
        r.contains("{\"text\":\"0\"}") && r.contains("\"error\":null"),
        "{}",
        r
    );
    // also in lower case, and quiet with ;
    let r = execute(&mut s, "load(rsa);");
    assert_eq!(r, "{\"outputs\":[],\"error\":null}");
}
