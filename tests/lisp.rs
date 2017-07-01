use std::process::Command;

#[test]
fn run_lisp_tests() {
    let status = Command::new("cargo")
        .args(&["test"])
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/lisp"))
        .spawn()
        .expect("should spawn tests OK")
        .wait()
        .expect("should wait OK");
    assert!(status.success());
}
