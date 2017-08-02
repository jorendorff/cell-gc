use std::process::Command;

#[test]
fn run_lisp_tests() {
    let mut args = vec!["test"];

    if !cfg!(debug_assertions) {
        args.push("--release");
    }

    let status = Command::new("cargo")
        .args(&args)
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/lisp"))
        .spawn()
        .expect("should spawn tests OK")
        .wait()
        .expect("should wait OK");
    assert!(status.success());
}
