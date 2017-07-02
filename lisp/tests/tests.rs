extern crate cell_gc;
extern crate lisp;

use std::fs;
use std::io::Read;
use std::path;

fn eval_test_file(file: path::PathBuf) {
    println!("Opening test file: {}", file.display());
    let mut file = fs::File::open(file).expect("Should open file OK");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Should read file OK");

    cell_gc::with_heap(|hs| {
        let expr = lisp::parser::parse(hs, &source).expect("Should parse s-exp OK");
        let env = lisp::vm::Value::default_env(hs);
        let _ = lisp::vm::eval(hs, expr, &env).expect("Should eval expr OK");
    });
}

macro_rules! test {
    ( $file:ident ) => {
        #[test]
        fn $file() {
            let file = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/", stringify!($file), ".lisp");
            let file = path::PathBuf::from(file);
            eval_test_file(file);
        }
    }
}

test!(closure);
test!(cons);
test!(double);
test!(y_combinator);
