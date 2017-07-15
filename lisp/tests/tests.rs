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

    println!("source =");
    println!("{}", source);

    cell_gc::with_heap(|hs| {
        let exprs = lisp::parser::parse(hs, &source).expect("Should parse s-exps OK");
        let env = lisp::vm::Environment::default_env(hs);
        for expr in exprs {
            lisp::vm::eval(hs, expr, env.clone()).expect("Should eval exprs OK");
        }
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

test!(begin);
test!(booleans);
test!(closure);
test!(cons);
test!(double);
test!(y_combinator);
test!(y_combinator_2);
test!(define);
test!(pairs_and_lists);
test!(set);
test!(vectors);
