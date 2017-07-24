extern crate cell_gc;
extern crate lisp;

use std::fs;
use std::io::Read;
use std::path::Path;

fn eval_test_file(file: &Path) {
    let mut file = fs::File::open(file).expect("Should open file OK");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Should read file OK");

    cell_gc::with_heap(|hs| {
        let exprs = lisp::parse::parse(hs, &source).expect("Should parse s-exps OK");
        let env = lisp::vm::Environment::default_env(hs);
        for expr in exprs {
            lisp::vm::eval(hs, expr, env.clone()).expect("Should eval exprs OK");
            hs.force_gc();
        }
    });
}

macro_rules! test {
    ( $file:ident ) => {
        #[test]
        fn $file() {
            let file = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/", stringify!($file), ".lisp");
            let file = Path::new(file);
            eval_test_file(file);
        }
    }
}

test!(begin);
test!(booleans);
test!(characters);
test!(closure);
test!(cons);
test!(double);
test!(equivalence);
test!(extensions);
test!(y_combinator);
test!(y_combinator_2);
test!(define);
test!(numerical);
test!(pairs_and_lists);
test!(primitive);
test!(print);
test!(set);
test!(symbols);
test!(tail_calls);
test!(vectors);
