extern crate cell_gc;
extern crate lisp;

use std::fs;
use std::io::Read;
use std::path::Path;

fn eval_test_file(expanded: bool, file: &Path) {
    let mut file = fs::File::open(file).expect("Should open file OK");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Should read file OK");

    cell_gc::with_heap(|hs| {
        let exprs = lisp::parse::parse(hs, &source).expect("Should parse s-exps OK");
        let env = if expanded {
            lisp::toplevel::expanded_env(hs)
        } else {
            lisp::toplevel::core_env(hs)
        };
        for expr in exprs {
            lisp::toplevel::eval(hs, &env, expr).expect("Should eval exprs OK");
            hs.force_gc();
        }
    });
}

macro_rules! expanded_test {
    ( $file:ident ) => {
        #[test]
        fn $file() {
            let file = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/", stringify!($file), ".scm");
            eval_test_file(true, Path::new(file));
        }
    }
}

macro_rules! core_test {
    ( $file:ident ) => {
        #[test]
        fn $file() {
            let file = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/", stringify!($file), ".scm");
            eval_test_file(false, Path::new(file));
        }
    }
}

core_test!(begin);
core_test!(booleans);
core_test!(characters);
core_test!(closure);
core_test!(cons);
expanded_test!(continuations);
core_test!(double);
expanded_test!(equivalence);
expanded_test!(eval);
core_test!(extensions);
core_test!(y_combinator);
core_test!(y_combinator_2);
core_test!(define);
core_test!(numerical);
expanded_test!(pairs_and_lists);
expanded_test!(primitive);
core_test!(print);
core_test!(set);
core_test!(symbols);
core_test!(tail_calls);
core_test!(vectors);
