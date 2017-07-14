extern crate cell_gc;
extern crate lisp;

use std::fs;
use std::io::Read;
use std::path;

fn eval_test_file(file: path::PathBuf) {
    println!("Opening test file: {}", file.display());
    let mut file = fs::File::open(file).expect("Should open file OK");

    // Hack: The parser only accepts one s-expression at a time. But a script
    // can be any number of s-exprs. Wrap in parentheses to work around this.
    let mut source = "(".to_string();
    file.read_to_string(&mut source)
        .expect("Should read file OK");
    source.push_str("\n)");

    println!("source =");
    println!("{}", source);

    cell_gc::with_heap(|hs| {
        let exprs = lisp::parser::parse(hs, &source).expect("Should parse s-exps OK");
        let env = lisp::vm::Environment::default_env(hs);
        let _ = lisp::vm::eval_block_body(hs, exprs, env).expect("Should eval exprs OK");
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
test!(y_combinator_2);
test!(define);
