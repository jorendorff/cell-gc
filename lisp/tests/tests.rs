extern crate cell_gc;
extern crate lisp;

use std::fs;
use std::io::Read;
use std::path::Path;

enum Mode {
    Core,
    Expanded,
    Full,
}

fn eval_test_file(mode: Mode, file: &Path) {
    let mut file = fs::File::open(file).expect("Should open file OK");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Should read file OK");

    cell_gc::with_heap(|hs| {
        let exprs = lisp::parse::parse(hs, &source).expect("Should parse s-exps OK");
        let env = match mode {
            Mode::Core => lisp::toplevel::core_env(hs),
            Mode::Expanded => lisp::toplevel::expanded_env(hs),
            Mode::Full => lisp::toplevel::default_env(hs),
        };
        for expr in exprs {
            lisp::toplevel::eval(hs, &env, expr).expect("Should eval exprs OK");
            hs.force_gc();
        }
    });
}

macro_rules! test {
    ( $kind:ident, $file:ident ) => {
        #[test]
        fn $file() {
            let file = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/", stringify!($file), ".scm");
            eval_test_file(Mode::$kind, Path::new(file));
        }
    }
}


test!(Core, begin);
test!(Core, booleans);
test!(Core, characters);
test!(Core, closure);
test!(Core, cons);
test!(Expanded, continuations);
test!(Core, double);
test!(Expanded, equivalence);
test!(Full, eval);
test!(Core, extensions);
test!(Core, y_combinator);
test!(Core, y_combinator_2);
test!(Core, define);
test!(Core, numerical);
test!(Expanded, pairs_and_lists);
test!(Expanded, primitive);
test!(Core, print);
test!(Core, set);
test!(Core, symbols);
test!(Core, tail_calls);
test!(Core, vectors);

test!(Full, r5rs_pitfall);
