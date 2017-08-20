extern crate cell_gc;
extern crate lisp;

use std::path::Path;

enum Mode {
    Core,
    Expanded,
    Full,
}

fn eval_test_file(mode: Mode, file: &Path) {
    cell_gc::with_heap(|hs| {
        let env = match mode {
            Mode::Core => lisp::toplevel::core_env(hs),
            Mode::Expanded => lisp::toplevel::expanded_env(hs),
            Mode::Full => lisp::toplevel::default_env(hs),
        };

        lisp::toplevel::load(hs, &env, file).unwrap();
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
