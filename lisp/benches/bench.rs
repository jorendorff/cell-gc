#![feature(test)]

extern crate cell_gc;
extern crate lisp;
extern crate test;

use lisp::parse;

macro_rules! bench {
    ( $filename:ident ) => {
        #[bench]
        fn $filename(b: &mut test::Bencher) {
            run_benchmark(
                b,
                include_str!(
                    concat!(env!("CARGO_MANIFEST_DIR"),
                            "/benches/",
                            stringify!($filename),
                            ".scm")
                ),
            );
        }
    }
}

fn run_benchmark(b: &mut test::Bencher, code: &str) {
    b.iter(|| {
        let mut heap = cell_gc::GcHeap::new();
        heap.enter(|hs| {
            let env = lisp::toplevel::default_env(hs);
            let exprs = parse::parse(hs, code).expect("should parse source OK");
            for expr in exprs {
                lisp::toplevel::eval(hs, &env, expr).expect("should eval OK");
            }
        });
    });
}

bench!(allocate_no_garbage);
bench!(allocate_all_garbage);
bench!(nboyer);
