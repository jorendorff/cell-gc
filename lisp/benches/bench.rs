#![feature(test)]

extern crate cell_gc;
extern crate lisp;
extern crate test;

use lisp::{parse, vm};
use std::fs;
use std::io::Read;

macro_rules! bench {
    ( $filename:ident ) => {
        #[bench]
        fn $filename(b: &mut test::Bencher) {
            let filename = concat!(env!("CARGO_MANIFEST_DIR"),
                                   "/benches/",
                                   stringify!($filename),
                                   ".scm");
            let mut file = fs::File::open(filename).expect("should open file OK");
            let mut source = String::new();
            file.read_to_string(&mut source).expect("should read file OK");

            b.iter(|| {
                cell_gc::with_heap(|hs| {
                    let env = vm::Environment::default_env(hs);
                    let exprs = parse::parse(hs, &source).expect("should parse source OK");
                    for expr in exprs {
                        vm::eval(hs, expr, env.clone()).expect("should eval OK");
                    }
                });
            });
        }
    }
}

bench!(allocate_no_garbage);
bench!(allocate_all_garbage);
