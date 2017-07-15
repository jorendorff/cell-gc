use cell_gc::with_heap;

#[test]
fn add_in_lambda() {
    with_heap(|hs| {
        let env = Environment::default_env(hs);
        let program = lisp!(
            ((lambda (x y z) (+ x (+ y z))) 3 4 5)
                , hs);
        let result = eval(hs, program, env.clone()).expect("Should eval OK");
        assert_eq!(result, Int(12));
    });
}
