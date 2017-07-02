use cell_gc::with_heap;

#[test]
fn add_in_lambda() {
    with_heap(|hs| {
        let mut env = Nil;
        env.push_env(
            hs,
            Arc::new("+".to_string()),
            Builtin(GcLeaf::new(BuiltinFnPtr(add))),
        );
        let program = lisp!(
            ((lambda (x y z) (+ x (+ y z))) 3 4 5)
                , hs);
        let result = eval(hs, program, &env);
        assert_eq!(result, Ok(Int(12)));
    });
}
