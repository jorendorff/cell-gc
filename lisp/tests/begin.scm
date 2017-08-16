(define x 0)

(assert (eq? (begin (set! x 5)
                    (+ x 1))
             6))

(assert (eq? x 5))
