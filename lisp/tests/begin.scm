(define x 0)

(assert (eq? (begin (set! x 5)
                    (+ x 1))
             6))

(assert (eq? x 5))

;; At toplevel, `begin` forms are splicing, and definitions can be interleaved
;; with expressions.
(begin (define y 2)
       (set! x (+ y 100))
       (define z 3))

(assert (eq? y 2))
(assert (eq? x 102))
(assert (eq? z 3))
