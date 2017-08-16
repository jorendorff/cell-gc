(define (fix f)
  ((lambda (x) (f (lambda (v) ((x x) v))))
   (lambda (x) (f (lambda (v) ((x x) v))))))

(define (fac-helper fac)
  (lambda (n)
    (if (eq? n 0)
        1
      (* n (fac (- n 1))))))

(assert (eq? 120 ((fix fac-helper) 5)))
