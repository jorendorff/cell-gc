((lambda (x)
   (assert (eq? x 5))

   (define x 1)
   (assert (eq? x 1))

   (define y 2)
   (assert (eq? y 2))

   (assert (eq? (define z 0) ()))

   (define b 2)
   (assert (eq? b 2))

   (define (f a)
     (define b 1)
     (assert (eq? b 1))
     (+ b a))

   (assert (eq? (f 5) 6))
   (assert (eq? b 2))

   )
 5)

;; Recursive function
(define (fac n)
  (if (eq? n 0)
      1
    (* n (fac (- n 1)))))

(assert (eq? (fac 8) (* 1 2 3 4 5 6 7 8)))

;; Mutually recursive functions
((lambda ()
   (define (even? n)
     (if (eq? n 0)
         #t
       (odd? (- n 1))))
   (define (odd? n)
     (if (eq? n 0)
         #f
       (even? (- n 1))))
   (assert (even? 0))
   (assert (not (even? 7)))))
