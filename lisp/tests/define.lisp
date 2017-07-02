((lambda (x)
   (assert (eq? x 5))

   (define x 1)
   (assert (eq? x 1))

   (define y 2)
   (assert (eq? y 2))

   (assert (eq? (define z 0) ()))

   (define b 2)
   (assert (eq? b 2))

   (define f
     (lambda (a)
       (define b 1)
       (assert (eq? b 1))
       (+ b a)))

   (assert (eq? (f 5) 6))
   (assert (eq? b 2))

   )
 5)
