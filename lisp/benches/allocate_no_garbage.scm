(define page-size 4096)
(define max-allocs (* 3 page-size))

(define (allocate n acc)
  (if (= n 0)
      acc
      (allocate (- n 1) (cons n acc))))

(allocate max-allocs '())
