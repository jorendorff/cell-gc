(define page-size 4096)
(define three-pages (* 3 page-size))

(define (allocate n)
  (if (= n 0)
      (cons 1 2)
      (begin
        (cons 1 2)
        (allocate (- n 1)))))

(allocate three-pages)
