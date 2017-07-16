(define (tail-recursive n)
  (if (eq? n 0)
      'ok
    (tail-recursive (- n 1))))

(assert (eq? 'ok (tail-recursive 9999)))
