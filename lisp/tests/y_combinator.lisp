(assert (eq? 120 (((lambda (f)
                     ((lambda (x) (f (lambda (v) ((x x) v))))
                      (lambda (x) (f (lambda (v) ((x x) v))))))
                   (lambda (fac)
                     (lambda (n)
                       (if (eq? n 0)
                           1
                         (* n (fac (- n 1)))))))
                  5)))
