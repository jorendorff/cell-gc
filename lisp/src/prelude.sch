;; I stole these lines of code from <https://www.bluishcoder.co.nz/jsscheme/>.
;; Original by Alex Yakovlev. Adapted by Chris Double.

(define (not x) (if x #f #t))
;
(define equal?
  (lambda (x y)
    ((lambda (eqv)
       (if eqv eqv
           (if (pair? x)
               (begin
                 (if (pair? y)
                     (if (equal? (car x) (car y))
                         (equal? (cdr x) (cdr y))
                         #f)
                     #f))
                 (if (vector? x)
                     (if (vector? y)
                         ((lambda (n)
                            (if (= (vector-length y) n)
                                ((begin
                                   (define loop
                                     (lambda (i)
                                       ((lambda (eq-len)
                                          (if eq-len
                                              eq-len
                                              (if (equal? (vector-ref x i)
                                                          (vector-ref y i))
                                                  (loop (+ i 1))
                                                  #f)))
                                        (= i n))))
                                   loop)
                                 0)
                                #f))
                          (vector-length x))
                         #f)
                     #f))))
     (eqv? x y))))
