;; R7RS 6.11 Exceptions

(letrec ((rlog '())
         (log (lambda (obj)
                (set! rlog (cons obj rlog)))))
  (assert (eq?
           (call-with-current-continuation
            (lambda (k)
              (with-exception-handler
               (lambda (x)
                 (log x)
                 (k 'exception))
               (lambda ()
                 (+ 1 (raise 'an-error))))))
           'exception))
  (assert (equal? (reverse rlog) '(an-error))))

(letrec ((rlog '())
         (log (lambda (obj)
                (set! rlog (cons obj rlog)))))
  (assert (eq?
           (call/cc (lambda (return)
                      (with-exception-handler
                       return
                       (lambda ()
                         (with-exception-handler
                          (lambda (x)
                            (log "something went wrong\n"))
                          (lambda ()
                            (+ 1 (raise 'an-error))))))))
           'an-error))
  (assert (equal? (reverse rlog) '("something went wrong\n"))))

(letrec ()
  (define (null-list? l)
    (cond ((pair? l) #f)
          ((null? l) #t)
          (else (error
                 "null-list?: argument out of domain"
                 l))))
  (define result (call/cc (lambda (return)
                            (with-exception-handler
                             (lambda (exc)
                               (return (cons 'error exc)))
                             (lambda ()
                               (null-list? "cowboy"))))))
  (assert (pair? result))
  (assert (eq? (car result) 'error) 2)
  (assert (error-object? (cdr result)) 3)
  (assert (equal? (error-object-message (cdr result))
                  "null-list?: argument out of domain") 4)
  (assert (equal? (error-object-irritants (cdr result))
                  (list "cowboy"))))

