;; R4RS 6.9 Control features

;; `(procedure? obj)` returns #t if obj is a procedure, otherwise returns #f.

(assert (eq? (procedure? car) #t))
(assert (eq? (procedure? 'car) #f))
(assert (eq? (procedure? (lambda (x) (* x x))) #t))
(assert (eq? (procedure? '(lambda (x) (* x x))) #f))
;;(assert (eq? (call-with-current-continuation procedure?) #t))


;; `(apply proc args)`
;; `(apply proc arg1 ... args)`
;;
;; `proc` must be a procedure and `args` must be a list. The first (essential)
;; form calls proc with the elements of args as the actual arguments. The
;; second form is a generalization of the first that calls proc with the
;; elements of the list `(append (list arg1 ...) args)' as the actual
;; arguments.

(assert (eq? (apply + (list 3 4))
             7))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

;;(assert (eq? ((compose sqrt *) 12 75)
;;             30))


