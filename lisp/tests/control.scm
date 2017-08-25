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

;; `(map proc list1 list2 ...)`
;;
;; The lists must be lists, and proc must be a procedure taking as many
;; arguments as there are lists. If more than one list is given, then they must
;; all be the same length. `map` applies proc element-wise to the elements of the
;; lists and returns a list of the results, in order from left to right. The
;; dynamic order in which proc is applied to the elements of the lists is
;; unspecified.

(assert (equal? (map cadr '((a b) (d e) (g h)))
                '(b e h)))

(define (expt a b) (if (= b 0) 1 (* a (expt a (- b 1)))))
(assert (equal? (map (lambda (n) (expt n n)) '(1 2 3 4 5))
                '(1 4 27 256 3125)))

(assert (equal? (map + '(1 2 3) '(4 5 6))
                '(5 7 9)))

;; `(dynamic-wind before thunk after)`
;;
;; Calls `thunk` without arguments, returning the result(s) of this
;; call. `Before` and `after` are called, also without arguments, as required
;; by the following rules [...]
(assert (equal?
         (let ((path '())
               (c #f))
           (let ((add (lambda (s)
                        (set! path (cons s path)))))
             (dynamic-wind
                 (lambda () (add 'connect))
                 (lambda ()
                   (add (call-with-current-continuation
                         (lambda (c0)
                           (set! c c0)
                           'talk1))))
                 (lambda () (add 'disconnect)))
             (if (< (length path) 4)
                 (c 'talk2)
                 (reverse path))))
         '(connect talk1 disconnect
           connect talk2 disconnect)))
