;; `pair?` returns #t if obj is a pair, and otherwise returns #f.

(assert (eq? (pair? '(a . b)) #t))
(assert (eq? (pair? '(a b c)) #t))
(assert (eq? (pair? '()) #f))
(assert (eq? (pair? '#(a b)) #f))


;; `cons` returns a newly allocated pair whose car is obj1 and whose cdr is
;; obj2. The pair is guaranteed to be different (in the sense of eqv?) from
;; every existing object.

(assert (equal? (cons 'a '()) '(a)))
(assert (equal? (cons '(a) '(b c d)) '((a) b c d)))
;;(assert (equal? (cons "a" '(b c)) '("a" b c)))
(assert (equal? (cons 'a 3) '(a . 3)))
(assert (equal? (cons '(a b) 'c) '((a b) . c)))


;; `car` returns the contents of the car field of pair. Note that it is an
;; error to take the car of the empty list.

(assert (eq? (car '(a b c)) 'a))
(assert (equal? (car '((a) b c d)) '(a)))
(assert (eq? (car '(1 . 2)) 1))
;;(assert (equal? (car '()) 'error))


;; `cdr` returns the contents of the cdr field of pair. Note that it is an
;; error to take the cdr of the empty list.

(assert (equal? (cdr '((a) b c d)) '(b c d)))
(assert (eq? (cdr '(1 . 2)) 2))
;;(assert (eq? (cdr '()) 'error))


;; `(null? obj)` returns #t if obj is the empty list, otherwise returns #f.

(assert (eq? (null? '()) #t))
(assert (eq? (null? '#()) #f))
(assert (eq? (null? '(())) #f))
(assert (eq? (null? #f) #f))

;; `(list? obj)` returns #t if obj is a list, otherwise returns #f. By
;; definition, all lists have finite length and are terminated by the empty
;; list.

(assert (eq? (list? '(a b c)) #t))
(assert (eq? (list? '()) #t))
(assert (eq? (list? '(a . b)) #f))
;;(assert (eq? (let ((x (list 'a)))
;;               (set-cdr! x x)
;;               (list? x))
;;             #f))
