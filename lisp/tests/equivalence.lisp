; R4RS 6.2 Equivalence predicates

(assert (eq? (eqv? 'a 'a) #t))
(assert (eq? (eqv? 'a 'b) #f))
(assert (eq? (eqv? 2 2) #t))
(assert (eq? (eqv? '() '()) #t))
(assert (eq? (eqv? 100000000 100000000) #t))
(assert (eq? (eqv? (cons 1 2) (cons 1 2)) #f))
(assert (eq? (eqv? (lambda () 1)
                   (lambda () 2))
             #f))
(assert (eq? (eqv? #f 'nil) #f))
;;(assert (eq? (let ((p (lambda (x) x)))
;;               (eqv? p p))
;;             #t))

;; The following examples illustrate cases in which the above rules do not
;; fully specify the behavior of eqv?. All that can be said about such cases is
;; that the value returned by eqv? must be a boolean.

;;(assert (boolean? (eqv? "" "")))
(assert (boolean? (eqv? '#() '#())))
(assert (boolean? (eqv? (lambda (x) x)
                        (lambda (x) x))))
(assert (boolean? (eqv? (lambda (x) x)
                        (lambda (y) y))))

;; The next set of examples shows the use of eqv? with procedures that have
;; local state. Gen-counter must return a distinct procedure every time, since
;; each procedure has its own internal counter. Gen-loser, however, returns
;; equivalent procedures each time, since the local state does not affect the
;; value or side effects of the procedures.

;;(define gen-counter
;;  (lambda ()
;;    (let ((n 0))
;;      (lambda () (set! n (+ n 1)) n))))
;;(assert (eq? (let ((g (gen-counter)))
;;               (eqv? g g))
;;             #t))
;;(assert (eq? (eqv? (gen-counter) (gen-counter))
;;             #f))
;;
;;(define gen-loser
;;  (lambda ()
;;    (let ((n 0))
;;      (lambda () (set! n (+ n 1)) 27))))
;;(assert (eq? (let ((g (gen-loser)))
;;               (eqv? g g))
;;             #t))
;;(assert (boolean? (eqv? (gen-loser) (gen-loser))))

;; (assert (boolean? (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
;;                            (g (lambda () (if (eqv? f g) 'both 'g))))
;;                     (eqv? f g))))
;; 
;; (assert (eq? (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
;;                       (g (lambda () (if (eqv? f g) 'g 'both))))
;;                (eqv? f g))
;;              #f))

;; Since it is an error to modify constant objects (those returned by literal
;; expressions), implementations are permitted, though not required, to share
;; structure between constants where appropriate. Thus the value of eqv? on
;; constants is sometimes implementation-dependent.

(assert (boolean? (eqv? '(a) '(a))))
;;(assert (boolean? (eqv? "a" "a")))
(assert (boolean? (eqv? '(b) (cdr '(a b)))))
;;(assert (eq? (let ((x '(a)))
;;               (eqv? x x))
;;             #t))

;; Eq? is similar to eqv? except that in some cases it is capable of discerning
;; distinctions finer than those detectable by eqv?.
;;
;; Eq? and eqv? are guaranteed to have the same behavior on symbols, booleans,
;; the empty list, pairs, and non-empty strings and vectors. Eq?'s behavior on
;; numbers and characters is implementation-dependent, but it will always
;; return either true or false, and will return true only when eqv? would also
;; return true. Eq? may also behave differently from eqv? on empty vectors and
;; empty strings.

(assert (eq? (eq? 'a 'a) #t))
(assert (boolean? (eq? '(a) '(a))))
;;(assert (eq? (eq? (list 'a) (list 'a)) #f))
;;(assert (boolean? (eq? "a" "a")))
;;(assert (boolean? (eq? "" "")))
(assert (eq? (eq? '() '()) #t))
(assert (boolean? (eq? 2 2)))
;;(assert (boolean? (eq? #\A #\A)))
(assert (eq? (eq? car car) #t))
;;(assert (boolean? (let ((n (+ 2 3)))
;;                    (eq? n n))))
;;(assert (eq? (let ((x '(a)))
;;               (eq? x x))
;;             #t))
;;(assert (eq? (let ((x '#()))
;;               (eq? x x))
;;             #t))
;;(assert (eq? (let ((p (lambda (x) x)))
;;               (eq? p p))
;;             #t))


;; `equal?` recursively compares the contents of pairs, vectors, and strings,
;; applying `eqv?` on other objects such as numbers and symbols. A rule of thumb
;; is that objects are generally equal? if they print the same. `equal?` may fail
;; to terminate if its arguments are circular data structures.

;;(assert (eq? (equal? 'a 'a) #t))
;;(assert (eq? (equal? '(a) '(a)) #t))
;;(assert (eq? (equal? '(a (b) c)
;;                     '(a (b) c)) #t))
;;(assert (eq? (equal? "abc" "abc") #t))
;;(assert (eq? (equal? 2 2) #t))
;;(assert (eq? (equal? (make-vector 5 'a)
;;                     (make-vector 5 'a))
;;             #t))
;;(assert (boolean? (equal? (lambda (x) x)
;;                          (lambda (y) y))))
