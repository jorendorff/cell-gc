;; R4RS 6.3 Pairs and lists

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
(assert (equal? (cons "a" '(b c)) '("a" b c)))
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
(assert (eq? (letrec ((x (list 'a)))
               (set-cdr! x x)
               (list? x))
             #f))


;; `(list obj ...)` returns a newly allocated list of its arguments.

(assert (equal? (list 'a (+ 3 4) 'c) '(a 7 c)))
(assert (eq? (list) '()))


;; `(length list)` returns the length of list.

(assert (eq? (length '(a b c)) 3))
(assert (eq? (length '(a (b) (c d e))) 3))
(assert (eq? (length '()) 0))


;; `(reverse list)` returns a newly allocated list consisting of the elements
;; of list in reverse order.

(assert (equal? (reverse '(a b c))
                '(c b a)))
(assert (equal? (reverse '(a (b c) d (e (f))))
                '((e (f)) d (b c) a)))


;; `(list-tail list k)` returns the sublist of list obtained by omitting the
;; first k elements.

;; `(list-ref list k)` returns the kth element of list. (This is the same as
;; the car of (list-tail list k).)

(assert (eq? (list-ref '(a b c d) 2) 'c))
;;(assert (eq? (list-ref '(a b c d)
;;                       (inexact->exact (round 1.8)))
;;             'c))


;; `(memq obj list)`
;; `(memv obj list)`
;; `(member obj list)`
;;
;; These procedures return the first sublist of list whose car is obj, where
;; the sublists of list are the non-empty lists returned by `(list-tail list k)`
;; for k less than the length of list. If obj does not occur in list, then #f
;; (not the empty list) is returned. `memq` uses `eq?` to compare obj with the
;; elements of list, while `memv` uses `eqv?` and `member` uses `equal?`.

(assert (equal? (memq 'a '(a b c))
                '(a b c)))
(assert (equal? (memq 'b '(a b c))
                '(b c)))
(assert (eq? (memq 'a '(b c d))
             #f))
(assert (eq? (memq (list 'a) '(b (a) c))
             #f))
(assert (equal? (member (list 'a) '(b (a) c))
                '((a) c)))
(assert (equal? (memv 101 '(100 101 102))
                '(101 102)))

;; `(assq obj alist)`
;; `(assv obj alist)`
;; `(assoc obj alist)`
;;
;; `alist` (for "association list") must be a list of pairs. These procedures
;; find the first pair in alist whose car field is obj, and returns that
;; pair. If no pair in alist has obj as its car, then #f (not the empty list)
;; is returned. `Assq` uses `eq?` to compare obj with the car fields of the pairs
;; in alist, while `assv` uses `eqv?` and `assoc` uses `equal?`.

(define e '((a 1) (b 2) (c 3)))
(assert (equal? (assq 'a e)
                '(a 1)))
(assert (equal? (assq 'b e)
                '(b 2)))
(assert (eq? (assq 'd e)
             #f))
(assert (eq? (assq (list 'a) '(((a)) ((b)) ((c))))
             #f))
(assert (equal? (assoc (list 'a) '(((a)) ((b)) ((c))))
                '((a))))
(assert (equal? (assv 5 '((2 3) (5 7) (11 13)))
                '(5 7)))


;; `(for-each proc list1 list2 ...)‌‌` procedure
;;
;; The lists should all have the same length. Proc should accept as many
;; arguments as there are lists. Proc should not mutate any of the lists.
;;
;; The for-each procedure applies proc element-wise to the elements of the
;; lists for its side effects, in order from the first elements to the
;; last. Proc is always called in the same dynamic environment as for-each
;; itself. The return values of for-each are unspecified.

;; (assert (equal? (let ((v (make-vector 5)))
;;                   (for-each (lambda (i)
;;                               (vector-set! v i (* i i)))
;;                             '(0 1 2 3 4))
;;                   v)
;;                 '#(0 1 4 9 16)))

(define unspecified (if #f #f))

(assert (eq? (for-each (lambda (x) x) '(1 2 3 4))
             unspecified))

(letrec ((even? (lambda (x) #t)))
  (assert (eq? (for-each even? '())
               unspecified)))
