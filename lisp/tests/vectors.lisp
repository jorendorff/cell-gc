;; R4RS 6.8 Vectors

;; `(vector? obj)` returns #t if obj is a vector, otherwise returns #f.

(assert (not (vector? '())))
(assert (not (vector? '(1 2 3))))
(assert (eq? (vector? (vector 'a 'b 'c)) #t))


;; essential procedure: (make-vector k)
;; procedure: (make-vector k fill)
;;
;; Returns a newly allocated vector of k elements. If a second argument is
;; given, then each element is initialized to fill. Otherwise the initial
;; contents of each element is unspecified.

(assert (eq? (vector-length (make-vector 3)) 3))
(assert (equal? (make-vector 7 0) '#(0 0 0 0 0 0 0)))
(letrec* ((pair (cons 3 4))
          (v (make-vector 2 pair)))  ; make-vector does not copy the pair
  (assert (eq? pair (vector-ref v 0)))
  (assert (eq? (vector-ref v 0)
               (vector-ref v 1))))


;; `vector` returns a newly allocated vector whose elements contain the given
;; arguments. Analogous to `list`.

(assert (equal? (vector 'a 'b 'c) '#(a b c)))


;; `(vector-length vector)` returns the number of elements in vector.

(assert (eq? (vector-length (vector 'a 'b 'c)) 3))


;; `(vector-ref vector k)` returns the contents of element k of vector.
;; k must be a valid index of vector.

(assert (eq? (vector-ref '#(1 1 2 3 5 8 13 21) 5)
             8))
;; (assert (eq? (vector-ref '#(1 1 2 3 5 8 13 21)
;;                          (inexact->exact
;;                           (round (* 2 (acos -1)))))
;;              13))


;; essential procedure: (vector-set! vector k obj)
;;
;; k must be a valid index of vector. Vector-set! stores obj in element k of
;; vector. The value returned by vector-set! is unspecified.

(assert (equal? (letrec ((vec (vector 0 '(2 2 2 2) "Anna")))
                  (vector-set! vec 1 '("Sue" "Sue"))
                  vec)
                '#(0 ("Sue" "Sue") "Anna")))

;;    (vector-set! '#(0 1 2) 1 "doe")
;;              ==>  error  ; constant vector

;; essential procedure: (vector->list vector)
;; essential procedure: (list->vector list)

;; Vector->list returns a newly allocated list of the objects contained in the
;; elements of vector. List->vector returns a newly created vector initialized
;; to the elements of the list list.

;;(assert (equal? (vector->list '#(dah dah didah))
;;                '(dah dah didah)))
(assert (equal? (list->vector '(dididit dah))
                '#(dididit dah)))

