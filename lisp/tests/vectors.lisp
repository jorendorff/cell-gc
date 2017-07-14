;; R4RS 6.8 Vectors

;; `(vector? obj)` returns #t if obj is a vector, otherwise returns #f. 

(assert (not (vector? '())))
(assert (not (vector? '(1 2 3))))
(assert (eq? (vector? (vector 'a 'b 'c)) #t))


;; `vector` returns a newly allocated vector whose elements contain the given
;; arguments. Analogous to `list`.

;;(assert (equal? (vector 'a 'b 'c) '#(a b c)))


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
