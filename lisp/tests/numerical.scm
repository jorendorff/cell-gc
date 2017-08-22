;; R4RS 6.5.5 Numerical operations

;; zero?, positive?, negative?, even?, odd?
(assert (zero? 0))
(assert (not (zero? -1)))

(assert (positive? 1))
(assert (not (positive? 0)))

(assert (negative? -1))
(assert (not (negative? 0)))

(assert (even? 0))
(assert (not (even? 1)))
(assert (even? -2))
(assert (even? -2147483648))

(assert (not (odd? 0)))
(assert (odd? -1))
(assert (odd? 2147483647))
(assert (not (odd? -2147483648)))

;; `(abs x)` returns the magnitude of its argument.
(assert (eq? (abs -7) 7))

;; quotient, remainder, modulo
(assert (= (modulo 13 4) 1))
(assert (= (remainder 13 4) 1))

(assert (= (modulo -13 4) 3))
(assert (= (remainder -13 4) -1))

(assert (= (modulo 13 -4) -3))
(assert (= (remainder 13 -4) 1))

(assert (= (modulo -13 -4) -1))
(assert (= (remainder -13 -4) -1))
