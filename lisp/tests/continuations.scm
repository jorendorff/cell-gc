;; Basics.
(assert (procedure? call/cc))
(assert (call/cc procedure?))

;; Examples from R4RS.
(assert (=
         (call-with-current-continuation
          (lambda (exit)
            (for-each (lambda (x)
                        (if (negative? x)
                            (exit x)))
                      '(54 0 37 -3 245 19))
            #t))
         -3))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return #f))))))
          (r obj))))))

(assert (= (list-length '(1 2 3 4))
           4))

(assert (eq? (list-length '(a b . c))
             #f))


;; Implementation of (amb) and other pieces for the SICP example

(define (fail)
  (error 'require "no solutions"))

(define backtrack fail)

(define (require ok)
  (if (not ok)
      (backtrack)))

;; Return one of the values in `values` such that all future `(require)` calls
;; are satisfied.
(define (amb . values)
  (let ((backtrack-further backtrack))
    (call/cc (lambda (ctn)
               (define (next values)
                 (if (null? values)
                     (backtrack-further)
                     (begin (set! backtrack
                                  (lambda () (next (cdr values))))
                            (ctn (car values)))))
               (next values)))))

(define (reset-amb)
  (set! backtrack fail))

(let ()
  (define x (amb 1 2 3))
  (require (= x 1))
  (assert (= x 1))
  (reset-amb))

(let ()
  (define x (amb 1 2 3))
  (require (not (= x 1)))
  (assert (not (= x 1)))
  (reset-amb))

(let ()
  (define x (amb 1 2 3 4 5))
  (require (> x 2))
  (assert (> x 2))
  (reset-amb))

(let ()
  (define x (amb 1 2 3 4 5 6 7 8 9))
  (define y (amb 1 2 3 4 5 6 7 8 9))
  (require (= (* x y) 24))
  (assert (= x 3))
  (assert (= y 8))
  (reset-amb))

(let ()
  (define x (amb 1 2 3 4 5 6 7 8 9))
  (define y (amb 1 2 3 4 5 6 7 8 9))
  (require (= (* x y) 24))
  (require (> x y))
  (assert (= x 6))
  (assert (= y 4))
  (reset-amb))

(let ()
  (define x (amb 1 2 3 4 5 6 7 8 9))
  (define y (amb 1 2 3 4 5 6 7 8 9))
  (require (= (* x y) 24))
  (require (> x y))
  (require (or (odd? x) (odd? y)))
  (assert (= x 8))
  (assert (= y 3))
  (reset-amb))


(define (distinct? values)
  (or (null? values)
      (and (not (memv (car values) (cdr values)))
           (distinct? (cdr values)))))


;; The SICP example.

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(assert (equal? (multiple-dwelling)
                '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))

