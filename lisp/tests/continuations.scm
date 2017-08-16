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
  (display "no solutions")
  (newline)
  *ERROR*)

(define backtrack fail)

(define (require ok)
  (if (not ok)
      (backtrack)))

(define (amb . values)
  (require (not (null? values)))
  (call/cc (lambda (ctn)
             (let ((older backtrack))
               (set! backtrack (lambda ()
                                 (set! backtrack older)
                                 (ctn (apply amb (cdr values))))))
             (car values))))

(define (for-all lst pred)
  (if (null? lst)
      #t
    (and (pred (car lst))
         (for-all (cdr lst) pred))))

(define (distinct? values)
  (or (null? values)
      (and (for-all (cdr values) (lambda (v)
                                   (not (= v (car values)))))
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
