;; R7RS 6.9 Bytevectors

(define v #u8(0 10 5))
(assert (= (bytevector-length v) 3))
(assert (= (bytevector-u8-ref v 0) 0))
(assert (= (bytevector-u8-ref v 1) 10))
(assert (= (bytevector-u8-ref v 2) 5))
(assert (equal? v (bytevector 0 10 5)))
(assert (not (equal? v (bytevector 0 10 6))))
(assert (not (equal? v (bytevector 0 10 5 0))))
(assert (not (equal? v (bytevector))))

(assert (equal? (make-bytevector 2 12)
                #u8(12 12)))
(assert (equal? (bytevector 1 3 5 1 3 5)
                #u8(1 3 5 1 3 5)))
(assert (equal? (bytevector)
                #u8()))

(assert (= (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21)
                               5)
           8))

(assert (equal?
         (let ((bv (bytevector 1 2 3 4)))
           (bytevector-u8-set! bv 1 3)
           bv)
         #u8(1 3 3 4)))

(define a #u8(1 2 3 4 5))
(assert (equal? (bytevector-copy a 2 4)
                #u8(3 4)))

(define a (bytevector 1 2 3 4 5))
(define b (bytevector 10 20 30 40 50))
(bytevector-copy! b 1 a 0 2)
(assert (equal? b
                #u8(10 1 2 40 50)))

(assert (equal? (bytevector-append #u8(0 1 2) #u8(3 4 5))
                #u8(0 1 2 3 4 5)))

(assert (equal? (bytevector-append)
                #u8()))

(assert (equal? (utf8->string #u8(65)) ;; #u8(#x41) in the original
               "A"))
(assert (equal? (string->utf8 "λ")
                #u8(206 187))) ;; #u8(#xCE #xBB) in the original
