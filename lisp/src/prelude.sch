;; I stole these lines of code from <https://www.bluishcoder.co.nz/jsscheme/>.
;; Original by Alex Yakovlev. Adapted by Chris Double.

(define (list . x) x)
(define (not x) (if x #f #t))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (zero? x) (= x 0))
(define (abs x) (if (< x 0) (- x) x))
(define magnitude abs)
;
(define (map f ls . more)
  (define (map1 l)
    (if (null? l)
      '()
      (if (pair? l)
          (cons (f (car l)) (map1 (cdr l)))
          (f l))))
  (define (map-more l m)
    (if (null? l)
        '()
        (if (pair? l)
            (cons (apply f (car l) (map car m))
                  (map-more (cdr l)
                            (map cdr m)))
            (apply f l m))))
  (if (null? more)
      (map1 ls)
      (map-more ls more)))
;
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
;
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
;
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
;
(define (length lst . x)
  (define l (if (null? x) 0 (car x)))
  (if (pair? lst) (length (cdr lst) (+ l 1)) l))
(define (length+ lst . x)
  (define l (if (null? x) 0 (car x)))
  (if (null? lst) l
      (if (pair? lst) (length+ (cdr lst) (+ l 1)) (+ l 1))))

(define (list-ref lst n)
  (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
(define (list-tail lst n)
  (if (= n 0) lst (list-tail (cdr lst) (- n 1))))
(define (reverse lst . l2)
  (define r (if (null? l2) l2 (car l2)))
  (if (null? lst) r
      (reverse (cdr lst) (cons (car lst) r))))
;
(define (memq+ x ls)
  (if (pair? ls)
      (if (eq? (car ls) x) ls
          (memq+ x (cdr ls)))
      (if (eq? x ls) ls #f)))
(define memq memq+)
(define (memv x ls)
  (if (pair? ls)
      (if (eqv? (car ls) x) ls
          (memv x (cdr ls)))
  (if (eqv? x ls) ls #f)))
(define (member x ls)
  (if (pair? ls)
      (if (equal? (car ls) x) ls
          (member x (cdr ls)))
  (if (equal? x ls) ls #f)))
;
(define (assq x ls)
  (if (null? ls) #f
      (if (eq? (caar ls) x) (car ls)
          (assq x (cdr ls)))))
(define (assv x ls)
  (if (null? ls) #f
      (if (eqv? (caar ls) x) (car ls)
          (assv x (cdr ls)))))
(define (assoc x ls)
  (if (null? ls) #f
      (if (equal? (caar ls) x) (car ls)
          (assoc x (cdr ls)))))
;
(define list?
  ((lambda ()
    (define (race h t)
      (if (pair? h)
          ((lambda (h)
             (if (pair? h)
                 (if (not (eq? h t))
                     (race (cdr h) (cdr t))
                     #f)
                 (null? h))) (cdr h))
          (null? h)))
    (lambda (x) (race x x)))))
;
(define equal?
  (lambda (x y)
    ((lambda (eqv)
       (if eqv eqv
           (if (pair? x)
               (begin
                 (if (pair? y)
                     (if (equal? (car x) (car y))
                         (equal? (cdr x) (cdr y))
                         #f)
                     #f))
                 (if (vector? x)
                     (if (vector? y)
                         ((lambda (n)
                            (if (= (vector-length y) n)
                                ((letrec* ((loop
                                            (lambda (i)
                                              ((lambda (eq-len)
                                                 (if eq-len
                                                     eq-len
                                                     (if (equal? (vector-ref x i)
                                                                 (vector-ref y i))
                                                         (loop (+ i 1))
                                                         #f)))
                                               (= i n)))))
                                    loop)
                                 0)
                                #f))
                          (vector-length x))
                         #f)
                     #f))))
     (eqv? x y))))
