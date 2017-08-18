;; R5RS 6.5 Eval

;; procedure:  (eval expression environment-specifier)
;;
;; Evaluates expression in the specified environment and returns its
;; value. Expression must be a valid Scheme expression represented as data, and
;; environment-specifier must be a value returned by one of the three
;; procedures described below. Implementations may extend eval to allow
;; non-expression programs (definitions) as the first argument and to allow
;; other values as environments, with the restriction that eval is not allowed
;; to create new bindings in the environments associated with null-environment
;; or scheme-report-environment.

(assert (eq? (eval '(* 7 3) (interaction-environment))
             21))

(assert (eq? (letrec ((f (eval '(lambda (f x) (f x x))
                               (interaction-environment))))
               (f + 10))
             20))

;; optional procedure:  (interaction-environment)
;;
;; This procedure returns a specifier for the environment that contains
;; implementation-defined bindings, typically a superset of those listed in the
;; report. The intent is that this procedure will return the environment in
;; which the implementation would evaluate expressions dynamically typed by the
;; user.

;; eval runs in the toplevel environment, not the caller's environment.
(define x 'interaction)
(assert (eq? (letrec ((x 'local))
               (eval 'x (interaction-environment)))
             'interaction))

;; eval can define variables in the interaction environment.
(eval '(define name 'jimb)
      (interaction-environment))
(assert (eq? name 'jimb))

(eval '(begin
        (define y 30)
        (define z '(a b c)))
      (interaction-environment))
(assert (eqv? y 30))
(assert (equal? z '(a b c)))
