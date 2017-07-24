;; R4RS 4.1 Primitive expression types

;; 4.1.1 Variable references
;;
;; essential syntax: <variable>
;;
;; An expression consisting of a variable (see section 3.1 Variables and
;; regions) is a variable reference. The value of the variable reference is the
;; value stored in the location to which the variable is bound. It is an error
;; to reference an unbound variable.
;;

(define x 28)
(assert (eq? x 28))

;; 4.1.2 Literal expressions
;;
;; essential syntax: quote <datum>
;; essential syntax: '<datum>
;; essential syntax: <constant>
;;
;; `(quote <datum>)' evaluates to <datum>. <Datum> may be any external
;; representation of a Scheme object (see section see section 3.3 External
;; representations). This notation is used to include literal constants in
;; Scheme code.

(assert (symbol? (quote a)))
(assert (eq? (quote a) 'a))

(assert (vector? (quote #(a b c))))
(assert (equal? (quote #(a b c)) (vector 'a 'b 'c)))

(assert (list? (quote (+ 1 2))))
(assert (equal? (quote (+ 1 2)) (list '+ 1 2)))

;; `(quote <datum>)' may be abbreviated as '<datum>. The two notations are equivalent in all respects.
;;
;;    'a                                     ==>  a
;;    '#(a b c)                              ==>  #(a b c)
;;    '()                                    ==>  ()
;;    '(+ 1 2)                               ==>  (+ 1 2)
;;    '(quote a)                             ==>  (quote a)
;;    ''a                                    ==>  (quote a)
;;
;;    Numerical constants, string constants, character constants, and boolean constants evaluate "to themselves"; they need not be quoted.

(assert (equal? '"abc" "abc"))
(assert (equal? "abc" "abc"))
(assert (eq? '145932 145932))
(assert (eq? '#t #t))

;; As noted in section 3.5 Storage model, it is an error to alter a constant
;; (i.e. the value of a literal expression) using a mutation procedure like
;; set-car! or string-set!.


;; 4.1.3 Procedure calls
;;
;; essential syntax: (<operator> <operand1> ...)
;;
;; A procedure call is written by simply enclosing in parentheses expressions
;; for the procedure to be called and the arguments to be passed to it. The
;; operator and operand expressions are evaluated (in an unspecified order) and
;; the resulting procedure is passed the resulting arguments.

(assert (eqv? (+ 3 4) 7))
(assert (eqv? ((if #f + *) 3 4) 12))

;; A number of procedures are available as the values of variables in the
;; initial environment; for example, the addition and multiplication procedures
;; in the above examples are the values of the variables + and *. New
;; procedures are created by evaluating lambda expressions (see section see
;; section 4.1.4 lambda expressions).
;;
;; Procedure calls are also called combinations.
;;
;; Note: In contrast to other dialects of Lisp, the order of evaluation is
;; unspecified, and the operator expression and the operand expressions are
;; always evaluated with the same evaluation rules.
;;
;; Note: Although the order of evaluation is otherwise unspecified, the effect
;; of any concurrent evaluation of the operator and operand expressions is
;; constrained to be consistent with some sequential order of evaluation. The
;; order of evaluation may be chosen differently for each procedure call.
;;
;; Note: In many dialects of Lisp, the empty combination, (), is a legitimate
;; expression. In Scheme, combinations must have at least one subexpression, so
;; () is not a syntactically valid expression.


;; 4.1.4 `lambda` expressions
;;
;; essential syntax: lambda <formals> <body>
;;
;; Syntax: <Formals> should be a formal arguments list as described below, and
;; <body> should be a sequence of one or more expressions.
;;
;; Semantics: A lambda expression evaluates to a procedure. The environment in
;; effect when the lambda expression was evaluated is remembered as part of the
;; procedure. When the procedure is later called with some actual arguments,
;; the environment in which the lambda expression was evaluated will be
;; extended by binding the variables in the formal argument list to fresh
;; locations, the corresponding actual argument values will be stored in those
;; locations, and the expressions in the body of the lambda expression will be
;; evaluated sequentially in the extended environment. The result of the last
;; expression in the body will be returned as the result of the procedure call.

(assert (procedure? (lambda (x) (+ x x))))
(assert (eqv? ((lambda (x) (+ x x)) 4)
              8))

(define reverse-subtract
  (lambda (x y) (- y x)))

(assert (eqv? (reverse-subtract 7 10) 3))

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(assert (eqv? (add4 6) 10))

;; <Formals> should have one of the following forms:
;;
;; *   `(<variable1> ...)`: The procedure takes a fixed number of arguments; when
;;     the procedure is called, the arguments will be stored in the bindings of
;;     the corresponding variables.
;;
;; *   `<variable>`: The procedure takes any number of arguments; when the
;;     procedure is called, the sequence of actual arguments is converted into
;;     a newly allocated list, and the list is stored in the binding of the
;;     <variable>.
;;
;; *   `(<variable1> ... <variable_n-1> . <variable_n>)`: If a space-delimited
;;     period precedes the last variable, then the value stored in the binding
;;     of the last variable will be a newly allocated list of the actual
;;     arguments left over after all the other actual arguments have been
;;     matched up against the other formal arguments.
;;
;;    It is an error for a <variable> to appear more than once in <formals>.

(assert (equal? ((lambda x x) 3 4 5 6)
                '(3 4 5 6)))

(assert (equal? ((lambda (x y . z) z) 3 4 5 6)
                '(5 6)))

;; Each procedure created as the result of evaluating a lambda expression is
;; tagged with a storage location, in order to make eqv? and eq? work on
;; procedures (see section see section 6.2 Equivalence predicates).


;; R4RS 4.1.5 Conditionals

;; (if <test> <consequent> <alternate>)
;; (if <test> <consequent>)
;;
;; <Test>, <consequent>, and <alternate> may be arbitrary expressions.
;;
;; An `if` expression is evaluated as follows: first, <test> is evaluated. If
;; it yields a true value (see section see section 6.1 Booleans), then
;; <consequent> is evaluated and its value is returned. Otherwise <alternate>
;; is evaluated and its value is returned. If <test> yields a false value and
;; no <alternate> is specified, then the result of the expression is
;; unspecified.

(assert (eq? (if (> 3 2) 'yes 'no)
             'yes))
(assert (eq? (if (> 2 3) 'yes 'no)
             'no))
(assert (eq? (if (> 3 2)
               (- 3 2)
               (+ 3 2))
             1))

(define x 1)
(assert (eq? (if #f #f)
             (set! x x)))  ;; the unspecified value
