;; simple-expander.scm - Hand-rolled mildly buggy expander for Scheme.
;;
;; To run this under Guile requires the following definition:
;;
;; (define assert
;;   (lambda (ok . args)
;;     (if (not ok)
;;         (apply error "assertion failed" args))))

;; Expand each expression in exprs.  Returns a list of expanded expressions.
(define expand-exprs
  (lambda (senv exprs)
    (map (lambda (expr) (expand-expr senv expr))
         exprs)))

;; Return the list of names bound by parameters in the given parameter-list.
(define param-names
  (lambda (params)
    (if (null? params)
        '()
        (if (symbol? params)
            (list params)
            (begin
              (assert (pair? params) "invalid parameters")
              (cons (car params) (param-names (cdr params))))))))

;; A senv is an alist whose keys are symbols and whose values are #f (for
;; variable bindings) or a procedure (for syntax bindings)

;; Prepend variable bindings for each of the given names to senv and return
;; the extended alist.
(define senv-extend-with-variable-bindings
  (lambda (senv names)
    (append (map (lambda (name)
                   (assert (symbol? name) "internal error: invalid syntactic binding")
                   (cons name #f))  ;; #f is significant
                 names)
            senv)))

;; Special symbol used in static environments to store #t if definitions are
;; allowed in the current syntactic context.
(define senv-defns-key (gensym))

;; Return a copy of senv with definitions allowed (if the argument defns is #t)
;; or forbidden (#f).
(define senv-with-defns
  (lambda (senv defns)
    (assert (boolean? defns))
    (cons (cons senv-defns-key defns) senv)))

;; True if definitions are allowed in the syntactic context senv.
(define senv-permits-defns?
  (lambda (senv)
    (cdr (assq senv-defns-key senv))))

;; Return the binding for a given keyword in the given static environment.
;; keyword must be a symbol. For variable bindings and unbound symbols, this
;; returns #f. For syntactic bindings, it returns a macro-expander procedure.
(define senv-lookup
  (lambda (senv keyword)
    (assert (not (eq? keyword senv-defns-key)))
    (letrec* ((binding (assq keyword senv)))
      (if binding
          (cdr binding)
          #f))))

;; Expanders for Scheme syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expand-lambda
  (lambda (senv expr)
    (assert (list? expr) "invalid lambda expression")
    (assert (>= (length expr) 3) "invalid lambda expression")
    (assert (eq? (car expr) 'lambda) "internal error: invalid lambda expression")
    (letrec* ((params (cadr expr))
              (body (cddr expr))
              (inner-senv (senv-extend-with-variable-bindings senv (param-names params))))
        (list 'lambda params (expand-body inner-senv body)))))

(define expand-quote
  (lambda (senv expr)
    (assert (list? expr) "invalid quote expression")
    (assert (= (length expr) 2) "invalid quote expression")
    (assert (eq? (car expr) 'quote))
    ;; Return it unchanged.
    expr))

(define expand-begin
  (lambda (senv expr)
    (assert (list? expr))
    (assert (not (null? expr)))
    (assert (eq? (car expr) 'begin))
    (cons 'begin (expand-exprs senv (cdr expr)))))

(define expand-define
  (lambda (senv expr)
    (assert (list? expr))
    (assert (>= (length expr) 3))
    (assert (eq? (car expr) 'define))
    (assert (senv-permits-defns? senv)
            "definition in expression context, where definitions are not allowed")
    (letrec* ((pattern (cadr expr))
              (body (cddr expr)))
      (if (pair? pattern)
          ;; function definition input: `(define (,name ,@params) ,@body)
          (letrec* ((name (car pattern))
                    (params (cdr pattern)))
            ;; output: `(define ,name (lambda ,params ,@body))
            ;; BUG: symbol 'lambda may have been redefined :(
            (expand-define senv (list 'define name
                                      (cons 'lambda (cons params body)))))
          (begin
            (assert (= (length body) 1))
            (list 'define
                  pattern
                  (expand-expr senv (car body))))))))

(define expand-if
  (lambda (senv expr)
    (assert (list? expr))
    (assert (>= (length expr) 3))
    (assert (<= (length expr) 4))
    (assert (eq? (car expr) 'if))
    (cons 'if (expand-exprs senv (cdr expr)))))

(define expand-let
  (lambda (senv expr)
    (assert (eq? (car expr) 'let))
    ;; input: `(let ((,name0 ,init-expr0) ...) ,@body)
    (letrec* ((bindings (cadr expr)))
      (for-each (lambda (binding)
                  (assert (= (length binding) 2)))
                bindings)
      (letrec* ((names (map car bindings))
                (init-exprs (map (lambda (binding) (expand-expr senv (cadr binding)))
                                 bindings))
                (nested-senv (senv-extend-with-variable-bindings senv names))
                (body-expr (expand-body nested-senv (cddr expr))))
        ;; output: `((lambda ,names @body-expr) ,@init-exprs)
        (cons (list 'lambda names body-expr)
              init-exprs)))))

(define expand-cond
  (lambda (senv expr)
    (assert (list? expr))
    (assert (>= (length expr) 2))
    (assert (eq? (car expr) 'cond))
    (if (= (length expr) 2)  ;; base case input: `(cond (else ,@else-body))
        (letrec* ((else-clause (cadr expr)))
          (assert (list? else-clause))
          (assert (eq? (car else-clause) 'else))
          ;; base case output: (begin ELSE-BODY)
          (expand-body senv (cdr else-clause)))
        ;; recursive case input: `(cond (,test ,@body) ,@clauses)
        (letrec* ((test (caadr expr))
                  (body (cdadr expr))
                  (clauses (cddr expr)))
          ;; recursive case output: `(if ,test (begin ,@body) (cond ,@clauses))
          ;; BUG: 'if may have been redefined
          (list 'if (expand-expr senv test)
                (expand-body senv body)
                (expand-cond senv (cons 'cond clauses)))))))

(define expand-and
  (lambda (senv expr)
    (assert (eq? (car expr) 'and))
    (if (null? (cdr expr))  ;; special case input: (and)
        #t
        (if (null? (cddr expr))  ;; base case input: (and e)
            (expand-expr senv (cadr expr))
            ;; usual case
            ;; input: `(and ,expr0 ,expr1 ...)
            ;; output: `(if ,expr0 ^(and ,expr1 ...)  #f)
            ;; BUG: 'if may have been redefined
            (list 'if
                  (expand-expr senv (cadr expr))
                  (expand-and senv (cons 'and (cddr expr)))
                  #f)))))

(define expand-or
  (lambda (senv expr)
    (assert (eq? (car expr) 'or))
    (if (null? (cdr expr))  ;; special case input: (or)
        #f
        (if (null? (cddr expr))  ;; base case input: (or e)
            (expand-expr senv (cadr expr))
            ;; usual case
            ;; input: `(or ,expr0 ,expr1 ...)
            ;; output: `(let ((x ,expr0)) (if x x ^(or ,expr1 ...)))
            ;; BUG: 'if may have been redefined ('let is not a problem)
            (letrec* ((tmp (gensym)))
              (expand-let senv (list 'let
                                     (list (list tmp (expand-expr senv (cadr expr))))
                                     (list 'if tmp tmp (cons 'or (cddr expr))))))))))

;; Bodies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define senv-has-original-binding
  (lambda (senv keyword original-svalue)
    (letrec* ((svalue (senv-lookup senv 'define)))
      (eqv? svalue original-svalue))))

;; True if form "looks like a definition" in the given syntactic context senv.
;; That is, it has the form (define x0 ...) or (begin) or (begin f0 f1 ...)
;; where f0 is a definition; and the corresponding syntactic keyword `define`
;; or `begin` is not shadowed by a local binding.
(define defn?
  (lambda (senv form)
    (if (list? form)
        (if (eq? (car form) 'define)
            (senv-has-original-binding senv 'define expand-define)
            (if (eq? (car form) 'begin)
                (if (senv-has-original-binding senv 'begin expand-begin)
                    (if (null? (cdr form))
                        #t  ;; treat empty (begin) as a definition form
                        (defn? senv (cadr form)))
                    #f)
                #f))
        #f)))

(define defn->defn-list
  (lambda (senv form)
    (if (eq? (car form) 'begin)
        (append (defn->defn-list senv (cadr form))
                (defn->defn-list senv (cons 'begin (cddr form))))
        (list form))))

;; Split a <body> into two parts: a list of `define` forms, and a list
;; containing the leftover forms after the last definition (in a correct
;; program, a list of expressions). Any `(begin d0 d1...)` forms in the <body>
;; are flattened out into the former list. Returns a pair of lists.
(define skim-defns
  (lambda (senv forms)
    (letrec* ((first (car forms))
              (first-expanded (expand-defn-or-expr senv first)))
      (if (defn? senv first-expanded)
          (letrec* ((first-defns (defn->defn-list senv first-expanded))
                    (pair (skim-defns senv (cdr forms)))
                    (other-defns (car pair))
                    (exprs (cdr pair)))
            (cons (append first-defns other-defns) exprs))
          (cons '() forms)))))

;; Expand a <body>, the tail of a lambda or let-like form: 0 or more
;; definitions, followed by at least one expression.
(define expand-body
  (lambda (senv body)
    (assert (list? body))
    (letrec* ((defn-senv (senv-with-defns senv #t))
              (answer (skim-defns defn-senv body))
              (defns (car answer))
              (defn-bindings (map (lambda (defn)
                                    (assert (list? defn) "definition expected")
                                    (assert (eq? (car defn) 'define) "definition expected")
                                    (assert (= (length defn) 3) "bad definition")
                                    (cdr defn))
                                  defns))
              (rest (cdr answer))
              ;; BUG: 'begin may have been redefined
              (rest-expanded (if (null? rest)
                                 (error "expression required")
                                 (if (= 1 (length rest))
                                     (expand-expr senv (car rest))
                                     (cons 'begin (expand-exprs senv rest))))))
      (if (null? defns)
          rest-expanded
          (list 'letrec* defn-bindings rest-expanded)))))

;; Toplevel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-senv
  (lambda ()
    (list (cons 'lambda expand-lambda)
          (cons 'quote expand-quote)
          (cons 'begin expand-begin)
          (cons 'define expand-define)
          (cons 'if expand-if)
          (cons 'let expand-let)
          (cons 'cond expand-cond)
          (cons 'and expand-and)
          (cons 'or expand-or)
          (cons senv-defns-key #t))))

(define expand-call
  (lambda (senv expr)
    (assert (list? expr))
    (expand-exprs (senv-with-defns senv #f) expr)))

(define self-evaluating?
  (lambda (expr)
    (if (number? expr) #t
        (if (char? expr) #t
            (if (string? expr) #t
                (if (boolean? expr) #t
                    #f))))))

(define expand-expr
  (lambda (senv expr)
    (expand-defn-or-expr (if (senv-permits-defns? senv)
                             (senv-with-defns senv #f)
                             senv)
                         expr)))

(define expand-defn-or-expr
  (lambda (senv expr)
    (if (pair? expr)
        (begin
          (assert (list? expr))
          (letrec* ((expander (if (symbol? (car expr))
                                 (senv-lookup senv (car expr))
                                 #f)))
            (if expander
                (expander senv expr)
                (expand-call senv expr))))
        (if (self-evaluating? expr)
            expr
            (if (symbol? expr)
                (begin
                  (assert (not (senv-lookup senv expr)))
                  expr)
                (assert #f "unrecognized expression" expr))))))

(assert (equal? (expand-body (default-senv) '((newline)))
                '(newline)))
(assert (equal? (expand-body (default-senv) '((write 3) (newline)))
                '(begin (write 3) (newline))))

(assert (equal? (expand-defn-or-expr (default-senv) '3) 3))
(assert (equal? (expand-defn-or-expr (default-senv) '#f) #f))
(assert (equal? (expand-defn-or-expr (default-senv) 'f) 'f))
(assert (equal? (expand-defn-or-expr (default-senv) '(quote ()))
                '(quote ())))
(assert (equal? (expand-defn-or-expr (default-senv) '(lambda (lambda) (lambda)))
                '(lambda (lambda) (lambda))))
(assert (equal? (expand-defn-or-expr (default-senv) '(+ 2 2)) '(+ 2 2)))
(assert (equal? (expand-defn-or-expr (default-senv) '(and)) #t))
(assert (equal? (expand-defn-or-expr (default-senv) '(or)) #f))
(assert (equal? (expand-defn-or-expr (default-senv) '(and a b)) '(if a b #f)))

(assert (equal? (expand-defn-or-expr (default-senv) '(let ((x (f))
                                                           (y (g x)))
                                                       (list x y)))
                '((lambda (x y) (list x y)) (f) (g x))))

(assert (equal? (expand-defn-or-expr (default-senv) '(let ((and 3) (or 4)) (+ and or)))
                '((lambda (and or) (+ and or)) 3 4)))

(assert (equal? (expand-defn-or-expr (default-senv) '(let () 33))
                '((lambda () 33))))

(assert (equal? (expand-defn-or-expr (default-senv) '(let () (define x 1) (define y x) (+ x y)))
                '((lambda () (letrec* ((x 1) (y x)) (+ x y))))))

(define expand-toplevel
  (lambda (senv form)
    (if (if (list? form)
            (eq? (car form) 'begin)
            #f)
        (cons 'begin (map (lambda (form) expand-toplevel senv form)
                          (cdr form)))
        (expand-defn-or-expr senv form))))

(define toplevel-senv (default-senv))
(lambda (form) (expand-toplevel toplevel-senv form))

