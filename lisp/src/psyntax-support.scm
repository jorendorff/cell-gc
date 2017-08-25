
;;; The following nonstandard procedures must be provided by the
;;; implementation for the portable syntax-case code to run.
;;;
;;; (void)
;;; returns the implementation's cannonical "unspecified value".
;;
;; (note: this also has to be present in the target environment,
;; so we define it in prelude.sch instead of here).

;;; (andmap proc list1 list2 ...)
;;; returns true if proc returns true when applied to each element of list1
;;; along with the corresponding elements of list2 ....
(define andmap
  (lambda (f first . rest)
    (if (null? first)
        #t
        (if (apply f (car first) (map car rest))
            (apply andmap f (cdr first) (map cdr rest))
            #f))))

;;; (ormap proc list1)
;;; returns the first non-false return result of proc applied to
;;; the elements of list1 or false if none.
(define ormap
  (lambda (f list1)
    (if (null? list1)
        #f
        ((lambda (result)
           (if result
               result
               (ormap f (cdr list1))))
         (apply f (car list1))))))

;;; (eval x)
;;; where x is always in the form ("noexpand" expr).
;;; returns the value of expr.  the "noexpand" flag is used to tell the
;;; evaluator/expander that no expansion is necessary, since expr has
;;; already been fully expanded to core forms.
;;;
;;; eval will not be invoked during the loading of psyntax.pp.  After
;;; psyntax.pp has been loaded, the expansion of any macro definition,
;;; whether local or global, results in a call to eval.  If, however,
;;; sc-expand has already been registered as the expander to be used
;;; by eval, and eval accepts one argument, nothing special must be done
;;; to support the "noexpand" flag, since it is handled by sc-expand.

(define (eval datum)
  (original-eval datum psyntax-environment))


;;; (error who format-string why what)
;;; where who is either a symbol or #f, format-string is always "~a ~s",
;;; why is always a string, and what may be any object.  error should
;;; signal an error with a message something like
;;;
;;;    "error in <who>: <why> <what>"
;;;
(define (error who format-string why what)
  (raise (vector 'error
                 (if who
                     (string-append (symbol->string who) ": " why)
                     why)
                 (list what))))

;;; (putprop symbol key value)
;;; (getprop symbol key)
;;; (remprop symbol key)
;;; key is always a symbol; value may be any object.  putprop should
;;; associate the given value with the given symbol and key in some way
;;; that it can be retrieved later with getprop.  getprop should return
;;; #f if no value is associated with the given symbol and key.  remprop
;;; should remove the association between the given symbol and key.

(define putprop #f)
(define getprop #f)
(define remprop #f)
(letrec ()
  ;; props has the type (alist symbol (alist key value)).
  (define props '())

  (define (get-or-create-entry-for-symbol symbol)
    (define existing (assq symbol props))
    (if existing
        existing
        (letrec ((entry (cons symbol '())))
          (set! props (cons entry props))
          entry)))

  (set! putprop (lambda (symbol key value)
                  (define symbol-entry (get-or-create-entry-for-symbol symbol))
                  (define key-entry (assq key (cdr symbol-entry)))
                  (if key-entry
                      ;; change existing entry
                      (set-cdr! key-entry value)
                      ;; push new entry onto symbol-entry
                      (set-cdr! symbol-entry
                                (cons (cons key value) (cdr symbol-entry))))))

  (set! getprop (lambda (symbol key)
                  (define symbol-entry (assq symbol props))
                  (if symbol-entry
                      (letrec ((key-entry (assq key (cdr symbol-entry))))
                        (if key-entry
                            (cdr key-entry)
                            #f))
                      #f)))

  (set! remprop (lambda (symbol key)
                  (define symbol-entry (assq symbol props))
                  (if symbol-entry
                      (letrec ((key-entry (assq key (cdr symbol-entry))))
                        (if key-entry
                            (set-cdr! key-entry #f)))))))

;; These R5RS primitives are possibly the worst idea ever.
(define values list)
(define (call-with-values f g)
  (apply g (f)))

;; `sc-expand` doesn't handle toplevel (begin) forms, but it's easy to add
;; support.
(define (sc-expand-toplevel form)
  (if (if (list? form)
          (eq? (car form) 'begin)
          #f)
      (cons 'begin (map sc-expand-toplevel (cdr form)))
      (sc-expand form)))
