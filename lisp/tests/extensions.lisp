(assert (symbol? (gensym)))
(assert (gensym? (gensym)))
(assert (not (eq? (gensym) (gensym))))

(define s (gensym))
(define t (string->symbol (symbol->string s)))
(assert (gensym? s))
(assert (not (gensym? t)))
(assert (not (eq? s t)))

(assert (not (gensym? 'ponies)))
(assert (not (gensym? '())))
