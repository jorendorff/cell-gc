;; R4RS 6.4 Symbols

;; (symbol? obj) returns #t if obj is a symbol, otherwise returns #f.

(assert (eq? (symbol? 'foo) #t))
(assert (eq? (symbol? (car '(a b))) #t))
(assert (eq? (symbol? "bar") #f))
(assert (eq? (symbol? 'nil) #t))
(assert (eq? (symbol? '()) #f))
(assert (eq? (symbol? #f) #f))


;; (symbol->string symbol) returns the name of symbol as an immutable string.
;;
;; NOTE: We use the R6RS version of this, since it's case-sensitive.

(assert (equal? (symbol->string 'flying-fish) "flying-fish"))
(assert (equal? (symbol->string 'Martin) "Martin"))
(assert (equal? (symbol->string (string->symbol "Malvina")) "Malvina"))


;; (string->symbol string)‌‌ returns the symbol whose name is string.

(assert (not (eq? 'mISSISSIppi 'mississippi)))
(assert (eq? (string->symbol "mISSISSIppi") 'mISSISSIppi))
(assert (eq? (eq? 'bitBlt (string->symbol "bitBlt")) #t))
(assert (eq? (eq? 'JollyWog
                  (string->symbol
                   (symbol->string 'JollyWog)))
             #t))

(assert (eq? (string=? "K. Harper, M.D."
                       (symbol->string
                        (string->symbol "K. Harper, M.D.")))
             #t))
