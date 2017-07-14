;; R4RS 6.1 Booleans

; Boolean constants evaluate to themselves, so they don't need to be quoted in programs. 
(assert (eq? #t '#t))
(assert (eq? #f '#f))
(assert (eq? '#f '#f))

; `not` returns #t if obj is false, and returns #f otherwise. 
(assert (eq? (not #t) #f))
(assert (eq? (not 3) #f))
;;(assert (eq? (not (list 3)) #f))
(assert (eq? (not #f) #t))
(assert (eq? (not '()) #f))
;;(assert (eq? (not (list)) #f))
(assert (eq? (not 'nil) #f))

; `boolean?` returns #t if obj is either #t or #f and returns #f otherwise.
(assert (eq? (boolean? #f) #t))
(assert (eq? (boolean? 0) #f))
(assert (eq? (boolean? '()) #f))
