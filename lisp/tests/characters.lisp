;; R4RS 6.6 Characters

(assert (char<? #\A #\B))
(assert (char<? #\a #\b))
(assert (char<? #\0 #\9))
(assert (char-ci=? #\A #\a))

; Two special cases.
(assert (eqv? (char-upcase #\ß) #\ß))
(assert (eqv? (char-downcase #\İ) #\i))
