;; R4RS 6.6 Characters

(assert (char=? #\  #\space))
(assert (char=? #\newline  #\
))

(assert (char<? #\A #\B))
(assert (char<? #\a #\b))
(assert (char<? #\0 #\9))
(assert (char-ci=? #\A #\a))

(assert (eqv? (char-upcase #\a) #\A))
(assert (eqv? (char-upcase #\A) #\A))
(assert (eqv? (char-downcase #\a) #\a))
(assert (eqv? (char-downcase #\A) #\a))

; Two special cases.
(assert (eqv? (char-upcase #\ß) #\ß))
(assert (eqv? (char-downcase #\İ) #\i))

(assert (= (char->integer #\space) 32))
(assert (= (char->integer #\ß) 223))
(assert (= (char->integer #\💯) 128175))
(assert (eqv? (integer->char 223) #\ß))
(assert (eqv? (integer->char 10) #\newline))
