;; repl.csm - The read-eval-print loop.
;;
;; This repl has an odd property that doesn't ordinarily come into play: if you
;; capture a continuation, then interact with the repl, then invoke that saved
;; continuation, the repl erases everything that's happened since the
;; continuation was captured from the terminal (using VT100 escape sequences)
;; and replays the whole interaction from that point forward.

(let ()
  (define (erase-line)
    (real-display "\r") ;; move cursor to start of line
    (real-display "\x1b[1A") ;; move up 1 line
    (real-display "\x1b[K")) ;; erase to end of line

  (define real-display display)
  (define real-write write)
  (define real-read-line read-line)

  (define terminal-todo '())

  (define (virtual-display s)
    (set! terminal-todo (cons (cons 'display s) terminal-todo)))

  (define (virtual-write v)
    (set! terminal-todo (cons (cons 'write v) terminal-todo)))

  (define (virtual-newline)
    (virtual-display "\n"))

  (define (flush-virtual-writes)
    (for-each (lambda (cmd)
                (case (car cmd)
                  ((display) (real-display (cdr cmd)))
                  ((write) (real-write (cdr cmd)))
                  ((erase-line) (erase-line))))
              (reverse terminal-todo))
    (set! terminal-todo '()))

  (define (virtual-read-line)
    (flush-virtual-writes)
    (real-read-line))

  (define (un action value)
    (if (and (not (null? terminal-todo))
             (equal? (cons action value) (car terminal-todo)))
        (set! terminal-todo (cdr terminal-todo))
        (if (eq? action 'display)
            (for-each (lambda (c)
                        (if (eqv? c #\newline)
                            (set! terminal-todo (cons '(erase-line) terminal-todo))))
                      (string->list value)))))

  (define (undisplay s) (un 'display s))
  (define (unwrite value) (un 'write value))

  ;; Stack of commands to redo when rolling forward.
  (define redo-stack '())

  ;; Read a line of input.
  (define (prompt-and-read-line prompt on-success on-eof)
    (display prompt)
    (let ((line (if (null? redo-stack)
                    (read-line)
                    (let ((recorded-line (car redo-stack)))
                      (set! redo-stack (cdr redo-stack))
                      (display recorded-line)
                      recorded-line))))
      (if (= (string-length line) 0)
          (begin (display "\n")
                 (flush-virtual-writes)
                 (undisplay "\n")
                 (undisplay prompt)
                 (on-eof))
          (dynamic-wind
              (lambda () #f)
              (lambda () (on-success line))
              (lambda ()
                (undisplay line)
                (undisplay prompt)
                (set! redo-stack (cons line redo-stack)))))))

  (define PS1 "» ")
  (define PS2 "… ")

  (define error-prefix "\x1b[31;1merror:\x1b[30;1m ")
  (define error-postfix "\x1b[0m\n")

  (define value-prefix "\x1b[36;1m")
  (define value-postfix "\x1b[0m\n")

  (define (read on-success on-error on-eof)
    (let loop ((prompt PS1) (previous-input ""))
      (prompt-and-read-line prompt
                            (lambda (line)
                              (let* ((input (string-append previous-input line))
                                     (result (parse input)))
                                (case (car result)
                                  ((ok) (on-success (cdr result)))
                                  ((error) (on-error (cdr result)))
                                  ((incomplete) (loop PS2 (string-append previous-input line)))
                                  (else (error "unexpected result from (parse)" result)))))
                            on-eof)))

  (define (cps-evaluate-forms forms ctn)
    (if (null? forms)
        (ctn)
        (let ((result (try (lambda ()
                             (eval (cons 'begin forms) (interaction-environment))))))
          (case (car result)
            ((ok) (cps-write (cdr result) ctn))
            ((error) (cps-display-error (cdr result) ctn))
            (else (error "unexpected result from (try)" result))))))

  (define (cps-display-error message ctn)
    (let ((full-message (string-append error-prefix message error-postfix)))
      (dynamic-wind
          (lambda () (display full-message))
          ctn
          (lambda () (undisplay full-message)))))

  (define (unspecified? value)
    (eq? value (if #f #f)))

  (define (cps-write value ctn)
    (if (unspecified? value)
        (ctn)
        (dynamic-wind
            (lambda ()
              (display value-prefix)
              (write value)
              (display value-postfix))
            ctn
            (lambda ()
              (undisplay value-postfix)
              (unwrite value)
              (undisplay value-prefix)))))

  (define (repl)
    (call/cc (lambda (exit)
               (read (lambda (forms) ;; on-success
                       (cps-evaluate-forms forms repl))
                     (lambda (message) ;; on-error
                       (cps-display-error message repl))
                     (lambda () ;; on-eof
                       (exit (if #f #f)))))))

  (set! display virtual-display)
  (set! newline virtual-newline)
  (set! write virtual-write)
  (set! read-line virtual-read-line)
  (repl))
