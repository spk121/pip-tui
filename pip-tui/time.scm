#!curly-infix

;;; Routines that deal with time and timing.

(define-module (pip-tui time)
  #:export (now))

(define (now)
  "Returns the time (in seconds) since 1970."
  (let ([t (gettimeofday)])
    {car(t) + {cdr(t) / 1000000.0}}))
