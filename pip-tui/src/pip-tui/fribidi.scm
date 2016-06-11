(define-module (pip-tui fribidi)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (RTL
	    LTR
	    WRTL
	    WLTR
	    ON
	    string-logical->visual
	    string-par-direction)
  )

(define (string-par-direction str)
  "Returns a guess as to the base paragraph direction for this string.
One of 'rtl, 'ltr, 'wrtl, 'wltr, or 'on, for right-to-left, left-to-right,
weak right-to-left, weak left-to-right, and ambiguous respectively"
  (let ((ret (%string-par-direction str)))
    (cond
     ((= ret RTL)
      'rtl)
     ((= ret LTR)
      'ltr)
     ((= ret WRTL)
      'wrtl)
     ((= ret WLTR)
      'wltr)
     ((= ret ON)
      'on))))

(define (string-logical->visual str paragraph-direction)
  "Given a string and a base paragraph direction for this string,
this returns a new string with the bidirectional text in visual order.
The paragraph direction is one of 'rtl, 'ltr, 'wrtl, 'wltr, or 'on,
for right-to-left, left-to-right, weak right-to-left, weak
left-to-right, and ambiguous respectively."
  (let ((D (cond
	    ((eqv? paragraph-direction 'rtl)
	     RTL)
	    ((eqv? paragraph-direction 'ltr)
	     LTR)
	    ((eqv? paragraph-direction 'wrtl)
	     WRTL)
	    ((eqv? paragraph-direction 'wltr)
	     WLTR)
	    ((eqv? paragraph-direction 'on)
	     ON))))
    (%string-logical->visual str D)))

(load-extension "libguile-fribidi" "bidi_init")

;; (setlocale LC_ALL "")
;; (string-par-direction "hello")
;; (define x (apply string (map (lambda (c) (integer->char (+ c 32))) (iota 10000))))
;; (write x)
;; (newline)
;; (newline)

;; (write (string-logical->visual x 'rtl))
;; (newline)

