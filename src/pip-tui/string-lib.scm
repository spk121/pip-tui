(define-module (pip-tui string-lib)
  #:use-module (pip-tui unistring)
  #:export (string-convert-crlf-to-lf
	    string-remove-trailing-whitespace
	    string-pad-beginning-to-width
	    string-pad-end-to-width
	    string-pad-beginning-and-end-to-width
	    string-untabify))

(define (string-convert-crlf-to-lf str)
  "Returns a copy of STR in which carrige return / linefeed pairs are
replace with a single linefeed."
  (let loop ((idx (1- (string-length str)))
	     (out-string ""))
    (cond
     ((and (>= idx 1)
	   (char=? (string-ref str idx) #\newline)
	   (char=? (string-ref str (1- idx)) #\return))
      (loop (- idx 2) (string-append "\n" out-string)))
     ((>= idx 0)
      (loop (1- idx) (string-append (string (string-ref str idx)) out-string)))
     (else
      out-string))))

(define (string-remove-trailing-whitespace str)
  "Returns a copy of STR, not including any whitespace at the end of
the string."
  (let ((idx (1- (string-length str))))
    (while (>= idx 0)
      (cond
       ((eqv? (char-general-category (string-ref str idx)) 'Zs)
	(set! idx (1- idx)))
       (else
	(break))))
    (substring/copy str 0 (1+ idx))))

(define (string-pad-end-to-width str width)
  "Returns a new copy of STR with spaces added to the end the string
to so that it occupies WIDTH cells. If STR is wider than WIDTH, the
STR is returned."
  (let ((len (string-width str)))
    (if (< len width)
	(string-append str (make-string (- width len) #\space))
	;; else
	str)))

(define (string-pad-beginning-to-width str width)
  "Returns a new copy of STR with spaces added to the beginning the
string to so that it occupies WIDTH cells. If STR is wider than WIDTH,
the STR is returned."
  (let ((len (string-width str)))
    (if (< len width)
	(string-append (make-string (- width len) #\space) str)
	;; else
	str)))

(define (string-pad-beginning-and-end-to-width str width)
  "Add spaces to the beginning and end of the string to so that it
occupies WIDTH cells. If STR is wider than WIDTH, the STR is
returned."
  (let* ((len (string-width str)))
    (if (< len width)
	(let* ((total-pad (- width len))
	       (beginning-pad (quotient total-pad 2))
	       (end-pad (- total-pad beginning-pad)))
	  (string-append
	   (make-string beginning-pad #\space)
	   str
	   (make-string end-pad #\space)))
	;; else
	str)))

(define (string-untabify str tabsize)
  "Returns a new string in which horizontal tab characters are replace
with TABSIZE spaces."
  (string-fold-right
   (lambda (char-cur out-string)
     (cond
      ((char=? char-cur #\tab)
       (string-append (make-string tabsize #\space) out-string))
      (else
       (string-append (string char-cur) out-string))))
   ""
   str))

(setlocale LC_ALL "")
(string-pad-beginning-to-width "hello" 40)
(string-untabify "hello\u4201\tworld" 8)
