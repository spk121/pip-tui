(define-module (pip-tui unistring)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (string-width
	    string-split-at-line-breaks)
  )

;; Typecheck macros
(define-syntax typecheck
  (syntax-rules ()
    ((_ val type-name type-test)
     (if (not (type-test val))
	 (error (format #f "expected type ~a" type-name) val)))
    ((_ val type-name type-test1 type-test2)
     (if (not (and (type-test1 val) (type-test2 val)))
	 (error (format #f "expected type ~a" type-name) val)))))

(define-syntax assert-bytevector
  (syntax-rules ()
    ((_ val)
     (typecheck val 'bytevector bytevector?))))
(define-syntax assert-exact-integer
  (syntax-rules ()
    ((_ val)
     (typecheck val 'exact-integer (lambda (x) (and (integer? x) (exact? x)))))))
(define-syntax assert-string
  (syntax-rules ()
    ((_ val)
     (typecheck val 'string string?))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define %unistring-library #f)
(define %u32-strwidth-c-function #f)
(define %u32-width-linebreaks #f)

(define UC_BREAK_UNDEFINED 0)
(define UC_BREAK_PROHIBITED 1)
(define UC_BREAK_POSSIBLE 2)
(define UC_BREAK_MANDATORY 3)
(define UC_BREAK_HYPHENATION 4)

;; int u32_strwidth (const uint32_t *s, const char *encoding)
(define (u32-strwidth utf32-bytevector encoding)
  "Given a bytevector that contains a NULL-terminated UTF-32 string
and a string that is a locale encoding, this returns the number of
columns that the bytevector's UTF-32 string would occupy."
  (assert-bytevector utf32-bytevector)
  (assert-string encoding)
  (let* ((p-utf32-bytevector (bytevector->pointer utf32-bytevector))
	 (p-encoding (string->pointer encoding "ASCII")))
    (%u32-strwidth-c-function p-utf32-bytevector p-encoding)))

;; int u32_width_linebreaks (const uint32_t *s, size_t n, int width, int start_column, int at_end_columns, const char *override, const char *encoding, char *p)
(define (u32-width-linebreaks s-utf32-bv n width start-column at-end-columns encoding p-bv)
  (assert-bytevector s-utf32-bv)
  (assert-exact-integer n)
  (assert-exact-integer width)
  (assert-exact-integer start-column)
  (assert-exact-integer at-end-columns)
  ;; (assert-bytevector override-bv)
  (assert-string encoding)
  (let ((p-utf32 (bytevector->pointer s-utf32-bv))
	(p-encoding (string->pointer encoding "ASCII"))
	(p-result (bytevector->pointer p-bv)))
    (%u32-width-linebreaks p-utf32
			   n
			   width
			   start-column
			   at-end-columns
			   ;; (bytevector->pointer override-bv)
			   %null-pointer
			   p-encoding
			   p-result)))

(define (string->null-terminated-utf32-bytevector str)
  (let* ((len (string-length str))
	 (bv (make-bytevector (* 4 (1+ len)) 0)))
    (do ((i 0 (1+ i)))
	((>= i len))
      (bytevector-u32-native-set! bv (* 4 i) (char->integer (string-ref str i))))
    (bytevector-u32-native-set! bv (* 4 len) 0)
    ;; (format #t "string ~s~%" str)
    ;;  (format #t "    bv ~s~%" bv)
    bv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string-width str)
  "Return the number of screen columns this string would occupy
on a character cell terminal."
  (assert-string str)
  ;; Create a null-terminated bytevector containing STR
  (u32-strwidth (string->null-terminated-utf32-bytevector str) "UTF-32"))

(define (string-width-linebreaks str width)
  "Returns a list that indicates if a character location should be a
line break if the width of a line is not to exceed WIDTH cells."
  (assert-string str)
  (assert-exact-integer width)
  (let* ((n (string-length str))
	 (s-utf32-bv (string->utf32 str (native-endianness)))
	 (output-bv (make-bytevector n UC_BREAK_UNDEFINED)))
    (u32-width-linebreaks s-utf32-bv n width 0 0 "ISO-8859-1" output-bv)
    (bytevector->u8-list output-bv)))
  
(define (string-split-at-line-breaks str width)
  (assert-string str)
  (assert-exact-integer width)
  (let* ((p (string-width-linebreaks str width))
	 (idx 0)
	 (last-idx 0)
	 (string-list '()))
    (for-each
     (lambda (brk)
       (cond
	((= brk UC_BREAK_MANDATORY)
	 (let ((line (substring/copy str last-idx idx)))
	   (set! string-list (append string-list (list line)))
	   (set! last-idx (1+ idx))
	   (set! idx (1+ idx))
	   line))
	((= brk UC_BREAK_POSSIBLE)
	 (let ((line (substring/copy str last-idx idx)))
	   (set! string-list (append string-list (list line)))
	   (set! last-idx idx)
	   (set! idx (1+ idx))
	   line))
	(else
	 (set! idx (1+ idx))
	 #f)))
     p)
    (if (< last-idx (string-length str))
	(set! string-list
	  (append string-list
		  (list (substring/copy str last-idx (string-length str))))))
    string-list))

(define (initialize)
  (set! %unistring-library (dynamic-link "libunistring"))
  (set! %u32-strwidth-c-function
    (pointer->procedure int
			(dynamic-func "u32_strwidth" %unistring-library)
			(list '* '*)))
  (set! %u32-width-linebreaks
    (pointer->procedure int
			(dynamic-func "u32_width_linebreaks" %unistring-library)
			(list '* size_t int int int '* '* '*))))

(initialize)


(string-width "hello")
