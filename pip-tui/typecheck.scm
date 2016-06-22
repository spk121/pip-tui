(define-module (pip-tui typecheck)
  #:use-module (rnrs bytevectors)
  #:export (typecheck
	    assert-bytevector
	    assert-exact-integer
	    assert-string))

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
