(define-module (pip-tui data-lib)
  #:export (->pair
	    ->string
	    ->number
	    ->string/number))

(define (->pair x)
  "Extracts the first two elements of vector-like or list-like object X as a pair.
If X is a thunk, evaluate it and try to extract a pair from the result."
  (cond
   [(and (pair? x) (not (list? x)))
    x]
   [(list? x)
    (cons (list-ref x 0) (list-ref x 1))]
   [(vector? x)
    (cons (vector-ref x 0) (vector-ref x 1))]
   [(uniform-vector? x)
    (cons (uniform-vector-ref x 0) (uniform-vector-ref x 1))]
   [(bitvector? x)
    (cons (bitvector-ref x 0) (bitvector-ref x 1))]
   [(array? x)
    (cons (array-ref x 0) (array-ref x 1))]
   [(procedure? x)
    (->pair (x))]
   [else
    (error (format #f "~s is not a list-like type with at least two elements" x))]))

(define (->string x)
  "Creates a string representation of X, which is a string, number,
boolean or a thunk that returns one of thost types."
  (cond
   [(string? x)
    x]
   [(number? x)
    (number->string x)]
   [(procedure? x)
    (->string (x))]
   [(boolean? x)
    (if x "TRUE" "FALSE")]
   [else
    (format #f "~a" x)]))

(define (->number x)
  "Creates a numeric representation of X, which is a number, string,
boolean or is a thunk that returns one of those types."
  (cond
   [(number? x)
    x]
   [(string? x)
    (string->number x)]
   [(procedure? x)
    (->number (x))]
   [(boolean? x)
    (if x 1 0)]
   [else
    (error (format #f "~s cannot be converted into a number" x))]))

(define (->string/number x)
  "Creates a pair containing a string and a number from the first two
elements of X, which is either a list-like variable or a thunk that
returns a list-like result."
  (cond
   [(procedure? x)
    (->string/number (x))]
   [else
    ((lambda (y)
       (cons
	(->string (car y))
	(->number (cdr y))))
     (->pair x))]))
     
