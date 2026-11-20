(use-modules (srfi srfi-64) (pip-tui data-lib))

(test-begin "data-lib")

(test-equal "->pair pair" '(1 . 2) (->pair '(1 . 2)))
(test-equal "->pair list" '(1 . 2) (->pair '(1 2 3)))
(test-equal "->pair vector" '(a . b) (->pair #(a b)))
(test-equal "->pair u8vector" '(7 . 8) (->pair #u8(7 8 9)))
(test-equal "->pair thunk" '(1 . 2) (->pair (lambda () '(1 2))))
(test-error "->pair bad" #t (->pair 'foo))

(test-equal "->string string" "abc" (->string "abc"))
(test-equal "->string number" "42" (->string 42))
(test-equal "->string boolean" "TRUE" (->string #t))
(test-equal "->string thunk" "7" (->string (lambda () 7)))
(test-equal "->string symbol" "foo" (->string 'foo))

(test-equal "->number number" 3 (->number 3))
(test-equal "->number string" 3.5 (->number "3.5"))
(test-equal "->number boolean" 0 (->number #f))
(test-error "->number bad" #t (->number 'foo))

(test-equal "->string/number" '("HP" . 34) (->string/number '("HP" "34")))

(test-end "data-lib")
