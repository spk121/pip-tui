(use-modules (srfi srfi-64) (pip-tui coords))

(test-begin "coords")

(define C (coords-new 1 2 3 4))

(test-assert "coords?" (coords? C))
(test-equal "start-y" 1 (coords-get-start-y C))
(test-equal "width" 4 (coords-get-width C))
(test-equal "coords-values" '(1 2 3 4)
  (call-with-values (lambda () (coords-values C)) list))
(test-equal "receive-coords" 10
  (receive-coords (y x h w) (coords-values C) (+ y x h w)))
(test-equal "coords-adjust" '(2 3 1 2)
  (call-with-values
      (lambda () (coords-values (coords-adjust C 1 1 -2 -2)))
    list))

(test-assert "zero area: zero width" (coords-zero-area? (coords-new 0 0 5 0)))
(test-assert "zero area: zero height" (coords-zero-area? (coords-new 0 0 0 5)))
(test-assert "nonzero area" (not (coords-zero-area? C)))

(test-end "coords")
