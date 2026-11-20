(use-modules (srfi srfi-64) (pip-tui hotspot))

(test-begin "hotspot")

(define spots (hotspots-from-string-list '("go [north] or" "[south" "east] now")))

(test-equal "two hotspots" 2 (length spots))

(let ((h (car spots)))
  (test-equal "single-line start" '(0 3)
    (list (hotspot-get-start-y h) (hotspot-get-start-x h)))
  (test-equal "single-line end" '(0 10)
    (list (hotspot-get-end-y h) (hotspot-get-end-x h)))
  (test-assert "inside" (in-hotspot? h 0 5))
  (test-assert "left of" (not (in-hotspot? h 0 2)))
  (test-assert "right of" (not (in-hotspot? h 0 11)))
  (test-assert "other line" (not (in-hotspot? h 1 5))))

(let ((h (cadr spots)))
  (test-assert "wrapped: first line" (in-hotspot? h 1 3))
  (test-assert "wrapped: second line" (in-hotspot? h 2 2))
  (test-assert "wrapped: past end" (not (in-hotspot? h 2 7))))

(test-end "hotspot")
