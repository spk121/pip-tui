(use-modules (srfi srfi-64) (pip-tui action) (pip-tui action-map) (pip-tui event))

(test-begin "action")

(define log '())
(define (record target event state)
  (set! log (cons (list target (car (event-get-data event)) state) log))
  #f)

(define A (action-new "rec" #t 'st record #f))

(test-assert "action?" (action? A))
(test-equal "name" "rec" (action-get-name A))

(action-activate A 'w1 (symbolic-event-new 'ping '()))
(test-equal "activate calls back" '((w1 ping st)) log)

(action-disable! A)
(action-activate A 'w1 (symbolic-event-new 'ping '()))
(test-equal "disabled action does nothing" 1 (length log))
(action-enable! A)

(define changed #f)
(define B (action-new "chg" #t 0 #f (lambda (s) (set! changed s))))
(action-change-state! B 5)
(test-equal "change-state" 5 (action-get-state B))
(test-equal "change-state callback" 5 changed)

;; Action maps
(set! log '())
(define amap (action-map-new '()))
(action-map-add-action! amap A 'w1)
(action-map-add-action! amap (action-new "rec" #t 'st2 record #f) 'w2)

(action-map-process-event amap (symbolic-event-new 'hello '()))
(test-equal "event reaches every entry in order"
  '((w1 hello st) (w2 hello st2)) (reverse log))

(test-equal "lookup by name" 2
  (length (action-map-lookup-action-entries-by-name amap "rec")))
(test-assert "lookup entry" (action-map-lookup-entry amap "rec" 'w2))

;; Adding a duplicate replaces the old entry.
(action-map-add-action! amap A 'w1)
(test-equal "duplicate replaced" 2
  (length (action-map-lookup-action-entries-by-name amap "rec")))

(action-map-remove-action-entries-by-target! amap 'w1)
(test-equal "remove by target" 1
  (length (action-map-lookup-action-entries-by-name amap "rec")))

(action-map-remove-action-entries-by-name! amap "rec")
(test-equal "remove by name" 0
  (length (action-map-lookup-action-entries-by-name amap "rec")))

;; Events
(test-assert "kbd-event?" (kbd-event? (kbd-event-new #\a)))
(test-assert "idle-event?" (idle-event? (idle-event-new 1.0 0.1)))
(test-equal "symbolic data" '(foo . 1) (event-get-data (symbolic-event-new 'foo 1)))

(test-end "action")
