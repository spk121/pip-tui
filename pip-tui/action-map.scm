(define-module (pip-tui action-map)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui time)
  #:use-module (pip-tui action)
  #:use-module (pip-tui event)
  #:export (;; action-entry?
	    ;; assert-action-entry
	    ;; action-entry-name=?
	    
	    action-map-new
	    action-map?
	    assert-action-map

	    action-map-lookup-entry
	    action-map-lookup-action-entries-by-name
	    action-map-remove-action-entries-by-name!
	    action-map-remove-action-entries-by-target!
	    action-map-add-action!
	    action-map-change-action-entry-state!
	    action-map-enable-action-entry!
	    action-map-disable-action-entry!
	    action-map-process-event
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION ENTRY

;; An action entry is an entry in the action map.  It is a
;; pair that associates an <action> with a TUI widget target.

;; (define-record-type <action-entry>
;;   (action-entry-new action
;; 		    target
;; 		    event-handler)

;;   action-entry?
;;   (action get-action set-action!)
;;   (target get-target set-target!)
;;   (event-handler get-event-handler set-event-handler!))

(define (action-entry? x)
  (and (pair? x)
       (action? (car x))))

(define-syntax assert-action-entry
  (syntax-rules ()
    ((_ val)
     (typecheck val 'action-entry action-entry?))))

(define (action-entry=? E1 E2)
  (assert-action-entry E1)
  (assert-action-entry E2)
  (and (equal? (car E1) (car E2))
       (equal? (cdr E1) (cdr E2))))

(define (action-entry-name=? entry name)
  (string=? name (action-get-name (car entry))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION MAP

;; An <action-map> is just a container for a list of action entries,
;; that is, a list of pairs of <action>s and targets.
;; The important method is process-event, which passes an event to
;; each of the actions to see if the action will process it.

;; The only reason to use this <action-map> container instead of using
;; a raw list is so that I can remove entries in the list without
;; extra set! bookkeeping.

;; Process event is at the heart of the main loop

(define-record-type <action-map>
  (action-map-new entries)
  action-map?
  (entries get-entries set-entries!))

(define-syntax assert-action-map
  (syntax-rules ()
    ((_ val)
     (typecheck val 'action-map action-map?))))

(define (action-map-lookup-entry amap name target)
  "Given an action map AMAP, the name of an action NAME, and a the
target of an action TARGET, this returns a pair (ACTION . TARGET), or
#f it the action/target pair was not found."
  (assert-action-map amap)
  (assert-string name)
  (find (lambda (entry)
	  (action-entry=? entry (cons name target)))
	(get-entries amap)))

(define (action-map-lookup-action-entries-by-name amap name)
  "This searches an action map AMAP for all actions with the name NAME (a string).
It returns and empty list of no actions with that name were found."
  (assert-action-map amap)
  (assert-string name)
  (filter (lambda (entry)
	    (action-entry-name=? entry name))
	  (get-entries amap)))

(define (action-map-remove-action-entry! amap name target)
  "This searches an action map AMAP for an action with the given
NAME and TARGET and removes it, if it exists."
  (assert-action-map amap)
  (assert-string name)
  (set-entries! amap
		(filter (lambda (E)
			  (or (not (string=? (action-get-name (car E)) name))
			      (not (equal? target (cdr E)))))
			(get-entries amap))))

(define (action-map-remove-action-entries-by-name! amap name)
  "This searches an action map AMAP for all actions with the given
NAME and removes them."
  (assert-action-map amap)
  (assert-string name)
  (set-entries! amap
		(filter (lambda (entry)
			  (not (action-entry-name=? entry name)))
			(get-entries amap))))

(define (action-entry-get-target AE)
  (assert-action-entry AE)
  (cdr AE))

(define (action-map-remove-action-entries-by-target! amap target)
  "This searches an action map AMAP for all actions with the given
TARGET and removes them."
  (assert-action-map amap)
  (set-entries! amap
		(filter (lambda (entry)
			  (not (equal? target (cdr entry))))
			(get-entries amap))))

(define (action-map-add-action! amap action target)
  "This adds a given <action> with its associated TARGET to an action
map AMAP."
  (assert-action-map amap)
  (assert-action action)
  ;; If this entry is duplicate, erase remove the original instance of
  ;; this action entry in the action map, so that this new instance
  ;; may be placed at the back of the list.
  (action-map-remove-action-entry! amap (action-get-name action) target)
  (set-entries! amap
		(append! (get-entries amap)
			 (list (cons action target)))))

(define (action-map-change-action-entry-state! amap name target state)
  "This searches an action map AMAP for an action with the given NAME
and TARGET.  If found, and if the action is active, it sets its STATE."
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (action-map-lookup-entry amap name target)))
    (when AE (action-change-state! (car AE) state))))

(define (action-map-enable-action-entry! amap name target)
  "This searches an action map AMAP for an action with the given NAME
and TARGET.  If found, and if the action set active."
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (action-map-lookup-entry amap name target)))
    (when AE (action-enable! (car AE)))))

(define (action-map-disable-action-entry! amap name target)
  "This searches an action map AMAP for an action with the given NAME
and TARGET.  If found, and if the action is set inactive."
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (action-map-lookup-entry amap name target)))
    (when AE (action-disable! (car AE)))))

(define (action-map-process-event amap event)
  "Send EVENT to the event handlers in the ACTION MAP.  Every <action>/TARGET
pair in the action map AMAP will receive the event."
  (assert-action-map amap)
  (assert-event event)
  (do ((i 0 (1+ i))) ((>= i (length (get-entries amap))))

    (let ([entry (list-ref (get-entries amap) i)])
      ;; (addstr (stdscr)
      ;;         (format #f "~s ~s  "
      ;;   	      (action-get-name (car entry)) (action-get-state (car entry))
      ;;   	      ;; (action-get-name (car entry)) (action-get-state (car entry))
      ;;   	      )
      ;;         #:y 1 #:x 0)
      ;; (refresh (stdscr))
      
      (action-activate (car entry)	; action
		       (cdr entry)	; target
		       event		; event
		       ))))


