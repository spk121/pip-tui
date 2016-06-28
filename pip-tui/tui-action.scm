(define-module (pip-tui tui-action)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui time)
  #:export ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIONS

;; <action> applies an <event> to a TUI widget target.
;; The main use for an <action> is to call its activate callback function.
;; The activate callback function takes an <event>, unpacks the event,
;; and then adjusts the TUI widget based on the contents of the <event>.

;; An <action> may be stateless, e.g. the modification of the TUI is based
;; only on the contents of the <event>.  An <action> may also carry a state
;; that is used when calling the activate callback function.

;; There may be a callback that is called when an <action>'s state is
;; updated.

(define-record-type <action>
  (action-new name
	      enabled
	      state
	      activate-cb
	      change-state-cb)
  action?
  (name action-get-name action-set-name!)
  (enabled action-get-enabled action-set-enabled!)
  (state action-get-state action-set-state!)
  (activate-cb action-get-activate-cb action-set-activate-cb!)
  (change-state-cb action-get-change-state-cb action-set-change-state-cb!))

;; name (string)

;; enabled (boolean) -- when #f, calls to action-activate and
;; action-change-state!  have no effect

;; state (freeform) -- a state of the action if the action has a state
;; machine, or #f otherwise

;; activate-cb (lambda (tui-target event #:optional state)) --
;; function that is called on receipt of an event.  It should return
;; #f if the event should be passed along to the next action, or #t if
;; the event need not be passed on to other actions.

;; change-state-cb (lambda (state)) -- function that is called
;; when state changes.

(define (action-activate action target event)
  "In response to EVENT, apply the action's activate callback function
to the TARGET."
  (assert-action action)
  (assert-event event)
  (when (action-get-enabled action)
    (let ((func (action-get-activate-cb action))
	  (state (action-get-state action)))
      (when func
	(if state
	    (func target event state)
	    ;; else
	    (func target event))))))

(define (action-change-state! action state)
  (assert-action action)
  (when (action-get-enabled action)
    (action-set-state! action state)
    (let ((func (action-get-change-state-cb action)))
      (when func
	(func state)))))

(define (action-enable! action)
  (assert-action action)
  (action-set-enabled! action #t))

(define (action-disable! action)
  (assert-action action)
  (action-set-enabled! action #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVENT

;; The event types are
;; 1. keyboard events
;; 2. mouse events
;; 3. signals
;; 4. resize
;; 5. symbolic -- a scheme symbol pushed onto the main loop
;; 6. tick -- a timestamp event fired automatically when idle
;; These are fired by the main loop

(define-record-type <event>
  (event-new type
	     data)
  event?
  (type event-get-type event-set-type!)
  (data event-get-data event-set-data!))

(define (kbd-event-new x)
  (assert-number x)
  (event-new 'kbd x))

(define (kbd-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'kbd))

(define (mouse-event-new x)
  (assert-mevent x)
  (event-new 'mouse x))

(define (mouse-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'mouse))

(define (signal-event-new x)
  (assert-signal-id x)
  (event-new 'signal x))

(define (signal-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'signal))

(define (resize-event-new x)
  (event-new 'resize x))

(define (resize-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'resize))

(define (symbolic-event-new x)
  (assert-symbol x)
  (event-new 'symbolic x))

(define (symbolic-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'symbolic))

(define (tick-event-new x)
  (event-new 'tick x))

(define (tick-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'tick))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVENT HANDLER

;; An event handler receives an EVENT, and it may send it
;; to an ACTION as an ACTIVATE.

;; If the handler decides that the event should not be passed on to
;; other ACTIONS, it returns #t.  If it decides the event may be of
;; interest to other ACTIONs it returns #f.

(define (default-kbd-event-handler evt)
  (assert-event evt)
  (if (kbd-event? evt)
      evt
      #f))

(define (default-mouse-event-handler evt)
  (assert-event evt)
  (if (mouse-event? evt)
      evt
      #f))

(define (default-signal-event-handler evt)
  (assert-event evt)
  (if (signal-event? evt)
      evt
      #f))

(define (default-resize-event-handler evt)
  (assert-event evt)
  (if (resize-event? evt)
      evt
      #f))

(define (default-symbolic-event-handler evt)
  (assert-event evt)
  (if (symbolic-event? evt)
      evt
      #f))

(define (default-tick-event-handler evt)
  (assert-event evt)
  (if (tick-event? evt)
      evt
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION ENTRY

;; an <action-entry> is an entry in the list of actions.  It is an
;; association between an action, a TUI widget target, and an event
;; handler.

(define-record-type <action-entry>
  (action-entry-new action
		    target
		    event-handler))
		  
  action-entry?
  (action get-action set-action!)
  (target get-target set-target!)
  (event-handler get-event-handler set-event-handler!))

(define (action-entry-name=? entry name)
  (string=? name (get-name (get-action entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION MAP

;; An <action-map> is just a list of <action-entry>.
;; The important method is process-event, which passes
;; an event to each of the actions to see if the action
;; will process it.

;; Process event is at the heart of the main loop

(define-record-type <action-map>
  (action-map-new entries)
  action-map?
  (entries get-entries set-entries!))

(define (lookup-action-entry-by-name amap name)
  (assert-action-map amap)
  (assert-string name)
  (find (lambda (entry)
	  (action-entry-name=? entry name))
	(get-entries amap)))

(define (remove-action-entry-by-name amap name)
  (assert-action-map amap)
  (assert-string name)
  (set-entries! amap
		(filter (lambda (entry)
			  (not (action-entry-name=? entry name)))
			(get-entries amap))))

(define (add-action-entry amap entry)
  (assert-action-map amap)
  (assert-action-entry entry)
  (remove-action amap (get-name (get-action entry)))
  (set-entries! amap
		(append (get-entries amap)
			(list entry))))

(define (activate-action-entry-by-name amap name event)
  (assert-action-map amap)
  (assert-string name)
  (assert-event event)
  (let ((AE (lookup-action-entry amap name)))
    (when AE (action-activate (get-action AE) (get-target AE) event))))

(define (change-action-entry-state-by-name amap name state)
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (lookup-action-entry amap name)))
    (when AE (action-change-state! (get-action AE) (get-target AE) state))))


(define (enable-action-entry-by-name amap name)
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (lookup-action-entry amap name)))
    (when AE (action-enable! (get-action AE) (get-target AE)))))

(define (disable-action-entry-by-name amap name)
  (assert-action-map amap)
  (assert-string name)
  (let ((AE (lookup-action-entry amap name)))
    (when AE (action-disable! (get-action AE) (get-target AE)))))

(define (process-event amap event)
  "Send EVENT to the event handlers in the ACTION MAP to
see if any of the actions wants to process the event."
  (assert-action-map amap)
  (assert-event event)
  (let ((entries (get-entries amap)))
    (unless (null-list? entries)

      (let loop ((entry (car entries))
		 (rest (cdr entries)))
	;; Send the event through the event handler to see if it is actionable.
	;; If the handler says that this is an actionable event, then
	;; send it to the action.
	
	(let ((actionable ((get-handler entry) event)))
	  (if actionable
	      (let ((result (activate (get-action event) (get-target-event) actionable)))
		(if (not result)
		    ;; When the result is false, this action has not consumed the event,
		    ;; so we continue passing it to the other possible actions.
		    (loop (car rest) (cdr rest))
		    ;; When the result is true, this action has consumed the event,
		    ;; and there is no need to continue passing it to the other actions.
		    result))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN EVENT LOOP

;; The main event loop gets events such as keypresses and mouse events
;; and passes them to the action map.  "Symbolic actions" can be
;; placed on the queue.  If no events are ready, "tick" or idle events
;; will be sent out at intervals governed by TICKS_PER_SECOND.  The
;; TUI will be repainted at intervals governed by FRAMES_PER_SECOND.

(define TICKS_PER_SECOND 60)
(define FRAMES_PER_SECOND 60)

(define *symbolic-actions* '())

(define (enqueue-symbolic-action sym)
  (assert-symbol sym)
  (set! *symbolic-actions*
    (append! *symbolic-actions* (list sym))))

(define (dequeue-symbolic-action)
  (if (null-list? *symbolic-actions*)
      #f
      (begin
	(let ([ret (car *symbolic-actions*)])
	  (set! *symbolic-actions* (cdr *symbolic-actions*))
	  ret))))

(define (main-loop amap)
  "Process events using the event handlers in the action map AMAP.
Loop continuously until the break signal is received or until some add
a 'main-loop-break signal to the queue."
  (let ([running #t]
	[verbose #f]
	[last-update-time (now)]
	[last-draw-time (now)]
	[tick-duration (/ 1.0 TICKS_PER_SECOND)])
    (while running
      ;; First, check the symbolic actions.  If there are no symbolic
      ;; actions in the queue, check for keypresses and mouse events.
      ;; Failing that, do an idle (tick) event.
      (let ([evt #f]
	    [sym (dequeue-symbolic-action)]
	    [time-cur (now)])
	(when (and sym (not evt))
	  (set! evt (symbolic-event-new sym)))
	(when (not evt)
	  (let ((c (getch (stdscr))))
	    (cond
	     [(= c KEY_RESIZE)
	      (set! evt (resize-event-new '()))]
	     [(= c KEY_MOUSE)
	      (set! evt (mouse-event-new (getmouse)))]
	     [(number? c)
	      (set! evt (kbd-event-new c))])))
	(unless evt
	  (set! evt (tick-event-new time-cur)))

	(when verbose
	  (move (stdscr) 0 0)
	  (addstr (stdscr) (format #f "~s:~s     " time-cur evt))
	  (refresh (stdscr))
	  )
	
	(cond
	 ;; Certain symbolic actions are process by the main loop
	 [(and (symbolic-event? evt) (eq? 'main-loop-break (event-data evt)))
	  (set! running #f)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-verbose (event-data evt)))
	  (set! verbose #t)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-quiet (event-data evt)))
	  (set! verbose #f)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-slow (event-data evt)))
	  (set! tick-duration 1.0)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-default (event-data evt)))
	  (set! tick-duration (/ 1.0 TICKS_PER_SECOND))]
	 [(and (symbolic-event? evt) (eq? 'main-loop-fast (event-data evt)))
	  (set! tick-duration 0.0)]

	 [else
	  (process-event amap evt)])
	
	(when (>= time-cur (+ last-draw-time (/ 1.0 FRAMES_PER_SECOND)))
	  (update-panels)
	  (doupdate)
	  (set! last-draw-time time-cur))

	(when (tick-event? evt)
	  (usleep (inexact->exact (round (* 1000000.0 tick-duration))))
	  (set! last-update-time time-cur))))))


	
