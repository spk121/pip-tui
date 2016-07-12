(define-module (pip-tui tui-action)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui time)
  ;;#:use-module (pip-tui action)
  #:use-module (pip-tui action-map)
  #:use-module (pip-tui event)
  #:export (
	    main-loop
	    enqueue-symbolic-action
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN EVENT LOOP

;; The main event loop gets events such as keypresses and mouse events
;; and passes them to the action map.  "Symbolic actions" can be
;; placed on the queue.  If no events are ready, "idle" or idle events
;; will be sent out at intervals governed by IDLES_PER_SECOND.  The
;; TUI will be repainted at intervals governed by FRAMES_PER_SECOND.

(define IDLES_PER_SECOND 1000)
(define FRAMES_PER_SECOND 60)

(define *symbolic-actions* '())

(define (enqueue-symbolic-action sym data)
  (assert-symbol sym)
  (set! *symbolic-actions*
    (append! *symbolic-actions* (list (cons sym data)))))

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
	[idle-duration (/ 1.0 IDLES_PER_SECOND)])
    (while running
      ;; First, check the symbolic actions.  If there are no symbolic
      ;; actions in the queue, check for keypresses and mouse events.
      ;; Failing that, do an idle (idle) event.
      (let ([evt #f]
	    [sym (dequeue-symbolic-action)]
	    [time-cur (now)])
	(when (and sym (not evt))
	  (set! evt (symbolic-event-new (car sym) (cdr sym))))
	(when (not evt)
	  (let ([c (getch (stdscr))])
	    (cond
	     [(equal? c KEY_RESIZE)
	      (set! evt (resize-event-new '()))]
	     [(equal? c KEY_MOUSE)
	      (set! evt (mouse-event-new (getmouse)))]
	     [(or (char? c) (number? c))
	      (set! evt (kbd-event-new c))])))
	(unless evt
	  (set! evt (idle-event-new time-cur (- time-cur last-update-time))))
	;  (set! evt (kbd-event-new KEY_ENTER)))

	(when verbose
	  (move (stdscr) 0 0)
	  (addstr (stdscr) (format #f "~s: ~s     " time-cur evt))
	  (refresh (stdscr))
	  )
	
	(cond
	 ;; Certain symbolic actions are process by the main loop
	 [(and (symbolic-event? evt) (eq? 'main-loop-break (car (event-get-data evt))))
	  (set! running #f)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-verbose (car (event-get-data evt))))
	  (set! verbose #t)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-quiet (car (event-get-data evt))))
	  (set! verbose #f)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-slow (car (event-get-data evt))))
	  (set! idle-duration 1.0)]
	 [(and (symbolic-event? evt) (eq? 'main-loop-default (car (event-get-data evt))))
	  (set! idle-duration (/ 1.0 IDLES_PER_SECOND))]
	 [(and (symbolic-event? evt) (eq? 'main-loop-fast (car (event-get-data evt))))
	  (set! idle-duration 0.0)]
	 
	 [(and (symbolic-event? evt) (eq? 'main-loop-detach (car (event-get-data evt))))
	  (action-map-remove-action-entries-by-target! amap (cdr (event-get-data evt)))]

	 [else
	  (action-map-process-event amap evt)])
	
	(when (>= time-cur (+ last-draw-time (/ 1.0 FRAMES_PER_SECOND)))
	  (update-panels)
	  (doupdate)
	  (set! last-draw-time time-cur))

	(when #t ;; (idle-event? evt)
	  (usleep (inexact->exact (round (* 1000000.0 idle-duration))))
	  (set! last-update-time time-cur))))))


	
