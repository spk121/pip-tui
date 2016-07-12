(define-module (pip-tui event)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (pip-tui typecheck)
  #:export (event-get-data
	    assert-event
	    kbd-event-new
	    kbd-event?
	    mouse-event-new
	    mouse-event?
	    signal-event-new
	    signal-event?
	    resize-event-new
	    resize-event?
	    symbolic-event-new
	    symbolic-event?
	    idle-event-new
	    idle-event?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVENT
;; An event is a simple class that represents some action
;; that occurs in the main loop.

;; The event types are
;; 1. keyboard events
;; 2. mouse events
;; 3. POSIX signals (unimplemented)
;; 4. terminal resize events, SIGWINCH (unimplemented)
;; 5. symbolic -- a scheme symbol pushed onto the main loop
;; 6. idle -- a timestamp event fired automatically when idle
;; These are fired by the main loop

(define-record-type <event>
  (event-new type
	     data)
  event?
  (type event-get-type event-set-type!)
  (data event-get-data event-set-data!))

(define-syntax assert-event
  (syntax-rules ()
    ((_ val)
     (typecheck val 'event event?))))

(define-syntax assert-mevent
  (syntax-rules ()
    ((_ val)
     (typecheck val 'mevent mevent?))))

(define (kbd-event-new key-or-char)
  ;; FIXME assert integer or char
  (event-new 'kbd key-or-char))

(define (kbd-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'kbd))

(define (mouse-event-new mevent)
  (assert-mevent mevent)
  (event-new 'mouse mevent))

(define (mouse-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'mouse))

(define (signal-event-new posix-signal-id)
  (event-new 'signal posix-signal-id))

(define (signal-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'signal))

(define (resize-event-new x)
  (event-new 'resize x))

(define (resize-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'resize))

(define (symbolic-event-new sym data)
  (event-new 'symbolic (cons sym data)))

(define (symbolic-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'symbolic))

(define (idle-event-new time-cur delta-time)
  (event-new 'idle (cons time-cur delta-time)))

(define (idle-event? x)
  (assert-event x)
  (eq? (event-get-type x) 'idle))
	
