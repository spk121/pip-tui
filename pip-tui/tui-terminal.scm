;;; A tui-terminal is a view-controller for a Guile string.

;;; It displays the string as green text in a borderless window.

;;; It has two state: a drawing state and a control state.

;;; In the drawing state, the text is drawn one character at a time at
;;; a constant rate as if it were being displayed over a low bps line.
;;; During the drawing phase, a left mouse click with just cause all
;;; the text to be displayed.
;;; Any change in the string will restart the drawing state.

;;; In the control state, mousing over a link will highlight the
;;; link and make a tick sound.  Clicking on a link will
;;; cause a callback to fire and will make a click sound.

;;; Any text enclosed in square brackets in the string will become
;;; a link.

(define-module (pip-tui tui-terminal)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui data-lib)
  #:use-module (pip-tui time)
  #:use-module (pip-tui render-lib)
  #:export (extract-hotspots-from-string-list
	    tui-terminal-new
	    tui-terminal-tick))

(define-record-type <hotspot>
  (hotspot-new start-y			; line #
	       start-x			; cell #
	       start-i			; codepoint #
	       end-y
	       end-x
	       end-i
	       text
	       active
	       direction)
  hotspot?
  (start-y hotspot-get-start-y hotspot-set-start-y!)
  (start-x hotspot-get-start-x hotspot-set-start-x!)
  (start-i hotspot-get-start-i hotspot-set-start-i!)
  (end-y hotspot-get-end-y hotspot-set-end-y!)
  (end-x hotspot-get-end-x hotspot-set-end-x!)
  (end-i hotspot-get-end-i hotspot-set-end-i!)
  (text hotspot-get-text hotspot-set-text!)
  (active hotspot-active? hotspot-set-active!)
  (direction hotspot-get-direction hotspot-set-direction!))

(define (hotspot-get-begyx HS)
  (list (hotspot-get-start-y HS) (hotspot-get-start-x HS)))

(define (hotspot-get-endyx HS)
  (list (hotspot-get-end-y HS) (hotspot-get-end-x HS)))

(define (hotspot-new-from-brace-pairs str-list brace-pairs-list)
  "Given a list of strings and a 4-element list of the form
START-LINE START-INDEX END-LINE END-INDEX, this creates a new
<hotspot>."
  (let ([y1 (first brace-pairs-list)]
	[i1 (second brace-pairs-list)]
	[y2 (third brace-pairs-list)]
	[i2 (fourth brace-pairs-list)])
    (let ([x1 (substring-width (list-ref str-list y1) 0 i1)]
	  [x2 (substring-width (list-ref str-list y2) 0 (1+ i2))])
      (hotspot-new y1 x1 i1 y2 x2 i2 (append (substring-list str-list y1 i1 y2 i2)) #f #f))))


(define (extract-hotspots-from-string-list strlist)
  (map
   (lambda (brace-pairs-list)
     (hotspot-new-from-brace-pairs strlist brace-pairs-list))
   (string-list-find-brace-pairs strlist #\[ #\])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <tui-terminal>
  (%tui-terminal-new panel
		     text
		     rendered-text
		     hotspots
		     state
		     draw-start-time
		     draw-last-update-time
		     hotspot-cur
		     )
  tui-terminal?
  (panel %panel %set-panel!)
  (text %text %set-text!)
  (rendered-text %rendered-text %set-rendered-text!)
  (hotspots %hotspots %set-hotspots!)
  (state %tui-terminal-get-state %tui-terminal-set-state!)
  (draw-start-time %draw-start-time %set-draw-start-time!)
  (draw-last-update-time %draw-last-update-time %set-draw-last-update-time!)
  (hotspot-cur %hotspot-cur %set-hotspot-cur!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax assert-tui-terminal
  (syntax-rules ()
    ((_ val)
     (typecheck val 'tui-terminal tui-terminal?))))

(define (render-text! TT)
  "Update the RENDERED-TEXT parameter in the <tui-terminal> from its
text."
  (assert-tui-terminal TT)
  (let ([render (string-render (->string (%text TT))
			       4 ;tabsize
			       (coords-get-width (window-relative-coords (%panel TT)))
			       'left
			       #f)])
    ;; (when (not (string-list=? render (rendered-text TT)))
    (%set-rendered-text! TT render)
    (%set-hotspots! TT (extract-hotspots-from-string-list (%rendered-text TT)))
    ))

(define* (tui-terminal-new y x width height text #:key draw-start-time (state 'drawing))
  "Creates a new terminal using TEXT, which is either a Guile string
or a Guile procedure that takes no arguments that, when called,
produces a string.  The DRAW-START-TIME is when the text of the
terminal will outputting, one character at a time, to the screen.  The
STATE is either 'drawing or 'control."
  (let* ((pan (newwin width height y x #:panel #t))
	 (TT (%tui-terminal-new pan	;panel
				text	; text
				#f	; rendered text
				#f	; hotspots
				state
				draw-start-time
				#f	; draw last update time
				#f)))	; current hotspot index
    (unless draw-start-time
      (%set-draw-start-time! TT (now)))
    (render-text! TT)
    (render TT)
    TT))

(define (compute-coords-list TT)
  (window-relative-coords (%panel TT)))

(define (do-background TT coords-list)
    (render-background (%panel TT)
		       #:coords-list coords-list))

(define (do-border TT coords-list)
  coords-list)
  
(define (do-padding TT coords-list)
  coords-list)

(define (do-text TT coords-list)
  (let ([panel (%panel TT)]
	[text (%text TT)]
	[n (inexact->exact (round (* 10 (- (now) (%draw-start-time TT)))))])
    (render-text panel text #:coords-list coords-list
		 #:line-wrap #t
		 #:max-codepoints n)))

(define (render TT)
  (do-text TT
	   (do-padding TT
		       (do-border TT
				  (do-background TT
						 (compute-coords-list TT))))))

(define (tui-terminal-tick TT)
  (render TT))
  


;; (define (tui-terminal-reset-drawing-mode TT)
;;   "Set the internal state of a tui-terminal to the initial conditions
;; of drawing mode"
;;   (%tui-terminal-set-state! TT 'drawing)
;;   (%tui-terminal-set-rendered-text!
;;    TT
;;    (render-text (%tui-terminal-get-text TT)))
;;   (%set-draw-start-time! TT now)
;;   (%set-draw-last-update-time! TT now)
;;   (%set-hotspot-cur! TT #f))

;; (define (tui-terminal-reset-control-mode TT)
;;   "Set the internal state of a tui-terminal to the initial conditions
;; of control mode"
;;   (%tui-terminal-set-state! TT 'drawing)
;;   (let ((strlist (render-text (%tui-terminal-get-text TT))))
;;     (%tui-terminal-set-rendered-text! TT strlist)
;;     (%tui-terminal-set-hotspots!
;;      TT (extract-hotspots-from-string-list strlist))
;;     (%tui-terminal-set-draw-time-start! TT now)
;;     (%tui-terminal-set-draw-time-cur! TT now)
;;     (%tui-terminal-set-hotspot-cur! TT #f)))

;; (define (tui-terminal-render-delta TT state now hotspot-cur)
;;   "Given the current state, time, and hotspot, this procedure updates
;; the panel. If we're in drawing mode, this renders only new characters
;; that need to painted.  If we're in select mode, if the active hotspot
;; has changed, the any old hotspot returns to A_NORMAL, and if we're in
;; a new hotspot, it goes to A_INVERSE"
;;   (if (not (eqv? state (%tui-terminal-get-state)))
;;       ;; We've changed state, so re-render everything
;;       (begin
;; 	[(eqv? state 'drawing)
;; 	 (tui-terminal-reset-drawing-mode TT)]
;; 	[(eqv? state 'select)
;; 	 (tui-terminal-reset-select-mode TT)]
;; 	(tui-terminal-render-full TT))
;;       ;; else we're in the same state
;;       (begin
;; 	[(eqv? state 'drawing)
;; 	 (tui-terminal-reset-drawing-mode-delta TT now)]
;; 	[(eqv? state 'select)
;; 	 (tui-terminal-reset-select-mode-delta TT hotspot-cur)])))

;; (define (render-tui-terminal-drawing-delta TT new)
;;   "This renders and new characters that need to be painted
;; since the last update, based on time NOW."
;;   ;; Figure out the delta-t since the last update.
;;   ;; convert it into characters.
;;   ;; Draw those characters
;;   ;; Tick!
;;   #t)

;; (define (render-tui-terminal-update-hotspot TT hotspot-cur)
;;   "This renders any active hotspot as A_INVERSE and any
;; no-longer-active hotspot as A_NORMAL"
;;   (unless (eqv? hotspot-cur (%tui-terminal-get-hotspot-cur TT))
;;     ;; If there is an old hotspot, et the attributes for the old
;;     ;; hotspot to A_NORMAL
    
;;     ;; If there is a new hotspot, set the attributes for the new
;;     ;; hotspot to A_INVERSE, and tick.
;;     #t)
;;   ))

;; (define (tui-terminal-render-full TT now)
;;   "Do a full rendering of a tui terminal.  If in drawing mode,
;; draw all the characters that should have been draw by time NOW.
;; If in select mode, draw all text and higlight the current
;; hotspot, if any."
    
;;   #t)


;; (define (tui-terminal-controller TT c m)
;;   "Process keypress C.  If C is KEY_MOUSE, process
;; mouse event M."
  
;;   ;; If the current state is the drawing state...
  
;;   ;; A BUTTON1_PRESSED or a KEY_ENTER quits the drawing state and
;;   ;; makes a click sound and puts the terminal in control state.
  
;;   ;; If the current state is the control state...

;;   ;; If we get mouse position events, and we mouse over a hotspot,
;;   ;; highlight the hot spot, and make a tick sound.  If move the mouse
;;   ;; off of a hotspot, remove the highlighting.

;;   ;; Mouse position events are highly dependent on the specifics of
;;   ;; the terminal.

;;   ;; If there is a button1 click on a hotspot, do a selection
;;   ;; callback, and make a "click" sound.
  
;;   #t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    #!
;;    (define-syntax append-val!
;;      (syntax-rules()
;;        ((append-val! lst entry)
;; 	(set! lst (append lst (list entry))))))


;;   ;; If we're in drawing mode, figure out how many milliseconds
;;   ;; have gone by since drawing started.  Compute the number of
;;   ;; cells (or characters? or codepoints?) that should have appeared.
;;   ;; Render the string.  Write that number of codepoints.
;;   ;; If that number of codepoints has gone up, make a "tick".
  
;;   ;; If we're in select mode.
;;   ;; wrap the text
;;   ;; figure out if there is a highlighted hotspot
;;   ;; draw the text, with any highlighted hotspot in INVERSE.
;;   #f)

;; (define (tui-terminal-resize TT)
;;   ;; Remember that resizing may move the hotspots, so the
;;   ;; mouse may no longer be mousing over a hotspot.  On resizing,
;;   ;; turn off any highlighting on hotspots.
;;   (resize ...))
;; !#
