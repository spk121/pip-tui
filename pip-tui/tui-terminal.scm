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
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui data-lib)
  #:use-module (pip-tui time)
  #:use-module (pip-tui render-lib)
  #:use-module (pip-tui tui-action)
  #:use-module (pip-tui hotspot)
  #:use-module (pip-tui event)
  #:use-module (pip-tui action)
  #:use-module (pip-tui tui-action)
  #:export (tui-terminal-new
            tui-terminal-tick
            TERMINAL_MICROSECONDS_PER_TICK
            ;; tui-terminal-process-event
            %set-hotspot-cur!
            tui-terminal-get-completion-cb
            tui-terminal-set-completion-cb!
            tui-terminal-kbd-action-activate
            tui-terminal-mouse-action-activate
            tui-terminal-tick-action-activate
            tui-terminal-hotspot-cur
            ))


(define-record-type <tui-terminal>
  (%tui-terminal-new panel
                     text
                     rendered-text
                     rendered-text-length
                     hotspots
                     state
                     draw-start-time
                     draw-last-update-time
                     hotspot-cur
                     hotspot-wide
                     completion-cb
                     )
  tui-terminal?
  (panel %panel %set-panel!)
  (text %text %set-text!)
  (rendered-text %rendered-text %set-rendered-text!)
  (rendered-text-length %rendered-text-length %set-rendered-text-length!)
  (hotspots %hotspots %set-hotspots!)
  (state %state %set-state!)
  (draw-start-time %draw-start-time %set-draw-start-time!)
  (draw-last-update-time %draw-last-update-time %set-draw-last-update-time!)
  (hotspot-cur %hotspot-cur %set-hotspot-cur!)
  (hotspot-wide %hotspot-wide %set-hotspot-wide!)
  (completion-cb %completion-cb %set-completion-cb!)
  )

;; (define TERMINAL_CHARACTERS_PER_SECOND 1)
;; (define TERMINAL_CHARACTERS_PER_SECOND 30) ; ~300 baud
(define TERMINAL_CHARACTERS_PER_SECOND 74) ; Fallout-4 like
;; (define TERMINAL_CHARACTERS_PER_SECOND 120) ; ~1200 baud
;; (define TERMINAL_CHARACTERS_PER_SECOND 960) ; ~9600 baud
;; (define TERMINAL_CHARACTERS_PER_SECOND 1440) ; ~14.4 kbps
;; (define TERMINAL_CHARACTERS_PER_SECOND 5600) ; ~56 kbps
(define TERMINAL_MICROSECONDS_PER_TICK (quotient 1000000 TERMINAL_CHARACTERS_PER_SECOND))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax assert-tui-terminal
  (syntax-rules ()
    ((_ val)
     (typecheck val 'tui-terminal tui-terminal?))))

(define (hotspot-count TT)
  (assert-tui-terminal TT)
  (length (%hotspots TT)))

(define (render-text! TT)
  "Update the RENDERED-TEXT parameter in the <tui-terminal> from its
text."
  (assert-tui-terminal TT)
  (let ([render (string-render (->string (%text TT))
                               4 ;tabsize
                               (coords-get-width (window-relative-coords (%panel TT)))
                               'left
                               #f)])
    (%set-rendered-text-length! TT (string-list-length render))
    (%set-rendered-text! TT render)
    (%set-hotspots! TT (hotspots-from-string-list (%rendered-text TT)))
    ))

(define* (tui-terminal-new y x width height text
                           #:key draw-start-time (state 'drawing)
                           (completion-cb #f))
  "Creates a new terminal using TEXT, which is either a Guile string
or a Guile procedure that takes no arguments that, when called,
produces a string.  The DRAW-START-TIME is when the text of the
terminal will outputting, one character at a time, to the screen.  The
STATE is either 'drawing or 'control."
  (let* ((pan (newwin width height y x #:panel #t))
         (TT (%tui-terminal-new pan     ;panel
                                text    ; text
                                #f      ; rendered text
                                #f      ; rendered text length
                                '()     ; hotspots
                                state
                                draw-start-time
                                #f      ; draw last update time
                                0       ; current hotspot index
                                #f      ; are hotspots "wide"? e.g. do they take a whole line
                                completion-cb ; a func to call when a selection is made
                                )))
    (unless draw-start-time
            (%set-draw-start-time! TT (now)))
    (%set-draw-last-update-time! TT (%draw-start-time TT))
    (render-text! TT)
    (render TT)
    TT))

(define (tui-terminal-get-completion-cb TT)
  (%completion-cb TT))

(define (tui-terminal-set-completion-cb! TT func)
  (%set-completion-cb! TT func))

(define (compute-coords-list TT)
  (window-relative-coords (%panel TT)))

(define (do-background TT coords-list)
  (render-background (%panel TT)
                     #:coords-list coords-list
                     #:bg-color COLOR_INDEX_BLACK
                     #:fg-color COLOR_INDEX_GREEN))

(define (do-border TT coords-list)
  coords-list)

(define (do-padding TT coords-list)
  coords-list)

(define (do-text TT coords-list)
  (let ([panel (%panel TT)]
        [text (%text TT)]
        [n (inexact->exact (round (* TERMINAL_CHARACTERS_PER_SECOND
                                     (- (now) (%draw-start-time TT)))))]
        [nprev (inexact->exact (round (* TERMINAL_CHARACTERS_PER_SECOND
                                         (- (%draw-last-update-time TT) (%draw-start-time TT)))))]
        )
    (when (and (eq? (%state TT) 'drawing) (> n (%rendered-text-length TT)))
          (%set-state! TT 'control)
          (enqueue-symbolic-action 'sound-terminal-drawing-end ' ()))
    
    (if (eq? (%state TT) 'drawing)
        (begin
          (render-text panel text #:coords-list coords-list
                       #:line-wrap #t
                       #:fg-color COLOR_INDEX_GREEN
                       #:bg-color COLOR_INDEX_BLACK
                       #:max-codepoints n)
          (when (> n nprev)
                (let* ((new-rendered-text (string-render (%text TT) 4 (coords-get-width coords-list) 'left #f))
                       (strlist (string-list-truncate! new-rendered-text n))
                       (new-text (apply string-append strlist)))
              
                  (move (stdscr) 0 0)
                  ;; (addstr (stdscr) (substring text nprev (- (string-length text) nprev)))
                  (addch (stdscr) (bold (string-ref new-text (1- (string-length new-text)))))
                  (refresh (stdscr))
                  (if (char-set-contains? char-set:graphic (string-ref new-text (1- (string-length new-text))))
                      (enqueue-symbolic-action 'sound-terminal-glyph-new '()))))
        )
        
        ;; else if state is control
        (begin
          
          (render-text panel text #:coords-list coords-list
                       #:fg-color COLOR_INDEX_GREEN
                       #:bg-color COLOR_INDEX_BLACK
                       #:line-wrap #t)
          (if (and (%hotspot-cur TT) (not (null-list? (%hotspots TT))))
              (let* ([highlight (list-ref (%hotspots TT) (%hotspot-cur TT))])
                (render-highlight panel
                                  (hotspot-get-start-y highlight)
                                  (hotspot-get-start-x highlight)
                                  (hotspot-get-end-y highlight)
                                  (hotspot-get-end-x highlight)
                                  #:coords-list coords-list
                                  #:hotspot-attr A_NORMAL
                                  #:hotspot-fg-color COLOR_INDEX_BLACK
                                  #:hotspot-bg-color COLOR_INDEX_GREEN
                                  #:full-width #t)))))
    (%set-draw-last-update-time! TT (now))))

(define (render TT)
  (do-text TT
           (do-padding TT
                       (do-border TT
                                  (do-background TT
                                                 (compute-coords-list TT))))))

(define (tui-terminal-tick TT)
  (render TT))


(define (check-for-hotspot TT y x)
  (list-index
   (lambda (hs)
     (in-hotspot? hs y x))
   (%hotspots TT)))

(define tui-terminal-kbd-action-activate
  (lambda (TT event state)
    (when (kbd-event? event)
          (let ((c (event-get-data event)))
            (cond
             ;; In 'drawing mode, KEY_ENTER or newline causes drawing to
             ;; complete.
             [(and (eq? 'drawing (%state TT))
                   (or (eqv? c KEY_ENTER)
                       (eqv? c #\newline)))
              (%set-state! TT 'control)
              (%set-hotspot-cur! TT 0)
              (render TT)

              ;; Return TRUE to indicate that we've process this event.
              #t]

             ;; In control mode, if we're acting like the blog-like entries in
             ;; Fallout 4...  A hotspot sits in its own line.  The whole line is
             ;; inverse if it is selected.

             ;; The 1st hotspot is initialially selected.

             ;; KEY_UP, KEY_DOWN move between entries (no wrap around).
             [(and (eq? 'control (%state TT))
                   (eqv? c KEY_UP))
              (%set-hotspot-cur! TT (max 0 (1- (%hotspot-cur TT))))
              (render TT)
              ;; Return TRUE to indicate that we've processed the event, but,
              ;; aren't complete.
              #t]

             [(and (eq? 'control (%state TT))
                   (eqv? c KEY_DOWN))
              (%set-hotspot-cur! TT (min (1- (hotspot-count TT)) (1+ (%hotspot-cur TT))))
              (render TT)
              ;; Return TRUE to indicate that we've processed the event, but,
              ;; aren't complete.
              #t]

             ;; KEY_ENTER selects the current entry.
             [(and (eq? 'control (%state TT))
                   (or (eqv? c KEY_ENTER)
                       (eqv? c #\newline)))
              (render TT)

              ;; Notify listeners that a selection has been made
              (when (procedure? (%completion-cb TT))
                    ((%completion-cb TT) TT))

              ;; Success, return the selection
              #t]
             
             [else #f])))))

(define tui-terminal-mouse-action-activate
  (lambda (TT event state)
    (when (mouse-event? event)
          (let ([m (event-get-data event)])
            (cond
             [(and (eq? 'drawing (%state TT))
                   (wenclose? (%panel TT) (third m) (second m))
                   (or (eq? BUTTON1_PRESSED (fifth m))
                       (eq? BUTTON1_CLICKED (fifth m)))
                   )
              (%set-state! TT 'control)
              (%set-hotspot-cur! TT 0)
              (render TT)
              ;; Return TRUE to indicate that we've used up this event.
              #t]

             [(and (eq? 'control (%state TT))
                   (or (eq? BUTTON1_PRESSED (fifth m))
                       (eq? BUTTON1_CLICKED (fifth m))))
              (let ([pos (mouse-trafo (%panel TT) (third m) (second m) #f)])
                (when pos
                      (let ([spot (check-for-hotspot TT (car pos) (cadr pos))])
                        (when spot
                              (%set-hotspot-cur! TT spot)
                              (render TT)

                              ;; Notify listeners that a selection has been made
                              (when (procedure? (%completion-cb TT))
                                    ((%completion-cb TT) TT))

                              ;; Return #t to indicate we've used up this event.
                              #t))))]

             [else #f])))))

(define tui-terminal-tick-action-activate
  (lambda (TT event state)
    (when (tick-event? event)
          (let ([m (event-get-data event)])
            (tui-terminal-tick TT)))))

(define (tui-terminal-hotspot-cur TT)
  (assert-tui-terminal TT)
  (%hotspot-cur TT))

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
;;      [(eqv? state 'drawing)
;;       (tui-terminal-reset-drawing-mode TT)]
;;      [(eqv? state 'select)
;;       (tui-terminal-reset-select-mode TT)]
;;      (tui-terminal-render-full TT))
;;       ;; else we're in the same state
;;       (begin
;;      [(eqv? state 'drawing)
;;       (tui-terminal-reset-drawing-mode-delta TT now)]
;;      [(eqv? state 'select)
;;       (tui-terminal-reset-select-mode-delta TT hotspot-cur)])))

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
;;      (set! lst (append lst (list entry))))))


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
