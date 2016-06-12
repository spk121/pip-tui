(define-module (pip-tui tui-progress-bar)
  #:use-module (ncurses curses)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui pip-colors)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui border)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
	    ;; From tui-progress-bar
	    tui-progress-bar-new
	    tui-progress-bar-set-border-type!
	    tui-progress-bar-get-border-type
	    tui-progress-bar-set-text-location!
	    tui-progress-bar-get-text-location
	    tui-progress-bar-set-fraction!
	    tui-progress-bar-get-fraction
	    tui-progress-bar-set-horizontal-padding!
	    tui-progress-bar-get-horizontal-padding
	    tui-progress-bar-set-vertical-padding!
	    tui-progress-bar-get-vertical-padding
	    tui-progress-bar-set-text!
	    tui-progress-bar-get-text
	    tui-progress-bar-set-text-attr!
	    tui-progress-bar-get-text-attr
	    tui-progress-bar-set-text-color!
	    tui-progress-bar-get-text-color
	    tui-progress-bar-set-border-color!
	    tui-progress-bar-get-border-color
	    tui-progress-bar-set-bg-color!
	    tui-progress-bar-get-bg-color
	    tui-progress-bar-set-bar-fg-color!
	    tui-progress-bar-get-bar-fg-color
	    tui-progress-bar-get-bar-bg-color!
	    tui-progress-bar-set-bar-bg-color
	    ;; Derived from panel
	    tui-progress-bar-show
	    tui-progress-bar-hide
	    tui-progress-bar-hidden?
	    tui-progress-bar-move
	    tui-progress-bar-top
	    tui-progress-bar-bottom
	    ;; Derived from window
	    tui-progress-bar-resize
	    tui-progress-bar-getbegyx
	    tui-progress-bar-getmaxyx)
  )

;; tui-progress-bar

;; A tui-progress-bar is a subclass of a panel.  It shows a bit of
;; text and a progress bar.  It is not an interactive widget.

;; There are two formats

;; 1. The text can go on top, and be left aligned with the beginning
;; of the progress bar.
;;
;; see loading screens.

;; 2. The text can go to the left of the progress bar and inline with
;; it. The text is bright.  The bar is bright.  The background is
;; dim. There is no vertical padding.  There is 1 space of horizontal
;; padding and between the text and the bar.
;;
;; see, VATS stat page, in the LEVEL box.  

(define-record-type <tui-progress-bar>
  (%tui-progress-bar-new panel
			 border-type
			 text-location
			 fraction
			 horizontal-padding
			 vertical-padding
			 text
			 text-attributes
			 text-color
			 border-color
			 bg-color
			 bar-fg-color
			 bar-bg-color)
  tui-progress-bar?
  (panel %tui-progress-bar-get-panel %tui-progress-bar-set-panel!)
  (border-type %tui-progress-bar-get-border-type %tui-progress-bar-set-border-type!)
  (text-location %tui-progress-bar-get-text-location %tui-progress-bar-set-text-location!)
  (fraction %tui-progress-bar-get-fraction %tui-progress-bar-set-fraction!)
  (horizontal-padding %tui-progress-bar-get-horizontal-padding %tui-progress-bar-set-horizontal-padding!)
  (vertical-padding %tui-progress-bar-get-vertical-padding %tui-progress-bar-set-vertical-padding!)
  (text %tui-progress-bar-get-text %tui-progress-bar-set-text!)
  (text-attributes %tui-progress-bar-get-text-attributes %tui-progress-bar-set-text-attributes!)
  (text-color %tui-progress-bar-get-text-color %tui-progress-bar-set-text-color!)
  (border-color %tui-progress-bar-get-border-color %tui-progress-bar-set-border-color!)
  (bg-color %tui-progress-bar-get-bg-color %tui-progress-bar-set-bg-color!)
  (bar-fg-color %tui-progress-bar-get-bar-fg-color %tui-progress-bar-set-bar-fg-color!)
  (bar-bg-color %tui-progress-bar-get-bar-bg-color %tui-progress-bar-set-bar-bg-color!)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Properties

;;; panel              #<window> type with panel properties
;;; border-type      'border-none 'border-light 'border-heavy 'border-rounded 'border-double 'border-block
;;; text-location    'top or 'left
;;  fraction         0.0 to 1.0
;;; horizontal-padding          >= 0, number of spaces added to left and right of bar
;;; vertical-padding          >= 0, number of spaces added above and below
;;; text             should be a single line
;;; text-attributes  an ncurses attribute like A_BOLD
;;; text-color       an xterm color index
;;; bg-color         an xterm color index
;;; bar-fg-color     an xterm color index
;;; bar-bg-color     an xterm color index

;; The 8 horizontal blocks
;; Left 1/8   ▏ U+258f
;; Left 1/4   ▎ U+258e
;; Left 3/8   ▍ U+258d
;; Left 1/2   ▌ U+258c
;; Left 5/8   ▋ U+258b
;; Left 3/4   ▊ U+258a
;; Left 7/8   ▉ U+2589
;; Full Block █ U+2588
(define *fraction-blocks* " ▏▎▍▌▋▊▉█")

(define (compute-coords-list TPB)
  "Given a TUI-PROGRESS_BAR, this returns a coords struct containing the
start-y, start-x, height, and width for the window."
  (window-relative-coords (%tui-progress-bar-get-panel TPB)))

(define (render-background TPB coords-list)
  "Given a TUI-PROGRESS-BARL and a coords-list, a 4-element list
containing start-y, start-x height, and width.  This paints the window
with spaces, and returns the same coords list."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(color (%tui-progress-bar-get-bg-color TPB))
	(panel (%tui-progress-bar-get-panel TPB)))

    (cond
     ;; If this window has no size, do nothing
     ((coords-zero-area? coords-list)
      coords-list)
     
     ;; If this window has any size, draw it with the background
     (else

      (attr-set! panel A_NORMAL (color-indices-get-color-pair-index COLOR_INDEX_BLACK color))
      (let ((space (normal #\space)))
	(do ((j 0 (1+ j)))
	    ((>= j height))
	  (do ((i 0 (1+ i)))
	      ((>= i width))
	    (addch panel space #:y (+ start-y j) #:x (+ start-x i)))))
      coords-list))))

(define (render-border TPB coords-list)
  (let ((panel (%tui-progress-bar-get-panel TPB))
	(fg-color (%tui-progress-bar-get-border-color TPB))
	(bg-color (%tui-progress-bar-get-bg-color TPB))
	(type (%tui-progress-bar-get-border-type TPB)))
    (border-draw-on-panel panel coords-list fg-color bg-color type)))

(define (render-padding TPB coords-list)
  "Given a TUI-PROGRESS-BAR and COORDS-LIST, a 4-element list
containing start-y, start-x, height, and width for the window inside
the border, this computes the effect of the the padding, if there is
any. It returns a 4-element list (start-y, start-x, width, height) of
the window region inside the padding.

The padding should actually have been drawn by render-background."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(hpad (%tui-progress-bar-get-horizontal-padding TPB))
	(vpad (%tui-progress-bar-get-vertical-padding TPB)))

    (cond

     ;; If there is no padding, do nothing
     ((and (zero? hpad) (zero? vpad))
      coords-list)

     ;; If this window has no size, do nothing
     ((coords-zero-area? coords-list)
      coords-list)

     ;; If all that remains of the window is padding
     ((or (<= width (* 2 hpad))
	  (<= height (* 2 vpad)))
      (coords-new start-y start-x 0 0))

     ;; If there is some space inside the padding
     (else
      (coords-adjust coords-list vpad hpad (* -2 vpad) (* -2 hpad))))))

(define (render-text TPB coords-list)
  "Given a TUI-PROGRESS-BAR and a coords list, this
renders the text portion of the progress bar.  It returns
a coords-list of the remaining portion of the window."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(text (%tui-progress-bar-get-text TPB))
	(attr (%tui-progress-bar-get-text-attributes TPB))
	(fg-color (%tui-progress-bar-get-text-color TPB))
	(bg-color (%tui-progress-bar-get-bg-color TPB))
	(panel (%tui-progress-bar-get-panel TPB)))

    
    (cond
     ((coords-zero-area? coords-list)
      ;; There is no room to draw anything
      coords-list)

     (else
      (let* ((text2 (substring-width text (1- width))) ; Note we subtract and extra 1 cell for the gutter between the text and the bar
	     (text2-width (string-width text2)))  ; the length of that
					; text, which might
					; be one cell less
					; that WIDTH for CJK
	(attr-set! panel attr 
		   (color-indices-get-color-pair-index fg-color bg-color))
	(addstr panel text2 #:y start-y #:x start-x)
	(if (eqv? (%tui-progress-bar-get-text-location TPB) 'top)
	    ;; If the text is above the progress bar, we have one
	    ;; fewer available line.
	    (coords-adjust coords-list 1 0 -1 0)
	    ;; If the text is to the left of the progress bar, we remove
	    ;; space from the left of the line.
	    ;; Remember to remove an extra cell for the gutter.
	    (coords-adjust coords-list 0 (1+ text2-width) 0 (- (1+ text2-width)))))))))

(define (render-bar TPB coords-list)
  "Given a TUI-PROGRESS-BAR and a coords list, this
renders the bar portion of the progress bar.  It returns
a coords-list of the remaining portion of the window."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(fraction (%tui-progress-bar-get-fraction TPB))
	(fg-color (%tui-progress-bar-get-bar-fg-color TPB))
	(bg-color (%tui-progress-bar-get-bar-bg-color TPB))
	(panel (%tui-progress-bar-get-panel TPB)))
    (cond
     ((or (zero? height) (zero? width))
      ;; There is no room to draw anything
      coords-list)
     (else
      ;; Convert the fraction into progress bar 1/8ths
      (let* ((eighths (inexact->exact (round (* 8 fraction width))))
	     (full-blocks (quotient eighths 8))
	     (last-block (remainder eighths 8))
	     (text (string-append (make-string full-blocks (string-ref *fraction-blocks* 8)))))
	;; Add the fraction block
	(unless (zero? last-block)
		(set! text (string-append text (string (string-ref *fraction-blocks* last-block)))))
	;; And the right hand side of the bar
	(set! text (string-append text (make-string (- width (string-length text)) #\space)))
	(attr-set! panel A_NORMAL
		   (color-indices-get-color-pair-index fg-color bg-color))
	(addstr panel text #:y start-y #:x start-x)
	(coords-adjust coords-list 1 0 -1 0))))))

(define (render TPB)
  (render-bar TPB
	      (render-text TPB
			   (render-padding TPB
					   (render-border TPB
							  (render-background TPB
									     (compute-coords-list TPB)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (tui-progress-bar-new start-y start-x width height
			       #:key
			       (text-location 'left)
			       (border-type 'border-none)
			       (fraction 0.0)
			       (horizontal-padding 0)
			       (vertical-padding 0)
			       (text "")
			       (text-attributes A_NORMAL)
			       (text-color COLOR_INDEX_WHITE)
			       (border-color COLOR_INDEX_BLACK)
			       (bg-color COLOR_INDEX_BLACK)
			       (bar-fg-color COLOR_INDEX_WHITE)
			       (bar-bg-color COLOR_INDEX_GREY37)
			       )
  (let* ((pan (newwin width height start-y start-x #:panel #t))
	 (TPB  (%tui-progress-bar-new pan
				      border-type
				      text-location
				      fraction
				      horizontal-padding
				      vertical-padding
				      text
				      text-attributes
				      text-color
				      border-color
				      bg-color
				      bar-fg-color
				      bar-bg-color)))
    (render TPB)
    TPB))

(define (tui-progress-bar-set-fraction! TPB fraction)
  (%tui-progress-bar-set-fraction! TPB fraction)
  (render TPB))

(define (tui-progress-bar-getmaxyx TPB)
  (let ((pan (%tui-progress-bar-get-panel TPB)))
    (getmaxyx pan)))
