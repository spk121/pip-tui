(define-module (pip-tui tui-progress-bar)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui pip-colors)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui border)
  #:use-module (pip-tui render-lib)
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

(define (compute-coords-list TPB)
  "Given a TUI-PROGRESS_BAR, this returns a coords struct containing the
start-y, start-x, height, and width for the window."
  (window-relative-coords (%tui-progress-bar-get-panel TPB)))

(define (do-background TPB coords-list)
  "Given a TUI-PROGRESS-BARL and a coords-list, a 4-element list
containing start-y, start-x height, and width.  This paints the window
with spaces, and returns the same coords list."
  (let ((color (%tui-progress-bar-get-bg-color TPB))
	(panel (%tui-progress-bar-get-panel TPB)))
    (render-background panel
		       #:coords-list coords-list
		       #:bg-color color)))

(define (do-border TPB coords-list)
  (let ((panel (%tui-progress-bar-get-panel TPB))
	(fg-color (%tui-progress-bar-get-border-color TPB))
	(bg-color (%tui-progress-bar-get-bg-color TPB))
	(type (%tui-progress-bar-get-border-type TPB)))
    (render-border panel
		   #:coords-list coords-list
		   #:fg-color fg-color
		   #:bg-color bg-color
		   #:border-type type)))

(define (do-padding TPB coords-list)
  "Given a TUI-PROGRESS-BAR and COORDS-LIST, a 4-element list
containing start-y, start-x, height, and width for the window inside
the border, this computes the effect of the the padding, if there is
any. It returns a 4-element list (start-y, start-x, width, height) of
the window region inside the padding.

The padding should actually have been drawn by render-background."
  (let ((hpad (%tui-progress-bar-get-horizontal-padding TPB))
	(vpad (%tui-progress-bar-get-vertical-padding TPB)))
    (render-padding #:coords-list coords-list
		    #:hpad hpad
		    #:vpad vpad)))

(define (do-text TPB coords-list)
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
    (render-text panel text
		 #:coords-list coords-list
		 #:attr attr
		 #:fg-color fg-color
		 #:bg-color bg-color)
    (if (eqv? (%tui-progress-bar-get-text-location TPB) 'top)
	;; If the text is above the progress bar, we have one
	;; fewer available line.
	(coords-adjust coords-list 1 0 -1 0)
	;; If the text is to the left of the progress bar, we remove
	;; space from the left of the line.
	;; Remember to remove an extra cell for the gutter.
	;; FIXME?: should rely on the coords-list from render-text to do this
	;; width calculation, somehow, instead of doing it here.
	(let ((text-width (string-width (string-trim-right-to-width text (1- width)))))
	  (coords-adjust coords-list 0 (1+ text-width) 0 (- (1+ text-width)))))))

(define (do-bar TPB coords-list)
  "Given a TUI-PROGRESS-BAR and a coords list, this
renders the bar portion of the progress bar.  It returns
a coords-list of the remaining portion of the window."
  (let ((fraction (%tui-progress-bar-get-fraction TPB))
	(fg-color (%tui-progress-bar-get-bar-fg-color TPB))
	(bg-color (%tui-progress-bar-get-bar-bg-color TPB))
	(panel (%tui-progress-bar-get-panel TPB)))
    (render-bar panel fraction #:coords-list coords-list
		#:fg-color fg-color
		#:bg-color bg-color)))

(define (render TPB)
  (do-bar TPB
	  (do-text TPB
		   (do-padding TPB
			       (do-border TPB
					  (do-background TPB
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
  "Creates a new ncurses panel of the given START-X START-Y WIDTH HEIGHT.
Returns a new <tui-progress-bar> that contains the panel plus many
properties that describe a progress bar.

The TEXT-LOCATION is 'left or 'top, for text inline and to the left of the
bar, or above the bar.

The BORDER-TYPE is 'border-none, 'border-light, 'border-rounded,
'border-heavy, 'border-double, or 'border-block.  The default
'border-none draws no border.  If a border is drawn, the region in the
panel available for content is reduced by two rows and two columns.

The FRACTION is the fill percentage of the progress bar, from 0.0 to 1.0.

The HORIZONTAL-PADDING is the number of rows between the border and
the content, a nonnegative integer. Each row of padding reduces the
height availabe for content by two rows.

The VERTICAL-PADDING is the number of columns between the border and
the content, a nonnegative integer.  Each column of padding reduces
the width available for content by two columns.

The TEXT is a simple Guile string containing the text for the label.

The TEXT-ATTRIBUTES are ncurses attributes constants, like A_NORMAL or
A_BOLD.  One can logically OR multiple constants to combine
attributes.

TEXT-COLOR BORDER-COLOR BG-COLOR BAR-FG-COLOR and BAR-BG-COLOR are xterm
color indices from 0 to 255.
"
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
  "Sets the FRACTION property of a <tui-progress-bar>, which is the
fill fraction of the progress bar, from 0.0 to 1.0."
  (%tui-progress-bar-set-fraction! TPB fraction)
  (render TPB))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DERIVED FROM WINDOW

(define (tui-progress-bar-resize TPB height width)
  "Resizes the ncurses panel of a <tui-progress-bar> and then
re-renders its contents."
  (resize (%tui-progress-bar-get-panel TPB) height width))

(define (tui-progress-bar-getbegyx TPB)
  "Gets the START-Y and START-X of a <tui-progress-bar>. Note that
if there is a border or padding, the top-left corner of the area
available for content may differ."
  (getbegyx (%tui-progress-bar-get-panel TPB)))

(define (tui-progress-bar-getmaxyx TPB)
  "Gets the height and width of a <tui-progress-bar>.  Note that
if there is a border or padding, the height and width available for content
will be reduced."
  (let ((pan (%tui-progress-bar-get-panel TPB)))
    (getmaxyx pan)))
