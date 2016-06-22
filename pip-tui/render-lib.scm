;; These are ncurses drawing operations that are shared among the
;; various pip-tui widgets.

(define-module (pip-tui render-lib)
  #:use-module (ice-9 optargs)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui border)
  #:use-module (pip-tui pip-colors)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui coords)
  #:export (render-background
	    render-border
	    render-padding
	    render-text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the default rendering parameters

;; Guess as to the most character that would ever appear on an screen

;; Most possible single-cell characters in a line. 4096 / 6
(define RENDER_MAX_CELLS_IN_LINE 682)

;(define RENDER_MAX_CODEPOINTS_IN_LINE 600)
;(define RENDER_MAX_LINES 200)
;(define RENDER_MAX_CELLS_IN_TEXT (* RENDER_MAX_CELLS_IN_LINE RENDER_MAX_LINES))

(define RENDER_DEFAULT_ATTRIBUTE A_NORMAL)
(define RENDER_DEFAULT_FG_COLOR COLOR_INDEX_BLACK)
(define RENDER_DEFAULT_BG_COLOR COLOR_INDEX_WHITE)
(define RENDER_DEFAULT_BORDER_TYPE 'border-light)
(define RENDER_DEFAULT_VERT_ALIGN 'top)
(define RENDER_DEFAULT_HORIZ_ALIGN 'left)
(define RENDER_DEFAULT_HORIZ_PAD 0)
(define RENDER_DEFAULT_VERT_PAD 0)
(define RENDER_DEFAULT_HORIZ_ELLIPSIZE #t)
(define RENDER_DEFAULT_VERT_ELLIPSIZE #t)
(define RENDER_DEFAULT_LINE_WRAP #t)
(define RENDER_DEFAULT_TAB_SIZE 4)	; 4 cells
; Most possible characters on a screen. (4096 x 2160) / (6x9)
(define RENDER_DEFAULT_CODEPOINT_COUNT_LIMIT 163840) 


(define* (render-background panel #:key (coords-list #f)
			    (attr RENDER_DEFAULT_ATTRIBUTE)
			    (fg-color RENDER_DEFAULT_FG_COLOR)
			    (bg-color RENDER_DEFAULT_BG_COLOR))
  "Given a panel, and optionally a coordslist, an ncurses attribute,
a foreground color index, and a background color-index, this paints
the window panel with spaces, and returns the same coords list."
  (unless coords-list
      (set! coords-list (window-relative-coords panel)))
  (let ([start-y (coords-get-start-y coords-list)]
	[start-x (coords-get-start-x coords-list)]
	[height (coords-get-height coords-list)]
	[width (coords-get-width coords-list)])

    (cond
     ;; If this window has no size, do nothing
     [(coords-zero-area? coords-list)
      coords-list]
     
     ;; If this window has any size, draw it with the background
     [else

      (attr-set! panel attr
		 (color-indices-get-color-pair-index fg-color bg-color))

      (let ((space (normal #\space)))
	(do ((j 0 (1+ j)))
	    ((>= j height))
	  (do ((i 0 (1+ i)))
	      ((>= i width))
	    (addch panel space #:y (+ start-y j) #:x (+ start-x i)))))
      coords-list])))

(define* (render-border panel
			#:key (coords-list #f)
			(attr RENDER_DEFAULT_ATTRIBUTE)
			(fg-color RENDER_DEFAULT_FG_COLOR)
			(bg-color RENDER_DEFAULT_BG_COLOR)
			(border-type RENDER_DEFAULT_BORDER_TYPE))
  "Given a panel, and optionally a coords-list, an ncurses attribute,
a fg and bg color index, and a border type, this draws a border on the panel.
It retuns a coords-list of the window region inside the border."
  (unless coords-list
    (set! coords-list (window-relative-coords panel)))
  (let ([start-y (coords-get-start-y coords-list)]
	[start-x (coords-get-start-x coords-list)]
	[height (coords-get-height coords-list)]
	[width (coords-get-width coords-list)])
    (let ([top (border-top-char border-type)]
	  [bottom (border-bottom-char border-type)]
	  [left (border-left-char border-type)]
	  [right (border-right-char border-type)]
	  [tl (border-top-left-char border-type)]
	  [tr (border-top-right-char border-type)]
	  [bl (border-bottom-left-char border-type)]
	  [br (border-bottom-right-char border-type)])

      (cond

       ;; If there is no border, do nothing
       [(eqv? border-type 'border-none)
	coords-list]
       
       ;; If this window has no size, do nothing
       [(coords-zero-area? coords-list)
	coords-list]
       
       ;; If this window has a height of 1, draw a horizontal line
       [(= 1 height)
	(attr-set! panel attr
		 (color-indices-get-color-pair-index fg-color bg-color))
	
	(hline panel
	       top
	       width
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0)]
       
       ;; If this window has a width of 1, draw a vertical line
       [(= 1 width)
	(attr-set! panel attr
		   (color-indices-get-color-pair-index fg-color bg-color))
	(vline panel
	       left
	       height
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0)]
       ;; Otherwise, draw a normal border
       [else
	(attr-set! panel attr
		   (color-indices-get-color-pair-index fg-color bg-color))
	(border panel
		left right top bottom
		tl tr bl br)
	(coords-adjust coords-list 1 1 -2 -2)]))))

(define* (render-padding #:key coords-list
			 (hpad RENDER_DEFAULT_HORIZ_PAD)
			 (vpad RENDER_DEFAULT_VERT_PAD))
  "Given a panel COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border,
this computes the effect of the the padding, if there is any. It
returns a coords-list (start-y, start-x, width, height) of the
window region inside the padding.

The padding is 'clear' so nothing is actually drawn."
  (let ([start-y (coords-get-start-y coords-list)]
	[start-x (coords-get-start-x coords-list)]
	[height (coords-get-height coords-list)]
	[width (coords-get-width coords-list)])
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

(define (render-line panel str y x width ellipsize)
  "Writes string str as position y, x in the panel.
If the string is wider than WIDTH, cells, it is truncated.  If the
tui-label has an ellipsize flag set, ellipses are added to the string
when it is truncated."
  (let* ((str2 (string-remove-trailing-whitespace str))
	 (str-width (string-width str2)))
    (cond
     ((<= str-width width)
      ;; String isn't over-long; just write it.
      (addstr panel str2 #:y y #:x x))

     (else
      (let ((available-width (if ellipsize (1- width) width)))
	(addstr panel (substring-width str2 available-width) #:y y #:x x)
	(if ellipsize
	    (addstr panel "…")))))))


(define* (render-text panel text #:key coords-list
		      (attr RENDER_DEFAULT_ATTRIBUTE)
		      (fg-color RENDER_DEFAULT_FG_COLOR)
		      (bg-color RENDER_DEFAULT_BG_COLOR)
		      (valign RENDER_DEFAULT_VERT_ALIGN)
		      (halign RENDER_DEFAULT_HORIZ_ALIGN)
		      (ellipsize RENDER_DEFAULT_HORIZ_ELLIPSIZE)
		      (vert-ellipsize RENDER_DEFAULT_VERT_ELLIPSIZE)
		      (line-wrap RENDER_DEFAULT_LINE_WRAP)
		      (max-codepoints RENDER_DEFAULT_CODEPOINT_COUNT_LIMIT))
  (let ([height (coords-get-height coords-list)]
	[width (coords-get-width coords-list)]
	[start-y (coords-get-start-y coords-list)]
	[start-x (coords-get-start-x coords-list)])
    (let* ([line-width (if line-wrap width RENDER_MAX_CELLS_IN_LINE)]
	   [strlist (string-render text
				   RENDER_DEFAULT_TAB_SIZE
				   line-width
				   halign
				   #f ;; BIDI
				   )])
      (set! strlist (string-list-truncate! strlist max-codepoints))

      (cond
       ;; If this window has no size, do nothing
       [(coords-zero-area? coords-list)
	coords-list]
       [else
	(cond
	 [(< (length strlist) height)
	  ;; Modify the start-y, start-x based on the vertical-alignment
	  ;; if there is free vertical space.
	  (set! vert-ellipsize #f)
	  (cond
	   [(eqv? valign 'bottom)
	    (set! start-y (+ start-y (- height (length strlist))))
	    (set! height (length strlist))]
	   [(eqv? valign 'center)
	    (set! start-y (+ start-y (quotient (- height (length strlist)) 2)))
	    (set! height (length strlist))]
	   [(eqv? valign 'top)
	    (set! height (length strlist))]
	   [else
	    (error "bad valign type in render-text" valign)])]
	 [(= (length strlist) height)
	  (set! vert-ellipsize #f)])

	;; Set up the text attributes
	(attr-set! panel attr
		   (color-indices-get-color-pair-index fg-color bg-color))

	;; Write the text
	(do ((j 0 (1+ j))) ((>= j height))
	  (if (and vert-ellipsize (= (1+ j) height))
	      (render-line panel "⋮" (+ start-y j) start-x width ellipsize)
	      (render-line panel (list-ref strlist j) (+ start-y j) start-x width ellipsize)))]))))

