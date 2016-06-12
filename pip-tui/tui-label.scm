(define-module (pip-tui tui-label)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui border)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui pip-colors)
  #:use-module (pip-tui pip-color-names)
  #:export (tui-label-new

	    tui-label-hide
	    tui-label-show
	    tui-label-resize
	    ))

;;(define *debug-port* (open-file "tui_label_debug.out" "w0"))


;;; tui-label

;;; A tui-label is a subclass of a panel that has methods to print a
;;; string into the panel.

;;; A tui-label is not an interactive widget.  The text therein cannot
;;; be edited or selected.  The widget itself cannot be selected.

(define-record-type <tui-label>
  (%tui-label-new panel
		  border-type
		  horizontal-padding
		  vertical-padding
		  attributes
		  text-color
		  bg-color
		  border-color
		  text
		  horizontal-alignment
		  vertical-alignment
		  ellipsize
		  line-wrap)
  tui-label?
  (panel %tui-label-get-panel %tui-label-set-panel!)
  (border-type %tui-label-get-border-type %tui-label-set-border-type!)
  (horizontal-padding %tui-label-get-horizontal-padding %tui-label-set-horizontal-padding!)
  (vertical-padding %tui-label-get-vertical-padding %tui-label-set-vertical-padding!)
  (attributes %tui-label-get-attributes %tui-label-set-attributes!)
  (text-color %tui-label-get-text-color %tui-label-set-text-color!)
  (bg-color %tui-label-get-bg-color %tui-label-set-bg-color!)
  (border-color %tui-label-get-border-color %tui-label-set-border-color!)
  (text %tui-label-get-text %tui-label-set-text!)
  (horizontal-alignment %tui-label-get-horizontal-alignment %tui-label-set-horizontal-alignment!)
  (vertical-alignment %tui-label-get-vertical-alignment %tui-label-set-vertical-alignment!)
  (ellipsize %tui-label-get-ellipsize %tui-label-set-ellipsize!)
  (line-wrap %tui-label-get-line-wrap %tui-label-set-line-wrap!)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Properties

;;; window           #<window>
;;; border-type           'border-none 'border-light 'border-heavy 'border-rounded 'border-double 'border-block
;;; horizontal-padding n >= 0
;;; vertical-padding   n >= 0
;;; attributes         an ncurses attribute like A_BOLD
;;; text-color         an xterm color index
;;; bg-color
;;; border-color
;;; text               A scheme string
;;; horizontal-alignment  'left 'center or 'right
;;; vertical-alignment    'top 'center or 'bottom
;;; ellipsize             boolean
;;; line-wrap             boolean


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API


;; This is a number of columns that is wider than the widest possible
;; window.
(define BIG_WIDTH 10000)


(define* (tui-label-new y x width height
			#:key
			(border-type 'border-none)
			(horizontal-padding 0)
			(vertical-padding 0)
			(attributes A_NORMAL)
			(text "")
			(text-color COLOR_INDEX_PIPGREEN1)
			(bg-color COLOR_INDEX_BLACK)
			(border-color COLOR_INDEX_PIPGREEN1)
			(color-pair-index 0)
			(horizontal-alignment 'left)
			(vertical-alignment 'top)
			(ellipsize #f)
			(line-wrap #t)
			)
  "Creates a new panel of a given size and position with the given
text inside it.  You can pass an empty string to make an empty
widget."
  (let* ((pan (newwin width height y x #:panel #t))
	 (TL (%tui-label-new pan
			     border-type
			     horizontal-padding
			     vertical-padding
			     attributes
			     text-color
			     bg-color
			     border-color
			     text
			     horizontal-alignment
			     vertical-alignment
			     ellipsize
			     line-wrap)))
    (tui-label-render-text! TL)
    TL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL FUNCTIONS

(define (compute-coords-list tui-label)
  "Given a TUI-LABEL, this returns a coords struct containing the
start-y, start-x, height, and width for the window. start-y and start-x
are 0,0"
  (window-relative-coords (%tui-label-get-panel tui-label)))

(define (render-background tui-label coords-list)
  "Given a TUI-LABEL and a coords-list, a 4-element list
containing start-y, start-x height, and width.  This
paints the window with spaces, and returns the
same coords list."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(attr (%tui-label-get-attributes tui-label))
	(text-color (%tui-label-get-text-color tui-label))
	(bg-color (%tui-label-get-bg-color tui-label))
	(panel (%tui-label-get-panel tui-label)))

    (cond
     ;; If this window has no size, do nothing
     ((coords-zero-area? coords-list)
      coords-list)
     
     ;; If this window has any size, draw it with the background
     (else

      (attr-set! panel attr
		 (color-indices-get-color-pair-index text-color bg-color))

      (let ((space (normal #\space)))
	(do ((j 0 (1+ j)))
	    ((>= j height))
	  (do ((i 0 (1+ i)))
	      ((>= i width))
	    (addch panel space #:y (+ start-y j) #:x (+ start-x i)))))
      coords-list))))

(define (render-border tui-label coords-list)
  "Given a TUI-LABEL and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window, this draw the border,
if there is one. It returns a 4-element list (start-y, start-x, width, height)
of the window region inside the border."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(panel (%tui-label-get-panel tui-label))
	(attr (%tui-label-get-attributes tui-label))
	(border-color (%tui-label-get-border-color tui-label))
	(bg-color (%tui-label-get-bg-color tui-label))
	(border-type (tui-label-get-border-type tui-label)))
    (let ((top (border-top-char border-type))
	  (bottom (border-bottom-char border-type))
	  (left (border-left-char border-type))
	  (right (border-right-char border-type))
	  (tl (border-top-left-char border-type))
	  (tr (border-top-right-char border-type))
	  (bl (border-bottom-left-char border-type))
	  (br (border-bottom-right-char border-type)))
      
      ;; (format *debug-port* "BEGIN render-border label ~s coords ~s~%" border-type coords-list)
      (cond
       
       ;; If there is no border, do nothing
       ((eqv? 'border-none border-type)
	coords-list)
       
       ;; If this window has no size, do nothing
       ((coords-zero-area? coords-list)
	coords-list)
       
       ;; If this window has a height of 1, draw a horizontal line
       ((= 1 height)
	(attr-set! panel attr
		 (color-indices-get-color-pair-index border-color bg-color))
	
	(hline panel
	       top
	       width
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0))
       
       ;; If this window has a width of 1, draw a vertical line
       ((= 1 width)
	(attr-set! panel attr
		   (color-indices-get-color-pair-index border-color bg-color))
	(vline panel
	       left
	       height
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0))
       
       ;; Otherwise, draw a normal border
       (else
	(attr-set! panel attr
		   (color-indices-get-color-pair-index border-color bg-color))
	(border panel
		left right top bottom
		tl tr bl br)
	(coords-adjust coords-list 1 1 -2 -2))))))
  
(define (render-padding tui-label coords-list)
  "Given a TUI-LABEL and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border,
this computes the effect of the the padding, if there is any. It
returns a 4-element list (start-y, start-x, width, height) of the
window region inside the padding.

The padding should actually have been drawn by render-background."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list))
	(hpad (%tui-label-get-horizontal-padding tui-label))
	(vpad (%tui-label-get-vertical-padding tui-label)))
    ;; (format *debug-port* "BEGIN render-padding label ~s coords ~s~%" tui-label coords-list)
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


(define (render-line tui-label str y x width)
  "Writes string str as position y, x in the panel.
If the string is wider than WIDTH, cells, it is truncated.
If the tui-label has an ellipsize flag set, ellipses are
added to the string when it is truncated."
  (let* ((panel (%tui-label-get-panel tui-label))
	 (ellipsize (%tui-label-get-ellipsize tui-label))
	 (str2 (string-remove-trailing-whitespace str))
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

(define (render-text tui-label coords-list)
  "Given a TUI-LABEL and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border and padding,
this draws the text, if there is any. The return value is not used."
  (let* ((start-y (coords-get-start-y coords-list))
	 (start-x (coords-get-start-x coords-list))
	 (height (coords-get-height coords-list))
	 (width (coords-get-width coords-list))
	 (attr (%tui-label-get-attributes tui-label))
	 (text-color (%tui-label-get-text-color tui-label))
	 (bg-color (%tui-label-get-bg-color tui-label))
	 (valign (%tui-label-get-vertical-alignment tui-label))
	 (halign (%tui-label-get-horizontal-alignment tui-label))
	 (panel (%tui-label-get-panel tui-label))
	 (ellipsize (%tui-label-get-ellipsize tui-label))
	 (vert-ellipsize ellipsize)
	 (line-width (if (%tui-label-get-line-wrap tui-label)
			 width
			 BIG_WIDTH))
	 (string-list (string-render (%tui-label-get-text tui-label)
				     4 ;; TABSIZE
				     line-width
				     halign
				     #f ;; BIDI
				     )))
    ;; (format *debug-port* "BEGIN render-text label ~s coords ~s~%" tui-label coords-list)
    (cond

     ;; If this window has no size, do nothing
     ((coords-zero-area? coords-list)
      coords-list)

     (else
      (cond
       ((< (length string-list) height)
	;; Modify the start-y, start-x based on the vertical-alignment
	;; if there is free vertical space.
	(set! vert-ellipsize #f)
	(cond
	 ((eqv? valign 'bottom)
	  (set! start-y (+ start-y (- height (length string-list))))
	  (set! height (length string-list)))
	 ((eqv? valign 'center)
	  (set! start-y (+ start-y (quotient (- height (length string-list)) 2)))
	  (set! height (length string-list)))
	 ((eqv? valign 'top)
	  (set! height (length string-list)))
	 (else
	  (error "bad valign type in render-text" valign))))
       ((= (length string-list) height)
	(set! vert-ellipsize #f)))

      ;; Set up the text attributes
      (attr-set! panel attr
		 (color-indices-get-color-pair-index text-color bg-color))


      ;; Write the text
      (do ((j 0 (1+ j)))
	  ((>= j height))
	;; (format *debug-port* "render-text, write a line j ~s height ~s start-y ~s start-x ~s~%" j height start-y start-x)
	(if (and vert-ellipsize (= (1+ j) height))
	    (render-line tui-label "⋮" (+ start-y j) start-x width)
	    (render-line tui-label (list-ref string-list j) (+ start-y j) start-x width)))))))

(define (tui-label-render-text! tui-label)
  (render-text tui-label
	       (render-padding tui-label
			       (render-border tui-label
					      (render-background tui-label
								 (compute-coords-list tui-label))))))


(define (tui-label-set-border-type! tui-label border-type)
    "Sets the border-type of the tui-label to 'border-none,
'border-light, 'border-heavy, 'border-rounded, 'border-double or
'border-block.  If the border is not 'border-none, the size available
for text will be reduced by one cell on all four sides of the
tui-label."
    (%tui-label-set-border-type! tui-label border-type)
    (tui-label-render-text! tui-label))

(define (tui-label-set-horizontal-padding! tui-label n-chars)
  "Sets the number of cells at the right and left of the widget that
are blank padding.  If there is a border, this padding is inside of
the border."
  (%tui-label-set-horizontal-padding! tui-label n-chars)
  (tui-label-render-text! tui-label))

(define (tui-label-set-vertical-padding! tui-label n-chars)
  "Sets the number of cells at the top and bottom of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  (%tui-label-set-vertical-padding! tui-label n-chars)
  (tui-label-render-text! tui-label))

(define (tui-label-set-text! tui-label str)
  "Sets the text within the tui-label widget.  It overwrites any
text that was there before."
  (%tui-label-set-text! tui-label str)
  (tui-label-render-text! tui-label))

(define (tui-label-set-attributes! tui-label tui-attr)
  "Sets the ncurses attributes of a tui-label, such as A_BOLD.  If the
attributes are different than the previous attributes, the tui-label
is refreshed."
  (unless (eqv? (%tui-label-get-attributes tui-label) tui-attr)
    (%tui-label-set-attributes! tui-label tui-attr)
    (tui-label-render-text! tui-label)))

(define (tui-label-set-horizontal-alignment! tui-label tui-horizontal-alignment)
  "Sets the alignment of the lines in the text of the label relative
to each other.  The allowable values are 'right, 'left, 'center"
  (%tui-label-set-horizontal-alignment! tui-label tui-horizontal-alignment)
  (tui-label-render-text! tui-label))

(define (tui-label-set-vertical-alignment! tui-label tui-vertical-alignment)
  "Sets the alignment of the text lines in the label.  'top, 'center or 'bottom"
  (%tui-label-set-vertical-alignment! tui-label tui-vertical-alignment)
  (tui-label-render-text! tui-label))

(define (tui-label-set-ellipsize! tui-label tui-ellipsize-mode)
  "Describes what type of ellipsization should be applied to a line
of text.  #t or #f"
  (%tui-label-set-ellipsize! tui-label tui-ellipsize-mode)
  (tui-label-render-text! tui-label))

(define (tui-label-set-line-wrap! tui-label wrap)
  "Sets line wrapping in the tui-label widget.  #t make it break
if text exceedes the widget's size.  #f lets the text get cut off
by the edge of the widget if it exceeds the widget size.

  If a desired wrap width was set with tui-label-set-width-chars!,
it will try to wrap so that no characters written to the right of that
width."
  (%tui-label-set-line-wrap! tui-label wrap)
  (tui-label-render-text! tui-label))

(define (tui-label-get-text tui-label)
  "Fetches the text from a label as a simple string."
  (%tui-label-get-text tui-label))

(define (tui-label-get-border-type tui-label)
  "Returns the current border-type, which should be one of
'border-none, 'border-light, 'border-heavy, 'border-rounded,
'border-double, or 'border-block"
  (%tui-label-get-border-type tui-label))



(define (tui-label-get-horizontal-padding tui-label)
  (%tui-label-get-horizontal-padding tui-label))

(define (tui-label-get-vertical-padding tui-label)
  (%tui-label-get-vertical-padding tui-label))

(define (tui-label-get-attributes tui-label)
  "Gets the attributes of the label as an ncurses attribute type."
  (%tui-label-get-attributes tui-label))

(define (tui-label-get-horizontal-alignment tui-label)
  "Gets the justification of a label as a tui-justification type."
  (%tui-label-get-horizontal-alignment tui-label))

(define (tui-label-get-vertical-alignment tui-label)
  "Gets the justification of a label as a tui-justification type. One
of 'top, 'center, or 'bottom."
  (%tui-label-get-vertical-alignment tui-label))

(define (tui-label-get-ellipsize tui-label)
  "Gets the ellipsizing flag of the label."
  (%tui-label-get-ellipsize tui-label))

(define (tui-label-get-line-wrap tui-label)
  "Returns #t if the tui-label has line wrapping enabled."
  (%tui-label-get-line-wrap tui-label))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from panel

(define (tui-label-show tui-label)
  (show-panel (%tui-label-get-panel tui-label)))
  
(define (tui-label-hide tui-label)
  (hide-panel (%tui-label-get-panel tui-label)))

(define (tui-label-hidden? tui-label)
  (panel-hidden? (%tui-label-get-panel tui-label)))

(define (tui-label-move tui-label y x)
  (move-panel (%tui-label-get-panel tui-label) y x))

(define (tui-label-top tui-label)
  (top-panel (%tui-label-get-panel tui-label)))

(define (tui-label-bottom tui-label)
  (bottom-panel (%tui-label-get-panel tui-label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from window

;; various get sizes and offsets

(define (tui-label-resize tui-label rows cols)
  (resize (%tui-label-get-panel tui-label) rows cols)
  (tui-label-render-text! tui-label))

(define (tui-label-getbegyx tui-label)
  (getbegyx (%tui-label-get-panel tui-label)))

(define (tui-label-getmaxyx tui-label)
  (getmaxyx (%tui-label-get-panel tui-label)))

