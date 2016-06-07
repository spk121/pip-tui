(define-module (pip-tui tui-label)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui string-lib)
  #:use-module (srfi srfi-1)
  #:export (tui-label-new))


;; The number of cells in the largest allowable window
(define BIG_WIDTH 1000000)

;;; tui-attr


;;; A tui-attr is the attributes of a set of printed text
;;; bold/normal/dim
;;; foreground color / background ocolor
;;; underline
;;; strikethrough


(define TUI_ALIGN_DEFAULT 0)
(define TUI_ALIGN_TOP 0)

;;; tui-wrap-mode
;;; TUI_WRAP_WORK
;;; TUI_WRAP_CHAR
;;; TUI_WRAP_WORD_CHAR

;;; panel-label

;;; A panel-label is a subclass of a panel that has methods to print a
;;; string into the panel.

;;; A tui-label is not an interactive widget.  The text therein
;;; cannot be edited or selected.  The widget itself cannot be
;;; selected.

(define-record-type <tui-label>
  (%tui-label-new panel
		  border-type
		  horizontal-padding
		  vertical-padding
		  attributes
		  color-pair
		  text
		  rendered-text
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
  (color-pair %tui-label-get-color-pair %tui-label-set-color-pair!)
  (text %tui-label-get-text %tui-label-set-text!)
  (rendered-text %tui-label-get-rendered-text %tui-label-set-rendered-text!)
  (horizontal-alignment %tui-label-get-horizontal-alignment %tui-label-set-horizontal-alignment!)
  (vertical-alignment %tui-label-get-vertical-alignment %tui-label-set-vertical-alignment!)
  (ellipsize %tui-label-get-ellipsize %tui-label-set-ellipsize!)
  (line-wrap %tui-label-get-line-wrap %tui-label-set-line-wrap!)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Properties

;;; window           #<window>
;;; border-type           'border-none 'border-light 'border-heavy 'border-rounded 'border-double
;;; horizontal-padding n >= 0
;;; vertical-padding   n >= 0
;;; attributes       tui-attr
;;; text             string
;;; horizontal-alignment  tui-horizontal-align
;;; vertical-alignment    'top 'center 'bottom
;;; ellipsize             tui-ellipsize
;;; line-wrap             boolean


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

(define* (tui-label-new y x width height
			#:key
			(border-type 'border-none)
			(horizontal-padding 0)
			(vertical-padding 0)
			(attributes A_NORMAL)
			(text "")
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
			     1 		;color pair
			     text
			     #f		; rendered text
			     horizontal-alignment
			     vertical-alignment
			     ellipsize
			     line-wrap)))
    (move pan y x)
    (addstr pan "TEXT")
    (tui-label-render-text! TL)
    TL))

(define (tui-label-set-border-type! tui-label border-type)
    "Sets the border-type of the tui-label to 'border-none,
'border-light, 'border-heavy, 'border-rounded,
'border-double.  If the border is not 'border-none, the size available
for text will be reduced by one cell on all four sides of the
tui-label."
    (%tui-label-set-border-type! tui-label border-type))

(define (tui-label-set-horizontal-padding! tui-label n-chars)
  "Sets the number of cells at the right or left of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  )

(define (tui-label-set-vertical-padding! tui-label n-chars)
  "Sets the number of cells at the right or left of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  )

(define (tui-label-set-text! tui-label str)
  "Sets the text within the tui-label widget.  It overwrites any
text that was there before."
  )

(define (tui-label-set-attributes! tui-label tui-attr)
  "Sets the attributes of a tui-label.  If the attributes are
different than the previous attributes, the tui-label is
refreshed."
  )

(define (tui-label-set-horizontal-alignment! tui-label tui-horizontal-alignment)
  "Sets the alignment of the lines in the text of the label relative
to each other.  The allowable values are 'right, 'left, 'center, and 'none"
  )

(define (tui-label-set-vertical-alignment! tui-label tui-vertical-alignment)
  "Sets the alignment of the text lines in the label.  'top, 'center or 'bottom"
  (%tui-label-set-vertical-alignment! tui-label tui-vertical-alignment))

(define (tui-label-set-ellipsize! tui-label tui-ellipsize-mode)
  "Describes what type of ellipsization should be applied to a line
of text.  TUI_ELLIPSIZE_NONE, TUI_ELLIPSIZE_END"
  )

(define (tui-label-set-line-wrap! tui-label wrap)
  "Toggles line wrapping in the tui-label widget.  #t make it break
if text exceedes the widget's size.  #f lets the text get cut off
by the edge of the widget if it exceeds the widget size.

  If a desired wrap width was set with tui-label-set-width-chars!,
it will try to wrap so that no characters written to the right of that
width."
  (%tui-label-set-line-wrap! tui-label wrap))

(define (tui-label-get-text tui-label)
  "Fetches the text from a label as a simple string."
  (%tui-label-get-text tui-label))

(define (tui-label-get-border-type tui-label)
  "Returns the current border-type, which should be one of
'border-none, 'border-light, 'border-heavy, 'border-rounded, 'border-double"
  (%tui-label-get-border-type tui-label))

(define *box-chars-none* "         ")
;; Light box
;; ┌─┐
;; │ │
;; └─┘
(define *box-chars-light* "┌─┐│ │└─┘")

;; Light round box
;; ╭─╮
;; │ │
;; ╰─╯
(define *box-chars-rounded* "╭─╮│ │╰─╯")

;; Heavy box
;; ┏━┓
;; ┃ ┃
;; ┗━┛
(define *box-chars-heavy* "┏━┓┃ ┃┗━┛")

;; Double box
;; ╔═╗
;; ║ ║
;; ╚═╝
(define *box-chars-double* "╔═╗║ ║╚═╝")

(define BOX_TOP_LEFT 0)
(define BOX_TOP 1)
(define BOX_TOP_RIGHT 2)
(define BOX_LEFT 3)
(define BOX_CENTER 4)
(define BOX_RIGHT 5)
(define BOX_BOTTOM_LEFT 6)
(define BOX_BOTTOM 7)
(define BOX_BOTTOM_RIGHT 8)

(define *box-alist*
  (list (cons 'border-none *box-chars-none*)
	(cons 'border-light *box-chars-light*)
	(cons 'border-rounded *box-chars-rounded*)
	(cons 'border-heavy *box-chars-heavy*)
	(cons 'border-double *box-chars-double*)))

(define (border-char tui-label pos)
  (normal (string-ref (assoc-ref *box-alist* (%tui-label-get-border-type tui-label)) pos)))

(define (tui-label-get-border-horiz-char tui-label)
  (border-char tui-label BOX_TOP))
(define (tui-label-get-border-vert-char tui-label)
  (border-char tui-label BOX_LEFT))
(define (tui-label-get-border-top-left-char tui-label)
  (border-char tui-label BOX_TOP_LEFT))
(define (tui-label-get-border-top-right-char tui-label)
  (border-char tui-label BOX_TOP_RIGHT))
(define (tui-label-get-border-bottom-left-char tui-label)
  (border-char tui-label BOX_BOTTOM_LEFT))
(define (tui-label-get-border-bottom-right-char tui-label)
  (border-char tui-label BOX_BOTTOM_RIGHT))


(define (tui-label-get-horizontal-padding tui-label)
  ""
  )

(define (tui-label-get-vertical-padding tui-label)
  ""
  )

(define (tui-label-get-attributes tui-label)
  "Gets the attributes of the label as a tui-attr type."
  )

(define (tui-label-get-horizontal-alignment tui-label)
  "Gets the justification of a label as a tui-justification type."
  )

(define (tui-label-get-vertical-alignment tui-label)
  "Gets the justification of a label as a tui-justification type. One
of 'top, 'center, or 'bottom."
  (%tui-label-get-vertical-alignment tui-label))

(define (tui-label-get-ellipsize tui-label)
  "Gets the ellipsizing position of the label."
  )

(define (tui-label-get-line-wrap tui-label)
  "Returns #t if the tui-label has line wrapping enabled."
  (%tui-label-get-line-wrap tui-label))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from panel

(define (tui-label-show tui-label)
  #t
  )

(define (tui-label-hide tui-label)
  #t
  )

(define (tui-label-hidden? tui-label)
  #T
  )

(define (tui-label-move tui-label y x)
  #t
  )

(define (tui-label-top tui-label)
  #T
  )

(define (tui-label-bottom tui-label)
  #t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from window

;; various get sizes and offsets

(define (tui-label-resize tui-label rows cols)
""
  )

(define (tui-label-getbegyx tui-label)
  (getbegyx (%tui-label-get-panel tui-label)))

(define (tui-label-getmaxyx tui-label)
  (getmaxyx (%tui-label-get-panel tui-label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL FUNCTIONS

(define (coord+ axy bxy)
  (fold (lambda (a b prev)
	  (append prev (list (+ a b))))
	'()
	axy bxy))

(define (coord-start-y coords-list)
  (first coords-list))
(define (coord-start-x coords-list)
  (second coords-list))
(define (coord-height coords-list)
  (third coords-list))
(define (coord-width coords-list)
  (fourth coords-list))

(define (compute-coords-list tui-label)
  "Given a TUI-LABEL, this returns a 4-element list containing the
start-y, start-x, height, and width for the window."
  (let ((ret (append (tui-label-getbegyx tui-label)
		     (tui-label-getmaxyx tui-label))))
    ;; (addstr (%tui-label-get-panel tui-label) (format #f "~s" ret))
    ret))

(define (render-background tui-label coords-list)
  "Given a TUI-LABEL and a coords-list, a 4-element list
containing start-y, start-x height, and width.  This
paints the window with spaces, and returns the
same coords list."
  (let ((start-y (first coords-list))
	(start-x (second coords-list))
	(height (third coords-list))
	(width (fourth coords-list))
	(attr (%tui-label-get-attributes tui-label))
	(color (%tui-label-get-color-pair tui-label))
	(panel (%tui-label-get-panel tui-label)))

    (cond
     ;; If this window has no size, do nothing
     ((or (zero? height) (zero? width))
      coords-list)
     
     ;; If this window has any size, draw it with the background
     (else

      (attr-set! panel attr color)
      ;; (move panel start-y start-x)
      ;; (addstr panel (format #f "~s" coords-list))
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
  (let ((start-y (first coords-list))
	(start-x (second coords-list))
	(height (third coords-list))
	(width (fourth coords-list))
	(panel (%tui-label-get-panel tui-label))
	(attr (%tui-label-get-attributes tui-label))
	(color (%tui-label-get-color-pair tui-label))
	(horiz (tui-label-get-border-horiz-char tui-label))
	(vert (tui-label-get-border-vert-char tui-label))
	(border-type (tui-label-get-border-type tui-label))
	(tl (tui-label-get-border-top-left-char tui-label))
	(tr (tui-label-get-border-top-right-char tui-label))
	(bl (tui-label-get-border-bottom-left-char tui-label))
	(br (tui-label-get-border-bottom-right-char tui-label)))
    ;; (move (stdscr) 20 20)
    ;; (addstr (stdscr) (format #f "BORDER ~s" coords-list))
    ;; (refresh (stdscr))
    ;; (sleep 2)
      
    (cond

     ;; If there is no border, do nothing
     ((eqv? 'border-none border-type)
      coords-list)

     ;; If this window has no size, do nothing
     ((or (zero? height) (zero? width))
      coords-list)
     
     ;; If this window has a height of 1, draw a horizontal line
     ((= 1 height)
      (attr-set! panel attr color)
      (hline panel
	     horiz
	     width
	     #:y start-y
	     #:x start-x)
      (list start-y start-x 0 0))

     ;; If this window has a width of 1, draw a vertical line
     ((= 1 width)
      (attr-set! panel attr color)
      (vline panel
	     vert
	     height
	     #:y start-y
	     #:x start-x)
      (list start-y start-x 0 0))
     
     ;; Otherwise, draw a normal border
     (else
      (attr-set! panel attr color)
      (border panel
	      vert vert horiz horiz
	      tl tr bl br)
      (list (1+ start-y) (1+ start-x)
	    (- height 2) (- width 2))))))
  
(define (render-padding tui-label coords-list)
  "Given a TUI-LABEL and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border,
this computes the effect of the the padding, if there is any. It
returns a 4-element list (start-y, start-x, width, height) of the
window region inside the padding.

The padding should actually have been drawn by render-background."
  (let ((start-y (first coords-list))
	(start-x (second coords-list))
	(height (third coords-list))
	(width (fourth coords-list))
	(hpad (%tui-label-get-horizontal-padding tui-label))
	(vpad (%tui-label-get-vertical-padding tui-label)))
    (cond

     ;; If there is no padding, do nothing
     ((and (zero? hpad) (zero? vpad))
      coords-list)

     ;; If this window has no size, do nothing
     ((or (zero? height) (zero? width))
      coords-list)

     ;; If all that remains of the window is padding
     ((or (<= width (* 2 hpad))
	  (<= height (* 2 vpad)))
      (list start-y start-x 0 0))

     ;; If there is some space inside the padding
     (else
      (list (+ vpad start-y)
	    (+ hpad start-x)
	    (- height (* 2 vpad))
	    (- width (* 2 hpad)))))))

(define (render-text tui-label coords-list)
  "Given a TUI-LABEL and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border and padding,
this draws the text, if there is any. The return value is not used."
  (let* ((start-y (first coords-list))
	 (start-x (second coords-list))
	 (height (third coords-list))
	 (width (fourth coords-list))
	 (attr (%tui-label-get-attributes tui-label))
	 (color (%tui-label-get-color-pair tui-label))
	 (valign (%tui-label-get-vertical-alignment tui-label))
	 (halign (%tui-label-get-horizontal-alignment tui-label))
	 (panel (%tui-label-get-panel tui-label))
	 (line-width (if (%tui-label-get-line-wrap tui-label)
			 width
			 BIG_WIDTH))
	 (string-list (string-render (%tui-label-get-text tui-label)
				     4 ;; TABSIZE
				     line-width
				     halign
				     #f ;; BIDI
				     )))
    (cond

     ;; If this window has no size, do nothing
     ((or (zero? height) (zero? width))
      coords-list)

     (else
      (when (< (length string-list) height)
	;; Modify the start-y, start-x based on the vertical-alignment
	;; if there is free vertical space.
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

      ;; Set up the text attributes
      (attr-set! panel attr color)

      ;; Write the text
      (do ((j 0 (1+ j)))
	  ((>= j height))
	(addstr panel (list-ref string-list j) #:y (+ start-y j) #:x start-x))))))

(define (tui-label-render-text! tui-label)
  (render-text tui-label
	       (render-padding tui-label
			       (render-border tui-label
					      (render-background tui-label
								 (compute-coords-list tui-label))))))
