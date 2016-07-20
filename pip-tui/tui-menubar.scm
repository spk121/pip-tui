(define-module (pip-tui tui-menubar)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (pip-tui string-lib)
  #:use-module (pip-tui event)
  #:use-module (pip-tui action)
  #:use-module (pip-tui action-map)
  #:use-module (pip-tui tui-action)
  #:use-module (pip-tui data-lib)
  #:use-module (pip-tui border)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui pip-colors)
  #:use-module (pip-tui pip-color-names)
  #:use-module (pip-tui render-lib)
  #:export (tui-menubar-new

	    tui-menubar-hide
	    tui-menubar-show
	    tui-menubar-resize
	    %tui-menubar-get-panel
	    tui-menubar-action-handler
	    ))

;;(define *debug-port* (open-file "tui_menubar_debug.out" "w0"))


;;; tui-menubar

;;; A tui-menubar is a subclass of a panel that has methods to print a
;;; single line menu of keys and labels

;;; A tui-menubar is an interactive widget.  It accepts single
;;; keypresses.

(define-record-type <tui-menubar>
  (%tui-menubar-new panel
		  border-type
		  horizontal-padding
		  vertical-padding
		  attributes
		  text-color
		  bg-color
		  border-color
		  key-label-alist
		  horizontal-alignment
		  vertical-alignment
		  ellipsize
		  line-wrap)
  tui-menubar?
  (panel %tui-menubar-get-panel %tui-menubar-set-panel!)
  (border-type %tui-menubar-get-border-type %tui-menubar-set-border-type!)
  (horizontal-padding %tui-menubar-get-horizontal-padding %tui-menubar-set-horizontal-padding!)
  (vertical-padding %tui-menubar-get-vertical-padding %tui-menubar-set-vertical-padding!)
  (attributes %tui-menubar-get-attributes %tui-menubar-set-attributes!)
  (text-color %tui-menubar-get-text-color %tui-menubar-set-text-color!)
  (bg-color %tui-menubar-get-bg-color %tui-menubar-set-bg-color!)
  (border-color %tui-menubar-get-border-color %tui-menubar-set-border-color!)
  (key-label-alist %tui-menubar-get-key-label-alist %tui-menubar-set-key-label-alist!)
  (horizontal-alignment %tui-menubar-get-horizontal-alignment %tui-menubar-set-horizontal-alignment!)
  (vertical-alignment %tui-menubar-get-vertical-alignment %tui-menubar-set-vertical-alignment!)
  (ellipsize %tui-menubar-get-ellipsize %tui-menubar-set-ellipsize!)
  (line-wrap %tui-menubar-get-line-wrap %tui-menubar-set-line-wrap!)
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
;;; key-label-alist       An alist where the key is an ncurses keypress and
;;;                       the label is a short text
;;; horizontal-alignment  'left 'center or 'right
;;; vertical-alignment    'top 'center or 'bottom
;;; ellipsize             boolean
;;; line-wrap             boolean


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API


;; This is a number of columns that is wider than the widest possible
;; window.
(define BIG_WIDTH 10000)


(define* (tui-menubar-new y x width height
			#:key
			(border-type 'border-none)
			(horizontal-padding 0)
			(vertical-padding 0)
			(attributes A_NORMAL)
			(key-label-alist '())
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
key/labels inside it."
  (let* ((pan (newwin width height y x #:panel #t))
	 (TL (%tui-menubar-new pan
			     border-type
			     horizontal-padding
			     vertical-padding
			     attributes
			     text-color
			     bg-color
			     border-color
			     key-label-alist
			     horizontal-alignment
			     vertical-alignment
			     ellipsize
			     line-wrap)))
    (tui-menubar-render-text! TL)
    TL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL FUNCTIONS

(define (compute-coords-list tui-menubar)
  "Given a TUI-MENUBAR, this returns a coords struct containing the
start-y, start-x, height, and width for the window. start-y and start-x
are 0,0"
  (window-relative-coords (%tui-menubar-get-panel tui-menubar)))

(define (do-background tui-menubar coords-list)
  "Given a TUI-MENUBAR and a coords-list, a 4-element list containing
start-y, start-x height, and width.  This paints the window with
spaces, and returns the same coords list."
  (let ((attr (%tui-menubar-get-attributes tui-menubar))
	(text-color (%tui-menubar-get-text-color tui-menubar))
	(bg-color (%tui-menubar-get-bg-color tui-menubar))
	(panel (%tui-menubar-get-panel tui-menubar)))
    (render-background panel
		       #:coords-list coords-list
		       #:attr attr
		       #:fg-color text-color
		       #:bg-color bg-color)))

(define (do-border tui-menubar coords-list)
  "Given a TUI-MENUBAR and COORDS-LIST, a 4-element struct containing
start-y, start-x, height, and width for the window, this draw the border,
if there is one. It returns a 4-element list (start-y, start-x, width, height)
of the window region inside the border."
  (let ((panel (%tui-menubar-get-panel tui-menubar))
	(attr (%tui-menubar-get-attributes tui-menubar))
	(border-color (%tui-menubar-get-border-color tui-menubar))
	(bg-color (%tui-menubar-get-bg-color tui-menubar))
	(border-type (%tui-menubar-get-border-type tui-menubar)))
    (render-border panel #:coords-list coords-list
		   #:attr attr
		   #:fg-color border-color
		   #:bg-color bg-color
		   #:border-type border-type)))
  
(define (do-padding tui-menubar coords-list)
  "Given a TUI-MENUBAR and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border,
this computes the effect of the the padding, if there is any. It
returns a 4-element list (start-y, start-x, width, height) of the
window region inside the padding.

The padding should actually have been drawn by render-background."
  (let ((hpad (%tui-menubar-get-horizontal-padding tui-menubar))
	(vpad (%tui-menubar-get-vertical-padding tui-menubar)))
    (render-padding #:coords-list coords-list
		    #:hpad hpad
		    #:vpad vpad)))


(define (key-label-alist->string kv)
  "Renders an alist where the CAR is an ncurses character and
CDR is a short string into a single string like the following..
'X) INSPECT  R) DROP  C) CYCLE DAMAGE  Q) FAV'"
  (let ([entry-strings (map
			(lambda (entry)
			  (let ([key (car entry)]
				[val (->string (cdr entry))])
			    (string-append
			     (keyname key)
			     ") "
			     val)))
			kv)])
    (if (<= (length entry-strings) 1)
	entry-string
	;; else
	(let loop ([result (car entry-strings)]
		   [cur (cadr entry-strings)]
		   [rest (cddr entry-strings)]
		   )
	  (cond
	   ((null-list? rest)
	    (string-append result "  " cur))
	   (else
	    (loop (string-append result "  " cur)
		  (car rest)
		  (cdr rest))))))))
	  

(define (do-text tui-menubar coords-list)
  "Given a TUI-MENUBAR and COORDS-LIST, a 4-element list containing
start-y, start-x, height, and width for the window inside the border
and padding, this draws the key/label text. The return value is not
used."
  (let ([panel (%tui-menubar-get-panel tui-menubar)]
	[keyval (%tui-menubar-get-key-label-alist tui-menubar)])
    (render-text panel (key-label-alist->string keyval)
		 #:coords-list coords-list
		 #:attr (%tui-menubar-get-attributes tui-menubar)
		 #:fg-color (%tui-menubar-get-text-color tui-menubar)
		 #:bg-color (%tui-menubar-get-bg-color tui-menubar)
		 #:valign (%tui-menubar-get-vertical-alignment tui-menubar)
		 #:halign (%tui-menubar-get-horizontal-alignment tui-menubar)
		 #:ellipsize (%tui-menubar-get-ellipsize tui-menubar)
		 #:vert-ellipsize (%tui-menubar-get-ellipsize tui-menubar)
		 #:line-wrap (%tui-menubar-get-line-wrap tui-menubar))))

(define (tui-menubar-render-text! tui-menubar)
  (do-text tui-menubar
	   (do-padding tui-menubar
		       (do-border tui-menubar
				  (do-background tui-menubar
						 (compute-coords-list tui-menubar))))))


(define (tui-menubar-set-border-type! tui-menubar border-type)
    "Sets the border-type of the tui-menubar to 'border-none,
'border-light, 'border-heavy, 'border-rounded, 'border-double or
'border-block.  If the border is not 'border-none, the size available
for text will be reduced by one cell on all four sides of the
tui-menubar."
    (%tui-menubar-set-border-type! tui-menubar border-type)
    (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-horizontal-padding! tui-menubar n-chars)
  "Sets the number of cells at the right and left of the widget that
are blank padding.  If there is a border, this padding is inside of
the border."
  (%tui-menubar-set-horizontal-padding! tui-menubar n-chars)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-vertical-padding! tui-menubar n-chars)
  "Sets the number of cells at the top and bottom of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  (%tui-menubar-set-vertical-padding! tui-menubar n-chars)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-text! tui-menubar str)
  "Sets the text within the tui-menubar widget.  It overwrites any
text that was there before."
  (%tui-menubar-set-text! tui-menubar str)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-attributes! tui-menubar tui-attr)
  "Sets the ncurses attributes of a tui-menubar, such as A_BOLD.  If the
attributes are different than the previous attributes, the tui-menubar
is refreshed."
  (unless (eqv? (%tui-menubar-get-attributes tui-menubar) tui-attr)
    (%tui-menubar-set-attributes! tui-menubar tui-attr)
    (tui-menubar-render-text! tui-menubar)))

(define (tui-menubar-set-horizontal-alignment! tui-menubar tui-horizontal-alignment)
  "Sets the alignment of the lines in the text of the menubar relative
to each other.  The allowable values are 'right, 'left, 'center"
  (%tui-menubar-set-horizontal-alignment! tui-menubar tui-horizontal-alignment)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-vertical-alignment! tui-menubar tui-vertical-alignment)
  "Sets the alignment of the text lines in the menubar.  'top, 'center or 'bottom"
  (%tui-menubar-set-vertical-alignment! tui-menubar tui-vertical-alignment)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-ellipsize! tui-menubar tui-ellipsize-mode)
  "Describes what type of ellipsization should be applied to a line
of text.  #t or #f"
  (%tui-menubar-set-ellipsize! tui-menubar tui-ellipsize-mode)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-set-line-wrap! tui-menubar wrap)
  "Sets line wrapping in the tui-menubar widget.  #t make it break
if text exceedes the widget's size.  #f lets the text get cut off
by the edge of the widget if it exceeds the widget size.

  If a desired wrap width was set with tui-menubar-set-width-chars!,
it will try to wrap so that no characters written to the right of that
width."
  (%tui-menubar-set-line-wrap! tui-menubar wrap)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-get-text tui-menubar)
  "Fetches the text from a menubar as a simple string."
  (%tui-menubar-get-text tui-menubar))

(define (tui-menubar-get-border-type tui-menubar)
  "Returns the current border-type, which should be one of
'border-none, 'border-light, 'border-heavy, 'border-rounded,
'border-double, or 'border-block"
  (%tui-menubar-get-border-type tui-menubar))

(define (tui-menubar-get-horizontal-padding tui-menubar)
  (%tui-menubar-get-horizontal-padding tui-menubar))

(define (tui-menubar-get-vertical-padding tui-menubar)
  (%tui-menubar-get-vertical-padding tui-menubar))

(define (tui-menubar-get-attributes tui-menubar)
  "Gets the attributes of the menubar as an ncurses attribute type."
  (%tui-menubar-get-attributes tui-menubar))

(define (tui-menubar-get-horizontal-alignment tui-menubar)
  "Gets the justification of a menubar as a tui-justification type."
  (%tui-menubar-get-horizontal-alignment tui-menubar))

(define (tui-menubar-get-vertical-alignment tui-menubar)
  "Gets the justification of a menubar as a tui-justification type. One
of 'top, 'center, or 'bottom."
  (%tui-menubar-get-vertical-alignment tui-menubar))

(define (tui-menubar-get-ellipsize tui-menubar)
  "Gets the ellipsizing flag of the menubar."
  (%tui-menubar-get-ellipsize tui-menubar))

(define (tui-menubar-get-line-wrap tui-menubar)
  "Returns #t if the tui-menubar has line wrapping enabled."
  (%tui-menubar-get-line-wrap tui-menubar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from panel

(define (tui-menubar-show tui-menubar)
  (show-panel (%tui-menubar-get-panel tui-menubar)))
  
(define (tui-menubar-hide tui-menubar)
  (hide-panel (%tui-menubar-get-panel tui-menubar)))

(define (tui-menubar-hidden? tui-menubar)
  (panel-hidden? (%tui-menubar-get-panel tui-menubar)))

(define (tui-menubar-move tui-menubar y x)
  (move-panel (%tui-menubar-get-panel tui-menubar) y x))

(define (tui-menubar-top tui-menubar)
  (top-panel (%tui-menubar-get-panel tui-menubar)))

(define (tui-menubar-bottom tui-menubar)
  (bottom-panel (%tui-menubar-get-panel tui-menubar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API Inherited from window

;; various get sizes and offsets

(define (tui-menubar-resize tui-menubar rows cols)
  (resize (%tui-menubar-get-panel tui-menubar) rows cols)
  (tui-menubar-render-text! tui-menubar))

(define (tui-menubar-getbegyx tui-menubar)
  (getbegyx (%tui-menubar-get-panel tui-menubar)))

(define (tui-menubar-getmaxyx tui-menubar)
  (getmaxyx (%tui-menubar-get-panel tui-menubar)))

(define (tui-menubar-idle TM)
  (tui-menubar-render-text! TM))

(define tui-menubar-kbd-action-activate
  (lambda (TM event state)
    (let ((key (event-get-data event)))
      ;; If a keypress matches a key in the key-label alist
      ;; enqueue a signal
      ((lambda (K)
	 (when K
	   (enqueue-symbolic-action 'menubar-keypress (list key K TM))))
       (assoc-ref (%tui-menubar-get-key-label-alist TM) key)))))

(define (tui-menubar-action-activate TT event state)
  (cond
   ((idle-event? event)
    (tui-menubar-idle TT))
   ((kbd-event? event)
    (tui-menubar-kbd-action-activate TT event state))))

(define (tui-menubar-action-handler)
  (action-new "tui-menubar" #t '() tui-menubar-action-activate #t))

