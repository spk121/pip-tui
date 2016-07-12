(define-module (pip-tui coords)
  #:use-module (ncurses curses)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (coords-new
	    coords-get-start-y
	    coords-get-start-x
	    coords-get-height
	    coords-get-width
	    coords-values
	    coords-set-start-y!
	    coords-set-start-x!
	    coords-set-height!
	    coords-set-width!
	    coords-adjust
	    window-relative-coords
	    window-absolute-coords
	    coords-zero-area?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COORDS

;; A struct that describes the size of a region on the screen.
;; start-y (integer): the line # of the top row of the region
;; start-x (integer): the column # of the left column of the region
;; height (integer): the number of rows in the region
;; width (integer): the number of columns in the region

;; 

(define-record-type <coords>
  (coords-new start-y
	      start-x
	      height
	      width)
  coords?
  (start-y coords-get-start-y coords-set-start-y!)
  (start-x coords-get-start-x coords-set-start-x!)
  (height coords-get-height coords-set-height!)
  (width coords-get-width coords-set-width!))

(define (coords-values C)
  "Returns as multiple values the elements of a <coords> type."
  (values (coords-get-start-y C)
	  (coords-get-start-x C)
	  (coords-get-height C)
	  (coords-get-width C)))

(define (coords-adjust C delta-start-y delta-start-x delta-height delta-width)
  "Returns a new <coords>, which is like <coords> C adjusted by 
DELTA-START-Y, DELTA-START-X, DELTA-HEIGHT, DELTA-WIDTH"
  (coords-new
   (+ (coords-get-start-y C) delta-start-y)
   (+ (coords-get-start-x C) delta-start-x)
   (+ (coords-get-height C) delta-height)
   (+ (coords-get-width C) delta-width)))

(define (window-relative-coords win)
  "Given an ncurses panel window WIN, this returns a COORDS struct
with the 0,0 as the top-left corner and dimensions of the panel."
  (let ((yx (getmaxyx win)))
    (coords-new 0 0 (first yx) (second yx))))

(define (window-absolute-coords win)
  "Given an ncurses window WIN, this returns a COORDS struct with the
top-left corner and the dimensions of the panel."
  (let ((maxyx (getmaxyx win))
	(begyx (getbegyx win)))
    (coords-new (first begyx) (second begyx) (first maxyx) (second maxyx))))

(define (coords-zero-area? C)
  "Returns #t if the area of the region is zero, e.g.
if the width is zero, the height is zero, or both are zero."
  (and (zero? (coords-get-width C))
       (zero? (coords-get-height C))))

(define-syntax-rule (receive-coords vars vals . body)
  (call-with-values (lambda () vals)
    (lambda vars . body)))
