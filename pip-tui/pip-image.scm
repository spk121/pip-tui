;;; A library of functions for pseudographics

(define-module (pip-tui pip-image)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (pip-tui pip-colors)
  #:use-module (ncurses curses)
  #:export (split-RGB32
	    image->pseudographics
	    render-image)
  )


;;  The top-left box is always foreground
;;  
;;  N  TL TR  BL  BR
;;  1   T  F   F   F
;;  2   T  T   F   F
;;  3   T  F   T   F
;;  4   T  T   T   F
;;  5   T  F   F   T
;;  6   T  T   F   T
;;  7   T  F   T   T
;;  8   T  T   T   T


(define PSEUDOGRAPHICS
  (list
   ;; TOP_LEFT = 1, TOP_RIGHT = 2, BOTTOM_LEFT = 4, BOTTOM_RIGHT = 8

   #\x2598				; quadrant upper left
   #\x2580				; upper half block
   #\x258C				; left half block
   #\x259b				; upper left, upper right, lower left
   
   #\x259a				; upper left, lower right
   #\x259c				; upper left, upper right, lower right
   #\x2599                              ; upper left, lower left, lower right
   #\x2588				; full block



   ;; #\space				;
   ;; #\x259D				; quadrant upper right
   ;; #\x2596				; quadrant lower left
   ;; #\x259e				; quadrant upper right and lower left
   ;; #\x2597				; lower right
   ;; #\x2590				; right half block
   ))

;; Given a 2D array, binds the dimensions or ARRAY
					;  to y-lo y-hi x-lo and x-hi
(define-syntax receive-array-shape
  (syntax-rules()
    ((receive-array-shape (ARR A B C D) body ...)
     (let ((ret (array-shape ARR)))
       (let ((A (car (car ret)))
	     (B (cadr (car ret)))
	     (C (car (cadr ret)))
	     (D (cadr (cadr ret))))
	 body
	 ...)))))

(define (split-RGB32 ARR)
  "Given and array of RGB32 colorvals, returns three arrays, red,
green, blue.  These arrays are signed 16-bit integers."
  (receive-array-shape (ARR ylo yhi xlo xhi)
    (let ([R (make-typed-array 's16 0 `(,ylo ,yhi) `(,xlo ,xhi))]
	  [G (make-typed-array 's16 0 `(,ylo ,yhi) `(,xlo ,xhi))]
	  [B (make-typed-array 's16 0 `(,ylo ,yhi) `(,xlo ,xhi))])
      (array-map! R (lambda (rgb)
		      (ash (logand #xFF0000 rgb) -16)) ARR)
      (array-map! G (lambda (rgb)
		      (ash (logand #x00FF00 rgb) -8)) ARR)
      (array-map! B (lambda (rgb)
		      (logand #x0000FF rgb)) ARR)
      (values R G B))))

(define (ceiling÷ a b)
  "Divide integer A by integer B, rounding
up if there is a remainder."
  (let ([q (quotient a b)]
	[rem (remainder a b)])
    (if (> rem 0)
	(1+ q)
	q)))

(define(× a b)
  (inexact->exact (floor (* a b))))


(define (array-inc! ARR delta y x)
  (let ((val (false-if-exception (array-ref ARR y x))))
    (when val
      (array-set! ARR (+ delta val) y x))))

(define (image->pseudographics argb32 palette dither)
  (receive-array-shape (argb32 ylo yhi xlo xhi)
    (receive (R G B) (split-RGB32 argb32)
      (let* (;; The number of screen cells
	     [jmax (ceiling÷ (- yhi ylo) 2)]
	     [imax (ceiling÷ (- xhi xlo) 2)]
	     ;; Color indices of the screen cells
	     [fg-color (make-typed-array 'u8 0 jmax imax)]
	     [bg-color (make-typed-array 'u8 0 jmax imax)]
	     ;; An index into the PSEUDOGRAPHICS array
	     [glyph (make-typed-array 'u8 0 jmax imax)])

	(define (match-subcell y x palette)
	  "Return the closest matching color in PALETTE at (x,y).
If DITHER is true, distribute any color mismatch errors to adjoining cells
using the Gran-Floyd-Steinberg dithering (LOL!)."
	  (let* ([top-left (and (zero? (remainder (- y ylo) 2))
				(zero? (remainder (- x xlo) 2)))])
	    (receive (idx dR dG dB) (palette-match palette
						   (array-ref R y x)
						   (array-ref G y x)
						   (array-ref B y x))
	      ;; If dithering, distribute the errors
	      (when dither
		;; distribute the errors
		(if top-left
		    (begin
		      (array-inc! R (× dR 8/16) y (1+ x))
		      (array-inc! G (× dG 8/16) y (1+ x))
		      (array-inc! B (× dB 8/16) y (1+ x))
		      
		      (array-inc! R (× dR 2/16) (1+ y) (1+ x))
		      (array-inc! G (× dG 2/16) (1+ y) (1+ x))
		      (array-inc! B (× dB 2/16) (1+ y) (1+ x))
		      
		      (array-inc! R (× dR 6/16) (1+ y) x)
		      (array-inc! G (× dG 6/16) (1+ y) x)
		      (array-inc! B (× dB 6/16) (1+ y) x))
		    ;; else
		    (begin
		      (array-inc! R (× dR 7/16) y (1+ x))
		      (array-inc! G (× dG 7/16) y (1+ x))
		      (array-inc! B (× dB 7/16) y (1+ x))
		      
		      (array-inc! R (× dR 1/16) (1+ y) (1+ x))
		      (array-inc! G (× dG 1/16) (1+ y) (1+ x))
		      (array-inc! B (× dB 1/16) (1+ y) (1+ x))
		      
		      (array-inc! R (× dR 5/16) (1+ y) x)
		      (array-inc! G (× dG 5/16) (1+ y) x)
		      (array-inc! B (× dB 5/16) (1+ y) x)
		      
		      (array-inc! R (× dR 3/16) (1+ y) (max 0 (1- x)))
		      (array-inc! G (× dG 3/16) (1+ y) (max 0 (1- x)))
		      (array-inc! B (× dB 3/16) (1+ y) (max 0 (1- x))))))
	      ;; Return the color index
	      idx)))

	(do ((j 0 (1+ j)))
	    ((>= j jmax))
	  (do ((i 0 (1+ i)))
	      ((>= i imax))
	    (let ([y (+ j j ylo)]
		  [x (+ i i xlo)])
	      
	      (let ((top-left-index (match-subcell y x palette))
		    (top-right-index (match-subcell y (1+ x) palette))
		    (bottom-left-index 0)
		    (bottom-right-index 0))

		;; The top-left cell in a 2x2 box is always going to be
		;; foreground.
		(array-set! fg-color top-left-index j i)
		(array-set! bg-color top-left-index j i)
		(if (not (= top-left-index top-right-index))
		    (begin
		      ;; The top-right is different that the top-left, so
		      ;; it is going to be the background color.
		      (array-set! bg-color top-right-index j i)
		      ;; Our palette is reduced to two colors for
		      ;; bottom-left and bottom-right
		      (set! bottom-left-index (match-subcell (1+ y) x `(,top-left-index ,top-right-index)))
		      (set! bottom-right-index (match-subcell (1+ y) (1+ x) `(,top-left-index ,top-right-index))))
		    ;; else 
		    ;; top-left == top-right
		    (begin
		      (set! bottom-left-index (match-subcell (1+ y) x palette))
		      (if (not (= bottom-left-index top-left-index))
			  (begin
			    ;; The bottom-left is different than the
			    ;; top-left and top-right, so it is going to
			    ;; be the background color.
			    (array-set! bg-color bottom-left-index j i)
			    ;; The palette is reduced to two color for the
			    ;; bottom-right.
			    (set! bottom-right-index (match-subcell (1+ y) (1+ x) `(,top-left-index ,bottom-left-index))))
			  ;; else
			  ;; top-left == top-right == bottom-left
			  (begin
			    (set! bottom-right-index (match-subcell (1+ y) (1+ x) palette))
			    ;; The other 3 blocks are the same color.  So
			    ;; this one is going to be the background
			    ;; color, regardless if it is the same as or
			    ;; different from the others.
			    (array-set! bg-color bottom-right-index j i)))))

		;; Now to figure out which glyph
		(array-set! glyph
			    (+  		; top left is always foreground
			       (if (= top-right-index top-left-index)
				   1
				   0)
			       (if (= bottom-left-index top-left-index)
				   2
				   0)
			       (if (= bottom-right-index top-left-index)
				   4
				   0))
			    j i)))))
	(list
	 glyph
	 fg-color
	 bg-color)))))

(define (render-image panel pixel-argb32 palette dither)
  "Draws an array of pixels to the given panel using
a given array of color indices.  If dither #t, it uses
Gran-Floyd-Steinberg dithering (LOL!) to distribute
color errors."
  (let* ([pgr (image->pseudographics pixel-argb32 palette dither)]
	 
	 [glyph (first pgr)]
	 [fg-color (second pgr)]
	 [bg-color (third pgr)])
    (do ((j 0 (1+ j)))
	((>= j (car (array-dimensions glyph))))
      (refresh panel)
      (do ((i 0 (1+ i)))
	  ((>= i (cadr (array-dimensions glyph))))
	(attr-set! panel A_NORMAL (color-indices-get-color-pair-index (array-ref fg-color j i)
								      (array-ref bg-color j i)))
	(addch panel (normal (list-ref PSEUDOGRAPHICS (array-ref glyph j i))) #:y j #:x i)))))
  
