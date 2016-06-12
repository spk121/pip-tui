(define-module (pip-tui border)
  #:use-module (ncurses curses)
  #:use-module (pip-tui coords)
  #:use-module (pip-tui pip-colors)
  #:export (border-left-char
	    border-right-char
	    border-top-char
	    border-bottom-char
	    border-top-left-char
	    border-top-right-char
	    border-bottom-left-char
	    border-bottom-right-char
	    border-draw-on-panel))

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

;; Block-char box
;; ▛▀▜
;; ▌ ▐
;; ▙▄▟
(define *box-chars-block* "▛▀▜▌ ▐▙▄▟")

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
	(cons 'border-double *box-chars-double*)
	(cons 'border-block *box-chars-block*)))

(define (border-char border-type pos)
  (normal (string-ref (assoc-ref *box-alist* border-type) pos)))

(define (border-left-char border-type)
  (border-char border-type BOX_LEFT))
(define (border-right-char border-type)
  (border-char border-type BOX_RIGHT))
(define (border-top-char border-type)
  (border-char border-type BOX_TOP))
(define (border-bottom-char border-type)
  (border-char border-type BOX_BOTTOM))
(define (border-top-left-char border-type)
  (border-char border-type BOX_TOP_LEFT))
(define (border-top-right-char border-type)
  (border-char border-type BOX_TOP_RIGHT))
(define (border-bottom-left-char border-type)
  (border-char border-type BOX_BOTTOM_LEFT))
(define (border-bottom-right-char border-type)
  (border-char border-type BOX_BOTTOM_RIGHT))

(define (border-draw-on-panel panel coords-list fg-color bg-color border-type)
  "Given a PANEL and COORDS-LIST, foreground and background color
indices, and a border-type-symbol such as 'border-none,
'border-light, 'border-rounded, 'border-heavy, 'border-double, or
'border-block, this will draw the border if there is one. It returns a
coords type of the window region inside the border."
  (let ((start-y (coords-get-start-y coords-list))
	(start-x (coords-get-start-x coords-list))
	(height (coords-get-height coords-list))
	(width (coords-get-width coords-list)))
    (let ((top (border-top-char border-type))
	  (bottom (border-bottom-char border-type))
	  (left (border-left-char border-type))
	  (right (border-right-char border-type))
	  (tl (border-top-left-char border-type))
	  (tr (border-top-right-char border-type))
	  (bl (border-bottom-left-char border-type))
	  (br (border-bottom-right-char border-type)))
      
      (cond
       
       ;; If there is no border, do nothing
       ((eqv? 'border-none border-type)
	coords-list)
       
       ;; If this window has no size, do nothing
       ((coords-zero-area? coords-list)
	coords-list)
       
       ;; If this window has a height of 1, draw a horizontal line
       ((= 1 height)
	(attr-set! panel A_NORMAL color)
	(hline panel
	       top
	       width
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0))
       
       ;; If this window has a width of 1, draw a vertical line
       ((= 1 width)
	(attr-set! panel A_NORMAL color)
	(vline panel
	       left
	       height
	       #:y start-y
	       #:x start-x)
	(coords-new start-y start-x 0 0))
       
       ;; Otherwise, draw a normal border
       (else
	(attr-set! panel A_NORMAL
		   (color-indices-get-color-pair-index fg-color bg-color))
	(border panel
		left right top bottom
		tl tr bl br)
	(coords-adjust coords-list 1 1 -2 -2))))))
  
