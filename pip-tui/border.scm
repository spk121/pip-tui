(define-module (pip-tui border)
  #:use-module (ncurses curses)
  #:export (border-left-char
	    border-right-char
	    border-top-char
	    border-bottom-char
	    border-top-left-char
	    border-top-right-char
	    border-bottom-left-char
	    border-bottom-right-char))

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
