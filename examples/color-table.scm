(use-modules (ncurses curses)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ice-9 format)
	     (srfi srfi-1))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)

;; Show the normal 256 color table

(addstr mainwin "DEFAULT COLOR TABLE" #:y 0 #:x 0)
(addstr mainwin
	(format #f "Colors: ~s  Color Pairs: ~s" (colors) (color-pairs))
	#:y 1 #:x 0)
(move mainwin 2 0)
(do ((i 1 (1+ i)))
    ((>= i (color-pairs)))
  (init-pair! i 0 i)
  (attr-set! mainwin A_NORMAL i)
  (let ((cc (color-content i)))
    (addstr mainwin (format #f "~s   " i))))
(refresh mainwin)
(getch mainwin)

;; Adjust table for maximum green

(define COLOR_BLACK 16)
(define COLOR_GREEN1 22)
(define COLOR_GREEN2 28)
(define COLOR_GREEN3 34)
(define COLOR_GREEN4 40)
(define COLOR_GREEN5 46)

(define COLORS_GREEN
  (list COLOR_INDEX_BLACK
	COLOR_INDEX_PIPGREEN5
	COLOR_INDEX_PIPGREEN4
	COLOR_INDEX_PIPGREEN3
	COLOR_INDEX_PIPGREEN2
	COLOR_INDEX_PIPGREEN1
	COLOR_INDEX_PIPLIGHTGREEN4
	COLOR_INDEX_PIPLIGHTGREEN3
	COLOR_INDEX_PIPLIGHTGREEN2
	COLOR_INDEX_PIPLIGHTGREEN1
	COLOR_INDEX_WHITE
	))

(erase mainwin)
(init-pair! 1 COLOR_INDEX_PIPLIGHTGREEN2 COLOR_INDEX_PIPGREEN5)
(attr-set! mainwin A_BOLD 1)
(addstr mainwin "    SO MUCH GREEN    " #:y 0 #:x 0)
(move mainwin 2 0)

(do ((i 0 (1+ i)))
    ((> i 10))
  (let ((idx (list-ref COLORS_GREEN i)))
    (if (< i 3)
	(init-pair! (1+ i) COLOR_INDEX_PIPLIGHTGREEN4 idx)
	(init-pair! (1+ i) COLOR_INDEX_BLACK idx))
    (attr-set! mainwin A_NORMAL (1+ i))
    (addstr mainwin (format #f "~3,d ~16,a ~6,'0x" idx
			    (car (list-ref COLOR_NAMES idx))
			    (car (assv-ref *xterm-colors* idx)))
	    #:y (+ i 2)
	    #:x 0))
  )

(refresh mainwin)
(getch mainwin)

(endwin)


