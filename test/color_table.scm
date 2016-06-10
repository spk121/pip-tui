(use-modules (ncurses curses)
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
  (list COLOR_BLACK COLOR_GREEN1 COLOR_GREEN2 COLOR_GREEN3 COLOR_GREEN4 COLOR_GREEN5))

(erase mainwin)
(addstr mainwin "GREENER COLOR PAIRS" #:y 0 #:x 0)
(addstr mainwin
	(format #f "Colors: ~s  Color Pairs: ~s" (colors) (color-pairs))
	#:y 1 #:x 0)
(move mainwin 2 0)
(define n 1)
(do ((j 0 (1+ j)))
    ((>= j 6))
  (do ((i 0 (1+ i)))
      ((>= i 6))
    (unless (= i j)
      (move mainwin (+ 2 j) (* i 8))
      (init-pair! n
		  (list-ref COLORS_GREEN i)
		  (list-ref COLORS_GREEN j))
      (attr-set! mainwin A_NORMAL n)
      (addstr mainwin (format #f "XX ~s XX" n))
      (set! n (1+ n)))))
(refresh mainwin)
(getch mainwin)

(endwin)


