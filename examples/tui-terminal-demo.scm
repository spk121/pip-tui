(use-modules (pip-tui tui-terminal)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (pip-tui string-lib)
	     (ncurses curses)
	     (ncurses panel))

(include "sample-text.scm")

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)
(cbreak!)
(keypad! mainwin #t)
(nodelay! mainwin #t)
(noecho!)
(mousemask (logior ALL_MOUSE_EVENTS REPORT_MOUSE_POSITION))



(define label3 (tui-terminal-new 15 40 40 40 ceefax-text-1))
(%set-hotspot-cur! label3 1)
(curs-set 0)
(update-panels)
(doupdate)
(define ret #f)
(while #t
  (let* ((c (getch mainwin))
	 (m (if (eqv? c KEY_MOUSE) (getmouse) #f)))
    (if c
	(begin
	  (move mainwin 0 0)
	  (addstr mainwin (format #f "~s ~s" (keyname c) m))
	  (refresh mainwin)
	  (set! ret (tui-terminal-process-event label3 c m))
	  (if (number? ret) (break)))
	;; else
	(usleep TERMINAL_MICROSECONDS_PER_TICK))
    (tui-terminal-tick label3)
    (update-panels)
    (doupdate)))

(endwin)
(display "user chose #")
(display ret)
(newline)


