(use-modules (pip-tui tui-terminal)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(include "sample-text.scm")
(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)


(define label3 (tui-terminal-new 15 40 40 40 japanese-string))

(curs-set 0)
(update-panels)
(doupdate)
(while (usleep 100000)
  (tui-terminal-tick label3)
  (update-panels)
  (doupdate))


(endwin)

