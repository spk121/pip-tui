(use-modules (pip-tui tui-label)
	     (pip-tui tui-progress-bar)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (pip-tui render-lib)
	     (pip-tui coords)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)

(define label3 (tui-label-new 15 40 4 40
			      #:text-color COLOR_INDEX_PIPGREEN2
			      #:bg-color COLOR_INDEX_BLACK
			      #:text "Strength is a measurement of your raw physical power. It affects how much you can carry, and the damage of all melee attacks."))
(render-highlight (%tui-label-get-panel label3) 0 10 3 38
		  #:coords-list (window-relative-coords (%tui-label-get-panel label3)))
(curs-set 0)
(update-panels)
(doupdate)
(getch mainwin)

(endwin)

