(use-modules (pip-tui tui-label)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)



(define label1 (tui-label-new 3 0 8 20
			      #:horizontal-padding 2
			      #:vertical-padding 1
			      #:color-pair-index (color-indices-get-color-pair-index COLOR_INDEX_BLUE COLOR_INDEX_BLACK)
			      #:border-type 'border-light
			      #:text "Later that day, he ate a carrot."))


(define label2 (tui-label-new 10 40 20 20
			      #:horizontal-padding 1
			      #:vertical-padding 1
			      #:color-pair-index (color-indices-get-color-pair-index COLOR_INDEX_GOLD COLOR_INDEX_PIPGREEN4)
			      #:attributes A_BOLD
			      #:border-type 'border-light
			      #:ellipsize #t
			      #:text "Radiation\n\nTwo centuries after the advent of nuclear war, radiation is still a very real danger in the Commonwealth.\n\nThe amount of rads you've accumulated is displayed in red in your HP bar."
			      ))

(update-panels)
(doupdate)
(getch mainwin)

(tui-label-resize label2 21 21)
(update-panels)
(doupdate)
(getch mainwin)

(tui-label-resize label2 22 22)
(update-panels)
(doupdate)
(getch mainwin)

(endwin)

