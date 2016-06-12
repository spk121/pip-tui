(use-modules (pip-tui tui-label)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)



(define label1 (tui-label-new 0 0 5 20
			      #:horizontal-padding 2
			      #:vertical-padding 1
			      #:color-pair-index (color-indices-get-color-pair-index COLOR_INDEX_BLUE COLOR_INDEX_BLACK)
			      #:border-type 'border-light
			      #:horizontal-alignment 'center
			      #:text "Later that day, he ate a carrot."))


(define label2 (tui-label-new 10 1 20 20
			      #:horizontal-padding 1
			      #:vertical-padding 1
			      #:color-pair-index (color-indices-get-color-pair-index COLOR_INDEX_PIPLIGHTGREEN3
										     COLOR_INDEX_BLACK)
			      #:attributes A_NORMAL
			      #:border-type 'border-block
			      #:ellipsize #t
			      #:vertical-alignment 'center
			      #:text "Radiation\n\nTwo centuries after the advent of nuclear war, radiation is still a very real danger in the Commonwealth.\n\nThe amount of rads you've accumulated is displayed in red in your HP bar."
			      ))
(curs-set 0)

(do ((i 0 (1+ i)))
    ((> i 72))
  (clear mainwin)
  (tui-label-resize label2 (1+ (quotient i 4)) i)
  (tui-label-resize label1 5 i)
  (update-panels)
  (doupdate)
  (usleep 500000))

(endwin)

