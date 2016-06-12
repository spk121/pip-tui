(use-modules (pip-tui tui-label)
	     (pip-tui tui-progress-bar)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)

(define label1 (tui-label-new 21 0 3 20
			      #:horizontal-padding 1
			      #:border-type 'border-block
			      #:border-color COLOR_INDEX_BLACK
			      #:text-color COLOR_INDEX_PIPGREEN1
			      #:bg-color COLOR_INDEX_PIPGREEN5
			      #:horizontal-alignment 'left
			      #:text "HP 345/368"))
(define bar (tui-progress-bar-new 21 20 3 40
				  #:fraction 0.2
				  #:text-location 'left
				  #:text "LEVEL 48"
				  #:border-type 'border-block
				  #:text-color COLOR_INDEX_PIPGREEN1
				  #:border-color COLOR_INDEX_BLACK
				  #:bg-color COLOR_INDEX_PIPGREEN5
				  #:bar-fg-color COLOR_INDEX_PIPGREEN1
				  #:bar-bg-color COLOR_INDEX_PIPGREEN4))

(define label2 (tui-label-new 21 60 3 20
			      #:horizontal-padding 1
			      #:border-color COLOR_INDEX_BLACK
			      #:text-color COLOR_INDEX_PIPGREEN1
			      #:bg-color COLOR_INDEX_PIPGREEN5
			      #:border-type 'border-block
			      #:horizontal-alignment 'right
			      #:text "AP 110/110"
			      ))

(define label3 (tui-label-new 15 40 4 40
			      #:text-color COLOR_INDEX_PIPGREEN2
			      #:bg-color COLOR_INDEX_BLACK
			      #:text "Strength is a measurement of your raw physical power. It affects how much you can carry, and the damage of all melee attacks."))

(curs-set 0)
(update-panels)
(doupdate)
(getch mainwin)

(endwin)

