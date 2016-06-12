(use-modules (pip-tui tui-progress-bar)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)
(curs-set 0)

(define bar (tui-progress-bar-new 3 0 3 20
				  #:horizontal-padding 1
				  #:vertical-padding 1
				  #:fraction 0.5
				  #:text-location 'left
				  #:text "HAPPINESS"
				  #:text-color COLOR_INDEX_PIPLIGHTGREEN1
				  #:bg-color   COLOR_INDEX_PIPGREEN5
				  #:bar-fg-color COLOR_INDEX_PIPLIGHTGREEN1
				  #:bar-bg-color COLOR_INDEX_PIPGREEN4))

(define bar2 (tui-progress-bar-new 8 0 3 40
				  #:horizontal-padding 0
				  #:vertical-padding 0
				  #:fraction 0.5
				  #:text-location 'left
				  #:text "ANGER"
				  #:border-type 'border-block
				  #:text-color COLOR_INDEX_PIPLIGHTGREEN1
				  #:border-color COLOR_INDEX_BLACK
				  #:bg-color   COLOR_INDEX_PIPGREEN5
				  #:bar-fg-color COLOR_INDEX_PIPLIGHTGREEN1
				  #:bar-bg-color COLOR_INDEX_PIPGREEN4))

(do ((i 0.0 (+ 0.01 i)))
    ((> i 1.0))
  (move mainwin 0 0)
  (tui-progress-bar-set-fraction! bar i)
  (tui-progress-bar-set-fraction! bar2 (expt (sin (* 3.14 i)) 2))
  (update-panels)
  (doupdate)
  (usleep 30000))

(endwin)

