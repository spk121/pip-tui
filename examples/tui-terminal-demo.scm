(use-modules
 (ncurses curses)
 (ncurses panel)
 (pip-tui pip-color-names)
 (pip-tui pip-colors)
 (pip-tui string-lib)
 (pip-tui tui-terminal)
 (pip-tui tui-action)
 )

(include "sample-text.scm")

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)
(cbreak!)
(keypad! mainwin #t)
(nodelay! mainwin #t)
(noecho!)
(mousemask (logior ALL_MOUSE_EVENTS REPORT_MOUSE_POSITION))


;; Make a terminal widget
(define (finalize term)
  (enqueue-symbolic-action 'main-loop-detach term)
  ;; (tui-terminal-hide term)
  (enqueue-symbolic-action 'main-loop-break term))

(define TT (tui-terminal-new 4 1 25 45 ceefax-text-1
			     #:completion-cb finalize))


;; Make an action map
(define amap (action-map-new '()))
(add-action-entry amap
		  (action-entry-new
		   (action-new "tui-terminal-mouse" #t #f tui-terminal-mouse-action-activate #f)
 		   TT default-mouse-event-handler))
(add-action-entry amap
		  (action-entry-new
		   (action-new "tui-terminal-kbd" #t #f tui-terminal-kbd-action-activate #f)
		   TT default-kbd-event-handler))
(add-action-entry amap
		  (action-entry-new
		   (action-new "tui-terminal-tick" #t #f tui-terminal-tick-action-activate #f)
		   TT default-tick-event-handler))

(main-loop amap)

(endwin)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(newline)
(format #t "+++++++++++++++++++++++++++++++++++++++++++++++~%")
(format #t "USER CHOSE ~S~%" (tui-terminal-hotspot-cur TT))
(newline)


