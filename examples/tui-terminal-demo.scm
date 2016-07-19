(use-modules
 (rnrs bytevectors)
 (ncurses curses)
 (ncurses panel)
 (pip-tui time)
 (pip-tui pip-color-names)
 (pip-tui pip-colors)
 (pip-tui string-lib)
 (pip-tui tui-terminal)
 (pip-tui action)
 (pip-tui action-map)
 (pip-tui event)
 (pip-tui tui-action)
 (pip-tui pulseaudio)
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
  (enqueue-symbolic-action 'main-loop-break term))

(define TT (tui-terminal-new 4 1 25 45 ceefax-text-1
			     #:completion-cb finalize))

;; Make an action map
(define amap (action-map-new '()))
(action-map-add-action! amap tui-terminal-action-handler TT)
(action-map-add-action! amap pulseaudio-idle-handler #f)

(define (sound-action-activate TT event state)
  (when (symbolic-event? event)
      (let ((c (event-get-data event)))
	(when (or (eqv? (car c) 'sound-terminal-glyph-new)
		  (eqv? (car c) 'sound-terminal-drawing-end))
              (simple-tone 0 0 (list 0.001 0.01 (+ 5000 (random 1)) 0.5 0.4))
              #f
              ))))

(action-map-add-action!
 amap
 (action-new "sound-terminal-glyph-new" #t '() sound-action-activate #f)
 TT)

(main-loop amap)

(endwin)

(newline)
(newline)
(newline)
(newline)
(format #t "+++++++++++++++++++++++++++++++++++++++++++++++~%")
(format #t "USER CHOSE ~S~%" (tui-terminal-hotspot-cur TT))
(newline)


