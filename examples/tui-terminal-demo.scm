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
  ;; (tui-terminal-hide term)
  (enqueue-symbolic-action 'main-loop-break term))

(define TT (tui-terminal-new 4 1 25 45 ceefax-text-1
			     #:completion-cb finalize))


;; Make an action map
(define amap (action-map-new '()))
(action-map-add-action!
 amap
 (action-new "tui-terminal-mouse" #t '() tui-terminal-mouse-action-activate #t)
 TT)

(action-map-add-action!
 amap
 (action-new "tui-terminal-kbd" #t '() tui-terminal-kbd-action-activate #f)
 TT)

(action-map-add-action!
 amap
 (action-new "tui-terminal-idle" #t '() tui-terminal-idle-action-activate #f)
 TT)

(define (audio-idle-action-activate TT event state)
  (when (idle-event? event)
        (audio-iterate)))

(action-map-add-action!
 amap
 (action-new "audio-idle" #t '() audio-idle-action-activate #f)
 #f)

(define (sound-action-activate TT event state)
  (when (symbolic-event? event)
      (let ((c (event-get-data event)))
	(when (or (eqv? (car c) 'sound-terminal-glyph-new)
		  (eqv? (car c) 'sound-terminal-drawing-end))
              (simple-tone 0 0 (list 0.001 0.01 (+ 5000 (random 1)) 0.5 0.4))
              ;; (bytevector-s16-set! (vector-ref %audio-buffers 0) 0 8000 (native-endianness))
              ;; (bytevector-s16-set! (vector-ref %audio-buffers 0) 1 -8000 (native-endianness))
              ;; (bytevector-s16-set! (vector-ref %audio-buffers 0) 0 8000 (native-endianness))
              ;; (bytevector-s16-set! (vector-ref %audio-buffers 0) 3 -8000 (native-endianness))
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


