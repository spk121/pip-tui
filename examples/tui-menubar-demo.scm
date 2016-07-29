(use-modules (srfi srfi-1)
	     (pip-tui tui-menubar)
	     (pip-tui action)
	     (pip-tui action-map)
	     (pip-tui event)
	     (pip-tui tui-action)
	     (pip-tui tui-progress-bar)
	     (pip-tui pip-color-names)
	     (pip-tui pip-colors)
	     (ncurses curses)
	     (ncurses panel))

(setlocale LC_ALL "")
(define mainwin (initscr))
(start-color!)
(cbreak!)
(keypad! mainwin #t)
(nodelay! mainwin #t)
(noecho!)
(mousemask (logior ALL_MOUSE_EVENTS REPORT_MOUSE_POSITION))

;; Make a menubar widget

(define menubar1 (tui-menubar-new 1 0 3 80
				  #:horizontal-padding 1
				  #:border-type 'border-light
				  #:border-color COLOR_INDEX_BLACK
				  #:text-color COLOR_INDEX_PIPGREEN1
				  #:bg-color COLOR_INDEX_PIPGREEN5
				  #:horizontal-alignment 'center
				  #:key-label-alist '((#\x . "SLEEP")
						      (#\y . "SNORE")
						      (#\z . "BLAMMO!"))))

(define menubar2 (tui-menubar-new 8 10 3 20
				  
				  #:horizontal-padding 1
				  #:border-type 'border-light
				  #:border-color COLOR_INDEX_BLACK
				  #:text-color COLOR_INDEX_PIPGREEN1
				  #:bg-color COLOR_INDEX_PIPGREEN5
				  #:horizontal-alignment 'center
				  #:key-label-alist '((#\f . "FART")
						      (#\b . "BURP"))))

;; Make an action map
(define amap (action-map-new '()))
(action-map-add-action! amap (tui-menubar-action-handler) menubar1)
(action-map-add-action! amap (tui-menubar-action-handler) menubar2)

(define (finalize widget)
  (enqueue-symbolic-action 'main-loop-detach widget)
  (enqueue-symbolic-action 'main-loop-break widget))

(define LASTKEY #f)
(define LASTLABEL #f)

(define (menubar-keypress-action-activate mbar event state)

  (when (symbolic-event? event)
    (let ((data (event-get-data event)))
      (when (eq? (first data) 'menubar-keypress)
	(let ([key (second data)]
	      [label (third data)]
	      [source (fourth data)])
	  (when (eq? mbar source)
	    (finalize mbar)
	    ;; (simple-tone 0 0 (list 0.001 0.01 (+ 5000 (random 1)) 0.5 0.4))
	    (set! LASTKEY key)
	    (set! LASTLABEL label)
	    #f
	    ))))))

(action-map-add-action!
 amap
 (action-new "menubar-keypress" #t '() menubar-keypress-action-activate #f)
 menubar1)

(action-map-add-action!
 amap
 (action-new "menubar-keypress" #t '() menubar-keypress-action-activate #f)
 menubar2)


;; (action-map-add-action!
;;  amap
;;  (action-new "sound-terminal-glyph-new" #t '() sound-action-activate #f)
;;  TT)

(main-loop amap)

(endwin)

(newline)
(newline)
(newline)
(newline)
(display LASTKEY) (newline)
(display LASTLABEL) (newline)

