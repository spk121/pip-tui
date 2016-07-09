(define-module (pip-tui pulseaudio)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (%audio-buffer
	    %audio-write-cb
	    %pa-iterate))

(load-extension "piptui" "pip_pulseaudio_init")

;; (setlocale LC_ALL "")
;; (string-par-direction "hello")
;; (define x (apply string (map (lambda (c) (integer->char (+ c 32))) (iota 10000))))
;; (write x)
;; (newline)
;; (newline)

;; (write (string-logical->visual x 'rtl))
;; (newline)

