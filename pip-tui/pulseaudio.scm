(define-module (pip-tui pulseaudio)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (%audio-buffer
	    %audio-write-cb
	    %pa-iterate))

(load-extension "piptui" "pip_pulseaudio_init")

