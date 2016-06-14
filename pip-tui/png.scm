(define-module (pip-tui png)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (read-png))

(define (read-png filename)
  "Opens a png named filename and returns
an array of the contents of the image as RGBA32."
  (%read-png filename))

(load-extension "piptui.so" "pip_png_init")
