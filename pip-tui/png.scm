(define-module (pip-tui png)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (read-png))

(define (read-png filename)
  "Opens a png named filename and returns
a list of 3 elements: the width of the imag in pixels,
the height of the image in pixels, and a bytevector
containing the contents of the image as RGBA32."
  (%read-png filename))

(load-extension "/home/mike/Projects/pip-tui/piptui.so" "png_init")
