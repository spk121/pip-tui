;; HOTSPOT

;; A hotspot is highlighted text in a text buffer.  If it occupies
;; more than one line, it wraps around in a text-like fashion.

(define-module (pip-tui hotspot)
  ;; #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  ;; #:use-module (ncurses curses)
  ;; #:use-module (pip-tui typecheck)
  ;; #:use-module (pip-tui pip-color-names)
  ;; #:use-module (pip-tui coords)
  #:use-module (pip-tui string-lib)
  ;; #:use-module (pip-tui data-lib)
  ;; #:use-module (pip-tui time)
  ;; #:use-module (pip-tui render-lib)
  ;; #:use-module (pip-tui tui-action)
  #:export (
            hotspot?
            hotspot-new
            hotspot-get-start-y
            hotspot-get-start-x
            hotspot-get-end-y
            hotspot-get-end-x
            hotspots-from-string-list
            in-hotspot?
            ))

(define-record-type <hotspot>
  (hotspot-new start-y                  ; line #
               start-x                  ; cell #
               start-i                  ; codepoint #
               end-y
               end-x
               end-i
               text)
  hotspot?
  (start-y hotspot-get-start-y hotspot-set-start-y!)
  (start-x hotspot-get-start-x hotspot-set-start-x!)
  (start-i hotspot-get-start-i hotspot-set-start-i!)
  (end-y hotspot-get-end-y hotspot-set-end-y!)
  (end-x hotspot-get-end-x hotspot-set-end-x!)
  (end-i hotspot-get-end-i hotspot-set-end-i!)
  (text hotspot-get-text hotspot-set-text!))

(define (in-hotspot? hotspot y x)
  "Checks to see if the position y,x is clickable location in a
hotspot."
  (let ((start-x (hotspot-get-start-x hotspot))
        (start-y (hotspot-get-start-y hotspot))
        (end-x (hotspot-get-end-x hotspot))
        (end-y (hotspot-get-end-y hotspot)))
    (or (and (= y start-y)
             (= y end-y)
             (>= x start-x)
             (<= x end-x))
        (and (not (= start-y end-y))
             (>= y start-y)
             (<= y end-y)
             (or (and (= y start-y) (>= x start-x))
                 (and (= y end-y) (<= x end-x))
                 (and (not (= y start-y)) (not (= y end-y))))))))

(define (hotspot-new-from-brace-pairs str-list brace-pairs-list)
  "Given a list of strings and a 4-element list of the form START-LINE
START-INDEX END-LINE END-INDEX, this creates a new <hotspot>."
  (let ([y1 (first brace-pairs-list)]
        [i1 (second brace-pairs-list)]
        [y2 (third brace-pairs-list)]
        [i2 (fourth brace-pairs-list)])
    (let ([x1 (substring-width (list-ref str-list y1) 0 i1)]
          [x2 (substring-width (list-ref str-list y2) 0 (1+ i2))])
      (hotspot-new y1 x1 i1 y2 x2 i2
                   (append (substring-list str-list y1 i1 y2 i2))))))

(define (hotspots-from-string-list strlist)
  "Returns a list of hotspots found in a list of strings.  Hotspots
are denoted in strings as text enclosed in bracket pairs."
  (map
   (lambda (brace-pairs-list)
     (hotspot-new-from-brace-pairs strlist brace-pairs-list))
   (string-list-find-brace-pairs strlist #\[ #\])))
