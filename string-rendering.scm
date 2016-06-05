(define-module (pip-tui string-rendering)
  #:use-module (ncurses extra)
  #:export (string-render))


;; The line separator characters
(define CR #\return)
(define NL #\newline)
(define NEL (integer->char #x85))
(define LS (integer->char #x2028))
(define PS (integer->char #x2029))
(define LINE_SEPARATORS (list CR NL NEL LS PS))

;; Directional Formatting Characters
(define LRE (integer->char #x202A))
(define RLE (integer->char #x202B))
(define LRO (integer->char #x202D))
(define RLO (integer->char #x202E))
(define PDF (integer->char #x202C))
(define LRI (integer->char #x2066))
(define RLI (integer->char #x2067))
(define FSI (integer->char #x2068))
(define PDI (integer->char #x2069))
(define LRM (integer->char #x200E))
(define RLM (integer->char #x200F))
(define ALM (integer->char #x061C))
(define DIRECTIONAL_FORMATTING_CHARACTERS
  (list LRE RLE LRO RLO PDF LRI RLI FSI PDI LRM RLM ALM))

(define (string-untabify in-string tabsize)
  (string-fold
   (lambda (c str)
     (if (char=? c #\tab)
	 (string-append str (make-string tabsize #\space))
	 (string-append str (string c))))
   ""
   in-string))

(define ISO_2047
  '(#\⎕					; NUL
    #\⌈					; SOH
    #\⊥					; STX
    #\⌋					; ETX
    #\⌁					; EOT
    #\⊠					; ENQ
    #\✓					; ACK
    #\⍾					; BEL
    #\⌫					; BS
    #\⪫					; TAB
    #\≡					; LF
    #\⩛					; VT
    #\↡					; FF
    #\⪪					; CR
    #\⊗					; SO
    #\⊙					; SI
    #\⊟					; DLE
    #\◴					; DC1
    #\◵					; DC2
    #\◶					; DC3
    #\◷					; DC4
    #\⍻					; NAK
    #\⊣					; ETB
    #\⧖					; CAN
    #\⍿					; EM
    #\␦					; SUB
    #\⊖					; ESC
    #\◰					; FS
    #\◱					; GS
    #\◲					; RS
    #\◳					; US
    #\△					; SPACE
    #\␥					; DEL
    ))

(define UNICODE_CONTROL_PICTURES
  '(#\␀					; NUL
    #\␁ 					; SOL
    #\␂					; STX
    #\␃					; ETX
    #\␄					; EOT
    #\␅					; ENQ
    #\␆					; ACK
    #\␇					; BEL
    #\␈					; BS
    #\␉					; HT
    #\␊					; LF
    #\␋					; VT
    #\␌					; FF
    #\␍					; CR
    #\␎					; SO
    #\␏					; SI
    #\␐					; DLE
    #\␑					; DC1
    #\␒					; DC2
    #\␓					; DC3
    #\␔					; DC4
    #\␕					; NAK
    #\␖					; SYN
    #\␗					; ETB
    #\␘					; CAN
    #\␙					; EM
    #\␚					; SUB
    #\␛					; ESC
    #\␜					; FS
    #\␝					; GS
    #\␞					; RS
    #\␟					; US
    #\␠					; Space
    #\␡					; DEL
    ))

(define CONTROL_PICTURES UNICODE_CONTROL_PICTURES)
;; (define CONTROL_PICTURES ISO_2047)

(define (char-replace-controls-and-pua c)
  (cond
   ;; Some special escapes are handled elsewhere in a rendering
   ;; pipeline.
   ((member c LINE_SEPARATORS)
    (member c DIRECTIONAL_FORMATTING_CHARACTERS)
    c)
   
   ;; All C0 Controls except return and newline are escaped with a
   ;; special glyph.  Note that this makes a glyph out of horizontal
   ;; tab as well.
   ((char<? c #\space)
    (list-ref CONTROL_PICTURES (char->integer c)))

   ;; Delete has its special glyph.
   ((char=? c #\delete)
    (list-ref CONTROL_PICTURES 33))
   
   ;; These get the generic replacement glyph.
   ((member (char-general-category c) (list 'Cc 'Cf 'Cs 'Co 'Cn))
    #\�)
   
   ;; Everthing else is not replaced
   (else
    c)))

(define (string-replace-controls-and-pua str)
  "Returns a new string where all the unassigned and control
characters are replaced with graphical replacement glyphs.  The only
controls that are not replaced are line separators and directional
formatting characters."
  (string-map char-replace-controls-and-pua str))

(define (string-list-pad-cells-right slist width)
  (map
   (lambda (entry)
     (let ((entry-width (wcwidth entry)))
       (if (< entry-width width)
	   (string-append entry (make-string (- width entry-width) #\space))
	   ;; else
	   entry)))
   slist))

(define (string-list-align-cells-right slist width)
  (map
   (lambda (entry)
     (let ((entry-width (wcwidth entry)))
       (if (< entry-width width)
	   (string-append (make-string (- width entry-width) #\space)
			  entry)
	   ;; else
	   entry)))
   slist))

(define (string-logical->visual str)
  str)

(define (string-render str tabsize n-cells alignment bidi)
  "Given a string STR, this converts the string into a list of strings
with each string contining N-CELLS cells or fewer.  Tabs are expanded
into TABSIZE spaces.  Any control characters are replaced with
replacement glyphs.  If alignment is 'left, lines are padded with
spaces at the end of each string to take up N-CELLS.  If ALIGNMENT is
'right, lines are padded with spaces on the left. If BIDI is true,
strings are converted from logical order to visual order."
  (define (do-bidi str)
    (if bidi
	(string-logical->visual str)
	str))
  (define (do-pad alignment str-list)
    (cond
     ((equal? alignment 'right)
      (string-list-align-cells-right str-list n-cells))
     ((equal? alignment 'left)
      (string-list-pad-cells-right str-list n-cells))
     (else
      str-list)))
  (define (do-wrap n-cells str)
    (string-split-at-line-breaks str n-cells))

  (do-pad alignment
	  (do-wrap n-cells
		   (do-bidi
		    (string-replace-controls-and-pua
		     (string-untabify str tabsize))))))
	 
    
