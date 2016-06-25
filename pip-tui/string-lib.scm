(define-module (pip-tui string-lib)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (pip-tui typecheck)
  #:use-module (pip-tui unistring)
  #:use-module (pip-tui fribidi)
  #:export (string-convert-crlf-to-lf
	    string-remove-trailing-whitespace
	    string-pad-beginning-to-width
	    string-pad-end-to-width
	    string-pad-beginning-and-end-to-width
	    string-untabify
	    string-render
	    substring-width
	    string-trim-right-to-width
	    substring-list
	    string-list-length
	    string-find-brace-pairs
	    string-list-find-brace-pairs
	    string-list-truncate!)
  #:re-export (string-width))

(define-syntax append-val!
  (syntax-rules()
    ((append-val! lst entry)
     (set! lst (append lst (list entry))))))


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
    #\␁					; SOL
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

(define* (char-replace-controls-and-pua c #:optional (iso #f))
  "Returns a new string with most escape characters except for line separators
and directional formatting characters replaced with replacement characters.
If the optional parameter ISO is true, C0 control characters are replaced
with ISO_2047 glyphs.  Otherwise C0 controls are replace with control pictures."
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
    (if (not iso)
	(list-ref UNICODE_CONTROL_PICTURES (char->integer c))
	;; else
	(list-ref ISO_2047 (char->integer c))))

   ;; Delete has its special glyph.
   ((char=? c #\delete)
    (if (not iso)
	(list-ref UNICODE_CONTROL_PICTURES 33)
	;; else
	(list-ref ISO_2047 33)))
   
   ;; These get the generic replacement glyph.
   ((member (char-general-category c) (list 'Cc 'Cf 'Cs 'Co 'Cn))
    #\�)
   
   ;; Everthing else is not replaced
   (else
    c)))

(define* (string-replace-controls-and-pua str #:optional (iso #f))
  "Returns a new string where all the unassigned and control characters
are replaced with graphical replacement glyphs.  The only controls that
are not replaced are line separators and directional formatting
characters.  If the optional parameter ISO is true, C0 controls are
replace with ISO-2047 glyphs; otherwise, they are replaced with control
pictures."
  (string-map (lambda (c)
		(char-replace-controls-and-pua c iso))
	      str))


(define (string-convert-crlf-to-lf str)
  "Returns a copy of STR in which carrige return / linefeed pairs are
replace with a single linefeed."
  (let loop ((idx (1- (string-length str)))
	     (out-string ""))
    (cond
     ((and (>= idx 1)
	   (char=? (string-ref str idx) #\newline)
	   (char=? (string-ref str (1- idx)) #\return))
      (loop (- idx 2) (string-append "\n" out-string)))
     ((>= idx 0)
      (loop (1- idx) (string-append (string (string-ref str idx)) out-string)))
     (else
      out-string))))

(define* (substring-width str start #:optional end)
  "Compute the number of cells occupied by a substring of STR"
  (if end
      (string-width (substring str start end))
      (string-width (substring str start))))

(define (string-remove-trailing-whitespace str)
  "Returns a copy of STR, not including any whitespace at the end of
the string."
  (let ((idx (1- (string-length str))))
    (while (>= idx 0)
      (cond
       ((eqv? (char-general-category (string-ref str idx)) 'Zs)
	(set! idx (1- idx)))
       (else
	(break))))
    (substring/copy str 0 (1+ idx))))

(define (string-pad-end-to-width str width)
  "Returns a new copy of STR with spaces added to the end the string
to so that it occupies WIDTH cells. If STR is wider than WIDTH, the
STR is returned."
  (let ((len (string-width str)))
    (if (< len width)
	(string-append str (make-string (- width len) #\space))
	;; else
	str)))

(define (string-pad-beginning-to-width str width)
  "Returns a new copy of STR with spaces added to the beginning the
string to so that it occupies WIDTH cells. If STR is wider than WIDTH,
the STR is returned."
  (let ((len (string-width str)))
    ;; (format #t "in string-pad len ~s width ~s str:~s~%" len width str)
    (if (< len width)
	(string-append (make-string (- width len) #\space) str)
	;; else
	str)))

(define (string-pad-beginning-and-end-to-width str width)
  "Add spaces to the beginning and end of the string to so that it
occupies WIDTH cells. If STR is wider than WIDTH, the STR is
returned."
  (let* ((len (string-width str)))
    (if (< len width)
	(let* ((total-pad (- width len))
	       (beginning-pad (quotient total-pad 2))
	       (end-pad (- total-pad beginning-pad)))
	  (string-append
	   (make-string beginning-pad #\space)
	   str
	   (make-string end-pad #\space)))
	;; else
	str)))

(define (string-untabify str tabsize)
  "Returns a new string in which horizontal tab characters are replaced
with TABSIZE spaces, and where the many Unicode whitespace characters
are replaced with zero, one, or two spaces."

  ;; FIXME: Zero-width spaces should not be handled this early in
  ;; string-render.  The probably need to be after text wrapping.
  (string-fold-right
   (lambda (char-cur out-string)
     (cond
      ((char=? char-cur #\tab)
       (string-append (make-string tabsize #\space) out-string))
      ((eqv? (char-general-category char-cur) 'Zs)
       (string-append (make-string (string-width (string char-cur)) #\space) out-string))
      (else
       (string-append (string char-cur) out-string))))
   ""
   str))

;; (define (string-untabify in-string tabsize)
;;   (string-fold
;;    (lambda (c str)
;;      (if (char=? c #\tab)
;; 	 (string-append str (make-string tabsize #\space))
;; 	 (string-append str (string c))))
;;    ""
;;    in-string))

(define (string-trim-right-to-width str width)
  "Return the longest substring of str that fits in WIDTH
cells."
  (let ((i (string-length str)))
    (while (> (string-length (substring str 0 i)) width)
      (set! i (1- i)))
    (substring str 0 i)))

(define (string-render str tabsize n-cells alignment bidi)
  "Given a string STR, this converts the string into a list of strings
with each string contining N-CELLS cells or fewer.  Tabs are expanded
into TABSIZE spaces.  Other Unicode spaces are replace with the common
space character.  Any control characters are replaced with
replacement glyphs.  If alignment is 'left, lines are padded with
spaces at the end of each string to take up N-CELLS.  If ALIGNMENT is
'right, lines are padded with spaces on the left. ALIGNMENT can also be 'center.
If ALIGNMENT is none of the above, no padding occurs. If BIDI is true,
strings are converted from logical order to visual order."
  (define (do-bidi str)
    (if bidi
	(string-logical->visual str (string-par-direction str))
	str))

  (define (do-pad str)
    (cond
     ((equal? alignment 'right)
      (string-pad-beginning-to-width str n-cells))
     ((equal? alignment 'left)
      (string-pad-end-to-width str n-cells))
     ((equal? alignment 'center)
      (string-pad-beginning-and-end-to-width str n-cells))
     (else
      str)))

  (define (do-wrap n-cells str)
    (string-split-at-line-breaks str n-cells))

  (define (do-untabify str)
    (string-untabify str tabsize))
  
  (define (do-split str)
    (string-split-at-line-breaks str n-cells))

  (let ((string-list
	 (do-split
	  (do-bidi
	   (string-replace-controls-and-pua
	    (do-untabify
	     (string-convert-crlf-to-lf
	      (string-normalize-nfc str))))))))
    (map
     (lambda (str)
       (do-pad
	(string-remove-trailing-whitespace str)))
     string-list)))
    

(define (string-find-brace-pairs str left-brace right-brace)
  "Given a string STR and two characters LEFT-BRACE and RIGHT-BRACE,
this returns the a list of pairs of string indices of matching
opening and closing braces. It does not handle nesting of braces."
  (let ([i 0]
	[len (string-length str)]
	[left-brace-found #f]
	[brace-pairs '()])
    (do ([i 0 (1+ i)]) ([= i len])
      (let ([char-cur (string-ref str i)])
	(cond
	 [(and (not left-brace-found)
	       (char=? left-brace char-cur))
	  (set! left-brace-found i)]
	 [(and left-brace-found (char=? right-brace char-cur))
	  (append-val! brace-pairs (list left-brace-found i))
	  (set! left-brace-found #f)]
	 [else
	  ;; Do nothing
	  #f])))
    brace-pairs))

(define* (string-list-truncate-entry! string-list line pos-start #:optional pos-end)
  (list-set! string-list line
	     (if pos-end
		 (substring (list-ref string-list line) pos-start pos-end)
		 (substring (list-ref string-list line) pos-start))))

(define (substring-list str-list line-start pos-start line-end pos-end)
  (if (= line-start line-end)
      ;; Single-line substring
      (let ((str (list-ref str-list line-start)))
	(list (substring str pos-start pos-end)))
      ;; Multiple-line substring
      (let ([sublist (list-head (list-tail str-list line-start) (1+ (- line-end line-start)))])
	(string-list-truncate-entry! sublist 0 pos-start)
	(string-list-truncate-entry! sublist (1- (length sublist)) 0 pos-end)
	sublist)))

(define (string-list-length strlist)
  "Returns the number of characters in a string list."
  (apply + (map string-length strlist)))
  
(define (string-list-find-brace-pairs str-list left-brace right-brace)
  "Given a list of strings STR-LIST and two characters LEFT-BRACE and RIGHT-BRACE,
this returns the a list of pairs of string indices of matching opening
and closing braces. Each entry of the output list has the form
ROW-START, COL-START, ROW-END, COL-END. It does not handle nesting of braces."
  (let ([j 0] [i 0]
	[maxj (length str-list)]
	[left-brace-found #f]
	[brace-pairs '()])
    (do ([j 0 (1+ j)]) ([= j maxj])
      (let* ([str (list-ref str-list j)]
	     [len (string-length str)])
	(do ([i 0 (1+ i)]) ([= i len])
	  (let ([char-cur (string-ref str i)])
	    (cond
	     [(and (not left-brace-found) (char=? left-brace char-cur))
	      (set! left-brace-found (list j i))]
	     
	     [(and left-brace-found (char=? right-brace char-cur))
	      (append-val! brace-pairs (append left-brace-found (list j i)))
	      (set! left-brace-found #f)]
	     [else
	      ;; Do nothing
	      #f])))))
    brace-pairs))

(define (string-list-truncate! strlist N)
  "Returns a list of strings with no more than N codepoints.  The original
list may be modified in the process."
  (cond
   [(null? strlist)
    strlist]
   [(= N 0)
    (take! strlist 0)]
   [else
    (let loop ([i 0] [n 0])
      (let* ([str (list-ref strlist i)]
	     [len (string-length str)])
	(cond
	 [(<= N (+ n len))
	  (list-set! strlist i (substring str 0 (- N n)))
	  (take! strlist (1+ i))]
	 [(= (1+ i) (length strlist))
	  strlist]
	 [else
	  (loop (1+ i) (+ n len))])))]))

;; (setlocale LC_ALL "")
;; (string-pad-beginning-to-width "hello" 40)
;; (string-untabify "hello\u4201\tworld" 8)
;; (define lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
;; (define hello "HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO")
;; (define (make-random-text)
;;   (let ((str (make-string 400)))
;;     (do ((i 0 (1+ i)))
;; 	((>= i 400))
;;       (if (= 0 (random 10))
;; 	  (string-set! str i
;; 		       (let loop ((c (integer->char (random 5000))))
;; 			 (if (eqv? (char-general-category c) 'Zs)
;; 			     c
;; 			     (loop (integer->char (random 5000))))))
;; 	  ;; else
;; 	  (string-set! str i
;; 		       (let loop ((c (integer->char (random 30000))))
;; 			 (if (member (char-general-category c) '(Lu Ll Lt Lm Lo Nd Nl No))
;; 			     c
;; 			     (loop (integer->char (random 30000))))))))
;;     str))
;; (define random-text (make-random-text))
;; (define codepoint (apply string (map integer->char (iota 10000))))
;; (define jp-text "或日の暮方の事である。一人の下人が、羅生門の下で雨やみを待っていた。　広い門
;; の下には、この男の外に誰もいない。ただ、所々丹塗の剥げた、大きな円柱に、きりぎ
;; りすが一匹とまっている。羅生門が、朱雀大路にある以上は、この男の外にも、雨やみ
;; をする市女笠や揉烏帽子が、もう二三人はありそうなものである。それが、この男の外
;; に誰もいない。")
;; (load "../../test/sample-text.scm")
;; (write random-text)

;; (for-each
;;  (lambda (x)
;;    (write x) (newline))
;;  (string-render random-text 8 21 'left #t))


