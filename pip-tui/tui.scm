

;;; tui-attr

;;; A tui-attr is the attributes of a set of printed text
;;; bold/normal/dim
;;; foreground color / background ocolor
;;; underline
;;; strikethrough

;;; tui-wrap-mode
;;; TUI_WRAP_WORK
;;; TUI_WRAP_CHAR
;;; TUI_WRAP_WORD_CHAR

;;; panel-label

;;; A panel-label is a subclass of a panel that has methods to print a
;;; string into the panel.

(define (tui-label-new y x width height
		       #:key (text "")
		       (attributes A_NORMAL)
		       (horizontal-alignment TUI_ALIGN_DEFAULT)
		       (vertical-alignment TUI_ALIGN_TOP)
		       (border TUI_BORDER_NONE)
		       )
  "Creates a new panel of a given size and position with the given
text inside it.  You can pass an empty string to make an empty
widget."
  )
  
(define (tui-label-set-border! tui-label tui-border)
    "Sets the border of the tui-label to NONE, LIGHT_DASHED,
HEAVY_DASHED, LIGHT, HEAVY.  If the border is not NONE, the
size available for text will be reduced by one cell on all
four sides of the tui-label."
    )

(define (tui-label-set-horizontal-padding! tui-label n-chars)
  "Sets the number of cells at the right or left of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  )

(define (tui-label-set-vertical-padding! tui-label n-chars)
  "Sets the number of cells at the right or left of the
widget that are blank padding.  If there is a border, this padding
is inside of the border."
  )

(define (tui-label-set-text! tui-label str)
  "Sets the text within the tui-label widget.  It overwrites any
text that was there before."
  )

(define (tui-label-set-attributes! tui-label tui-attr)
  "Sets the attributes of a tui-label.  If the attributes are
different than the previous attributes, the tui-label is
refreshed."
  )

(define (tui-label-set-horizontal-alignment! tui-label tui-justification)
  "Sets the alignment of the lines in the text of the label relative
to each other. TUI_ALIGN_LEFT, TUI_ALIGN_RIGHT, TUI_ALIGN_CENTER,
TUI_JUSTIFY_DEFAULT."
  )

(define (tui-label-set-ellipsize! tui-label tui-ellipsize-mode)
  "Describes what type of ellipsization should be applied to a line
of text.  TUI_ELLIPSIZE_NONE, TUI_ELLIPSIZE_END"
  )

(define (tui-label-set-width-chars! tui-label n-chars)
  "For multi-line labels that wrap text, this indicates the
desired number of characters in a line before wrapping.  If not
set, it is the same as the width of the panel."
  )

(define (tui-label-set-line-wrap! tui-label wrap)
  "Toggles line wrapping in the tui-label widget.  #t make it break
if text exceedes the widget's size.  #f lets the text get cut off
by the edge of the widget if it exceeds the widget size.

  If a desired wrap width was set with tui-label-set-width-chars!,
it will try to wrap so that no characters written to the right of that
width."
  )

(define (tui-label-set-line-wrap-mode! tui-label tui-wrap-mode)
  "Sets the wrapping mode for the tui-label to none, character boundary,
or word boundary."
  )

(define (tui-label-get-text tui-label)
  "Fetches the text from a label as a simple string."
  )

(define (tui-label-get-border tui-label)
  ""
  )

(define (tui-label-get-horizontal-padding tui-label)
  ""
  )

(define (tui-label-get-vertical-padding tui-label)
  ""
  )

(define (tui-label-get-attributes tui-label)
  "Gets the attributes of the label as a tui-attr type."
  )

(define (tui-label-get-justify tui-label)
  "Gets the justification of a label as a tui-justification type."
  )

(define (tui-label-get-ellipsize tui-label)
  "Gets the ellipsizing position of the label."
  )

(define (tui-label-get-width-chars tui-label)
  "Gets the desired wrap width of the tui-label."
  )

(define (tui-label-get-line-wrap tui-label)
  "Returns #t if the tui-label has line wrapping enabled."
  )

(define (tui-label-get-line-wrap-mode tui-label)
  "Returns tui-wrap-mode of the tui-label."
  )


