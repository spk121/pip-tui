(define-module (pip-tui menu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (tui-menu-new))

;;; menu

;;; A menu is an interactive widget allows a use to interact
;;; with a list-like collection (list, vector, etc) of
;;; string-like objects (strings, numbers).

;;; The entries are in a vertically-aligned single-column
;;; list.  Each entry occupies a single line.  If an entry
;;; is too wide, it is ellipsized.  Entries are green on black
;;; the currently selected entry is inverse.

;;; up/down arrows move the selection.  Enter selects it.

;;; There are two variants

;;; VARIANT 1: green and black, alphabetized, no border

;;; If there are more entries than would fit on the screen,
;;; scrolling past the bottom displayed entry will cause all
;;; the entries to scroll up one line with the new bottom
;;; entry being the selected one.

;;; There are triple-up-arrow and triple-down-arrow glyphs
;;; that replace the top-most or bottom-most line to indicate
;;; that there are more lines than could fit on the screen.
;;; I can't find them in Unicode but U+2BED and U+2BEF might do.
;;; U+2191 and U+2193 are probably the mostl likely to be
;;; in a font.
;;; ⯭⯯⯅⯆↑↓

;;; Because of the up/down triple arrow glyphs, the smallest
;;; allowed menu size is four rows.

;;; Variant 2: not alphabetized, green on darkest green, top/bottom border

;;; In this variant, the menu should have as many lines as
;;; entries.  There is no scrolling.  There is a top/bottom border.

;;; In this variant, elements can be disabled
