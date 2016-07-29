(define-module (pip-tui listmenu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (list-menu-new))

;;; list menu

;;; A list menu is an interactive widget allows a use to interact with
;;; a list-like collection (list, vector, etc) of string-like objects
;;; (strings, numbers).

;;; The entries are in a vertically-aligned single-column list.  Each
;;; entry occupies a single line.  If an entry is too wide, it is
;;; ellipsized.  Entries are green on black the currently selected
;;; entry is inverse.

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
;;; U+2191 and U+2193 are probably the most likely to be
;;; in a font.
;;; ⯭⯯⯅⯆↑↓
;;; But this isn't supported by the underlying menu library.

;;; Because of the up/down triple arrow glyphs, the smallest allowed
;;; menu size is four rows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variant 2: not alphabetized, green on darkest green, top/bottom
;;; border

;;; In this variant, the menu should have as many lines as entries.
;;; There is no scrolling.  There is a top/bottom border.

;;; In this variant, elements can be disabled


(define-record-type <listmenu>
  (%listmenu-new panel
		 entry-list
		 menu)
  listmenu?
  (panel %listmenu-get-panel %listmenu-set-panel!)
  (entry-list %listmenu-get-entry-list %listmenu-set-entry-list!)
  (menu %listmenu-get-menu %listmenu-set-menu!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Properties

;;; panel           #<panel>
;;; entry-list      a listlike collection of string-like elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API


(define (listmenu-new y x width height elements)
  "Creates a new panel of a given size and position that encloses a
listmenu.  HEIGHT must be at least 3.  There must be at least one
element in the ELEMENTS list."
  (let* ((pan (newwin width height y x #:panel #t))
	 (subpan (derwin pan (- width 2) (- height 2) 1 1))
	 (self (%listmenu-new
		pan
		elements
		(new-menu (element-list->mitem-list elements))))
	 (M (%listmenu-get-menu self)))
    (set-menu-fore! M (logior A_BOLD
			      (color-indices-get-color-pair-index COLOR_INDEX_BLACK COLOR_INDEX_GREEN)))
    (set-menu-back! M 
		    (color-indices-get-color-pair-index COLOR_INDEX_GREEN COLOR_INDEX_BLACK))
    (set-menu-back! M 
		    (color-indices-get-color-pair-index COLOR_INDEX_PIPGREEN2 COLOR_INDEX_BLACK))
    (set-menu-format! M height 1)
    (menu-opts-off! M O_SHOWDESC)
    (set-menu-win! M pan
    (set-menu-sub! M subpan)

    
    (listmenu-render! self)
    self))


(define (element-list->mitem-list elements sort?)
  "Returns a new list of <mitem> types containing string
representation of the entries in the elements list"
  
  (define (maybe-sort-alphabetically! x)
    (if sort?
	(sort! x string-ci<?)
	x))
  
  (define (string->menu-item str) (new-item str ""))
  
  (map string->menu-item
       (maybe-sort-alphabetically!
	(map ->string elements))))

(define (listmenu-render! self)
  "Renders a listmenu to its panel, with the current
element highlighted."
