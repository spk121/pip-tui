(use-modules (srfi srfi-64) (pip-tui string-lib))

(test-begin "string-lib")

(test-equal "width ascii" 5 (string-width "hello"))
(test-equal "width wide CJK" 4 (string-width "日本"))
(test-equal "substring-width" 2 (substring-width "a日b" 1 2))

(test-equal "crlf->lf" "a\nb\nc" (string-convert-crlf-to-lf "a\r\nb\r\nc"))
(test-equal "remove trailing ws" "abc" (string-remove-trailing-whitespace "abc   "))
(test-equal "remove trailing ws: all ws" "" (string-remove-trailing-whitespace "   "))

(test-equal "pad end" "ab   " (string-pad-end-to-width "ab" 5))
(test-equal "pad beginning" "   ab" (string-pad-beginning-to-width "ab" 5))
(test-equal "pad both" " ab  " (string-pad-beginning-and-end-to-width "ab" 5))
(test-equal "pad wide" "日 " (string-pad-end-to-width "日" 3))
(test-equal "pad: already wide enough" "abcdef" (string-pad-end-to-width "abcdef" 3))

(test-equal "untabify" "a    b" (string-untabify "a\tb" 4))

(test-equal "trim to width" "abc" (string-trim-right-to-width "abcdef" 3))
(test-equal "trim to width, wide chars" "日" (string-trim-right-to-width "日本" 3))

(test-equal "render left" '("one  " "two  ")
  (string-render "one two" 4 5 'left #f))
(test-equal "render right" '("  one" "  two")
  (string-render "one two" 4 5 'right #f))
(test-equal "render newline" '("ab" "cd")
  (string-render "ab\ncd" 4 10 'none #f))

(test-equal "find brace pairs" '((1 3) (6 8))
  (string-find-brace-pairs "a[b]cd[e]" #\[ #\]))
(test-equal "find brace pairs, list" '((0 1 1 1))
  (string-list-find-brace-pairs '("a[bc" "d]e") #\[ #\]))

(test-equal "substring-list single" '("bc") (substring-list '("abcd") 0 1 0 3))
(test-equal "substring-list multi" '("cd" "ef" "g")
  (substring-list (list "abcd" "ef" "ghi") 0 2 2 1))

(test-equal "string-list-length" 6 (string-list-length '("ab" "cd" "ef")))
(test-equal "truncate!" '("ab" "c")
  (string-list-truncate! (list "ab" "cd" "ef") 3))
(test-equal "remainder" '("d" "ef")
  (string-list-remainder (list "ab" "cd" "ef") 3))

(test-end "string-lib")
