;;; (pip-tui pip-tui) -- the full import module of the Pip TUI toolkit.

;;; Importing this module gives access to the public API of every
;;; finished pip-tui widget and of the event framework:
;;;
;;;   (use-modules (pip-tui pip-tui))
;;;
;;; Multimedia modules ((pip-tui png), (pip-tui pip-image),
;;; (pip-tui pulseaudio)) are built separately by `make media' and
;;; are not re-exported here.

(define-module (pip-tui pip-tui))

(define %re-exported-modules
  '((pip-tui action)
    (pip-tui action-map)
    (pip-tui event)
    (pip-tui tui-action)
    (pip-tui border)
    (pip-tui coords)
    (pip-tui pip-color-names)
    (pip-tui pip-colors)
    (pip-tui tui-label)
    (pip-tui tui-progress-bar)
    (pip-tui tui-terminal)
    (pip-tui tui-menubar)))

;; Make every binding exported by the modules above part of this
;; module's public interface.
(let ((iface (module-public-interface (current-module))))
  (for-each (lambda (name)
              (module-use! iface (resolve-interface name)))
            %re-exported-modules))
