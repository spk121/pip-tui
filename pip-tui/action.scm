(define-module (pip-tui action)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (pip-tui typecheck)
  #:export (action-new
            action?
            assert-action
            action-activate
            action-change-state!
            action-enable!
            action-disable!
            action-get-name
            action-get-state
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIONS

;; <action> applies an <event> to a TUI widget target.

;; The main use for an <action> is to call its ACTIVATE-CB callback
;; function.  The activate callback function takes an <event> and the
;; action's STATE, unpacks the event, and then adjusts the TUI widget
;; based on the contents of the <event>.

;; An <action> also has a STATE that is sent to the activate callback
;; function.

;; If CHANGE-STATE-CB is a procedure, it is called when an <action>'s
;; state is updated.

(define-record-type <action>
  (action-new name
              enabled
              state
              activate-cb
              change-state-cb)
  action?
  (name action-get-name action-set-name!)
  (enabled action-get-enabled action-set-enabled!)
  (state action-get-state action-set-state!)
  (activate-cb action-get-activate-cb action-set-activate-cb!)
  (change-state-cb action-get-change-state-cb action-set-change-state-cb!))

;; name (string)

;; enabled (boolean) -- when #f, calls to action-activate and
;; action-change-state!  have no effect

;; state (freeform) -- some freeform data that is passed to
;; the activate callback function.

;; activate-cb (lambda (tui-target event state)) --
;; function that is called on receipt of an event.  It should return
;; #f if the event should be passed along to the next action, or #t if
;; the event need not be passed on to other actions.

;; change-state-cb (lambda (state)) -- function that is called
;; when state changes.

(define-syntax assert-action
  (syntax-rules ()
    ((_ val)
     (typecheck val 'action action?))))


(define (action-activate action target event)
  "If the <action> is enabled, call <action>'s ACTIVATE-CB callback
with TARGET, EVENT and the <action>'s current STATE as parameters."
  (assert-action action)
  (when (action-get-enabled action)
    (let ([func (action-get-activate-cb action)])
      (when func
        (func target event (action-get-state action))))))

(define (action-change-state! action state)
  "If the <action> is enabled, change the <action>'s STATE parameter."
  (assert-action action)
  (when (action-get-enabled action)
    (action-set-state! action state)
    (let ((func (action-get-change-state-cb action)))
      (when func
        (func state)))))

(define (action-enable! action)
  "Enable the <action> to be able to call its ACTIVATE-CB parameter
and to change its STATE"
  (assert-action action)
  (action-set-enabled! action #t))

(define (action-disable! action)
  "Disable the <action> so that it cannot call its ACTIVATE-CB
parameter or change its STATE."
  (assert-action action)
  (action-set-enabled! action #f))
