#lang s-exp rosette
; make a circle follow the mouse while the button is pressed
; This captures one key aspect of dragging (although not all - see quadrilateral.rkt for a
; more realistic dragging example that also includes selection).
(require "reactive.rkt")

(define follow-mouse%
  (class reactive-thing%
    (inherit button-pressed? image mouse-position)
    (super-new)
    (send this set-image! (make-circle this))
    (while (button-pressed?)
           (assert (equal? (circle-center (image)) (mouse-position))))
    (send this solve)))


(define p (new follow-mouse%))
(make-viewer p)
