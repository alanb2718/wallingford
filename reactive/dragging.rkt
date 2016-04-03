#lang s-exp rosette
; drag a circle while the button is pressed
(require "reactive.rkt")

(define dragging%
  (class reactive-thing%
    (inherit button-pressed? image mouse-position)
    (super-new)
    (send this set-image! (make-circle this))
    (while (button-pressed?)
           (assert (equal? (circle-center (image)) (mouse-position))))
    (send this solve)))


(define p (new dragging%))
(make-viewer p)
