#lang s-exp rosette
; drag a circle while the button is pressed
(require "reactive.rkt")

(define dragging%
  (class reactive-thing%
    (inherit button-going-down? button-going-up? button-pressed? mouse-position image seconds)
    (super-new)
    (send this set-image! (make-circle this))
    (while (button-pressed?)
           #:interesting-time (button-going-up?)
           (assert (equal? (circle-center (image)) (mouse-position))))
    (send this solve)))


(define p (new dragging%))
(make-viewer p)
