#lang s-exp rosette
; pulsing circle example
(require "reactive.rkt")
(provide pulser%)

; define a pulser class as a subclass of reactive-thing%
(define pulser%
  (class reactive-thing%
    (inherit seconds image)
    (super-new)
    (send this set-image! (make-circle this))
    (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
    (send this solve)))
