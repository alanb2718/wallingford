#lang s-exp rosette
; pulsing circle example
(require "reactive.rkt")
(provide pulser%)

; define a pulser class as a subclass of reactive-thing%
(define pulser%
  (class reactive-thing%
    (inherit seconds image)
    (super-new [init-image (make-circle)])
    
    (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))

; evaluate these lines to make a new pulser and a viewer on it:
; (wally-clear)
; (make-viewer (new pulser%))
