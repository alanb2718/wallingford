#lang s-exp rosette
; hand compiled code for pulsing circle example
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")
(provide compiled-pulser%)

(define compiled-pulser%
  (class compiled-reactive-thing%
    (inherit seconds image)
    (super-new [init-image (circle (point 150 150) 50 (color "blue"))])
    
    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      '(pull))
    (define/override (update-mysolution)
      ; compiling for this constraint:
      ;    (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
      ; later: fix this to use something that automatically gets non-changed fields from the old value
      (send this update-myimage (circle (point 150 150) (+ 60 (* 50 (sin (seconds)))) (color "blue"))))
    (define/override (find-time target)
      target)
    (define/override (advance-time-helper target)
      (send this set-my-time target))
  ))

(make-viewer (new compiled-pulser%) #:title "Compiled version of pulser" #:sleep-time 0.01)
