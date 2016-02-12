#lang s-exp rosette
; colored pulsing circle example (changes size with sin of time, changes color every 2 seconds)
(require "reactive.rkt")
(require "pulser-class.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define colored-pulser%
  (class pulser%
    (inherit milliseconds image previous)
    (super-new)
    
    ; this constraint is inherited:
    ; (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
    
    (when (zero? (remainder (milliseconds) 2000))
      (assert (equal? (circle-color (image))
                      (flip (previous (circle-color (image)))))))))


(define p (new colored-pulser%))
(make-viewer p #:title "Colored pulser")
