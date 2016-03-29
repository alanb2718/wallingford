#lang s-exp rosette
; colored pulsing circle example (changes size with sin of time, flips color on button press)
(require "reactive.rkt")
(require "pulser-class.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define flipping-pulser%
  (class pulser%
    (inherit button-going-down? image previous)
    (super-new)
    
    ; this constraint is inherited:
    ; (always (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
    
    (when (button-going-down?)
      (assert (equal? (circle-color (image))
                      (flip (previous (circle-color (image)))))))))


(define f (new flipping-pulser%))
(make-viewer f #:title "Flipping pulser")
