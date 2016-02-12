#lang s-exp rosette
; colored  circle example (changes color every 2 seconds)
(require "reactive.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define timed-flipper%
  (class reactive-thing%
    (inherit milliseconds image previous)
    (super-new [init-image (make-circle)])
    
    (when (zero? (remainder (milliseconds) 2000))
      (assert (equal? (circle-color (image))
                      (flip (previous (circle-color (image)))))))))

(define p (new timed-flipper%))
(make-viewer p #:title "Timed flipper")
