#lang s-exp rosette
; circle that changes colors on button press
(require "reactive.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define flipper%
  (class reactive-thing%
    (inherit button-pressed image previous)
    (super-new [init-image (make-circle)])
    
    (when (button-pressed)
      (assert (equal? (circle-color (image))
                      (flip (previous (circle-color (image)))))))))


(define p (new flipper%))
(make-viewer p)
