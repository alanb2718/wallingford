#lang s-exp rosette
; hand compiled code for pulsing circle example
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define compiled-flipping-pulser%
  (class compiled-reactive-thing%
    (inherit seconds image button-pressed)
    (super-new [init-image (circle (point 150 150) 50 (color "blue"))])
    
    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      '(push pull))
    (define/override (update-mysolution)
      ; compiling for these constraints:
      ;    (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
      ;(when (button-pressed)
      ;  (assert (equal? (circle-color (image))
      ;                  (flip (previous (circle-color (image)))))))))
      (let ([newcolor (if (button-pressed) (flip (circle-color (image))) (circle-color (image)))])
        (send this update-myimage (circle (point 150 150) (+ 60 (* 50 (sin (seconds)))) newcolor))))
    (define/override (find-time target)
      ; if there is a button press between the current time and target, advance to that, and otherwise to target
      ; get-button-down-event-times
      (let* ([now (send this milliseconds)]
             [potential-targets (filter (lambda (t) (and (> t now) (< t target)))
                                        (send this get-button-down-event-times))])
        (if (null? potential-targets) target (car potential-targets))))
    (define/override (advance-time-helper target)
      (send this set-my-time (send this find-time target)))
  ))

(make-viewer (new compiled-flipping-pulser%) #:sleep-time 0.01)
