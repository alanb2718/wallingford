#lang s-exp rosette
; hand compiled code for flipper
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define compiled-flipper%
  (class compiled-reactive-thing%
    (inherit seconds image button-pressed)
    (super-new)
    (send this set-image! (circle (point 150 150) 50 (color "blue")))
    
    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      '(push))
    (define/override (update-mysolution)
      ; compiling for this constraint:
      ; (when (button-pressed)
      ;  (assert (equal? (circle-color (image))
      ;                  (flip (previous (circle-color (image)))))))))
      ; So if the button is pressed update the image with the flipped color
      (cond [(button-pressed)
             (send this set-image!
                   (struct-copy circle (image) [color (flip (circle-color (image)))]))]))
    (define/override (find-time mytime target)
      ; if there is a button press between the current time and target, advance to that, and otherwise to target
      ; get-button-down-event-times
      (let ([potential-targets (filter (lambda (t) (and (> t mytime) (< t target)))
                                       (send this get-button-down-event-times))])
        (if (null? potential-targets) target (car potential-targets))))))

(make-viewer (new compiled-flipper%) #:title "Compiled version of flipper" #:sleep-time 0.01)
