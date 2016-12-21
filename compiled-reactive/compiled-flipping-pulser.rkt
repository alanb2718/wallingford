#lang s-exp rosette
; hand compiled code for pulsing circle example
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")
(require "../reactive/abstract-reactive-thing.rkt")
(require "compiled-reactive-thing.rkt")

(define (flip c)
  (if (equal? c (color "blue")) (color "red") (color "blue")))

(define compiled-flipping-pulser%
  (class compiled-reactive-thing%
    (inherit seconds image button-going-down?)
    (super-new)
    (send this set-image! (circle (point 150 150) 50 (color "blue")))
    
    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      '(push pull))
    (define/override (update-mysolution)
      ; compiling for these constraints:
      ;    (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
      ;(when (button-going-down?)
      ;  (assert (equal? (circle-color (image))
      ;                  (flip (previous (circle-color (image)))))))))
      (let ([newcolor (if (button-going-down?) (flip (circle-color (image))) (circle-color (image)))])
        (send this set-image! (struct-copy circle (image)
                                           [radius  (+ 60 (* 50 (sin (seconds))))]
                                           [color newcolor]))))
      ; no need to send notify-watchers-changed since we are always in push pull sampling mode
    (define/override (find-time mytime target)
      ; if there is a button press between the current time and target, advance to that, and otherwise to target
      (let ([potential-targets (filter (lambda (e) (and (> (mouse-event-time e) mytime)
                                                        (< (mouse-event-time e) target)
                                                        (eq? (mouse-event-button-state e) 'going-down)))
                                       (send this get-mouse-events))])
        (values (if (null? potential-targets) target (apply min (map mouse-event-time potential-targets))) #f)))))

(make-viewer (new compiled-flipping-pulser%) #:sleep-time 0.05)
