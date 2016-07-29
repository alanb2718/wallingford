#lang s-exp rosette
; hand compiled code for follow-mouse.rkt
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "abstract-reactive-thing.rkt")
(require "compiled-reactive-thing.rkt")

(define compiled-follow-mouse%
  (class compiled-reactive-thing%
    (inherit seconds image button-pressed? button-going-up? mouse-position)
    (super-new)
    (send this set-image! (circle (point 150 150) 50 (color "blue")))
    
    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      (let ([s (send this button-state)])
        (if (or (eq? s 'going-down) (eq? s 'down) (eq? s 'going-up)) '(push pull) '(push))))
    (define/override (update-mysolution)
      ; compiling for this constraint:
      ;     (while (button-pressed?)
      ;            (assert (equal? (circle-center (image)) (mouse-position))))
      ; So while the button is pressed update the circle's center to follow the mouse.
      (cond [(button-pressed?)
             (send this set-image! (struct-copy circle (image) [center (mouse-position)]))])
      (cond [(button-going-up?)
             (send this notify-watchers-changed)]))
    (define/override (find-time mytime target)
      ; if there is a button press or release between the current time and target, advance to that, and otherwise to target
      (let ([potential-targets (filter (lambda (e) (and (> (mouse-event-time e) mytime)
                                                        (< (mouse-event-time e) target)
                                                        (or (eq? (mouse-event-button-state e) 'going-down)
                                                            (eq? (mouse-event-button-state e) 'going-up))))
                                       (send this get-mouse-events))])
        (if (null? potential-targets) target (apply min (map mouse-event-time potential-targets)))))))

(make-viewer (new compiled-follow-mouse%) #:title "Compiled version of follow-mouse" #:sleep-time 0.01)


