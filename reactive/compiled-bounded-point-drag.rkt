#lang racket
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")
(require "../applications/geothings.rkt")
(require "../../rhea-rkt/rhea.rkt")

(define compiled-point-drag-thing%
  (class compiled-reactive-thing%
    (inherit button-down button-up button-state mouse-position image)

    (define sampling-method '(push))
    (define pt-x (new cassowary-variable% [initial-value 150]))
    (define pt-y (new cassowary-variable% [initial-value 150]))
    (define mouse-x (new cassowary-variable%))
    (define mouse-y (new cassowary-variable%))
    (define solver (new cassowary%))

    ;; always (and (<= (point 100 100) pt) (>= (point 200 200) pt))
    (send solver add-constraint (send pt-x <= 200))
    (send solver add-constraint (send pt-x >= 100))
    (send solver add-constraint (send pt-y <= 200))
    (send solver add-constraint (send pt-y >= 100))
    ;; always (= pt mouse)
    (send solver add-constraint (send pt-x = mouse-x))
    (send solver add-constraint (send pt-y = mouse-y))
    
    (super-new)
    
    (send this set-image! (circle (point 150 150) 5 (color "blue")))

    (define/override (get-sampling)
      sampling-method)
    
    (define/override (find-time mytime target)
      ; if there are button presses between the current time and target, advance to the earliest one, and otherwise to target
      (let ([potential-targets (filter (lambda (t) (and (> t mytime) (< t target)))
                                       (append
                                        (send this get-button-down-event-times)
                                        (send this get-button-up-event-times)
                                        (send this get-mouse-move-event-times)))])
        (if (null? potential-targets) target (apply min potential-targets))))
    
    (define/override (update-mysolution)
      ; compiling for these constraints:
      ;   (always* (if (button-pressed) (equal? pt (mouse-position))))
      ; the instant the button is pressed, we begin editing
      (cond
        [(button-down)
         (set! sampling-method '(push pull))
         (send solver add-edit-var mouse-x)
         (send solver add-edit-var mouse-y)
         (send solver begin-edit)
         (printf "begin edit\n")
         ]
        [(button-up)
         (set! sampling-method '(push))
         (send solver end-edit)
         (printf "end edit\n")
         ]
        [(eq? (button-state) 'down)
         (send solver edit-value mouse-x (point-x (mouse-position)))
         (send solver edit-value mouse-y (point-y (mouse-position)))
         (printf "editing: ~s\n" (point-y (mouse-position)))
         (send solver resolve)])
      (send this set-image! (circle (point (send pt-x value) (send pt-y value)) 5 (color "blue"))))
))

(make-viewer (new compiled-point-drag-thing%))
