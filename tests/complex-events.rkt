#lang s-exp rosette
;; unit tests for events involving complex expressions (not just seconds, milliseconds, and buttons)

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide complex-events)

; helper function to test for approximate equality
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) 1e-5)))

(define (when-linear)
  (test-case
   "when constraint whose condition involves a variable that is a linear function of time"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y real?)
       (always (equal? x (- (* 2 (milliseconds)) 1)))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 5)
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send r get-x) 199)
   (check equal? (send r get-y) 3)
   (send-syncd r advance-time-syncd 200)
   (check equal? (send r get-x) 399)
   (check equal? (send r get-y) 3)))

(define (when-nonlinear)
  (test-case
   "when constraint whose condition involves a variable that is a nonlinear function of time"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x y real?)
       ; using (milliseconds) causes an error since sin expects a concrete value
       ; (always (equal? x (sin (milliseconds))))
       (always (equal? x (sin (milliseconds-evaluated))))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 1/2)
         (printf "when is active\n")
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   ; these tests don't work (and have the wrong values anyway) -- right now the 'when' is never triggered
   ; (check equal? (send r get-x) 199)
   ; (check equal? (send r get-y) 3)
   (send-syncd r advance-time-syncd 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   ; (check equal? (send r get-x) 399)
   ; (check equal? (send r get-y) 3)
   ))

(define complex-events
  (test-suite+
   "unit tests for events involving complex expressions"
   (when-linear)
   (when-nonlinear)
   ))

(time (run-tests complex-events))
