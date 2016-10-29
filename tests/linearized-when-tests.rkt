#lang s-exp rosette
;; unit tests for when constraints that use a linearized condition

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide linearized-when-tests)

; helper function to test for approximate equality
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) 1e-2)))

(define (when-with-linearized-equality-test)
  (test-case
   "when with a simple linearized test"
   (define count 0)
   (define (get-count) count)
   (define one-when-tester%
     (class reactive-thing%
       (inherit milliseconds milliseconds-evaluated)
       (super-new)
       (when (equal? (milliseconds) 25) #:linearize
         (set! count (+ 1 count)))))   
   (define r (new one-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   (send-thing r advance-time 8)
   (check equal? (send-syncd r milliseconds-syncd) 8)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   (send-thing r advance-time 49)
   (check equal? (send-syncd r milliseconds-syncd) 49)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)
   (send-thing r advance-time 300)
   (check equal? (send-syncd r milliseconds-syncd) 300)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)))

(define (when-nonlinear)
  (test-case
   "when constraint with nonlinear condition, linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (when (equal? (expt (milliseconds) 2) 200) #:linearize   #:dt 1
         (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check approx-equal? (send r get-x) (sqrt 200))
   ))


(define linearized-when-tests
  (test-suite+
   "unit tests for when constraints that use a linearized condition"
   (when-with-linearized-equality-test)
   (when-nonlinear)
   ))

(time (run-tests linearized-when-tests))
