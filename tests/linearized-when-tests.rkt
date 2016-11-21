#lang s-exp rosette
;; unit tests for when constraints that use a linearized test

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide linearized-when-tests)

; helper function to test for approximate equality (very coarse)
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) (* 0.1 x))))


(define (when-nonlinear)
  (test-case
   "when constraint with nonlinear test, linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (when (equal? (expt (milliseconds) 2) 200) #:linearize
         (printf "**** when is active **** time ~a \n" (milliseconds-evaluated))
         (assert (equal? x (milliseconds-evaluated))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send r get-x) (sqrt 200))
   ))


(define linearized-when-tests
  (test-suite+
   "unit tests for when constraints that use a linearized test"
   (when-nonlinear)
   ))

(time (run-tests linearized-when-tests))
