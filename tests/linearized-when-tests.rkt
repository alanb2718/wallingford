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
   "when constraint with nonlinear test, linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (when (equal? (expt (milliseconds) 2) 200) #:linearize #:dt 1
         (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check approx-equal? (send r get-x) (sqrt 200))
   ))

(define (when-constant-velocity)
  (test-case
   "when constraint with test involving an integral (constant velocity), linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       ; x is the x position of an object, moving at velocity 10 units/millisecond
       ; for simplicity ignore the object's y position
       (define-public-symbolic* x y real?)
       (always (equal? x (integral 10)))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 230) #:linearize #:dt 1
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send r get-x) 1000)
   (check approx-equal? (send r get-y) 23)
   ))

(define (when-acceleration-numeric-integrals)
  (test-case
   "when constraint with test involving two numeric integrals (accelerating object), linearized when test"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       ; x is the x position of an object, with an acceleration of 1/10 unit per millisecond squared
       (define-public-symbolic* a v x y real?)
       (always (equal? a 1/10))
       (always (equal? v (integral a #:numeric)))
       (always (equal? x (integral v #:numeric)))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 23) #:linearize #:dt 1
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check approx-equal? (send r get-x) 500)
   (check approx-equal? (send r get-y) (sqrt 460))  ; 460 is the true value, using the formula x = 1/2 a t^2
   ))

(define (when-acceleration-partly-symbolic)
  (test-case
   "like when-acceleration-numeric-integrals except that one integral is symbolic"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       ; x is the x position of an object, with an acceleration of 1/10 unit per millisecond squared
       (define-public-symbolic* v x y real?)
       ; given simplistic symbolic integration, need to use a constant instead of variable a
       (always (equal? v (integral 1/10 #:symbolic)))
       (always (equal? x (integral v #:numeric)))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 23) #:linearize #:dt 1
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check approx-equal? (send r get-x) 500)
   (check approx-equal? (send r get-y) (sqrt 460))  ; 460 is the true value, using the formula x = 1/2 a t^2
   ))



(define linearized-when-tests
  (test-suite+
   "unit tests for when constraints that use a linearized test"
   (when-with-linearized-equality-test)
   (when-nonlinear)
   (when-constant-velocity)
   (when-acceleration-numeric-integrals)
   (when-acceleration-partly-symbolic)
   ))

(time (run-tests linearized-when-tests))
