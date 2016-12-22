#lang s-exp rosette
;; unit tests for integrals used in when tests

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide integrals-and-events)

; helper function to test for approximate equality
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) 1e-5)))

(define (simple-integral-in-event)
  (test-case
   "test a when constraint whose test involves a variable that is constrained to be the integral of something"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y real?)
       (assert (equal? x 0))
       (assert (equal? y 0))
       (send this solve)
       (stay x)
       (stay y)
       (while (<= (milliseconds) 10)
         ; this alternate test doesn't work:
         ; (while (<= x 50)
         #:interesting-time (if (equal? (milliseconds) 10) 'last #f)
         (assert (equal? x (integral (milliseconds))))
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 200)
   (check equal? (send r get-x) 50)
   (check equal? (send r get-y) 10)))


(define integrals-and-events
  (test-suite+
   "unit tests for integrals used in when tests"
   (simple-integral-in-event)
   ))

(time (run-tests integrals-and-events))
