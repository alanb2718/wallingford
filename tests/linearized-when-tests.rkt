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


(define (when-linear)
  (test-case
   "same as when-linear in when-tests.rkt, except that the when is linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y real?)
       (always (equal? x (- (* 2 (milliseconds)) 1)))
       (assert (equal? y 0))
       (send this solve)
       (stay y)
       (when (equal? x 5) #:linearize
         (assert (equal? y (milliseconds))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send r get-x) 199)
   (check equal? (send r get-y) 3)
   (send-syncd r advance-time-syncd 200)
   (check equal? (send r get-x) 399)
   (check equal? (send r get-y) 3)))

(define (when-nonlinear-expt)
  (test-case
   "when constraint with nonlinear test using expt, linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (when (equal? (expt (milliseconds) 2) 200) #:linearize
         ; (printf "**** when is active **** time ~a \n" (exact->inexact (milliseconds-evaluated)))
         (assert (equal? x (milliseconds-evaluated))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check approx-equal? (send r get-x) (sqrt 200))
   ))

(define (when-nonlinear-*)
  (test-case
   "when constraint with nonlinear test using *, linearized"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (when (equal? (* (milliseconds) (milliseconds)) (+ (milliseconds) 6)) #:linearize
         (printf "**** when is active **** time ~a \n" (exact->inexact (milliseconds-evaluated)))
         (assert (equal? x (milliseconds-evaluated))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   ; 3*3 = 3+6
   (check approx-equal? (send r get-x) 3)
   ))


(define linearized-when-tests
  (test-suite+
   "unit tests for when constraints that use a linearized test"
   (when-linear)
   (when-nonlinear-expt)
   ; ** this test doesn't work yet -- see status comment at end of file **
   ; (when-nonlinear-*)
   ))

(time (run-tests linearized-when-tests))

; Status as of October 2017:
; Test (when-nonlinear-*) not yet working - it gets into an infinite search for a time to advance to.
; The behavior is that it successfully advances time to just before 3.0 (2.9997079439252334).  So far so good.
; But then it tries to advance time again, getting to something even closer to 3.0, namely 2.9999964024825063,
; and so on, until find-time gives an error ("unable to find a time to advance to that is greater than the current time").
; This should be fixed somewhere in reactive-thing (probably in advance-time-helper, but maybe in find-time).
; It somehow needs to take a bigger step after advancing to the first number it finds, namely 2.9997079439252334.
;
; Additional tests needed that include integral.  (Tests involving sin don't work since sin isn't lifted in Rosette.)
