#lang s-exp rosette
;; unit tests for wallingford core.  These can be run from this file, or from all-tests.rkt

(require rackunit rackunit/text-ui)
(require "../core/wallingford.rkt")

(provide wallingford-core-tests)

(define (soft-cn-test)
  (test-case
   "simple test of a soft always constraint"
   (wally-clear)
   (define-symbolic x number?)
   ; We used to need the following call to solve to give x a value in the current solution -
   ; otherwise Rosette would stop without finding a solution.  The initial value for the
   ; keep-going parameter in wally-solve in wallingford.rkt now works around this problem.
   ; (solve (assert (equal? x 0)))
   (always (equal? x 2) #:priority low)
   (wally-solve)
   (check-equal? (evaluate x) 2)))

(define (explicit-solution-test)
  (test-case
   "test that wally-solve is returning a solution to the required constraints"
   (wally-clear)
   (define-symbolic x number?)
   (always (equal? x 2))
   (let ([soln (wally-solve)])
     (check-equal? (evaluate x soln) 2))))

(define (explicit-soft-solution-test)
  (test-case
   "test that wally-solve is returning a solution to the soft constraints"
   (wally-clear)
   (define-symbolic x number?)
   (always (equal? x 2) #:priority low)
   (let ([soln (wally-solve)])
     (check-equal? (evaluate x soln) 2))))

(define (cn-priorities-test)
  (test-case
   "test always constraints with different priorities"
   (wally-clear)
   (define-symbolic x number?)
   (always (equal? x 2) #:priority low)
   (always (equal? x 3) #:priority high)
   (wally-solve)
   (check-equal? (evaluate x) 3)
   (always (equal? x 5))  ; should default to required priority
   (wally-solve)
   (check-equal? (evaluate x) 5)))

(define (cn-count-test)
  (test-case
   "test that 2 low priority constraints are satisfied in preference to 1 low priority constraint"
   (wally-clear)
   (define-symbolic x number?)
   (always (equal? x 5) #:priority low)
   (always (equal? x 7) #:priority low)
   (always (equal? x 7) #:priority low)
   (wally-solve)
   (check-equal? (evaluate x) 7)
   (always (equal? x 5) #:priority low)
   (always (equal? x 5) #:priority low)
   ; now we've got 2 constraints that say x=7, and 3 that say x=5
   (wally-solve)
   (check-equal? (evaluate x) 5)))

(define (simultaneous-eqn-test)
  (test-case
   "test solving simultaneous linear equations with different priorities"
   (wally-clear)
   (define-symbolic x y number?)
   (always (equal? (+ (* 2 x) (* 3 y)) 8) #:priority medium)
   (always (equal? (+ x y) 3) #:priority medium)
   (always (equal? (+ x (* 7 y)) 0) #:priority low)
   (wally-solve)
   (check-equal? (evaluate x) 1)
   (check-equal? (evaluate y) 2)))

(define (update-test)
  (test-case
   "initialize x, y, z; then change y, then change x"
   (wally-clear)
   (define-symbolic x y z number?)
   (define xyz (list x y z))  ; to simplify checks
   (always (equal? z (+ x y)))
   (stay x)  ; this should default to lowest priority
   (stay y #:priority low)
   (stay z #:priority medium)
   (assert (equal? x 2))
   (assert (equal? y 3))
   (assert (equal? z 5))
   (wally-solve)
   (check-equal? (evaluate xyz) '(2 3 5))
   (assert (equal? y 1))
   (wally-solve)
   ; the stay on x is weaker than the stay on z, so x should change
   (check-equal? (evaluate xyz) '(4 1 5))
   (assert (equal? x 8))
   (wally-solve)
   ; the stay on y is weaker than the stay on z, so y should change
   (check-equal? (evaluate xyz) '(8 -3 5))))

(define (required-stay-test)
  (test-case
   "check that required stays are in fact required"
   (wally-clear)
   (define-symbolic x number?)
   (assert (equal? x 5))
   (wally-solve)
   (stay x #:priority required)
   ; x should now be stuck at 5, so the following constraint is unsatisfiable
   (always (equal? x 0))
   (check-exn
    exn:fail?
    (lambda () (wally-solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts)))

(define (unsatisfiable-required-cn-test)
  (test-case
   "an unsatisfiable required constraint should raise an exception"
   (wally-clear)
   (define-symbolic x number?)
   (always (equal? x 2))
   (always (equal? x 3))
   (check-exn
    exn:fail?
    (lambda () (wally-solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts)))

(define wallingford-core-tests 
  (test-suite 
   "run general tests for wallingford"
   (soft-cn-test)
   (cn-priorities-test)
   (cn-count-test)
   (simultaneous-eqn-test)
   (update-test)
   (required-stay-test)
   (unsatisfiable-required-cn-test)
   ))
