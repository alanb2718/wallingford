#lang s-exp rosette
;; Unit tests for wallingford core.  Run from all-tests.rkt

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")

(provide wallingford-core-tests)

(define (soft-cn-test)
  (test-case
   "simple test of a soft always constraint"
   (define c (new thing%))
   (define-symbolic x integer?)
   ; We used to need the following call to solve to give x a value in the current solution -
   ; otherwise Rosette would stop without finding a solution.  The initial value for the
   ; keep-going parameter in wally-solve in wallingford.rkt now works around this problem.
   ; (solve (assert (equal? x 0)))
   (always (equal? x 2) #:priority low #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 2)))

(define (explicit-solution-test)
  (test-case
   "test that wally-solve is returning a solution to the required constraints"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:owner c)
   (let ([soln (send c solve)])
     (check-equal? (send c wally-evaluate x soln) 2))))

(define (explicit-soft-solution-test)
  (test-case
   "test that wally-solve is returning a solution to the soft constraints"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:priority low #:owner c)
   (let ([soln (send c solve)])
     (check-equal? (send c wally-evaluate x soln) 2))))

(define (cn-priorities-test)
  (test-case
   "test always constraints with different priorities"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:priority low #:owner c)
   (always (equal? x 3) #:priority high #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 3)
   (always (equal? x 5) #:owner c)  ; should default to required priority
   (send c solve)
   (check-equal? (send c wally-evaluate x) 5)))

(define (cn-count-test)
  (test-case
   "test that 2 low priority constraints are satisfied in preference to 1 low priority constraint"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 5) #:priority low #:owner c)
   (always (equal? x 7) #:priority low #:owner c)
   (always (equal? x 7) #:priority low #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 7)
   (always (equal? x 5) #:priority low #:owner c)
   (always (equal? x 5) #:priority low #:owner c)
   ; now we've got 2 constraints that say x=7, and 3 that say x=5
   (send c solve)
   (check-equal? (send c wally-evaluate x) 5)))

(define (simultaneous-eqn-test)
  (test-case
   "test solving simultaneous linear equations with different priorities"
   (define c (new thing%))
   (define-symbolic x y integer?)
   (always (equal? (+ (* 2 x) (* 3 y)) 8) #:priority medium #:owner c)
   (always (equal? (+ x y) 3) #:priority medium #:owner c)
   (always (equal? (+ x (* 7 y)) 0) #:priority low #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 1)
   (check-equal? (send c wally-evaluate y) 2)))

(define (update-test)
  (test-case
   "initialize x, y, z; then change y, then change x"
   (define c (new thing%))
   (define-symbolic x y z integer?)
   (define xyz (list x y z))  ; to simplify checks
   (always (equal? z (+ x y)) #:owner c)
   (stay x  #:owner c)  ; this should default to lowest priority
   (stay y #:priority low #:owner c)
   (stay z #:priority medium #:owner c)
   (assert (equal? x 2))
   (assert (equal? y 3))
   (assert (equal? z 5))
   (send c solve)
   (check-equal? (send c wally-evaluate xyz) '(2 3 5))
   (assert (equal? y 1))
   (send c solve)
   ; the stay on x is weaker than the stay on z, so x should change
   (check-equal? (send c wally-evaluate xyz) '(4 1 5))
   (assert (equal? x 8))
   (send c solve)
   ; the stay on y is weaker than the stay on z, so y should change
   (check-equal? (send c wally-evaluate xyz) '(8 -3 5))))

(define (required-stay-test)
  (test-case
   "check that required stays are in fact required"
   (define c (new thing%))
   (define-symbolic x integer?)
   (assert (equal? x 5))
   (send c solve)
   (stay x #:priority required #:owner c)
   ; x should now be stuck at 5, so the following constraint is unsatisfiable
   (always (equal? x 0) #:owner c)
   (check-exn
    exn:fail?
    (lambda () (send c solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts!)))

(define (unsatisfiable-required-cn-test)
  (test-case
   "an unsatisfiable required constraint should raise an exception"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:owner c)
   (always (equal? x 3) #:owner c)
   (check-exn
    exn:fail?
    (lambda () (send c solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts!)))

(define (explicit-required-priority-test)
  (test-case
   "test providing an explicit priority of required"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:priority required #:owner c)
   (always (equal? x 3) #:priority required #:owner c)
   (check-exn
    exn:fail?
    (lambda () (send c solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts!)))

(define (always-with-class-test)
  (test-case
   "test always in a class (so owner is implicit)"
   (define testclass%
     (class thing%
       (super-new)
       (define-symbolic x integer?)
       ; have one constraint with an explicit priority, one with a default (which should be required)
       (always (equal? x 2) #:priority low)
       (always (equal? x 3))
       (define/public (test1)
         (send this solve)
         (equal? (send this wally-evaluate x) 3))))
   (define thing (new testclass%))
   (check-true (send thing test1))))

(define (stay-with-class-test)
  (test-case
   "test stay in a class (so owner is implicit)"
   (define testclass%
     (class thing%
       (super-new)
       (define-symbolic x y integer?)
       (assert (equal? x 2))
       (assert (equal? y 3))
       ; have one stay with an explicit priority, one with a default (which should be required)
       (stay x #:priority required)
       (stay y)
       (send this solve)
       (define/public (test1)
         (send this solve) ; this should be the second call to solve
         (and (equal? (send this wally-evaluate x) 2) (equal? (send this wally-evaluate y) 3)))))
   (define thing (new testclass%))
   (check-true (send thing test1))))

(define wallingford-core-tests 
  (test-suite+ ; Rosette specific test-suite that will clear relevant rosette state upon finishing 
   "general unit tests for wallingford"
   (soft-cn-test)
   (cn-priorities-test)
   (cn-count-test)
   (simultaneous-eqn-test)
   (update-test)
   (required-stay-test)
   (unsatisfiable-required-cn-test)
   (explicit-required-priority-test)
   (always-with-class-test)
   (stay-with-class-test)
   ))

(time (run-tests wallingford-core-tests))
