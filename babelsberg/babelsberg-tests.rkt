#lang s-exp rosette
;; unit tests for babelsberg additions to the wallingford core

(require rackunit rackunit/text-ui rosette/lib/util/roseunit)
(require "babelsberg.rkt")

(define (initialize-then-update-test)
  (test-case
   "test initialize"
   (wally-clear)
   (define-symbolic x integer?)
   (wally-set! x 3)
   (check-equal? (evaluate x) 3)
   (wally-set! x (+ x 2))
   (check-equal? (evaluate x) 5)))

(define (two-numbers-test)
  (test-case
   "test two separate number vars"
   (wally-clear)
   (define-symbolic x y integer?)
   (stay x)
   (stay y)
   (wally-set! x 3)
   (wally-set! y x)
   ; x and y are NOT aliased ... so making y=10 shouldn't change x
   (always (equal? y 10))
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 10)))

(define (two-points-test)
  (struct point (x y) #:transparent)
  (test-case
   "test two separate points"
   (wally-clear)
   (define-symbolic p q point?)
   (stay p)
   (stay q)
   (wally-set! p (point 2 3))
   (wally-set! q p)
   ;p and q are not aliased ... so changing p shouldn't change q
   (wally-set! p (point 10 10))
   (check-equal? (evaluate p) (point 10 10))
   (check-equal? (evaluate q) (point 2 3))))

(define (two-mutable-points-test)
  (struct point (x y) #:transparent #:mutable)
  (test-case
   "test two aliased mutable points"
   (wally-clear)
   (define p (point 2 3))
   (define q p)
   (stay p)
   (stay q)
   ;p and q ARE aliased ... so changing p should change q
   (set-point-x! p 10)
   (check-equal? (evaluate p) (point 10 3))
   (check-equal? (evaluate q) (point 10 3))))

(define (eq-test)
  (struct test (x) #:transparent #:mutable) ; has object identity since it's mutable
  (test-case
   "test identity constraints"
   (wally-clear)
   (define p (test 5))
   (define q p)
   (always (eq? p q))
   (wally-set! p (test 10))
   ; due to the identity constraint both p and q should be updated
   (check-equal? (test-x p) 10)
   (check-equal? (test-x q) 10)
   (check-eq? p q)))

(define (multiple-eq-test)
  (struct test (x) #:transparent #:mutable)
  (test-case
   "test multiple identity constraints"
   (wally-clear)
   (define p (test 5))
   (define q p)
   (always (eq? p q))
   (define r (test 20))
   (define s r)
   (always (eq? r s))
   (wally-set! p (test 10))
   (wally-set! s (test 30))
   ; due to the identity constraint both p and q should be updated
   (check-equal? (test-x p) 10)
   (check-equal? (test-x q) 10)
   (check-eq? p q)
   ; r and s should be identical but not the same as p and q
   (check-equal? (test-x r) 30)
   (check-equal? (test-x s) 30)
   (check-eq? r s)
   ; now set p to s -- all of p, q, r, and s should be identical now
   (wally-set! p s)
   (check-eq? p q)
   (check-eq? p r)
   (check-eq? p s)))

(define (eq-scope-test)
  (struct test (x) #:transparent #:mutable)
  (test-case
   "test identity constraints with different scopes"
   (wally-clear)
   (define p (test 5))
   (define q null)
   ; now make another p within a let
   (let ([p (test 100)])
     (set! q p)
     (always (eq? p q))
     (check-eq? p q)
     (wally-set! p (test 20))
     ; due to the identity constraint both p and q should be updated
     (check-equal? (test-x p) 20)
     (check-equal? (test-x q) 20)
     (check-eq? p q))
   ; but the outer p should be unaffected
   (check-equal? (test-x p) 5)))


; some tests from the step-by-step Babelsberg semantics tech report

(define (test1)
  (test-case
   "test1"
   (wally-clear)
   (define-symbolic x integer?)
   (wally-set! x 3)
   (check-equal? (evaluate x) 3)
   (wally-set! x 4)
   (check-equal? (evaluate x) 4)
   (always (>= x 10))
   (check-true (>= (evaluate x) 10))))

(define (test4)
  (test-case
   "test4"
   (wally-clear)
   (define-symbolic x y z integer?)
   (wally-set! x 0)
   (wally-set! y 0)
   (wally-set! z 0)
   (always (= (+ x y (* 2 z)) 10))
   (always (= (+ (* 2 x) y z) 20))
   (wally-set! x 100)
   (check-equal? (evaluate x) 100)
   (check-equal? (evaluate y) -270)
   (check-equal? (evaluate z) 90)))

(define (test5)
  (test-case
   "test5"
   (wally-clear)
   (define-symbolic x integer?)
   (wally-set! x 5)
   (always (<= x 10))
   ; the RackUnit exception handler test doesn't play well with Rosette,
   ; so do it by hand
   (define triggered #f)
   (with-handlers ([exn:fail? (lambda (e) (set! triggered #t))])
     (wally-set! x (+ x 15)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (check-true triggered)
   (clear-asserts!)))

; This is a bit different from the test in the report.  If we just share
; records in the Wallingford version of Babelsberg, they will be aliased.
; So make a new symbolic var for both p and q.
(define (test12)
  (test-case
   "test12"
   (symbolic-struct rec ([x integer?] [y integer?]))
   (wally-clear)
   (wally-define-symbolic p q rec?)
   (wally-define-symbolic a integer?)
   (stay (rec-x p))
   (stay (rec-y p))
   (stay a)
   (wally-set! p (rec 2 5))
   (wally-set! a (rec-x p))
   (wally-set! q p)
   (check-equal? (evaluate (rec-x p)) 2)
   (check-equal? (evaluate p) (rec 2 5))
   (check-equal? (evaluate q) (rec 2 5))
   (check-equal? (evaluate a) 2)
   (always (equal? (rec-x p) 100))
   (always (equal? p q))
   (check-equal? (evaluate p) (rec 100 5))
   (always (equal? (rec-y q) 20))
   (check-equal? (evaluate p) (rec 100 20))
   (check-equal? (evaluate q) (rec 100 20))))

(define babelsberg-tests 
  (test-suite+ 
   "run tests for babelsberg extensions to wallingford"
   (initialize-then-update-test)
   (two-numbers-test)
   (two-points-test)
   (two-mutable-points-test)
   (eq-test)
   (multiple-eq-test)
   (eq-scope-test)
   (test1)
   (test4)
   (test5)
   (test12)
   ))

(printf "running babelsberg-tests\n")
(time (run-tests babelsberg-tests))
