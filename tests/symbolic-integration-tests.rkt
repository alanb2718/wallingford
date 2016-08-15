#lang s-exp rosette
;; unit tests for symbolic-integration helper function

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../reactive/symbolic-integration.rkt")

(provide symbolic-integration-tests)

; The only tests with various different expressions to integrate are integral-in-always and integral-with-explict-variable-of-integration.
; The other tests just use a constant (because arguably the other functionality being tested is independent of the symbolic integration function).

(define (atomic-expression-tests)
  (test-case
   "simple cases for symbolic integration -- expression is atomic"
   (check equal? (symbolic-integral 2 'x) '(* 2 x))
   (check equal? (symbolic-integral 'x 'x) '(* 0.5 x x))
   ; case of (milliseconds) -- variable of integration is a function call (kind of funky - maybe fix this later)
   (check equal? (symbolic-integral 2 '(milliseconds)) '(* 2 (milliseconds)))
   (check equal? (symbolic-integral '(milliseconds) '(milliseconds)) '(* 0.5 (milliseconds) (milliseconds)))))

(define (compound-expression-tests)
  (test-case
   "integral of more complex expressions"
   (check equal? (symbolic-integral '(+ x 3) 'x) '(+ (* 0.5 x x) (* 3 x)))
   (check equal? (symbolic-integral '(+ 3 x) 'x) '(+ (* 3 x) (* 0.5 x x)))
   (check equal? (symbolic-integral '(* x 3) 'x) '(* 3 (* 0.5 x x) ))
   (check equal? (symbolic-integral '(* 3 x) 'x) '(* 3 (* 0.5 x x) ))
   (check equal? (symbolic-integral '(expt x 3)'x) '(/ (expt x 4) 4))
   ))

(define (too-difficult-expression-test)
  (test-case
   "an expression that is too difficult"
   (check-exn exn:fail? (lambda () (symbolic-integral '(sin x) 'x)))))

(define symbolic-integration-tests 
  (test-suite+
   "run tests for symbolic integration function"
   (atomic-expression-tests)
   (compound-expression-tests)
   (too-difficult-expression-test)
   ))

(time (run-tests symbolic-integration-tests))
