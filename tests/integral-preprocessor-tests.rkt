#lang s-exp rosette
;; unit tests for integration preprocessor

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../reactive/integral-preprocessor.rkt")

(provide integral-preprocessor-tests)

; The only tests with various different expressions to integrate are integral-in-always and integral-with-explict-variable-of-integration.
; The other tests just use a constant (because arguably the other functionality being tested is independent of the symbolic integration function).

(define (atomic-expression-tests)
  (test-case
   "simple cases for symbolic integration -- expression is atomic"
   (check equal? (symbolic-integral 2 'x) '(* 2 x))
   (check equal? (symbolic-integral 'x 'x) '(* 0.5 (expt x 2)))
   ; case of (milliseconds) -- variable of integration is a function call (kind of funky - maybe fix this later)
   (check equal? (symbolic-integral 2 '(milliseconds)) '(* 2 (milliseconds)))
   (check equal? (symbolic-integral '(milliseconds) '(milliseconds)) '(* 0.5 (expt (milliseconds) 2)))))

(define (compound-expression-tests)
  (test-case
   "integral of more complex expressions"
   (check equal? (symbolic-integral '(+ x 3) 'x) '(+ (* 0.5 (expt x 2)) (* 3 x)))
   (check equal? (symbolic-integral '(+ 3 x) 'x) '(+ (* 3 x) (* 0.5 (expt x 2))))
   (check equal? (symbolic-integral '(* x 3) 'x) '(* 3 (* 0.5 (expt x 2))))
   (check equal? (symbolic-integral '(* 3 x) 'x) '(* 3 (* 0.5 (expt x 2))))
   (check equal? (symbolic-integral '(/ x 3) 'x) '(/ (* 0.5 (expt x 2)) 3))
   (check equal? (symbolic-integral '(expt x 3)'x) '(/ (expt x 4) 4))
   ))

(define (double-triple-integral-tests)
  (test-case
   "double and triple integrals"
   (check equal? (symbolic-integral '(integral 10) 'x) '(* 10 (* 0.5 (expt x 2))))
   (check equal? (symbolic-integral '(integral x) 'x) '(* 0.5 (/ (expt x 3) 3)))
   (check equal? (symbolic-integral '(integral (integral 10)) 'x) '(* 10 (* 0.5 (/ (expt x 3) 3))))
   ))

(define (too-difficult-expression-test)
  (test-case
   "expressions that are too difficult for the symbolic integrator - these should return #f"
   (check-false (symbolic-integral '(sin x) 'x))
   (check-false (symbolic-integral '(* x x) 'x))  ; needs to use expt
   (check-false (symbolic-integral '(/ 2 x) 'x))
   ))

(define integral-preprocessor-tests 
  (test-suite+
   "unit tests for symbolic integration function"
   (atomic-expression-tests)
   (compound-expression-tests)
   (double-triple-integral-tests)
   (too-difficult-expression-test)
   ))

(time (run-tests integral-preprocessor-tests))
