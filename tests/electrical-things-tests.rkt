#lang s-exp rosette

(require rackunit rackunit/text-ui)
(require "../core/wallingford.rkt")
(require "../applications/electrical-things.rkt")

(provide electrical-things-tests)

(define (battery-resistor-fixed-voltage-resistance-test)
  (test-case
   "test a circuit with a battery and a resistor with fixed voltage and resistance"
   (wally-clear)
   (define b (make-battery 10))
   (define r (make-resistor 5))
   (define g (make-ground))
   (connect (list (battery-minus b) (resistor-lead2 r) g))
   (connect (list (battery-plus b) (resistor-lead1 r)))
   (wally-solve)
   (check-equal? (evaluate (lead-current (battery-plus b))) 2)))

(define (voltage-divider-test)
  (test-case
   "test a voltage divider circuit"
   (wally-clear)
   (define b (make-battery 30))
   (define r1 (make-resistor 5))
   (define r2 (make-resistor 10))
   (define g (make-ground))
   (connect (list (battery-minus b) (resistor-lead2 r2) g))
   (connect (list (battery-plus b) (resistor-lead1 r1)))
   (connect (list (resistor-lead2 r1) (resistor-lead1 r2)))
   (wally-solve)
   (check-equal? (evaluate (lead-current (battery-plus b))) 2)
   (check-equal? (evaluate (lead-voltage (resistor-lead2 r1))) 20)))

(define (parallel-resistors-test)
  (test-case
   "two resistors in parallel"
   (wally-clear)
   (define b (make-battery 10))
   (define r1 (make-resistor 5))
   (define r2 (make-resistor 20))
   (define g (make-ground))
   (connect (list (battery-minus b) (resistor-lead2 r1) (resistor-lead2 r2) g))
   (connect (list (battery-plus b) (resistor-lead1 r1) (resistor-lead1 r2)))
   (wally-solve)
   (check-equal? (evaluate (lead-current (battery-plus b))) 2.5)))

(define (battery-resistor-changing-voltage-resistance-test)
  (test-case
   "test a circuit with a battery and a resistor with changing voltage and resistance"
   (wally-clear)
   (define b (make-battery))
   (define r (make-resistor))
   (define g (make-ground))
   (connect (list (battery-minus b) (resistor-lead2 r) g))
   (connect (list (battery-plus b) (resistor-lead1 r)))
   (assert (equal? (battery-internal-voltage b) 20))
   (assert (equal? (resistor-resistance r) 10))
   (wally-solve)
   (check-equal? (evaluate (lead-current (battery-plus b))) 2)
   ; now solve for the resistance
   (assert (equal? (battery-internal-voltage b) 12))
   (assert (equal? (lead-current (battery-plus b)) 3))
   (wally-solve)
   (check-equal? (evaluate (resistor-resistance r)) 4)))

(define electrical-things-tests 
  (test-suite 
   "run all electrical things tests"
   (battery-resistor-fixed-voltage-resistance-test)
   (voltage-divider-test)
   (printf "skipping parallel-resistors-test (probably needs support for reals to pass)\n")
   ; (parallel-resistors-test)
   (printf "skipping battery-resistor-changing-voltage-resistance-test (probably needs support for reals to pass)\n")
   ; (battery-resistor-changing-voltage-resistance-test)
   ))
