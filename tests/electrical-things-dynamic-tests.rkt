#lang s-exp rosette

(require rackunit rackunit/text-ui rosette/lib/util/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/electrical-things-dynamic.rkt")

(provide electrical-things-dynamic-tests)

; the first tests are the same as without the ability to add and remove components dynamically
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
   (check equal? (evaluate (lead-current (battery-plus b))) 2)))

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
   (check equal? (evaluate (lead-current (battery-plus b))) 2)
   (check equal? (evaluate (node-voltage (lead-node (resistor-lead2 r1)))) 20)))

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
   (check equal? (evaluate (lead-current (battery-plus b))) 2)
   ; now solve for the resistance
   (assert (equal? (battery-internal-voltage b) 12))
   (assert (equal? (lead-current (battery-plus b)) 3))
   (wally-solve)
   (check equal? (evaluate (resistor-resistance r)) 4)))

(define (add-resistor-test)
  (test-case
   "start with battery-resistor-fixed-voltage-resistance-test, then dynamically change the circuit by adding another resistor"
   (wally-clear)
   (define b (make-battery 10))
   (define r1 (make-resistor 5))
   (define g (make-ground))
   (connect (list (battery-minus b) (resistor-lead2 r1) g))
   (connect (list (battery-plus b) (resistor-lead1 r1)))
   (wally-solve)
   (check equal? (evaluate (lead-current (battery-plus b))) 2)
   (define r2 (make-resistor 20))
   (connect (list (resistor-lead1 r1) (resistor-lead1 r2)))
   (connect (list (resistor-lead2 r1) (resistor-lead2 r2)))
   (wally-solve)
   (check equal? (evaluate (lead-current (battery-plus b))) 2.5)))

; what about changing the resistance, or adding a new resistor in parallel?

(define electrical-things-dynamic-tests 
  (test-suite+ 
   "run all electrical things tests"
   (battery-resistor-fixed-voltage-resistance-test)
   (voltage-divider-test)
   (battery-resistor-changing-voltage-resistance-test)
   (add-resistor-test)
   ))

(time (run-tests electrical-things-dynamic-tests))