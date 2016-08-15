#lang s-exp rosette
;; unit tests for integral in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide integral-tests)

; The only tests with various different expressions to integrate are integral-in-always and integral-with-explict-variable-of-integration.
; The other tests just use a constant (because arguably the other functionality being tested is independent of the symbolic integration function).

; helper function to test for approximate equality
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) 1e-5)))

(define (integral-in-always)
  (test-case
   "test call to integral in an always"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x y real?)
       (always (equal? x (integral 2)))
       (always (equal? y (integral (milliseconds))))
       (define/public (get-x) (send this wally-evaluate x))
       (define/public (get-y) (send this wally-evaluate y))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 20)
   (check equal? (send r get-y) 50)
   (send-syncd r advance-time-syncd 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 100)
   (check equal? (send r get-y) 1250)))

(define (integral-in-simple-while-hit-start)
  (test-case
   "test calls to integral in a simple while that is true for one time interval (and happen to get time when the interval begins)"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              (assert (equal? x (integral 2))))
       (define/public (get-x) (send-syncd this evaluate-syncd (lambda () (send this wally-evaluate x))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 5)
   (check equal? (send r get-x) 0) ; outside of while at this time, so x should have its initial value
   (send-syncd r advance-time-syncd 10)  ; while should now be active (but just starting)
   (check equal? (send r get-x) 0)
   (send-syncd r advance-time-syncd 60)  ; while still active
   (check equal? (send r get-x) 100)
   (send-syncd r advance-time-syncd 75)
   (check equal? (send r get-x) 130)
   (send-syncd r advance-time-syncd 200) ; while becomes inactive at 100
   (check equal? (send r get-x) 180)))

(define (integral-in-simple-while-miss-start)
  (test-case
   "test calls to integral in a simple while that is true for one time interval (but miss the time the interval begins)"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              (assert (equal? x (integral 2))))
       (define/public (get-x) (send-syncd this evaluate-syncd (lambda () (send this wally-evaluate x))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 5)
   (check equal? (send r get-x) 0)
   (send-syncd r advance-time-syncd 60)
   (check equal? (send r get-x) 100)
   (send-syncd r advance-time-syncd 75)
   (check equal? (send r get-x) 130)
   (send-syncd r advance-time-syncd 200) ; while becomes inactive at 100
   (check equal? (send r get-x) 180)))

(define (integral-in-repeating-while)
  (test-case
   "test calls to integral in a while that is true for multiple time intervals"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       ; the while condition holds for the first 10 milliseconds of every 100 millisecond interval
       (while (<= (remainder (milliseconds) 100) 10)
              #:interesting-time (let ([r (remainder (milliseconds) 100)])
                                   (cond [(zero? r) 'first]
                                         [(equal? r 10) 'last]
                                         [else #f]))
              (assert (equal? x (integral 2))))
       (define/public (get-x) (send-syncd this evaluate-syncd (lambda () (send this wally-evaluate x))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 5)
   (check equal? (send r get-x) 10)
   (send-syncd r advance-time-syncd 10)
   (check equal? (send r get-x) 20)
   (send-syncd r advance-time-syncd 50)
   (check equal? (send r get-x) 20)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send r get-x) 0)
   (send-syncd r advance-time-syncd 105)
   (check equal? (send r get-x) 10)
   (send-syncd r advance-time-syncd 150)
   (check equal? (send r get-x) 20)))

(define (integral-with-explict-variable-of-integration)
  (test-case
   "test call to integral in an always"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds)
       (super-new)
       (define-symbolic* s x1 x2 y1 y2 z1 z2 real?)
       (always (equal? s (* 2 (milliseconds))))
       (always (equal? x1 (integral 2 (milliseconds))))
       (always (equal? x2 (integral (milliseconds) (milliseconds))))
       (always (equal? y1 (integral 2 (seconds))))
       (always (equal? y2 (integral (seconds) (seconds))))
       (always (equal? z1 (integral 2 s)))
       (always (equal? z2 (integral s s)))
       (define/public (get-s) (send this wally-evaluate s))
       (define/public (get-x1) (send this wally-evaluate x1))
       (define/public (get-x2) (send this wally-evaluate x2))
       (define/public (get-y1) (send this wally-evaluate y1))
       (define/public (get-y2) (send this wally-evaluate y2))
       (define/public (get-z1) (send this wally-evaluate z1))
       (define/public (get-z2) (send this wally-evaluate z2))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send r get-x1) 200)
   (check equal? (send r get-x2) 5000)
   (check-true (approx-equal? (send r get-y1) 0.2))
   ; or this version works too, but not an exact test with 0.2
   ; (check equal? (send r get-y1) 1/5)
   (check-true (approx-equal? (send r get-y2) 0.005))
   (check equal? (send r get-z1) 400)
   (check equal? (send r get-z2) 20000)))


(define integral-tests 
  (test-suite+
   "run tests for integral in reactive-thing"
   (integral-in-always)
   (integral-in-simple-while-hit-start)
   (integral-in-simple-while-miss-start)
   (integral-in-repeating-while)
   (integral-with-explict-variable-of-integration)
   ))

(time (run-tests integral-tests))
