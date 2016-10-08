#lang s-exp rosette
;; unit tests for integral using a numeric solution
;; These are like integral-symbolic-tests, except for the first one (integral-in-always), which has some additional time advances
;; to exercise that part of the code.

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide integral-numeric-tests)

; helper function to test for approximate equality (pretty coarse for integration tests)
(define (approx-equal? x y)
  (or (and (zero? x) (zero? y))
      (< (abs (- x y)) 0.01)))

(define (integral-in-always)
  (test-case
   "test call to integral in an always"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y real?)
       (always (equal? x (integral 2 #:numeric)))
       (always (equal? y (integral (milliseconds) #:numeric)))
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 4)
   (check equal? (send-syncd r milliseconds-syncd) 4)
   (check equal? (send r get-x) 8)
   (check equal? (send r get-y) 8)
   (send-syncd r advance-time-syncd 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 100)
   (check equal? (send r get-y) 1250)
   ))

(define (integral-in-simple-while-hit-start)
  (test-case
   "test calls to integral in a simple while that is true for one time interval (and happen to get time when the interval begins)"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 110))
              (assert (equal? x (integral 2 #:numeric))))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 5)
   (check equal? (send r get-x) 0) ; outside of while at this time, so x should have its initial value
   (send-syncd r advance-time-syncd 34)
   (check equal? (send r get-x) 0) ; still outside of while
   (send-syncd r advance-time-syncd 42)
   (check equal? (send r get-x) 0) ; still outside of while
   (send-syncd r advance-time-syncd 50)  ; while should now be active (but just starting)
   (check equal? (send r get-x) 0)
   (send-syncd r advance-time-syncd 60)  ; while still active
   (check equal? (send r get-x) 20)
   (send-syncd r advance-time-syncd 75)
   (check equal? (send r get-x) 50)
   (send-syncd r advance-time-syncd 200) ; while becomes inactive at 110
   (check equal? (send r get-x) 120)
   (send-syncd r advance-time-syncd 350)
   (check equal? (send r get-x) 120)))

(define (integral-in-simple-while-miss-start)
  (test-case
   "test calls to integral in a simple while that is true for one time interval (but miss the time the interval begins)"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              (assert (equal? x (integral 2 #:numeric))))))
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
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       ; the while condition holds for the first 10 milliseconds of every 100 millisecond interval
       (while (<= (remainder (milliseconds) 100) 10)
              #:interesting-time (let ([r (remainder (milliseconds) 100)])
                                   (cond [(zero? r) 'first]
                                         [(equal? r 10) 'last]
                                         [else #f]))
              (assert (equal? x (integral 2  #:numeric))))))
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
   "test call to integral with an explicit variable of integration"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds)
       (super-new)
       (define-public-symbolic* s x1 x2 y1 y2 z1 z2 real?)
       (always (equal? s (* 2 (milliseconds))))
       (always (equal? x1 (integral 2 #:var (milliseconds) #:numeric)))
       (always (equal? x2 (integral (milliseconds) #:var (milliseconds) #:numeric)))
       (always (equal? y1 (integral 2 #:var (seconds) #:numeric)))
       (always (equal? y2 (integral (seconds) #:var (seconds) #:numeric)))
       (always (equal? z1 (integral 2 #:var s #:numeric)))
       (always (equal? z2 (integral s #:var s #:numeric)))))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send r get-x1) 200)
   (check equal? (send r get-x2) 5000)
   (check-true (approx-equal? (send r get-y1) 0.2))
   ; or this version works too, but not an equal? test with 0.2 (since y1 will be an exact number rather than a float)
   ; (check equal? (send r get-y1) 1/5)
   (check-true (approx-equal? (send r get-y2) 0.005))
   (check equal? (send r get-z1) 400)
   (check equal? (send r get-z2) 20000)))

(define (integral-sin-milliseconds)
  (test-case
   "test call to integral with a nonlinear expression: (sin (milliseconds-evaluated))"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (always (equal? x (integral (sin (milliseconds-evaluated)) #:numeric #:dt 0.1)))
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 0.6)
   (check approx-equal? (send-syncd r milliseconds-syncd) 0.6)
   ; (- (- (cos 0.6)) (- (cos 0.0))) is the symbolic solution
   (check approx-equal? (send r get-x) (- (- (cos 0.6)) (- (cos 0.0))))
   (send-syncd r advance-time-syncd 3.0)
   (check equal? (send-syncd r milliseconds-syncd) 3.0)
   (check approx-equal? (send r get-x) (- (- (cos 3.0)) (- (cos 0.0))))
   ))

(define (integral-sin-milliseconds-one-thousandth)
  (test-case
   "test call to integral with another nonlinear expression: (sin (* 0.001 (milliseconds-evaluated)))"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x real?)
       (always (equal? x (integral (sin (* 0.001 (milliseconds-evaluated))))))
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 500)
   (check approx-equal? (send-syncd r milliseconds-syncd) 500)
   (check approx-equal? (send r get-x) (- 1000.0 (* 1000.0 (cos 0.5))))
   (send-syncd r advance-time-syncd 1000)
   (check approx-equal? (send r get-x) (- 1000.0 (* 1000.0 (cos 1.0))))
   ))

(define (integral-sin-seconds)
  (test-case
   "test call to integral with another nonlinear expression: (sin (seconds))"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds seconds)
       (super-new)
       (define-public-symbolic* x real?)
       (always (equal? x (integral (sin (seconds)) #:var (seconds))))
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 500)
   (check approx-equal? (send-syncd r milliseconds-syncd) 500)
   (check approx-equal? (send r get-x) (- (- (cos 0.5)) (- (cos 0.0))))
   (send-syncd r advance-time-syncd 1200)
   (check equal? (send-syncd r milliseconds-syncd) 1200)
   (check approx-equal? (send r get-x) (- (- (cos 1.2)) (- (cos 0.0))))
   ))

(define (integral-multiple-dts)
  (test-case
   "test call to integrals with multiple values for dt"
   ; this test uses a hack (side effect in the integral expression) to test that we advance to the correct times
   (define tester%
     (class reactive-thing%
       (inherit milliseconds milliseconds-evaluated)
       (super-new)
       (define-public-symbolic* x y z real?)
       (define times (mutable-set))
       (define/public (get-times) times)
       (always (equal? x (integral (begin (set-add! times (milliseconds-evaluated)) 2) #:numeric #:dt 7)))
       (always (equal? y (integral (begin (set-add! times (milliseconds-evaluated)) 3) #:numeric #:dt 13)))
       (always (equal? z (integral (begin (set-add! times (milliseconds-evaluated)) 5) #:numeric)))  ; uses default of 10
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 40)
   (check equal? (send-syncd r milliseconds-syncd) 40)
   (check equal? (send r get-x) 80)
   (check equal? (send r get-y) 120)
   (check equal? (send r get-z) 200)
   (check equal? (send r get-times) (list->mutable-set '(0.0 7.0 14.0 21.0 28.0 35.0 40.0)))
   ))

(define integral-numeric-tests 
  (test-suite+
   "unit tests for integral using a numeric solution"
   (integral-in-always)
   (integral-in-simple-while-hit-start)
   (integral-in-simple-while-miss-start)
   (integral-in-repeating-while)
   (integral-with-explict-variable-of-integration)
   (integral-sin-milliseconds)
   (integral-sin-milliseconds-one-thousandth)
   (integral-sin-seconds)
   (integral-multiple-dts)
   ))

(time (run-tests integral-numeric-tests))
