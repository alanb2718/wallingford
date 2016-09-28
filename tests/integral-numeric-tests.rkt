#lang s-exp rosette
;; unit tests for integral using a numeric solution

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide integral-numeric-tests)

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
       (define-public-symbolic* x y real?)
       (always (equal? x (integral 2 #:numeric)))
      ; (always (equal? y (integral (milliseconds) #:numeric)))
       ))
   (define r (new tester%))
   (send r start)
   (send-syncd r advance-time-syncd 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 20)
  ; (check equal? (send r get-y) 50)
   (send-syncd r advance-time-syncd 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 100)
  ; (check equal? (send r get-y) 1250)
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
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              (assert (equal? x (integral 2))))))
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
       (define-public-symbolic* x real?)
       (assert (equal? x 0))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              (assert (equal? x (integral 2))))))
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
              (assert (equal? x (integral 2))))))
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
       (always (equal? x1 (integral 2 #:var (milliseconds))))
       (always (equal? x2 (integral (milliseconds) #:var (milliseconds))))
       (always (equal? y1 (integral 2 #:var (seconds))))
       (always (equal? y2 (integral (seconds) #:var (seconds))))
       (always (equal? z1 (integral 2 #:var s)))
       (always (equal? z2 (integral s #:var s)))))
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


(define integral-numeric-tests 
  (test-suite+
   "unit tests for integral using a numeric solution"
   (integral-in-always)
;   (integral-in-simple-while-hit-start)
;   (integral-in-simple-while-miss-start)
;   (integral-in-repeating-while)
;   (integral-with-explict-variable-of-integration)
   ))

(time (run-tests integral-numeric-tests))
