#lang s-exp rosette
;; unit tests for integral in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide integral-tests)

(define (integral-in-always)
  (test-case
   "test call to integral in an always"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (always (equal? x (integral 2)))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send r initialize)
   (send-syncd r advance-time-syncd 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 20)
   (send-syncd r advance-time-syncd 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 100)))

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
   (send r initialize)
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
   (send r initialize)
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
   (send r initialize)
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

(define integral-tests 
  (test-suite+
   "run tests for integral in reactive-thing"
   (integral-in-always)
   (integral-in-simple-while-hit-start)
   (integral-in-simple-while-miss-start)
   (integral-in-repeating-while)
   ))

(time (run-tests integral-tests))
