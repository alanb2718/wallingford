#lang s-exp rosette
;; unit tests for 'while' in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide while-tests)

(define (one-while-no-interesting)
  (test-case
   "test advance time with one while and no interesting times identified"
   (define times '())
   (define (get-times) times)
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r advance-time 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send-syncd r evaluate-syncd get-times) '(50))
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send-syncd r evaluate-syncd get-times) '(75 50))
   (send-thing r advance-time 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send-syncd r evaluate-syncd get-times) '(100 75 50))
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-times) '(100 75 50))))
   
(define (one-while-no-interesting-hop-over)
  (test-case
   "similar to one-while-no-interesting but hop over the interval in which the while is true"
   (define times '())
   (define (get-times) times)
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-times) '())))
   
(define (one-while-soft)
  (test-case
   "test advance time with one while - detect the effect using a soft constraint that can be satisfied when the 'while' is off"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x integer?)
       (always (equal? x 1) #:priority low)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x 2)))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 1)
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send r get-x) 1)))
   
(define (one-while-track-times)
  (test-case
   "test advance time with one while, keeping track of the times at which the body of the while is evaluated"
   (define times '())
   (define (get-times) times)
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60.0)
   (check equal? (send-syncd r evaluate-syncd get-times) '(60 50))
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send-syncd r evaluate-syncd get-times) '(75 60 50))
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-times) '(100 75 60 50))))
   
(define (one-while-track-times-exact)
  (test-case
   "same as one-while-track-times, but hit the start and end times of the while exactly"
   (define times '())
   (define (get-times) times)
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r advance-time 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send-syncd r evaluate-syncd get-times) '(50))
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send-syncd r evaluate-syncd get-times) '(75 50))
   (send-thing r advance-time 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send-syncd r evaluate-syncd get-times) '(100 75 50))
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-times) '(100 75 50))))
   
(define (one-while-hop-over)
  (test-case
   "test advance time with one while - hop over the entire interval the condition is true and make sure its effect persists"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x integer?)
       (assert (equal? x 1))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x 2)))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 1)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   ; even though we hopped over 50<time<100 we still should have asserted x=2
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 180)
   (check equal? (send-syncd r milliseconds-syncd) 180)
   (check equal? (send r get-x) 2)))
   

(define while-tests 
  (test-suite+
   "run tests for while in reactive-thing"
   (one-while-no-interesting)
   (one-while-no-interesting-hop-over)
   (one-while-soft)
   (one-while-track-times)
   (one-while-track-times-exact)
   (one-while-hop-over)
   ))

(time (run-tests while-tests))
