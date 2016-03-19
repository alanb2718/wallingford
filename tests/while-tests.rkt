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
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       ; To detect times the while fires, we have a variable x that is asserted to be equal to (milliseconds)
       ; in the body of the while.  The stay causes it to remain at that value afterward.
       (define-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              (assert (equal? x (milliseconds))))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)  ; x has its original value
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send r get-x) 60)
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send r get-x) 75)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send r get-x) 75)))  ; x stays at 75 since we don't fire the while at 200
   
(define (one-while-no-interesting-hop-over)
  (test-case
   "similar to one-while-no-interesting but hop over the interval in which the while is true"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              (assert (equal? x (milliseconds))))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send r get-x) 5)))
   
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
   
(define (one-while-interesting)
  (test-case
   "test advance time with one while and the start and end of the while identified as interesting times"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x (milliseconds))))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send r get-x) 60)
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send r get-x) 75)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send r get-x) 100)))

(define (one-while-interesting-hop-over)
  (test-case
   "test advance time with one while and the start and end of the while identified as interesting times, hopping over the whole interval"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x (milliseconds))))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send r get-x) 100)))

(define while-tests 
  (test-suite+
   "run tests for while in reactive-thing"
   (one-while-no-interesting)
   (one-while-no-interesting-hop-over)
   (one-while-soft)
   (one-while-interesting)
   (one-while-interesting-hop-over)
   ))

(time (run-tests while-tests))
