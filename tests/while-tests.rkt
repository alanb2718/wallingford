#lang s-exp rosette
;; unit tests for 'while' in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide while-tests)

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
   "same as one-while-interesting, but hop over the whole interval"
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

(define while-tests 
  (test-suite+
   "run tests for while in reactive-thing"
   (one-while-interesting)
   (one-while-interesting-hop-over)
   (one-while-soft)
   ))

(time (run-tests while-tests))
