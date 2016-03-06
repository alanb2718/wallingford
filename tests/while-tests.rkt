#lang s-exp rosette
;; unit tests for 'while' in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide while-tests)

(define (one-while)
  (test-case
   "test advance time with one while - don't exactly hit the time the condition changes"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x integer?)
       (always (equal? x 1) #:priority low)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
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
   
(define (one-while-exact)
  (test-case
   "test advance time with one while - do exactly hit the time the condition changes"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x integer?)
       (always (equal? x 1) #:priority low)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              (assert (equal? x 2)))
       (define/public (get-x) (send this wally-evaluate x))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 1)
   (send-thing r advance-time 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 100)
   (check equal? (send-syncd r milliseconds-syncd) 100)
   (check equal? (send r get-x) 2)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send r get-x) 1)))

(define while-tests 
  (test-suite+
   "run tests for while in reactive-thing"
   (one-while)
   (one-while-exact)
   ))

(time (run-tests while-tests))
