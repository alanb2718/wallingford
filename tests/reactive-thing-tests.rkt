#lang s-exp rosette
;; basic unit tests for reactive-thing% (sampling, mouse, button, when, and while tests are in separate files)

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../reactive/reactive.rkt")

(provide reactive-thing-tests)

(define (advance-time-simple)
  (test-case
   "test advance time with no whens or whiles"
   (define r (new reactive-thing%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (send-thing r advance-time 30)
   (check equal? (send-syncd r milliseconds-syncd) 30)
   (send-thing r advance-time 2000)
   (check equal? (send-syncd r milliseconds-syncd) 2000)))

(define reactive-thing-tests 
  (test-suite+
   "basic unit tests for reactive-thing%"
   (advance-time-simple)
   ))

(time (run-tests reactive-thing-tests))
