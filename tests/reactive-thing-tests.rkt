#lang s-exp rosette
;; basic unit tests for reactive-thing% (mouse, button, when, and while tests are in separate files)

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
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

(define (sampling-tests)
  (test-case
   "test get-sampling"
   (define r1 (new reactive-thing%))
   ; kind of a stupid always* constraint but it shouldn't require pull sampling
   (always* (equal? (send r1 image) null) #:owner r1)
   (check equal? (send r1 get-sampling) '())
   ;
   (define test-always%
     (class reactive-thing%
       (inherit seconds image)
       (super-new)
       (send this set-image! (make-circle this))
       (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
   (define r2 (new test-always%))
   (check equal? (send r2 get-sampling) '(pull))
   ;
   (define test-when%
     (class reactive-thing%
       (inherit seconds image)
       (super-new)
       (send this set-image! (make-circle this))
       (when (send r3 button-going-down?)
         (void))))
   (define r3 (new test-when%))
   (check equal? (send r3 get-sampling) '(push))
   (define test-always-and-when%
     (class reactive-thing%
       (inherit seconds image)
       (super-new)
       (send this set-image! (make-circle this))
       (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
       (when #f (void))))
   (define r4 (new test-always-and-when%))
   (check equal? (send r4 get-sampling) '(push pull))))

(define reactive-thing-tests 
  (test-suite+
   "run tests for reactive-thing"
   (advance-time-simple)
   (sampling-tests)
   ))

(time (run-tests reactive-thing-tests))
