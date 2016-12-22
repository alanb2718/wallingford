#lang s-exp rosette
;; sampling tests for reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide sampling-tests)

(define (sampling-static)
  (test-case
   "test get-sampling for static thing"
   (define r1 (new reactive-thing%))
   ; kind of a stupid always constraint but it shouldn't require pull sampling
   (always (equal? (send r1 image) null) #:owner r1)
   (check equal? (send r1 get-sampling) '())))

(define (sampling-temporal-always)
  (test-case
   "test get-sampling for temporal always constraint"
   (define test-always%
     (class reactive-thing%
       (inherit seconds image)
       (super-new)
       (send this set-image! (make-circle this))
       (always (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
   (define r1 (new test-always%))
   (check equal? (send r1 get-sampling) '(pull))))

(define (sampling-with-when)
  (test-case
   "test get-sampling with a when constraint"
   (define test-when%
     (class reactive-thing%
       (inherit seconds image button-going-down?)
       (super-new)
       (send this set-image! (make-circle this))
       (when (button-going-down?)
         (void))))
   (define r1 (new test-when%))
   (check equal? (send r1 get-sampling) '(push))))

(define (sampling-with-always-and-when)
  (test-case
   "test get-sampling with an always and a when constraint"
   (define test-always-and-when%
     (class reactive-thing%
       (inherit seconds image)
       (super-new)
       (send this set-image! (make-circle this))
       (always (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
       (when #f (void))))
   (define r1 (new test-always-and-when%))
   (check equal? (send r1 get-sampling) '(push pull))))

(define (sampling-with-while)
  (test-case
   "test get-sampling with a while constraint - should dynamically change from () to (pull) and back to ()"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* x real?)
       (while (and (<= 50 (milliseconds)) (<= (milliseconds) 100))
         (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   ; need to send get-sampling syncd -- otherwise the thread will likely lag behind
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (send-thing r advance-time 50)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))
   (send-thing r advance-time 70)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))
   (send-thing r advance-time 200)
   (check equal? (send-syncd r get-sampling-syncd) '(push))))

(define (sampling-with-while-and-button-press)
  (test-case
   "test get-sampling with a while constraint whose test is button-pressed?"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds button-pressed?)
       (super-new)
       (define-symbolic* x real?)
       (while (button-pressed?)
         (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send-thing r mouse-event   0 0 0 'up)
   (send-thing r mouse-event 100 0 0 'going-down)
   (send-thing r mouse-event 110 0 0 'down)
   (send-thing r mouse-event 200 0 0 'going-up)
   (send-thing r advance-time 10)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (send-thing r advance-time 120)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))
   (send-thing r advance-time 500)
   (check equal? (send-syncd r milliseconds-syncd) 500)
   (check equal? (send-syncd r get-sampling-syncd) '(push))))

(define (sampling-with-while-always-true)
  (test-case
   "test get-sampling with a while constraint whose test is just #t"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds button-pressed?)
       (super-new)
       (define-symbolic* x real?)
       (while #t
         #:interesting-time (if (equal? (milliseconds) 0) 'first #f)
         (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   ; Hack: call get-sampling when time is 0 so that the reactive-thing knows that
   ; pull sampling should be active at time 0.  (Also see the comment in the get-sampling
   ; method in reactive-thing.) This really ought to be done automatically as part of
   ; initializing the new reactive-thing instance but there is an ordering problem, since
   ; we want to do this *after* the while constraints are declared.
   (send r get-sampling)
   (send-thing r advance-time 10)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))))



(define sampling-tests 
  (test-suite+
   "unit tests for sampling in reactive-thing%"
   (sampling-static)
   (sampling-temporal-always)
   (sampling-with-when)
   (sampling-with-always-and-when)
   (sampling-with-while)
   (sampling-with-while-and-button-press)
   (sampling-with-while-always-true)
   ))

(time (run-tests sampling-tests))
