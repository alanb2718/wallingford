#lang s-exp rosette
;; unit tests for reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/util/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide reactive-thing-tests)

(define (advance-time-no-whens)
  (test-case
   "test advance time with no whens"
   (define r (new reactive-thing%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (send-thing r advance-time 30)
   (check equal? (send-syncd r milliseconds-syncd) 30)
   (send-thing r advance-time 2000)
   (check equal? (send-syncd r milliseconds-syncd) 2000)))

(define (advance-time-one-when)
  (test-case
   "test advance time with one when"
   (define count 0)
   (define (get-count) count)
   (define one-when-tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (when (equal? (milliseconds) 100)
         (set! count (+ 1 count)))))
   
   (define r (new one-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   
   (send-thing r advance-time 30)
   (check equal? (send-syncd r milliseconds-syncd) 30)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)
   
   (send-thing r advance-time 300)
   (check equal? (send-syncd r milliseconds-syncd) 300)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)))

(define (advance-time-one-when-separate-var)
  (test-case
   "test advance time with one when, but with a separate var in the condition"
   (define count 0)
   (define (get-count) count)
   (define one-when-tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* m real?)
       (always* (equal? m (milliseconds)))
       (when (equal? m 100)
         (set! count (+ 1 count)))))
   
   (define r (new one-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   (send-thing r advance-time 30)
   (check equal? (send-syncd r milliseconds-syncd) 30)
   (check equal? (send-syncd r evaluate-syncd get-count) 0)
   
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)
   
   (send-thing r advance-time 300)
   (check equal? (send-syncd r milliseconds-syncd) 300)
   (check equal? (send-syncd r evaluate-syncd get-count) 1)
   ))

(define (advance-time-one-when-separate-var-soft-cn)
  (test-case
   "test advance time with one when, but with a separate var in the condition and a soft constraint"
   ; this becomes too hard for the solver to find a new time to advance to
   (define one-when-tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* m real?)
       (always* (equal? m (milliseconds)) #:priority low)
       (when (equal? m 100)
         #t)))
   (define r (new one-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   ; do the with-handlers call inside evaluate-syncd so that it is evaluated in the thing's thread
   (check eq? (send-syncd r evaluate-syncd (lambda () (with-handlers ([exn:fail? (lambda (exn) 'got-error)])
                                            (send r advance-time-helper 30))))
          'got-error)))

(define (advance-time-multiple-whens)
  (test-case
   "test advance time with multiple whens"
   (define times '())
   (define (get-times) times)
   (define multi-when-tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (when (equal? (milliseconds) 100)
         (set! times (cons (list "when 100" (send this wally-evaluate (send r milliseconds))) times)))
       (when (equal? (milliseconds) 200)
         (set! times (cons (list "when 200" (send this wally-evaluate (send r milliseconds))) times)))))
   
   (define r (new multi-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   
   (send-thing r advance-time 30)
   (check equal? (send-syncd r milliseconds-syncd) 30)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send-syncd r evaluate-syncd get-times) '(("when 100" 100)))
   
   (send-thing r advance-time 300)
   (check equal? (send-syncd r milliseconds-syncd) 300)
   (check equal? (send-syncd r evaluate-syncd get-times) '(("when 200" 200) ("when 100" 100)))))

(define (button-events)
  (test-case
   "test button event handling (just with reactive thing programatically, not with a viewer)"
   (define times '())
   (define (get-times) times)
   (define button-events-tester%
     (class reactive-thing%
       (inherit milliseconds button-pressed)
       (super-new)
       (when (button-pressed)
         (set! times (cons (list "button" (send this wally-evaluate (milliseconds))) times)))))
   (define r1 (new button-events-tester%))
   (send-thing r1 advance-time 50)
   (check equal? (send-syncd r1 milliseconds-syncd) 50)
   (check equal? (send-syncd r1 evaluate-syncd get-times) '())
   (send-thing r1 button-down-event 100 0 0)
   (send-thing r1 button-down-event 200 0 0)
   (send-thing r1 advance-time 300)
   (check equal? (send-syncd r1 milliseconds-syncd) 300)
   (check equal? (send-syncd r1 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   ; test button event handling advancing to the same time as the button down
   (set! times '())
   (define r2 (new button-events-tester%))
   (send-thing r2 advance-time 50)
   (check equal? (send-syncd r2 milliseconds-syncd) 50)
   (check equal? (send-syncd r2 evaluate-syncd get-times) '())
   ; button down sent before advance time (but both to time 100)
   ; sending advance time first then button down doesn't work (system isn't clairvoyant)
   (send-thing r2 button-down-event 100 0 0)
   (send-thing r2 advance-time 100)
   (check equal? (send-syncd r2 milliseconds-syncd) 100)
   (check equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 advance-time 150)
   (check equal? (send-syncd r2 milliseconds-syncd) 150)
   (check equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 button-down-event 200 0 0)
   (send-thing r2 advance-time 200)
   (check equal? (send-syncd r2 milliseconds-syncd) 200)
   (check equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   (send-thing r2 advance-time 250)
   (check equal? (send-syncd r2 milliseconds-syncd) 250)
   (check equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))))

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
       (when (send r3 button-pressed)
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
   (advance-time-no-whens)
   (advance-time-one-when)
   (advance-time-one-when-separate-var)
   (advance-time-one-when-separate-var-soft-cn)
   (advance-time-multiple-whens)
   (button-events)
   (sampling-tests)
   ))

(time (run-tests reactive-thing-tests))
