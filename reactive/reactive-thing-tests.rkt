#lang s-exp rosette
;; unit tests for reactive-thing%.  Run these from all-tests.rkt

(require rackunit rackunit/text-ui)
; (require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "reactive.rkt")

(provide reactive-thing-tests)

(define (advance-time-no-whens)
  (test-case
   "test advance time with no whens"
   (wally-clear)
   (define r (new reactive-thing%))
   (check-equal? (send-syncd r milliseconds-syncd) 0)
   (send-thing r advance-time 30)
   (check-equal? (send-syncd r milliseconds-syncd) 30)
   (send-thing r advance-time 2000)
   (check-equal? (send-syncd r milliseconds-syncd) 2000)))

(define (advance-time-one-when)
  (test-case
   "test advance time with one when"
   (wally-clear)
   (define r (new reactive-thing%))
   (define count 0)
   (define (get-count) count)
   (when (thing r) (equal? (send r milliseconds) 100)
     (set! count (+ 1 count)))
   (check-equal? (send-syncd r milliseconds-syncd) 0)
   (check-equal? (send-syncd r evaluate-syncd get-count) 0)
   
   (send-thing r advance-time 30)
   (check-equal? (send-syncd r milliseconds-syncd) 30)
   (check-equal? (send-syncd r evaluate-syncd get-count) 0)
   
   (send-thing r advance-time 200)
   (check-equal? (send-syncd r milliseconds-syncd) 200)
   (check-equal? (send-syncd r evaluate-syncd get-count) 1)
   
   (send-thing r advance-time 300)
   (check-equal? (send-syncd r milliseconds-syncd) 300)
   (check-equal? (send-syncd r evaluate-syncd get-count) 1)))

(define (advance-time-multiple-whens)
  (test-case
   "test advance time with multiple whens"
   (wally-clear)
   (define r (new reactive-thing%))
   (define times '())
   (define (get-times) times)
   
   (when (thing r) (equal? (send r milliseconds) 100)
     (set! times (cons (list "when 100" (evaluate (send r milliseconds))) times)))
   (when (thing r) (equal? (send r milliseconds) 200)
     (set! times (cons (list "when 200" (evaluate (send r milliseconds))) times)))
   (check-equal? (send-syncd r milliseconds-syncd) 0)
   (check-equal? (send-syncd r evaluate-syncd get-times) '())
   
   (send-thing r advance-time 30)
   (check-equal? (send-syncd r milliseconds-syncd) 30)
   (check-equal? (send-syncd r evaluate-syncd get-times) '())
   
   (send-thing r advance-time 150)
   (check-equal? (send-syncd r milliseconds-syncd) 150)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("when 100" 100)))
   
   (send-thing r advance-time 300)
   (check-equal? (send-syncd r milliseconds-syncd) 300)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("when 200" 200) ("when 100" 100)))))

(define (button-events)
  (test-case
   "test button event handling (just with reactive thing programatically, not with a viewer)"
   (wally-clear)
   (define r (new reactive-thing%))
   (define times '())
   (define (get-times) times)
   (when (thing r) (send r button-pressed)
     (set! times (cons (list "button" (evaluate (send r milliseconds))) times)))
   (send-thing r advance-time 50)
   (check-equal? (send-syncd r milliseconds-syncd) 50)
   (check-equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r button-down-event 100 0 0)
   (send-thing r button-down-event 200 0 0)
   (send-thing r advance-time 300)
   (check-equal? (send-syncd r milliseconds-syncd) 300)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   ))

(define (button-events-same-times)
  (test-case
   "test button event handling advancing to the same time as the button down"
   (wally-clear)
   (define r (new reactive-thing%))
   (define times '())
   (define (get-times) times)
   (when (thing r) (send r button-pressed)
     (set! times (cons (list "button" (evaluate (send r milliseconds))) times)))
   (send-thing r advance-time 50)
   (check-equal? (send-syncd r milliseconds-syncd) 50)
   (check-equal? (send-syncd r evaluate-syncd get-times) '())
   ; button down sent before advance time (but both to time 100)
   ; sending advance time first then button down doesn't work (should it?)
   (send-thing r button-down-event 100 0 0)
   (send-thing r advance-time 100)
   (check-equal? (send-syncd r milliseconds-syncd) 100)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("button" 100)))
   (send-thing r advance-time 150)
   (check-equal? (send-syncd r milliseconds-syncd) 150)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("button" 100)))
   (send-thing r button-down-event 200 0 0)
   (send-thing r advance-time 200)
   (check-equal? (send-syncd r milliseconds-syncd) 200)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   (send-thing r advance-time 250)
   (check-equal? (send-syncd r milliseconds-syncd) 250)
   (check-equal? (send-syncd r evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   ))

(define (sampling-tests)
  (test-case
   "test get-sampling"
   ;
   (wally-clear)
   (define r1 (new reactive-thing%))
   ; kind of a stupid always* constraint but it shouldn't require pull sampling
   (always* (equal? (send r1 image) null))
   (check-equal? (send r1 get-sampling) '())
   ;
   (wally-clear)
   (define test-always%
     (class reactive-thing%
       (inherit seconds image)
       (super-new [init-image (make-circle)])
       (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))))
   (define r2 (new test-always%))
   (check-equal? (send r2 get-sampling) '(pull))
   ;
   (wally-clear)
   (define r3 (new reactive-thing%))
   (when (thing r3) (send r3 button-pressed)
     (void))
   (check-equal? (send r3 get-sampling) '(push))
   (wally-clear)
   (define test-always-and-when%
     (class reactive-thing%
       (inherit seconds image)
       (super-new [init-image (make-circle)])
       (always* (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
       (when #f (void))))
   (define r4 (new test-always-and-when%))
   (check-equal? (send r4 get-sampling) '(push pull))
   ))

(define reactive-thing-tests 
  (test-suite 
   "run tests for reactive-thing"
   (advance-time-no-whens)
   (advance-time-one-when)
   (advance-time-multiple-whens)
   (button-events)
   (button-events-same-times)
   (sampling-tests)
   ))
