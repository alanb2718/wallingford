#lang s-exp rosette
;; unit tests for 'while' in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide while-tests)

(define (one-while)
  (test-case
   "test advance time with one while and the start and end of the while identified as interesting times"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x (milliseconds))))))
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

(define (one-while-hop-over)
  (test-case
   "same as one-while, but hop over the whole interval"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send r get-x) 100)))

(define (one-while-hop-over-synthesize-interesting-time)
  (test-case
   "same as one-while-hop-over, but let the system synthesize the #:interesting-time function"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (<= 50 (milliseconds)) (<= (milliseconds) 100))
              (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send r get-x) 100)))

(define (one-while-hop-over-synthesize-interesting-time-variant)
  (test-case
   "same as one-while-hop-over-synthesize-interesting-time except for a slightly different test"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (and (<= 50 (milliseconds)) (>= 100 (milliseconds)))
              (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send r get-x) 100)))

(define (too-hard-to-synthesize-interesting-time)
  (test-case
   "a while with a condition that is too hard for the system to synthesize the #:interesting-time function"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (+ 1 (milliseconds)) 100))
              (assert #t))))
   (check-exn
    exn:fail?
    (lambda () (new tester%)))))

(define (one-while-soft)
  (test-case
   "test advance time with one while - detect the effect using a soft constraint that can be satisfied when the 'while' is off"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x integer?)
       (always (equal? x 1) #:priority low)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x 2)))))
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

(define (one-while-button-pressed-synthesize-interesting-time)
  (test-case
   "let the system synthesize the #:interesting-time function for a button-pressed? test"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (assert (equal? x 5))
       (send this solve)
       (stay x)
       (while (button-pressed?)
              (assert (equal? x (milliseconds))))))
   (define r (new tester%))
   (send-thing r mouse-event   0 0 0 'up)
   (send-thing r mouse-event 100 0 0 'going-down)
   (send-thing r mouse-event 110 0 0 'down)
   (send-thing r mouse-event 200 0 0 'going-up)
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 120)
   (check equal? (send-syncd r milliseconds-syncd) 120)
   (check equal? (send r get-x) 120)
   (send-thing r advance-time 500)
   (check equal? (send-syncd r milliseconds-syncd) 500)
   (check equal? (send r get-x) 200)))

(define (one-while-track-times)
  (test-case
   "test advance time, keeping track of the times at which the body of the while is evaluated using a hack"
   ; CAUTION! This unit test checks the exact times the while is evaluated with a series of advance-time calls,
   ; including ones that exactly hit the interesting times and ones that go over an interesting time.  However,
   ; it does so by using a side effect in the 'while' (which formally isn't allowed), and further, the
   ; interesting-times are arbitrarily chosen so that we can test the functionality - but they don't make sense
   ; in terms of times we'd actually need to stop to account for the effects of the 'while'.
   (define times '())
   (define (get-times) times)
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 80))
              (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r evaluate-syncd get-times) '())
   (send-thing r advance-time 50) ; should advance to 50
   (check equal? (send-syncd r milliseconds-syncd) 50.0)
   (check equal? (send-syncd r evaluate-syncd get-times) '(50))
   (send-thing r advance-time 75) ; should advance to 75
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send-syncd r evaluate-syncd get-times) '(75 50))
   (send-thing r advance-time 200) ; should advance to 80 then 200
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r evaluate-syncd get-times) '(80 75 50))))


(define while-tests 
  (test-suite+
   "unit tests for while in reactive-thing%"
   (one-while)
   (one-while-hop-over)
   (one-while-hop-over-synthesize-interesting-time)
   (one-while-hop-over-synthesize-interesting-time-variant)
   (too-hard-to-synthesize-interesting-time)
   (one-while-soft)
   (one-while-button-pressed-synthesize-interesting-time)
   (one-while-track-times)
   ))

(time (run-tests while-tests))
