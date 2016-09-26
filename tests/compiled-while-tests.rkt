#lang s-exp rosette
;; unit tests for compiled versions of 'while', based on while-tests.rkt

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")
(require "../compiled-reactive/compiled-reactive-thing.rkt")

(provide compiled-while-tests)

(define (one-while)
  (test-case
   "test advance time with one while and the start and end of the while identified as interesting times"
   (define tester%
     (class compiled-reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define x 5)
       ; (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
       ;       #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
       ;       (assert (equal? x (milliseconds))))
       (define/override (get-sampling)
         (if (and (>= (milliseconds) 50) (<= (milliseconds) 100))
             '(push pull)
             '(push)))
       (define/override (update-mysolution)
         (cond [(and (>= (milliseconds) 50) (<= (milliseconds) 100))
                (set! x (milliseconds))
                (send this notify-watchers-changed)]))
       (define/override (find-time mytime target)
         (cond [(and (< mytime 50) (> target 50)) 50]
               [(and (< mytime 100) (> target 100)) 100]
               [else target]))
       (define/public (get-x) x)))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))
   (check equal? (send r get-x) 60)
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send-syncd r get-sampling-syncd) '(push pull))
   (check equal? (send r get-x) 75)
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (check equal? (send r get-x) 100)))

(define (one-while-hop-over)
  (test-case
   "same as one-while-interesting, but hop over the whole interval"
   (define tester%
     (class compiled-reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define x 5)
       ; (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
       ;       #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
       ;       (assert (equal? x (milliseconds))))
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (cond [(and (>= (milliseconds) 50) (<= (milliseconds) 100))
                (set! x (milliseconds))]))
       (define/override (find-time mytime target)
         (cond [(and (< mytime 50) (> target 50)) 50]
               [(and (< mytime 100) (> target 100)) 100]
               [else target]))
       (define/public (get-x) x)))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (check equal? (send r get-x) 5)
   (send-thing r advance-time 150)
   (check equal? (send-syncd r milliseconds-syncd) 150)
   (check equal? (send-syncd r get-sampling-syncd) '(push))
   (check equal? (send r get-x) 100)))

;; compiled versions of these tests omitted, since the compiler would in effect be in charge
;; of synthesizing the interesting-time function
;;   one-while-hop-over-synthesize-interesting-time
;;   one-while-hop-over-synthesize-interesting-time-variant

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
     (class compiled-reactive-thing%
       (inherit milliseconds)
       (super-new)
       ; (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
       ;       #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 80))
       ;       (set! times (cons (send this wally-evaluate (milliseconds)) times)))))
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (cond [(and (>= (milliseconds) 50) (<= (milliseconds) 80))
                (set! times (cons (send this wally-evaluate (milliseconds)) times))]))
       (define/override (find-time mytime target)
         (cond [(and (< mytime 50) (> target 50)) 50]
               [(and (< mytime 80) (> target 80)) 80]
               [else target]))))
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


(define compiled-while-tests 
  (test-suite+
   "unit tests for compiled versions of while"
   (one-while)
   (one-while-hop-over)
   (one-while-track-times)
   ))

(time (run-tests compiled-while-tests))
