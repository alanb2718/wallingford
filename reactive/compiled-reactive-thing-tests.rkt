#lang s-exp rosette
;; unit tests for compiled-reactive-thing% -- these should be parallel to the tests for reactive-thing%

(require rackunit rackunit/text-ui)
; (require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")

(provide compiled-reactive-thing-tests)

(define (advance-time-no-whens)
  (test-case
   "test advance time with no whens"
   (wally-clear)
   (define no-constraints-tester%
     (class compiled-reactive-thing%
       (super-new)
       (define/override (get-sampling)
         '())
       (define/override (update-mysolution)
         (void))
       (define/override (find-time mytime target)
         target)))
   (define r (new no-constraints-tester%))
   (check-equal? (send-syncd r milliseconds-syncd) 0)
   (send-thing r advance-time 30)
   (check-equal? (send-syncd r milliseconds-syncd) 30)
   (send-thing r advance-time 2000)
   (check-equal? (send-syncd r milliseconds-syncd) 2000)))

(define (advance-time-one-when)
  (test-case
   "test advance time with one when"
   (wally-clear)
   (define count 0)
   (define (get-count) count)
   (define one-when-tester%
     (class compiled-reactive-thing%
       (super-new)
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (if (equal? (send this milliseconds) 100)
             (set! count (+ 1 count))
             (void)))
       (define/override (find-time mytime target)
         (if (and (< mytime 100) (> target 100)) 100 target))))
   
   (define r (new one-when-tester%))
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
   (define times '())
   (define (get-times) times)
   (define multi-when-tester%
     (class compiled-reactive-thing%
       (super-new)
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (let ([now (send this milliseconds)])
           (cond [(equal? now 100)
                  (set! times (cons (list "when 100" now) times))]
                 [(equal? now 200)
                  (set! times (cons (list "when 200" now) times))]
                 [else (void)])))
       (define/override (find-time mytime target)
         (cond [(and (< mytime 100) (> target 100)) 100]
               [(and (< mytime 200) (> target 200)) 200]
               [else target]))))
   
   (define r (new multi-when-tester%))
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
   (define times '())
   (define (get-times) times)
   ; compiling for this constraint:
   ;   (when (send r button-pressed)
   ;      (set! times (cons (list "button" (evaluate (send r milliseconds))) times)))))
   (define button-events-tester%
     (class compiled-reactive-thing%
       (inherit milliseconds button-pressed)
       (super-new)
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (cond [(button-pressed)
                (set! times (cons (list "button" (milliseconds)) times))]))
       (define/override (find-time mytime target)
         (let ([potential-targets (filter (lambda (t) (and (> t mytime) (< t target)))
                                          (send this get-button-down-event-times))])
           (if (null? potential-targets) target (apply min potential-targets))))))
   (define r1 (new button-events-tester%))
   (send-thing r1 advance-time 50)
   (check-equal? (send-syncd r1 milliseconds-syncd) 50)
   (check-equal? (send-syncd r1 evaluate-syncd get-times) '())
   (send-thing r1 button-down-event 100 0 0)
   (send-thing r1 button-down-event 200 0 0)
   (send-thing r1 advance-time 300)
   (check-equal? (send-syncd r1 milliseconds-syncd) 300)
   (check-equal? (send-syncd r1 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   ; now test button event handling advancing to the same time as the button down
   (wally-clear)
   (set! times '())
   (define r2 (new button-events-tester%))
   (send-thing r2 advance-time 50)
   (check-equal? (send-syncd r2 milliseconds-syncd) 50)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '())
   ; button down sent before advance time (but both to time 100)
   (send-thing r2 button-down-event 100 0 0)
   (send-thing r2 advance-time 100)
   (check-equal? (send-syncd r2 milliseconds-syncd) 100)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 advance-time 150)
   (check-equal? (send-syncd r2 milliseconds-syncd) 150)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 button-down-event 200 0 0)
   (send-thing r2 advance-time 200)
   (check-equal? (send-syncd r2 milliseconds-syncd) 200)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   (send-thing r2 advance-time 250)
   (check-equal? (send-syncd r2 milliseconds-syncd) 250)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))))

; sampling-tests omitted, since get-sampling should be provided by compiled classes

(define compiled-reactive-thing-tests 
  (test-suite 
   "run tests for compiled-reactive-thing"
   (advance-time-no-whens)
   (advance-time-one-when)
   (advance-time-multiple-whens)
   (button-events)
   ))
