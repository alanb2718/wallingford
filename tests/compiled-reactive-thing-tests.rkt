#lang s-exp rosette
;; unit tests for compiled-reactive-thing% -- these should be parallel to the tests for reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")
(require "../reactive/abstract-reactive-thing.rkt")
(require "../reactive/compiled-reactive-thing.rkt")

(provide compiled-reactive-thing-tests)

(define (advance-time-simple)
  (test-case
   "test advance time with no whens"
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

(define (button-events)
  (test-case
   "test button event handling (just with reactive thing programatically, not with a viewer)"
   (define times '())
   (define (get-times) times)
   ; compiling for this constraint:
   ;   (when (button-going-down?)
   ;      (set! times (cons (list "button" (evaluate (send r milliseconds))) times)))))
   (define button-events-tester%
     (class compiled-reactive-thing%
       (inherit milliseconds button-going-down?)
       (super-new)
       (define/override (get-sampling)
         '(push))
       (define/override (update-mysolution)
         (cond [(button-going-down?)
                (set! times (cons (list "button" (milliseconds)) times))]))
       (define/override (find-time mytime target)
         (let ([potential-targets (filter (lambda (e) (and (> (mouse-event-time e) mytime)
                                                           (< (mouse-event-time e) target)
                                                           (eq? (mouse-event-button-state e) 'going-down)))
                                          (send this get-mouse-events))])
           (if (null? potential-targets) target (apply min (map mouse-event-time potential-targets)))))))
   (define r1 (new button-events-tester%))
   (send-thing r1 advance-time 50)
   (check-equal? (send-syncd r1 milliseconds-syncd) 50)
   (check-equal? (send-syncd r1 evaluate-syncd get-times) '())
   (send-thing r1 mouse-event 100 0  0 'going-down)
   (send-thing r1 mouse-event 110 0  0 'going-up)
   (send-thing r1 mouse-event 200 0  0 'going-down)
   (send-thing r1 advance-time 300)
   (check-equal? (send-syncd r1 milliseconds-syncd) 300)
   (check-equal? (send-syncd r1 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   ; now test button event handling advancing to the same time as the button down
   (set! times '())
   (define r2 (new button-events-tester%))
   (send-thing r2 advance-time 50)
   (check-equal? (send-syncd r2 milliseconds-syncd) 50)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '())
   ; button down sent before advance time (but both to time 100)
   (send-thing r2 mouse-event 100 0  0 'going-down)
   (send-thing r2 advance-time 100)
   (check-equal? (send-syncd r2 milliseconds-syncd) 100)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 mouse-event 110 0  0 'going-up)
   (send-thing r2 advance-time 150)
   (check-equal? (send-syncd r2 milliseconds-syncd) 150)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 100)))
   (send-thing r2 mouse-event 200 0  0 'going-down)
   (send-thing r2 advance-time 200)
   (check-equal? (send-syncd r2 milliseconds-syncd) 200)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))
   (send-thing r2 advance-time 250)
   (check-equal? (send-syncd r2 milliseconds-syncd) 250)
   (check-equal? (send-syncd r2 evaluate-syncd get-times) '(("button" 200) ("button" 100)))))

; sampling-tests omitted, since get-sampling should be provided by compiled classes

(define compiled-reactive-thing-tests 
  (test-suite+
   "run tests for compiled-reactive-thing"
   (advance-time-simple)
   (button-events)
   ))

(time (run-tests compiled-reactive-thing-tests))
