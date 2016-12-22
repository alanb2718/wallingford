#lang s-exp rosette
;; unit tests for 'when' in compiled-reactive-thing% -- these should be parallel to when-tests.rkt

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")
(require "../compiled-reactive/compiled-reactive-thing.rkt")

(provide compiled-when-tests)

(define (advance-time-one-when)
  (test-case
   "test advance time with one when"
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
         (values (if (and (< mytime 100) (> target 100)) 100 target) #f))))
   
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
         (values (cond [(and (< mytime 100) (> target 100)) 100]
                       [(and (< mytime 200) (> target 200)) 200]
                       [else target])
                 #f))))
   
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

(define compiled-when-tests 
  (test-suite+
   "unit tests for when in compiled-reactive-thing"
   (advance-time-one-when)
   (advance-time-multiple-whens)
   ))

(time (run-tests compiled-when-tests))
