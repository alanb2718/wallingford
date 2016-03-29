#lang s-exp rosette
;; unit tests for 'when' in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide when-tests)

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
       (always (equal? m (milliseconds)))
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
   "test advance time for a case that is too hard (should get an exception)"
   (define one-when-tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-symbolic* m real?)
       (always (equal? m (milliseconds)) #:priority low)
       (when (equal? m 100)
         #t)))
   (define r (new one-when-tester%))
   (check equal? (send-syncd r milliseconds-syncd) 0)
   ; do the with-handlers call inside evaluate-syncd so that it is evaluated in the thing's thread
   (check equal? (send-syncd r evaluate-syncd
                             (lambda () (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
                                          (send r advance-time-helper 30))))
          "find-time: can only advance time by an infinitesimal amount")))

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

(define when-tests 
  (test-suite+
   "run tests for when in reactive-thing"
   (advance-time-one-when)
   (advance-time-one-when-separate-var)
   (advance-time-one-when-separate-var-soft-cn)
   (advance-time-multiple-whens)
   ))

(time (run-tests when-tests))
