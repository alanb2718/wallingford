#lang s-exp rosette
; one test from the whuile-tests.rkt test file, separated out so that it can be run one statement at a time


(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")


(define tester%
  (class reactive-thing%
    (inherit milliseconds)
    (super-new)
    (define-symbolic* x integer?)
    (always (equal? x 1) #:priority low)
    (while (and (>= (milliseconds) 50) (<= (milliseconds) 100))
           (assert (equal? x 2)))
    (define/public (get-x) (send this wally-evaluate x))))
(define r (new tester%))

;(send-thing r advance-time 10)
;
;(check equal? (send-syncd r milliseconds-syncd) 10)
;(check equal? (send r get-x) 1)
;(send-thing r advance-time 60)
;(check equal? (send-syncd r milliseconds-syncd) 60)
;(check equal? (send r get-x) 2)
;(send-thing r advance-time 75)
;(check equal? (send-syncd r milliseconds-syncd) 75)
;(check equal? (send r get-x) 2)
;(send-thing r advance-time 200)
;(check equal? (send-syncd r milliseconds-syncd) 200)
;(check equal? (send r get-x) 1)
;
