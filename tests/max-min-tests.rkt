#lang s-exp rosette
;; unit tests for max-value and min-value in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")

(provide max-min-tests)

(define (max-min-in-always)
  (test-case
   "test calls to max-value and min-value in an always"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y z real?)
       ; x should keep getting bigger as time advances, y should keep getting smaller, and z should stay stuck at 0
       (always (equal? x (* 2 (max-value (milliseconds)))))
       (always (equal? y (* 2 (min-value (- 0 (milliseconds))))))
       (always (equal? z (* 2 (min-value (milliseconds)))))))
   (define r (new tester%))
   (send-thing r advance-time 10)
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 20)
   (check equal? (send r get-y) -20)
   (check equal? (send r get-z) 0)
   (send-thing r advance-time 50)
   (check equal? (send-syncd r milliseconds-syncd) 50)
   (check equal? (send r get-x) 100)
   (check equal? (send r get-y) -100)
   (check equal? (send r get-z) 0)
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send r get-x) 120)
   (check equal? (send r get-y) -120)
   (check equal? (send r get-z) 0)))

(define (max-min-in-while)
  (test-case
   "test calls to max-value and min-value in a while"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x y z real?)
       (assert (equal? x 1))
       (assert (equal? y 2))
       (assert (equal? z 3))
       (send this solve)
       (stay x)
       (stay y)
       (stay z)
       (while (and (>= (milliseconds) 10) (<= (milliseconds) 100))
              #:interesting-time (or (equal? (milliseconds) 50) (equal? (milliseconds) 100))
              (assert (equal? x (max-value (milliseconds))))
              ; y is equal to 3 times the number of milliseconds up to ms=50, then goes back down
              ; (note that 50 is thus an interesting time!)
              (assert (equal? y (* 3 (max-value (let ([m (milliseconds)]) (if (<= m 50) m (- 100 m)))))))
              ; z is the negation of y, basically
              (assert (equal? z (* 3 (min-value (let ([m (milliseconds)]) (if (<= m 50) (- m) (- m 100))))))))))
   (define r (new tester%))
   
   (send-thing r advance-time 5)
   (check equal? (send-syncd r milliseconds-syncd) 5)
   ; outside of while at this time, so x, y, and z shoud have their initial values
   (check equal? (send r get-x) 1)  
   (check equal? (send r get-y) 2)
   (check equal? (send r get-z) 3)
   
   (send-thing r advance-time 10)
   ; while should now be active
   (check equal? (send-syncd r milliseconds-syncd) 10)
   (check equal? (send r get-x) 10)
   (check equal? (send r get-y) 30)
   (check equal? (send r get-z) -30)
   
   (send-thing r advance-time 60)
   (check equal? (send-syncd r milliseconds-syncd) 60)
   (check equal? (send r get-x) 60)
   (check equal? (send r get-y) 150)  ; should stop at t=50 and update y and z
   (check equal? (send r get-z) -150)
   
   (send-thing r advance-time 75)
   (check equal? (send-syncd r milliseconds-syncd) 75)
   (check equal? (send r get-x) 75)
   (check equal? (send r get-y) 150)
   (check equal? (send r get-z) -150)
   
   (send-thing r advance-time 200)
   (check equal? (send-syncd r milliseconds-syncd) 200)
   (check equal? (send r get-x) 100) ; x stops at 100
   (check equal? (send r get-y) 150)
   (check equal? (send r get-z) -150)))


(define max-min-tests 
  (test-suite+
   "run tests for max and min in reactive-thing"
   (max-min-in-always)
   (max-min-in-while)
   ))

(time (run-tests max-min-tests))
