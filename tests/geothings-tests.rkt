#lang s-exp rosette
;; unit tests for geothings, using a midpoint example

(require rackunit rackunit/text-ui rosette/lib/util/roseunit)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(provide geothings-tests)

; In Racket, (equal? 2 2.0) is false.  In Rosette, it is true, because equal? on numbers 
; behaves like =.  Since the check-equal? macro from rackunit uses Racket's equal? rather 
; than Rosette's equal?, we will use the syntax (check equal? ...) for tests instead.

(define (midpoint-test)
  (test-case
   "create a midpointline and move the endpoints and then the midpoint"
   (wally-clear)
   (define mpl (make-midpointline))
   (define line1 (midpointline-line mpl))
   (define midpoint (midpointline-midpoint mpl))
   ; the midpoint constraint and stays on the endpoints of the line
   ; We want to put the stays on the two endpoints of the line rather than the line as a whole, so 
   ; that we prefer solutions that leave one endpoint where it was even if we need to move the other.
   ; And we don't put the stays all the way down on the x and y values, to avoid the split stay problem.
   (always (equal? midpoint (point-scale (point-plus (line-end1 line1) (line-end2 line1)) 0.5)))
   (stay (line-end1 line1) #:priority low)
   (stay (line-end2 line1) #:priority low)
   (stay midpoint #:priority lowest)
   ; initialize the line's location
   (assert (equal? line1 (line (point 10 10) (point 20 50))))
   (wally-solve)
   (check equal? (evaluate line1) (line (point 10 10) (point 20 50)))
   (check equal? (evaluate midpoint) (point 15 30))
   "move end1 of the line"
   (assert (equal? (line-end1 line1) (point 0 0)))
   (wally-solve)
   (check equal? (evaluate line1) (line (point 0 0) (point 20 50)))
   (check equal? (evaluate midpoint) (point 10 25))
   "move end1 of the line again"
   (assert (equal? (line-end1 line1) (point 20 70)))
   ; since the stays on the line's endpoints are higher priority than the stay on the midpoint,
   ; the midpoint should move rather than end2 of the line
   (wally-solve)
   (check equal? (evaluate line1) (line (point 20 70) (point 20 50)))
   (check equal? (evaluate midpoint) (point 20 60))
   "move the midpoint"
   (assert (equal? midpoint (point 30 30)))
   ; since the stays on the two endpoints of the line have the same priority, it's not specified which
   ; one will win
   (wally-solve)
   (check-true (or (equal? (evaluate line1) (line (point 20 70) (point 40 -10)))
                   (equal? (evaluate line1) (line (point 40 10) (point 20 50)))
                   (equal? (evaluate midpoint) (point 30 30))))))

(define geothings-tests 
  (test-suite+ 
   "run all geothings tests"
   (midpoint-test)
   ))

(time (run-tests geothings-tests))