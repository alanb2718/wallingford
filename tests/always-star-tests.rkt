#lang s-exp rosette

(require rackunit rackunit/text-ui)
(require "../core/wallingford.rkt")

(provide always-star-tests)

;; Tests of always vs. always*

(define (assign-always-test)
  (test-case
   "test reassiging a variable (should leave the constraint on the old contents)"
   (wally-clear)
   (define-symbolic x y number?)
   (always (equal? x 3) #:priority low)
   (always (equal? y 4) #:priority high)
   (wally-solve)
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 4)
   (set! y x)
   (wally-solve)
   ; the (always (equal? y 4) constraint applies to the old binding for y,
   ; so we should get x=y=3 at this point
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 3)
   ))

(define (assign-always*-test)
  (test-case
   "test reassiging a variable using always* (constraint should apply to new binding)"
   (wally-clear)
   (define-symbolic x y number?)
   (always* (equal? x 3) #:priority low)
   (always* (equal? y 4) #:priority high)
   (wally-solve)
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 4)
   (set! y x)
   (wally-solve)
   ; the (always* (equal? y 4) constraint applies to the current binding for y,
   ; so we should get x=y=4 at this point
   (check-equal? (evaluate x) 4)
   (check-equal? (evaluate y) 4)
   ))

(define (assign-always*-required-test)
  (test-case
   "test reassiging a variable using a required always* (mostly to test the optional priority parameter)"
   (wally-clear)
   (define-symbolic x y number?)
   (always* (equal? x 3) #:priority low)
   (always* (equal? y 4))
   (wally-solve)
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 4)
   (set! y x)
   (wally-solve)
   ; the (always* (equal? y 4) constraint applies to the current binding for y,
   ; so we should get x=y=4 at this point
   (check-equal? (evaluate x) 4)
   (check-equal? (evaluate y) 4)
   ))

(struct test-struct (fld) #:transparent #:mutable)

(define (struct-always-set-test)
  (test-case
   "test setting a field of a mutable struct"
   (wally-clear)
   (define-symbolic x y number?)
   (define s (test-struct x))
   (always (equal? x 3) #:priority low)
   (always (equal? y 4) #:priority low)
   (always (equal? (test-struct-fld s) 10) #:priority high)
   (wally-solve)
   (check-equal? (evaluate (test-struct-fld s)) 10)
   (check-equal? (evaluate x) 10)
   (check-equal? (evaluate y) 4)
   (set-test-struct-fld! s y)
   ; the always constraint on the struct applies to the old contents of fld
   (wally-solve)
   (check-equal? (evaluate (test-struct-fld s)) 4)
   (check-equal? (evaluate x) 10)
   (check-equal? (evaluate y) 4)
   ))
 
(define (struct-always*-set-test)
  (test-case
   "test setting a field of a mutable struct"
   (wally-clear)
   (define-symbolic x y number?)
   (define s (test-struct x))
   (always* (equal? x 3) #:priority low)
   (always* (equal? y 4) #:priority low)
   (always* (equal? (test-struct-fld s) 10) #:priority high)
   (wally-solve)
   (check-equal? (evaluate (test-struct-fld s)) 10)
   (check-equal? (evaluate x) 10)
   (check-equal? (evaluate y) 4)
   (set-test-struct-fld! s y)
   (wally-solve)
   ; the always constraint on the struct applies to the current contents of fld
   (check-equal? (evaluate (test-struct-fld s)) 10)
   (check-equal? (evaluate x) 3)
   (check-equal? (evaluate y) 10)
   ))


(define always-star-tests 
  (test-suite 
   "run always vs always* tests"
   (assign-always-test)
   (assign-always*-test)
   (assign-always*-required-test)
   (struct-always-set-test)
   (struct-always*-set-test)
   ))
