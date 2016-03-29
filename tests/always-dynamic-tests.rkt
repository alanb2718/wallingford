#lang s-exp rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require "../core/wallingford.rkt")

(provide always-dynamic-tests)

;; Additional tests of always constraints -- dynamically re-assign something

(define (assign-always-test)
  (test-case
   "test reassiging a variable using always (constraint should apply to new binding)"
   (define c (new thing%))
   (define-symbolic x y integer?)
   (always (equal? x 3) #:priority low #:owner c)
   (always (equal? y 4) #:priority high #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 3)
   (check-equal? (send c wally-evaluate y) 4)
   (set! y x)
   (send c solve)
   ; the (always (equal? y 4) constraint applies to the current binding for y,
   ; so we should get x=y=4 at this point
   (check-equal? (send c wally-evaluate x) 4)
   (check-equal? (send c wally-evaluate y) 4)
   ))

(define (assign-always-required-test)
  (test-case
   "test reassiging a variable using a required always (mostly to test the optional priority parameter)"
   (define c (new thing%))
   (define-symbolic x y integer?)
   (always (equal? x 3) #:priority low #:owner c)
   (always (equal? y 4) #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate x) 3)
   (check-equal? (send c wally-evaluate y) 4)
   (set! y x)
   (send c solve)
   ; the (always (equal? y 4) constraint applies to the current binding for y,
   ; so we should get x=y=4 at this point
   (check-equal? (send c wally-evaluate x) 4)
   (check-equal? (send c wally-evaluate y) 4)
   ))

(struct test-struct (fld) #:transparent #:mutable)

(define (struct-always-set-test)
  (test-case
   "test setting a field of a mutable struct"
   (define c (new thing%))
   (define-symbolic x y integer?)
   (define s (test-struct x))
   (always (equal? x 3) #:priority low #:owner c)
   (always (equal? y 4) #:priority low #:owner c)
   (always (equal? (test-struct-fld s) 10) #:priority high #:owner c)
   (send c solve)
   (check-equal? (send c wally-evaluate (test-struct-fld s)) 10)
   (check-equal? (send c wally-evaluate x) 10)
   (check-equal? (send c wally-evaluate y) 4)
   (set-test-struct-fld! s y)
   (send c solve)
   ; the always constraint on the struct applies to the current contents of fld
   (check-equal? (send c wally-evaluate (test-struct-fld s)) 10)
   (check-equal? (send c wally-evaluate x) 3)
   (check-equal? (send c wally-evaluate y) 10)
   ))

(define (explicit-required-priority-test)
  (test-case
   "test providing an explicit priority of required"
   (define c (new thing%))
   (define-symbolic x integer?)
   (always (equal? x 2) #:priority required #:owner c)
   (always (equal? x 3) #:priority required #:owner c)
   (check-exn
    exn:fail?
    (lambda () (send c solve)))
   ; clear assertions, since they are in an unsatisfiable state at this point
   (clear-asserts!)))


(define always-dynamic-tests 
  (test-suite+
   "run always tests"
   (assign-always-test)
   (assign-always-required-test)
   (struct-always-set-test)
   (explicit-required-priority-test)
   ))

(time (run-tests always-dynamic-tests))
