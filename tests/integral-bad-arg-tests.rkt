#lang s-exp rosette
;; unit tests for bad argument combinations for integral in reactive-thing%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require syntax/macro-testing)
(require "../core/wallingford.rkt")
(require "../reactive/reactive.rkt")

(provide integral-bad-arg-tests)

(define (both-symbolic-and-numeric)
  (test-case
   "test call to integral with both #:symbolic and #:numeric"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (convert-compile-time-error (always (equal? x (integral 2 #:symbolic #:numeric))))))
   (check-exn #rx"can only specify one of #:numeric and #:symbolic" (lambda () (new tester%)))))

(define (dt-with-symbolic)
  (test-case
   "test call to integral with #:symbolic and #:dt"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (convert-compile-time-error (always (equal? x (integral 2 #:symbolic #:dt 1))))))
   (check-exn #rx"#:dt can only be specified in conjunction with #:numeric" (lambda () (new tester%)))))

(define (dt-alone)
  (test-case
   "test call to integral with #:dt alone (no #:numeric as required)"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (convert-compile-time-error (always (equal? x (integral 2 #:dt 1))))))
   (check-exn #rx"#:dt can only be specified in conjunction with #:numeric" (lambda () (new tester%)))))

(define (symbolic-but-too-hard)
  (test-case
   "test call to integral with #:symbolic and an expression that is too difficult to find the symbolic integral"
   (define tester%
     (class reactive-thing%
       (inherit milliseconds)
       (super-new)
       (define-public-symbolic* x real?)
       (convert-compile-time-error (always (equal? 0 (integral (sin x) #:symbolic))))))
   (check-exn #rx"#:symbolic was specified but unable to find symbolic integral" (lambda () (new tester%)))))


(define integral-bad-arg-tests 
  (test-suite+
   "unit tests for integral with bad arguments"
   (both-symbolic-and-numeric)
   (dt-with-symbolic)
   (dt-alone)
   (symbolic-but-too-hard)
   ))

(time (run-tests integral-bad-arg-tests))
