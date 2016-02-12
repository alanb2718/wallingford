#lang racket
;; run all unit tests for reactive (temporarily in a separate file)

(require rackunit rackunit/text-ui)

(require "reactive-thing-tests.rkt")
(require "viewer-tests.rkt")

(define all-tests
  (test-suite
   "run all tests"
   reactive-thing-tests
   viewer-tests))

(printf "running all-tests\n")
(time (run-tests all-tests))
