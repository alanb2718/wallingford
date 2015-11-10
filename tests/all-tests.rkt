#lang racket
;; run all unit tests for wallingford

(require rackunit rackunit/text-ui)

(require "wallingford-core-tests.rkt")
(require "always-star-tests.rkt")
(require "geothings-tests.rkt")
(require "electrical-things-tests.rkt")
(require "electrical-things-dynamic-tests.rkt")

(define all-tests
  (test-suite
   "run all tests"
   wallingford-core-tests
   always-star-tests
   geothings-tests
   electrical-things-tests
   electrical-things-dynamic-tests))

(printf "running all-tests\n")
(time (run-tests all-tests))
