#lang racket
;; run all unit tests for wallingford

(require rosette/lib/util/roseunit)

(run-all-tests
 "wallingford-core-tests.rkt"
 "always-star-tests.rkt"
 "geothings-tests.rkt"
 "electrical-things-tests.rkt"
 "electrical-things-dynamic-tests.rkt")

