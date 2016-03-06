#lang racket
;; run all unit tests for wallingford

(require rosette/lib/roseunit)

(run-all-tests
 "wallingford-core-tests.rkt"
 "always-star-tests.rkt"
 "geothings-tests.rkt"
 "electrical-things-tests.rkt"
 "electrical-things-dynamic-tests.rkt"
 "reactive-thing-tests.rkt"
 "when-tests.rkt"
 "while-tests.rkt"
 "viewer-tests.rkt"
 "compiled-reactive-thing-tests.rkt"
 "compiled-when-tests.rkt")
