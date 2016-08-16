#lang racket
;; run all unit tests for wallingford

(require rosette/lib/roseunit)

(run-all-tests
 "wallingford-core-tests.rkt"
 "always-dynamic-tests.rkt"
 "geothings-tests.rkt"
 "electrical-things-tests.rkt"
 "electrical-things-dynamic-tests.rkt"
 "reactive-thing-tests.rkt"
 "sampling-tests.rkt"
 "mouse-tests.rkt"
 "when-tests.rkt"
 "while-tests.rkt"
 "max-min-tests.rkt"
 "symbolic-integration-tests.rkt"
 "integral-tests.rkt"
 "viewer-tests.rkt"
 "compiled-reactive-thing-tests.rkt"
 "compiled-when-tests.rkt"
 "compiled-while-tests.rkt"
 )
