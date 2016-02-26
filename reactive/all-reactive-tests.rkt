#lang racket
;; run all unit tests for reactive (temporarily in a separate file)

(require rosette/lib/util/roseunit)

(run-all-tests
 "reactive-thing-tests.rkt"
 "viewer-tests.rkt"
 "compiled-reactive-thing-tests.rkt")