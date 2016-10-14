#lang racket
; constants for reactive things
; (there aren't that many, but putting them in a separate file makes it easier to import them various places)

(provide default-variable-of-integration default-dt)

(define default-variable-of-integration '(milliseconds))

; default time step for numeric integration and piecewise linear approximations (in milliseconds)
(define default-dt 10)
