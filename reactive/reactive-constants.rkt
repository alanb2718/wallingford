#lang racket
; constants for reactive things
; (there aren't that many, but putting them in a separate file makes it easier to import them various places)

(provide default-variable-of-integration default-dt default-epsilon)

(define default-variable-of-integration '(milliseconds))

; default time step for numeric integration and piecewise linear approximations (in milliseconds)
(define default-dt 10)
; default time interval for testing whether a linear approximation of a when test is close enough
; (we keep halving the time for which the linear approximation applies until it is less than epsilon)
(define default-epsilon 1/10)
