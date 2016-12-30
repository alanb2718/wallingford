#lang racket
; constants for reactive things

(provide default-linearize-dt default-linearize-epsilon default-variable-of-integration default-integration-dt)

;; *** constants for piecewise linear approximations ***

; default time step for piecewise linear approximations (in milliseconds)
(define default-linearize-dt 10)

; default time interval for testing whether a linear approximation of a when test is close enough
; (we keep halving the time for which the linear approximation applies until it is less than epsilon)
(define default-linearize-epsilon 1/10)


;; *** constants for integration ***

(define default-variable-of-integration '(milliseconds))

; default time step for numeric integration (in milliseconds)
(define default-integration-dt 10)
