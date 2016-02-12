#lang s-exp rosette
; pulsing circle example
(require "reactive.rkt")
(require "pulser-class.rkt")

; make a new pulser and a viewer on it:
(wally-clear)
(make-viewer (new pulser%) #:title "Pulser")