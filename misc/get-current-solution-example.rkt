#lang s-exp rosette
;; example of different versions of current-solution

(require "../reactive/reactive.rkt")


(define r (new reactive-thing%))

; these are different:

(printf "\nmodel returned from (send r get-current-solution) \n~a \n\n" (send r get-current-solution))

(printf "\nmodel returned from (send-syncd r evaluate-syncd (lambda () (send r get-current-solution))) \n~a \n\n" 
        (send-syncd r evaluate-syncd (lambda () (send r get-current-solution))))

