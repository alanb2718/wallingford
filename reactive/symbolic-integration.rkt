#lang s-exp rosette

; Function to do symbolic integration at compile time -- super simple to start with.
; This doesn't do any simplification of the result -- which seems fine, since it is for evaluation
; rather than human consumption, and the code is going to be really fast with or without simplication.
; It is small, but putting it in a separate module makes it easier to test independently.

(provide symbolic-integral)

(define (symbolic-integral expr var)
  (match expr
    [v #:when (equal? v var) (list '* 0.5 (list 'expt var 2))]
    [(? number? n) (list '* n var)]
    [(list '+ x y) (list '+ (symbolic-integral x var) (symbolic-integral y var))]
    [(cons '* (list-no-order (? number? n) x)) (list '* n (symbolic-integral x var))]
    [(list '/ x (? number? n)) (list '/ (symbolic-integral x var) n)]
    [(list 'expt v (? number? n)) #:when (equal? v var) (list '/ (list 'expt v (+ n 1)) (+ n 1))]
    [(list 'integral e) (symbolic-integral (symbolic-integral e var) var)]
    [_ (error "unable to find the symbolic integral of ~a \n" expr)]))
