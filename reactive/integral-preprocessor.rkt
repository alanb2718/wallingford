#lang s-exp rosette

(provide integral-preprocessor symbolic-integral)

; default values for variable of integration and dt (for numeric integration only)
(define default-variable-of-integration '(milliseconds))
(define default-dt 10)

; The integral-preprocessor function takes the expression being integrated, e.g. (integral expr)
; plus additional arguments, representing keyword arguments in the source.
;   var is the variable of integration - this has a default value of (milliseconds), provided by the caller though
;   numeric-kw is #t if the keyword #:numeric was provided
;   symbolic-kw is #t if the keyword #:symbolic was provided
;   dt is the time step if the keyword #:dt and an argument was provided
; Only one of #:numeric or #:symbolic can be given -- a Racket macro ninja would check this in the macro, but
; here this function checks it.  If neither the code first tries to find a symbolic integral, and if that doesn't
; work, it returns code to compute it numerically.  #:dt can only be provided if #:numeric is specified.  Again,
; a macro ninja would check this in the macro rather than here.
; It returns 3 values:
;   symbolic?  #t if the result is a symbolic integration, #f if numeric
;   expr       if symbolic is #t, the symbolic integral; and otherwise null
;   dt         if symbolic is #f, the time step for numeric integration; and otherwise null
(define (integral-preprocessor expr var numeric-kw symbolic-kw dt)
  (when (and numeric-kw symbolic-kw) (error "can only specify one of #:numeric and #:symbolic"))
  (when (and (not numeric-kw) dt) (error "#:dt can only be specified in conjunction with #:numeric"))
  ; try to compute the symbolic integral and bind it to s, unless #:numeric is specified
  (let* ([v (if var var default-variable-of-integration)]
         [d (if dt dt default-dt)]
         [s (if numeric-kw #f (symbolic-integral expr v))])
    (cond [(and symbolic-kw (not s))
           (error "#:symbolic was specified but unable to find symbolic integral")])
    (if s (values #t s #f) (values #f (null) d))))

; Function to do symbolic integration at compile time -- super simple to start with.
; This doesn't do any simplification of the result -- which seems fine, since it is for evaluation
; rather than human consumption, and the code is going to be really fast with or without simplication.
; It is small, but putting it in a separate module makes it easier to test independently.
(define (symbolic-integral expr var)
  (match expr
    [v #:when (equal? v var) (list '* 0.5 (list 'expt var 2))]
    [(? number? n) (list '* n var)]
    [(list '+ x y) (list '+ (symbolic-integral x var) (symbolic-integral y var))]
    [(cons '* (list-no-order (? number? n) x)) (list '* n (symbolic-integral x var))]
    [(list '/ x (? number? n)) (list '/ (symbolic-integral x var) n)]
    [(list 'expt v (? number? n)) #:when (equal? v var) (list '/ (list 'expt v (+ n 1)) (+ n 1))]
    [(list 'integral e) (symbolic-integral (symbolic-integral e var) var)]
    [_ #f]))
