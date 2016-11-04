; macro definitions for reactive-thing%
#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")
(require "reactive-constants.rkt")

(provide when while max-value min-value integral pull-sampling? interesting-time?
         when-holder-test when-holder-body when-holder-id
         linearized-when-holder-op linearized-when-holder-linearized-test linearized-when-holder-dt
         while-holder-test while-holder-body while-holder-id while-holder-interesting while-holder-pull?)

; structs to hold whens and whiles -- the test and body are both thunks (anonymous lambdas)
; id is a unique symbol for that when or while
; for when, if the test is approximated as a piecewise linear function, use linearized-when-holder.  dt should be the interval size
(struct when-holder (test body id) #:transparent)
(struct linearized-when-holder when-holder (op linearized-test dt) #:transparent)
; for while-holder, pull? is #t if pull sampling should be used while this 'while' is active, and #f if not
(struct while-holder (test body id interesting pull?) #:transparent)

; interesting-time? will be rebound when evaluating a while -- it will be the value of the interesting-time function
; for that while at the current time.  It is used for accumulating operators such as max-value.
(define interesting-time? (make-parameter #f))

; Definition of 'when' and 'while' macros.  These should be used within an instance of reactive-thing
; or a subclass since they reference 'this'.
;
; 'when' macro.  This overrides the built-in Racket 'when' - use 'racket-when' or 'cond' for that.
; There is an optional flag #:linearize, which means do a piecewise linear approximation of the test.  This can
; take an additional #:dt argument for the time step to use.  when constraints that use #:linearize have a restricted
; form: a comparison operator followed by two expressions, e.g. (equal? (sin (seconds)) x)
(define-syntax when
  (syntax-rules ()
    ((when (op e1 e2) #:linearize #:dt dt e ...)
     (send this add-linearized-when-holder (linearized-when-holder (lambda () (op e1 e2)) (lambda () e ...) (gensym) op (lambda () (- e1 e2)) dt)))
    ((when test #:linearize e ...)
     (when test #:linearize #:dt default-dt e ...))
    ((when test e ...)
     (send this add-when-holder (when-holder (lambda () test) (lambda () e ...) (gensym))))))

; 'while' macro.  #:interesting-time is an optional argument - it is a function that returns true if the current symbolic-time
; is an 'interesting' time, i.e., advance-time should stop at that time and evaluate because something may happen in the
; 'while' that will affect future state.  There are currently two simple cases for which the system can synthesize
; #:interesting-time, namely checking for button-pressed? and checking for milliseconds within a given range (with a
; rigid syntax for this).  Otherwise if no explicit #:interesting-time function is given it's an error.
(define-syntax while
  (syntax-rules (and <= >= button-pressed? milliseconds)
    ((while test #:interesting-time interesting e ...)
     (add-while test interesting e ...))
    ((while (button-pressed?) e ...)
     (add-while (send this button-pressed?)
                (cond [(send this button-going-down?) 'first]
                      [(send this button-going-up?) 'last]
                      [else #f])
                e ...))
    ; The versions that check if milliseconds is within a given range assume a test like this:
    ;   (while (and (<= 50 (milliseconds)) (<= (milliseconds) 100)) ...
    ; or this:
    ;   (while (and (<= 50 (milliseconds)) (>= 100 (milliseconds))) ...
    ; plus the analogous versions for the lower bound test, so 4 possible combinations in all.
    ; NOTE: this should be rewritten in better style -- see e.g. the integral macro, which uses syntax-parse
    ((while (and (<= lower (milliseconds)) (<= (milliseconds) upper)) e ...)
     (add-while-with-time-bounds lower upper e ...))
    ((while (and (<= lower (milliseconds)) (>= upper (milliseconds))) e ...)
     (add-while-with-time-bounds lower upper e ...))
    ((while (and (>= (milliseconds) lower) (<= (milliseconds) upper)) e ...)
     (add-while-with-time-bounds lower upper e ...))
    ((while (and (>= (milliseconds) lower) (>= upper (milliseconds))) e ...)
     (add-while-with-time-bounds lower upper e ...))
    ((while test e ...)
     (error 'while "unable to automatically synthesize #:interesting-time function"))))
; add-while and add-while-with-time-bounds are helper macros (just for internal use)
; if the body of the while has temporal constraints then we need to use pull sampling
(define-syntax-rule (add-while test interesting e ...)
  (send this add-while-holder (while-holder (lambda () test)
                                            (lambda () e ...)
                                            (gensym)
                                            (lambda () interesting)
                                            (pull-sampling? '(e ...)))))
(define-syntax-rule (add-while-with-time-bounds lower upper e ...)
  (add-while (and (<= lower (send this milliseconds)) (<= (send this milliseconds) upper))
             (cond [(equal? lower (send this milliseconds)) 'first]
                   [(equal? upper (send this milliseconds)) 'last]
                   [else #f])
             e ...))

; max-value and min-value macros
(define-syntax-rule (max-value expr)
  (max-or-min max expr))
(define-syntax-rule (min-value expr)
  (max-or-min min expr))
(define-syntax (max-or-min stx)
  (syntax-case stx ()
    [(_ fn expr)
     (with-syntax ([id (datum->syntax stx (gensym))])
       #'(send this max-min-helper fn (lambda () (send this wally-evaluate expr)) 'id (interesting-time?)))]))

; integral macro
; The form is (integral expr) with additional optional keyword arguments as follows:
;     #:var v  -- variable of integration (note that an expression is allowed here)
;     #:numeric or #:symbolic -- which kind of integration to use.  Can provide at most one of these, or omit.
;         The default is to try symbolic, and if that doesn't work, use numeric.  However, if #:symbolic is listed
;         explicitly, then either symbolic integration must succeed or the system raises an exception.
;     #:dt d -- time step (only allowed with #:numeric)
; Example: (integral (sin x) #:var x #:numeric #:dt 1)
; A Racket macro ninja would check the restrictions in the macro itself, but here the integral-preprocessor function checks them.
; The integral-preprocessor function includes definitions of the default values for #:var and #:dt
(require (for-syntax "integral-preprocessor.rkt"))
(require (for-syntax syntax/parse))
(define-syntax (integral stx)
  (syntax-parse stx
    [(integral e:expr (~or (~optional (~seq #:var v:expr))
                           (~optional (~seq (~and #:numeric numeric-kw)))
                           (~optional (~seq (~and #:symbolic symbolic-kw)))
                           (~optional (~seq #:dt dt:expr))) ...)
     (let-values ([(symbolic? var symbolic-integral dt)
                   (integral-preprocessor (syntax->datum #'e)
                                          (if (attribute v) (syntax->datum #'v) #f)
                                          (attribute numeric-kw)
                                          (attribute symbolic-kw)
                                          (if (attribute dt) (syntax->datum #'dt) #f))])
       (if symbolic? 
           (with-syntax ([s (datum->syntax stx symbolic-integral)]  ; symbolic version
                         [id (datum->syntax stx (gensym))])
             #'(send this integral-symbolic-run-time-fn (lambda () (send this wally-evaluate s)) 'id (interesting-time?)))
           (with-syntax ([v (datum->syntax stx var)]
                         [d (datum->syntax stx dt)]
                         [id (datum->syntax stx (gensym))])  ; numeric version
             #'(send this integral-numeric-run-time-fn (lambda () (send this wally-evaluate v))
                     (lambda () (send this wally-evaluate e)) 'id (interesting-time?) d))))]))

; Helper functions to test whether to use pull sampling
(define (pull-sampling? code)
  (includes-one-of code '((seconds) (milliseconds) (mouse-position)
                          (button-pressed?) (button-going-down?) (button-going-up?) (button-going-up-or-down?))))
(define (includes-one-of code items)
  ; items should be a list of temporal function calls. Return true if code is or contains one of the calls.
  (cond [(member code items) #t]
        [(pair? code) (or (includes-one-of (car code) items) (includes-one-of  (cdr code) items))]
        [else #f]))
