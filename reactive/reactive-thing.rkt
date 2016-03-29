#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")
; make Racket's version of 'when' available also
(require (only-in racket [when racket-when]))

(provide when while racket-when reactive-thing%)

; Definition of 'when' and 'while' macros.  These should be used within an instance of reactive-thing
; or a subclass since they reference 'this'.
; Note that 'when' overrides the built-in Racket 'when' - use 'racket-when' or 'cond' for that.
(define-syntax-rule (when test e ...)
  (send this add-when (when-holder (lambda () test) (lambda () e ...))))
; #:interesting-time is an optional argument - it is a function that returns true if the current symbolic-time is an
; 'interesting' time, i.e., advance-time should stop at that time and evaluate because something may happen in the
; 'while' that will affect future state.  There are currently two simple cases for which the system can synthesize
; #:interesting-time, namely checking for button-pressed? and checking for milliseconds within a given range (with a
; rigid syntax for this).  Otherwise if no explicit #:interesting-time function is given it's an error.
(define-syntax while
  (syntax-rules (and <= >= button-pressed? milliseconds)
    ((while test #:interesting-time interesting e ...)
     (send this add-while (while-holder (lambda () test) (lambda () interesting) (lambda () e ...))))
    ((while (button-pressed?) e ...)
     (send this add-while (while-holder (lambda () (send this button-pressed?))
                                        (lambda () (send this button-going-up?))
                                        (lambda () e ...))))
    ; for some reason this version doesn't work:
    ; ((while (button-pressed?) e ...)
    ;  (while (button-pressed?) #:interesting-time (lambda () (send this button-going-up?)) e ...))
    ;
    ; The versions that check if milliseconds is within a given range assume a test like this:
    ;   (while (and (<= 50 (milliseconds)) (<= (milliseconds) 100)) ...
    ; or this:
    ;   (while (and (<= 50 (milliseconds)) (>= 100 (milliseconds))) ...
    ; The lower bound test is assumed to be syntactically correct - no error checking on it
    ((while (and lower-test (<= (milliseconds) upper)) e ...)
     (send this add-while (while-holder (lambda () (and lower-test (<= (send this milliseconds) upper)))
                                        (lambda () (equal? (send this milliseconds) upper))
                                        (lambda () e ...))))
    ((while (and lower-test (>= upper (milliseconds))) e ...)
     (send this add-while (while-holder (lambda () (and lower-test (>= upper (send this milliseconds))))
                                        (lambda () (equal? (send this milliseconds) upper))
                                        (lambda () e ...))))
    ((while test e ...)
     (error 'while "unable to automatically synthesize #:interesting-time function"))))
; structs to hold whens and whiles -- the condition and body are both thunks (anonymous lambdas)
(struct when-holder (condition body) #:transparent)
(struct while-holder (condition interesting body) #:transparent)

(define reactive-thing%
  (class abstract-reactive-thing%
    (super-new)
    (define when-holders '())  ; list of whens
    (define while-holders '())
    ; symbolic-time is this thing's idea of the current time.  It is in milliseconds, and is relative to
    ; time-at-startup.
    (define-symbolic* symbolic-time real?)
    ; start time out at 0
    (assert (equal? symbolic-time 0))
    (send this solve)
    (stay symbolic-time)
    ; to implement previous, we just need to evaluate the expression in the current (i.e. old) solution
    (define/public (previous expr)
      (send this wally-evaluate expr))
    
    ; sampling says how to sample.  It starts out as #f, and then is set the first time the (get-sampling)
    ; function is called.  After that it is one of '(push) '(pull) '(push pull) or '()
    (define sampling #f)
    (define/override (get-sampling)
      ; if sampling is #f, compute what it should be and set it
      ; (this doesn't support dynamically adding constraints during the time the thing is running)
      (cond [(not sampling)
             (set! sampling null)
             ; if any of the always constraints include a temporal reference, sampling should include pull
             (cond [(includes-one-of (send this get-always-code) '((seconds) (milliseconds)))
                    (set! sampling (cons 'pull sampling))])
             ; if there are when constraints, sampling should include push
             (cond [(not (null? when-holders))
                    (set! sampling (cons 'push sampling))])
             ; if there are while constraints, for now sampling should be push pull
             ; (since this subsumes the other possibilities, just override)
             (cond [(not (null? while-holders))
                    (set! sampling '(push pull))])])
      sampling)
    ; Helper function for get-sampling.  items should be a list of temporal function calls.
    ; Return true if code contains one of the calls.
    (define (includes-one-of code items)
      (cond [(member code items) #t]
            [(pair? code) (or (includes-one-of (car code) items) (includes-one-of  (cdr code) items))]
            [else #f]))
        
    ; handling 'when' and 'while'
    (define/public (add-when holder)
      (set! when-holders (cons holder when-holders)))
    (define/public (add-while holder)
      (set! while-holders (cons holder while-holders)))
    
    (define/override (milliseconds)
      symbolic-time)
    (define/override (milliseconds-evaluated)
      (send this wally-evaluate symbolic-time))
    
    (define/override (concrete-image)
      (send this wally-evaluate (send this image)))

    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true or is an interesting time for a 'while'.  If there aren't any such values
    ; between the current time and the target, then just return the target.  Note that the calls to solve in this
    ; method don't change the current solution, which is held in an instance variable defined in thing%.
    (define/override (find-time mytime target)
      ; If there aren't any when or while statements, just return the target time, otherwise solve for the time
      ; to which to advance.
      (cond [(and (null? when-holders) (null? while-holders)) target]
            [else (define solver (current-solver)) ; can use direct calls to solver b/c we aren't doing finitization!
                  (define-symbolic* found-when found-while boolean?)
                  (assert (> symbolic-time mytime))
                  (assert (or (equal? symbolic-time target)
                              (and (< symbolic-time target)
                                   (or (ormap (lambda (w) ((when-holder-condition w))) when-holders) ; is a when condition true?
                                       (ormap (lambda (w) ((while-holder-interesting w))) while-holders)))))
                  ; add all required always constraints and stays to the solver
                  (send this solver-add-required solver)
                  (solver-assert solver (asserts))
                  (solver-minimize solver (list symbolic-time)) ; ask Z3 to minimize the symbolic time objective
                  (define sol (solver-check solver))
                  (define min-time (evaluate symbolic-time sol))
                  ; make sure that this is indeed a minimum (not an infinitesimal)
                  ; trying to advance time by an infinitesimal amount could loop forever
                  (solver-assert solver (list (< symbolic-time min-time)))
                  (unless (unsat? (solver-check solver))
                    (error 'find-time "can only advance time by an infinitesimal amount"))
                  (clear-asserts!)
                  (solver-clear solver)
                  ; make sure we aren't stuck
                  (racket-when (equal? mytime min-time)
                               (error 'find-time "unable to find a time to advance to that is greater than the current time"))
                  min-time]))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true or is an
    ; interesting time for a 'while'.  Solve all constraints in active when and while constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define/override (advance-time-helper target)
      (let ([mytime (send this milliseconds-evaluated)])
        ; make sure we haven't gone by the target time already - if we have, don't do anything
        (cond [(< mytime target)
               (let ([next-time (find-time mytime target)])
                 (assert (equal? symbolic-time next-time))
                 (define saved-asserts (asserts))
                 ; Solve all constraints and then find which when conditions hold.  Put those whens in active-whens.
                 (send this solve)
                 (define active-whens (filter (lambda (w) (send this wally-evaluate ((when-holder-condition w))))
                                              when-holders))
                 (define active-whiles (filter (lambda (w) (send this wally-evaluate ((while-holder-condition w))))
                                              while-holders))
                 ; Assert the constraints in all of the bodies of the active whens and whiles.  Also, solving clears the
                 ; global assertion store, so add that back in.  This includes the assertion that symbolic-time
                 ; equals next-time.  Then solve again.
                 (for-each (lambda (w) ((when-holder-body w))) active-whens)
                 (for-each (lambda (w) ((while-holder-body w))) active-whiles)
                 (for-each (lambda (a) (assert a)) saved-asserts)
                 (send this solve)
                 ; If any whens or whiles were activated tell the viewers that this thing might have changed.  (It might
                 ; not actually have changed but that's ok -- we just don't want to miss telling them if it did.)
                 ; TODO: this test ought to be that a while is active and will become inactive on any further advance-time
                 (cond [(and (null? active-whens) (null? active-whiles)) (void)]
                       [else (send this notify-watchers)])
                 ; if we didn't get to the target try again
                 (cond [(< next-time target) 
                        (advance-time-helper target)]))])))))
