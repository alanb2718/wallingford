#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")
; make Racket's version of 'when' available also
(require (only-in racket [when racket-when]))

(provide when racket-when reactive-thing%)

; Definition of 'when' macro.  'when' should be used within an instance of reactive-thing or a subclass
; since it references 'this'.
; Note that this overrides the built-in Racket 'when' - use 'racket-when' or 'cond' for that.
(define-syntax-rule (when test e ...)
  (send this add-when (when-holder (lambda () test) (lambda () e ...))))
; struct to hold whens -- the condition and body are both thunks (anonymous lambdas)
(struct when-holder (condition body) #:transparent)

(define reactive-thing%
  (class abstract-reactive-thing%
    (super-new)
    (define when-holders '())  ; list of whens
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
             ; if any of the always* constraints include a temporal reference, sampling should include pull
             (cond [(includes-one-of (send this get-always*-code) '((seconds) (milliseconds)))
                    (set! sampling (cons 'pull sampling))])
             ; if there are when constraints, sampling should include push
             (cond [(not (null? when-holders))
                    (set! sampling (cons 'push sampling))])])
      sampling)
    ; Helper function for get-sampling.  items should be a list of temporal function calls.
    ; Return true if code contains one of the calls.
    (define (includes-one-of code items)
      (cond [(member code items) #t]
            [(pair? code) (or (includes-one-of (car code) items) (includes-one-of  (cdr code) items))]
            [else #f]))
        
    ; handling 'when'
    (define/public (add-when holder)
      (set! when-holders (cons holder when-holders)))
    
    (define/override (milliseconds)
      symbolic-time)
    (define/override (milliseconds-evaluated)
      (send this wally-evaluate symbolic-time))
    
    (define/override (concrete-image)
      (send this wally-evaluate (send this image)))

    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.  Note that the calls to solve in this method don't
    ; change the current solution (held in an instance variable defined in thing%).
    (define/override (find-time mytime target)
      ; if there aren't any when statements, just return the target time, otherwise solve for the time to jump to
      (cond [(null? when-holders) target]
            [else (define solver (current-solver)) ; can use direct calls to solver b/c we aren't doing finitization!
                  (assert (> symbolic-time mytime))
                  (assert (or (equal? symbolic-time target)
                              (and (< symbolic-time target) 
                                   (ormap (lambda (w) ((when-holder-condition w))) when-holders))))
                  ; add all required always, always*, and stays to the solver
                  (send this solver-add-required solver)
                  (solver-assert solver (asserts))

                  (solver-minimize solver (list symbolic-time)) ; ask Z3 to minimize the symbolic time objective
                  (define sol (solver-check solver))
                  (define min-time (evaluate symbolic-time sol))
                  ; make sure that this is indeed a minimum (not an infinitesimal)
                  ; trying to advance time by an infinitesimal amount would loop forever
                  (solver-assert solver (list (< symbolic-time min-time)))
                  (unless (unsat? (solver-check solver))
                    (error 'find-time "can only advance time by an infinitesimal amount"))
                  (clear-asserts!)
                  (solver-clear solver)
                  ; make sure we aren't stuck
                  (racket-when (equal? mytime min-time)
                               (error 'find-time "unable to find a time to advance to that is greater than the current time"))
                  min-time]))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
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
                 ; Assert the constraints in all of the bodies of the active whens.  Also, solving clears the
                 ; global assertion store, so add that back in.  This includes the assertion that symbolic-time
                 ; equals next-time.  Then solve again.
                 (for-each (lambda (w) ((when-holder-body w))) active-whens)
                 (for-each (lambda (a) (assert a)) saved-asserts)
                 (send this solve)
                 ; If any whens were activated tell the viewers that this thing changed.  (It might not actually
                 ; have changed but that's ok -- we just don't want to miss telling them if it did.)
                 (cond [(not (null? active-whens))
                        (send this notify-watchers)])
                 ; if we didn't get to the target try again
                 (cond [(< next-time target) 
                        (advance-time-helper target)]))])))))
