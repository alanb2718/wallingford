#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")

(provide when reactive-thing%)

; struct to hold whens -- the condition and body are both thunks (anonymous lambdas)
(struct when-holder (condition body) #:transparent)
; Definition of 'when' macro.
; Note that this overrides the built-in Racket 'when' - but easy to just use 'cond' for that.
; 'when' should be used within an instance of reactive-thing or a subclass since it references 'this'
(define-syntax-rule (when test e ...)
  (send this add-when (when-holder (lambda () test) (lambda () e ...))))

(define reactive-thing%
  (class abstract-reactive-thing%
    (define when-holders '())  ; list of whens
    ; symbolic-time is this thing's idea of the current time.  It is in milliseconds, and is relative to
    ; time-at-startup.  Due to current Rosette limitations this is an integer rather than a float or real,
    ; although the semantics should be that time is continuous.  (If we can make it a real, it might as
    ; well be in seconds.)
    (define-symbolic* symbolic-time number?)
    ; start time out at 0
    (assert (equal? symbolic-time 0))
    ; mysolution is the current solution (this is stored explicitly since Rosette has a
    ; different current-solution for different threads)
    (define mysolution (wally-solve))
    (stay symbolic-time)
    
    (super-new)
        
    (define/public (previous x)
      (evaluate x mysolution))
    
    ; sampling says how to sample.  It starts out as #f, and then is set the first time the (get-sampling)
    ; function is called.  After that it is one of '(push) '(pull) '(push pull) or '()
    (define sampling #f)
    (define/override (get-sampling)
      ; if sampling is #f, compute what it should be and set it
      ; (this doesn't support dynamically adding constraints during the time the thing is running)
      (cond [(not sampling)
             (set! sampling null)
             ; if any of the always* constraints include a temporal reference, sampling should include pull
             (cond [(includes-one-of always*-code '((seconds) (milliseconds)))
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
      (evaluate symbolic-time mysolution))
    
    (define/override (concrete-image)
      (evaluate (send this image) mysolution))

    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.  Note that the calls to solve in this function use a
    ; separate assertion stack, leaving alone the global stack and solution.
    (define/override (find-time mytime target)
      ; if there aren't any when statements, just return the target time, otherwise solve for the time to jump to
      (cond [(null? when-holders) target]
            [else (assert (> symbolic-time mytime))
                  (assert (or (equal? symbolic-time target)
                              (and (< symbolic-time target) (ormap (lambda (w) ((when-holder-condition w))) when-holders))))
                  ; in wallingford.rkt we needed to hack around the start condition:
                  ;     (let minimize ([keep-going (<= 0 (abs total-penalty))])
                  (let search ([keep-going #t])
                    (with-handlers ([exn:fail? void])    ; if unsat we are done: (current-solution) holds the minimum value for my-time
                      ; (when debug (printf "in minimize - keep-going: ~a\n" keep-going))
                      ; keep-going is true if we can find a symbolic-time that is less than the one in the current solution
                      ; if the value of this expression is true then one of the 'when' conditions holds
                      ;     (ormap (lambda (w) ((when-holder-condition w))) when-holders)
                      (solve (assert keep-going)) ; the best solution seen so far.
                      (search (< symbolic-time (evaluate symbolic-time)))))
                  (clear-asserts!)
                  ; symbolic-time still retains its value in the solution even though we are clearing asserts
                  (evaluate symbolic-time)]))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define/override (advance-time-helper target)
      (let ([mytime (send this milliseconds-evaluated)])
        ; make sure we haven't gone by the target time already - if we have, don't do anything
        (cond [(< mytime target)
               (let ([next-time (find-time mytime target)])
                 (assert (equal? symbolic-time next-time))
                 ; Solve all constraints and then find which when conditions hold.  Put those whens in active-whens.
                 (define save-solution mysolution)
                 (set! mysolution (wally-solve mysolution))
                 (define active-whens (filter (lambda (w) (evaluate ((when-holder-condition w)) mysolution))
                                              when-holders))
                 ; assert the constraints in all of the bodies of the active whens and solve again
                 (for-each (lambda (w) ((when-holder-body w))) active-whens)
                 ; need to re-assert that symbolic-time equals next-time since wally-solve clears assertions
                 (assert (equal? symbolic-time next-time))
                 ; update mysolution starting with the saved-solution, so that 'previous' works correctly
                 (set! mysolution (wally-solve save-solution))
                 ; If any whens were activated tell the viewers that this thing changed.  (It might not actually
                 ; have changed but that's ok -- we just don't want to miss telling them if it did.)
                 (cond [(not (null? active-whens))
                        (send this notify-watchers)])
                 ; if we didn't get to the target try again
                 (if (< next-time target) (advance-time-helper target) (void)))])))))
    
