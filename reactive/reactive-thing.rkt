#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")
; make Racket's version of 'when' available also
(require (only-in racket [when racket-when]))

(provide when while racket-when max-value min-value reactive-thing% interesting-time?)

; Definition of 'when' and 'while' macros.  These should be used within an instance of reactive-thing
; or a subclass since they reference 'this'.
;
; 'when' macro.  This overrides the built-in Racket 'when' - use 'racket-when' or 'cond' for that.
(define-syntax-rule (when test e ...)
  (send this add-when-holder (when-holder (lambda () test) (lambda () e ...))))
; 'while' macro.  #:interesting-time is an optional argument - it is a function that returns true if the current symbolic-time
; is an 'interesting' time, i.e., advance-time should stop at that time and evaluate because something may happen in the
; 'while' that will affect future state.  There are currently two simple cases for which the system can synthesize
; #:interesting-time, namely checking for button-pressed? and checking for milliseconds within a given range (with a
; rigid syntax for this).  Otherwise if no explicit #:interesting-time function is given it's an error.
(define-syntax while
  (syntax-rules (and <= >= button-pressed? milliseconds)
    ((while condition #:interesting-time interesting e ...)
     (add-while condition interesting e ...))
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
    ; plus the analogous versions for the lower bound test, so 4 possible combinations in all.  (Is there a better way to do this??)
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
(define-syntax-rule (add-while condition interesting e ...)
  (send this add-while-holder (while-holder (if (pull-sampling? '(e ...)) (gensym) #f)
                                            (lambda () condition)
                                            (lambda () interesting)
                                            (lambda () e ...))))
(define-syntax-rule (add-while-with-time-bounds lower upper e ...)
  (add-while (and (<= lower (send this milliseconds)) (<= (send this milliseconds) upper))
             (cond [(equal? lower (send this milliseconds)) 'first]
                   [(equal? upper (send this milliseconds)) 'last]
                   [else #f])
             e ...))
; structs to hold whens and whiles -- the condition and body are both thunks (anonymous lambdas)
(struct when-holder (condition body) #:transparent)
; for while-holder, pull-id is either a unique symbol (if pull sampling should be used while this 'while' is active, or #f if not
(struct while-holder (pull-id condition interesting body) #:transparent)
; Helper functions for get-sampling and adding while constraints.
(define (pull-sampling? code)
  (includes-one-of code '((seconds) (milliseconds) (mouse-position)
                          (button-pressed?) (button-going-down?) (button-going-up?) (button-going-up-or-down?))))
(define (includes-one-of code items)
  ; items should be a list of temporal function calls. Return true if code is or contains one of the calls.
  (cond [(member code items) #t]
        [(pair? code) (or (includes-one-of (car code) items) (includes-one-of  (cdr code) items))]
        [else #f]))
; interesting-time? will be rebound when evaluating a while -- it will be the value of the interesting-time function
; for that while at the current time.  It is used for accumulating operators such as max-value.
(define interesting-time? (make-parameter #f))

; max-value and min-value macros
; temporary version hacked to have a symbol as an additional argument.  This is used to save the current max in a hash.
;(define-syntax-rule (max-value expr id)
;   (send this max-helper (lambda () (send this wally-evaluate expr)) id (interesting-time?)))
; and here are the real versions, which use gensym
(define-syntax-rule (max-value expr)
  (max-or-min max expr))
(define-syntax-rule (min-value expr)
  (max-or-min min expr))
(define-syntax (max-or-min stx)
  (syntax-case stx ()
    [(_ fn expr)
     (with-syntax ([id (datum->syntax stx (gensym))])
       #'(send this max-min-helper fn (lambda () (send this wally-evaluate expr)) 'id (interesting-time?)))]))
; integration
(define-syntax (integral stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([intgl (symbolic-integral expr)]
                   [id (datum->syntax stx (gensym))])
       #'(send this integral-helper fn (lambda () (send this wally-evaluate intgl)) 'id (interesting-time?)))]))

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

    ; these methods are for use by the macros (not for general public use)
    (define/public (add-when-holder holder)
      (set! when-holders (cons holder when-holders)))
    (define/public (add-while-holder holder)
      (set! while-holders (cons holder while-holders)))
    ; For max and min, accumulator-values holds the current max (or min) value for a max-value or min-value
    ; expression, indexed by a gensym'd id for that occurence of max-value or min-value.
    ; For integral, accumulator-values holds the value of the expression for the integral at the start of
    ; the program or the start of the current 'while' in which it occurs.
    (define accumulator-values (make-hasheq))
    (define/public (max-min-helper max-or-min f id interesting)
      ; For max-value: if max-value isn't initialized yet or if this is the first time in the while, save the current maximum
      ; and otherwise update it by finding the max of the old and new values.
      ; And analogously for min-value.
      (let ([updated-value (if (or (not (hash-has-key? accumulator-values id)) (eq? interesting 'first))
                               (f)
                               (max-or-min (f) (hash-ref accumulator-values id)))])
        (hash-set! accumulator-values id updated-value)
        updated-value))
    ; symbolic integration - super simple to start with
    (define (symbolic-integral expr var)
      (match expr
        [v #:when (equal? v var) (list '* 0.5 var var)]
        [(? number? n) (list '* n var)]
        [_ (error "unable to find the symbolic integral of ~a \n" expr)]))
    (define/public (integral-helper f id interesting)
      )


    (define/override (milliseconds)
      symbolic-time)
    (define/override (milliseconds-evaluated)
      (send this wally-evaluate symbolic-time))
    
    (define/override (concrete-image)
      (send this wally-evaluate (send this image)))
    
    ; The variables push-sampling and pull-sampling say whether to do push or pull sampling respectively.
    ; push-sampling starts out as null, and then is set to #t or #f the first time the (get-sampling) function
    ; is called.  It doesn't change after that.  pull-sampling starts out as an empty set.  Whenever we enter
    ; a 'while' that needs pull sampling, we add the id for that 'while' to the pull-sampling set, and when we
    ; leave the 'while' we remove it.  In addition, if there are 'always' constraints that imply we need to do
    ; pull sampling for the lifetime of the entire program, we add an id to pull-sampling (and never remove it).
    ; So if pull-sampling is a non-empty set, we do pull sampling at that time.
    (define push-sampling null)
    (define pull-sampling (mutable-set))
    (define current-sampling #f)  ; for saving the sampling, so that we can notify viewers if it changes
    ; The get-sampling method should return one of '() '(push) '(pull) or '(push pull)
    (define/override (get-sampling)
      (cond [(null? push-sampling)  ; need to initialize things
             ; if there are when or while constraints, sampling should include push
             (set! push-sampling (or (not (null? when-holders)) (not (null? while-holders))))
             ; If any of the always constraints include a temporal reference, sampling should always include pull,
             ; so add a token (for no good reason named 'always) to the set that will always be there.
             (cond [(pull-sampling? (send this get-always-code)) (set-add! pull-sampling 'always)])
             ; If any while constraints are first active at the current time, add their ids to pull-sampling.
             ; Temporary (?) hack: if there are while constraints that are active at time 0, be sure and call
             ; get-sampling after the object is created so that this will be initialized properly.
             (for ([w while-holders])
                  (let ([why (send this wally-evaluate ((while-holder-interesting w)))]
                        [id (while-holder-pull-id w)])
                    ; 'why' is why this time is interesting, or #f if it's not
                    ; 'id' is the unique ID for this while if it should use pull sampling, or #f if not
                    (cond [(and (eq? why 'first) id) (set-add! pull-sampling id)])))])
      ; Once we get here, the variables are initialized.  Return the kind of sampling to use.
      (append (if push-sampling '(push) '()) (if (set-empty? pull-sampling) '() '(pull))))
    
    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true or is an interesting time for a 'while'.  If there aren't any such values
    ; between the current time and the target, then just return the target.  Note that the calls to solve in this
    ; method don't change the current solution, which is held in an instance variable defined in thing%.
    (define/override (find-time mytime target)
      ; If there aren't any when or while statements, just return the target time, otherwise solve for the time
      ; to which to advance.
      (cond [(and (null? when-holders) (null? while-holders)) target]
            [else (define solver (current-solver)) ; can use direct calls to solver b/c we aren't doing finitization!
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
               (let ([next-time (find-time mytime target)]
                     [notify-changed #f])
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
                 ; provide a parameter interesting-time? that is bound to a value indicating whether the current time is interesting,
                 ; and evaluate the while-holder's body in that environment
                 (for-each (lambda (w) (parameterize ([interesting-time? (send this wally-evaluate ((while-holder-interesting w)))])
                                         ((while-holder-body w))))
                           active-whiles)
                 (for-each (lambda (a) (assert a)) saved-asserts)
                 (send this solve)
                 ; Update the sampling regime if necessary, and if this object might have changed, notify viewers.
                 ; First check if interesting-time is true for any while constraints.  Do this before potentially
                 ; updating the sampling regime and notifying any viewers, since this potentially affects both.
                 ; We check all of the while constraints, not just the active ones, since in some cases interesting-time
                 ; is true just before the while becomes active.  If interesting-time is true for any while constraints:
                 ; - Set notify-changed to true.  (Even if we are using pull sampling, we want to do an extra push notification
                 ;   so that the viewer updates immediately.  This is important for the end of the interval in which a 'while'
                 ;   holds, since often that will be the last state the object is in.  Also, for good responsiveness we want to
                 ;   do it at the beginning of the interval in which the 'while' holds.  And if it's just some other interesting
                 ;   time, well, maybe it's interesting for some reason ... so update then as well (this last is less clear-cut).
                 ; - If this is the first time the while constraint holds, and if it includes temporal constraints in
                 ;   the body, then viewers of this thing should use pull notification as long as the constraint is
                 ;   active.  To set this up, add the token for the while to the set pull-sampling.
                 ; - If this is the last time that the while constraint holds, remove its token from the set pull-sampling
                 ;   if present.
                 (for ([w while-holders])
                   (let ([why (send this wally-evaluate ((while-holder-interesting w)))]
                         [id (while-holder-pull-id w)])
                     ; 'why' is why this time is interesting, or #f if it's not
                     ; 'id' is the unique ID for this while if it should use pull sampling, or #f if not
                     (cond [why (set! notify-changed #t)])
                     (cond [(and (eq? why 'first) id) (set-add! pull-sampling id)]
                           [(and (eq? why 'last) id) (set-remove! pull-sampling id)])))
                 ; notify watchers if the sampling regime has changed (and remember it in current-sampling)
                 (let ([new-sampling (send this get-sampling)])
                   (cond [(not (equal? new-sampling current-sampling))
                          (set! current-sampling new-sampling)
                          (send this notify-watchers-update-sampling)]))
                 ; If notify-changed is #t, or if any whens were activated, tell the viewers that this thing might
                 ; have changed.  (It might not actually have changed but that's ok -- we just don't want to miss
                 ; telling them if it did.)
                 (cond [(or notify-changed (not (null? active-whens)))
                        (send this notify-watchers-changed)])
                 ; if we didn't get to the target try again
                 (cond [(< next-time target) 
                        (advance-time-helper target)]))])))))
