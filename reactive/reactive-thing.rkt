#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(provide when reactive-thing% send-thing send-syncd)
; Each reactive thing has its own thread.  Any messages from another thread should use send-thing
; or send-syncd for synchronization, rather than sending an ordinary message to the thing.
; Internally there are some helper functions, and also some methods for use in constraint conditions
; like (seconds) and (milliseconds).  When used in a when constraint they can be called directly,
; since the constraint condition or body will be evaluated in the thing's thread -- otherwise they
; should be called using evaluate-syncd.

; all times are relative to the time the program is started, to keep the number of bits down
(define time-at-startup (current-milliseconds))

; struct to hold whens -- the condition and body are both thunks (anonymous lambdas)
(struct when-holder (condition body) #:transparent)
; Definition of 'when' macro.
; Note that this overrides the built-in Racket 'when' - but easy to just use 'cond' for that.
; 'when' should be used within an instance of reactive-thing or a subclass since it references 'this'
(define-syntax-rule (when test e ...)
  (send this add-when (when-holder (lambda () test) (lambda () e ...))))

; send-thing is like send, except that the message is directed to the thing's thread
(define-syntax-rule (send-thing thing msg args ...)
  (thread-send (send thing get-thread) (list 'msg args ...)))
; send-syncd is similar, but blocks until there is a response on the channel.  It's a way to get
; a value back, but can also be used just for synchronization.
(define-syntax-rule (send-syncd thing msg args ...)
  (let ([c (make-channel)])
    (thread-send (send thing get-thread) (list 'msg c args ...))
    (channel-get c)))

(define reactive-thing%
  (class object%
    ; initialization argument:
    ;   image is a graphical object that is returned in response to the 'get-image' message
    (init [init-image null])
    (define myimage init-image)
    ; sampling says how to sample.  It starts out as #f, and then is set the first time the (get-sampling)
    ; function is called.  After that it is one of '(push) '(pull) '(push pull) or '()
    (define sampling #f)
    ; viewers on this thing
    (define watchers (mutable-set))
    (define when-holders '())  ; list of whens
    ; alert is either a thread for the current alert, or null if none
    (define alert null)
    ; list of times that a button down event occurred, and a matching list of locations
    (define button-down-event-times '())
    (define button-down-locations '())
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
    
    ; Loop for receiving thread messages.  The 'match' part is in a separate method named
    ; match-thread-receive to allow it to be overridden or augmented in subclasses.
    (define mythread (thread
                      (lambda ()
                        (let loop ()
                          ; check for done outside the loop part ...
                          (let ([r (thread-receive)])
                            (match r
                              ['(exit) (printf "exiting \n")]
                              [_ (send this match-thread-receive r)
                                 (loop)]))))))
    (super-new)
    
    ; Match the message received by mythread and do the appropriate action.  If overridden
    ; in a subclass, the overriding method should also invoke this method using super.
    ; Messages are lists consisting of the message name (a symbol) followed by any arguments.
    (define/public (match-thread-receive r)
      (match r
        [(list 'show dc)
         (send dc clear)
         (showthing (evaluate myimage mysolution) dc)]
        [(list 'show-syncd ch dc)
         (send dc clear)
         (showthing (evaluate myimage mysolution) dc)
         (channel-put ch null)]
        [(list 'advance-time target)
         (advance-time-helper target)]
        [(list 'advance-time)  ; version with default arg for target
         (advance-time-helper (current-time))]
        [(list 'advance-time-syncd ch target)
         (advance-time-helper target)
         (channel-put ch null)]
        [(list 'advance-time-syncd ch)
         (advance-time-helper (current-time))
         (channel-put ch null)]
        [(list 'watched-by-syncd ch v)
         (set-add! watchers v)
         ; if this is the first watcher set up an alert to do push notification
         (cond [(equal? 1 (set-count watchers)) (set-alert-helper)])
         (channel-put ch null)]
        [(list 'unwatched-by-syncd ch v)
         (set-remove! watchers v)
         ; if this was the last watcher terminate any alert
         (terminate-old-alert)]
        [(list 'button-down-event event-time mx my)
         ; to avoid cycles, assume the button down event occurred at least 1 millisecond after the current symbolic-time
         (set! button-down-event-times
               (cons (max (+ 1 (evaluate symbolic-time mysolution)) (if (null? event-time) (current-time) event-time))
                     button-down-event-times))
         (set! button-down-locations (cons (point mx my) button-down-locations))
         ; revise when to wake up next if need be (could be wake up right now)
         ; only pay attention to the button press (in terms of setting an alert) if this thing is being observed
         (cond [(not (set-empty? watchers)) (set-alert-helper)])]
        [(list 'milliseconds-syncd ch)
         (channel-put ch (evaluate symbolic-time mysolution))]
        [(list 'set-alert)
         (set-alert-helper)]
        ; evaluate thunk for side effects only (no syncronization)
        [(list 'do-evaluate thunk)
         (thunk)]
        [(list 'evaluate-syncd ch thunk)
         (channel-put ch (thunk))]
        [_
         (error "thread message not understood: ~a\n" r)]))
    
    (define/public (previous x)
      (evaluate x mysolution))
    
    (define/public (get-sampling)
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
    
    ; button events
    (define/public (button-pressed)
      (member symbolic-time button-down-event-times))
    ; if we don't have a record of mouse-position at the current time, return (point 0 0)
    (define/public (mouse-position)
      (or (for/first ([t button-down-event-times]
                      [p button-down-locations]
                      #:when (equal? (evaluate symbolic-time mysolution) t))
            p)
          (point 0 0)))
    
    
    ; handling 'when'
    (define/public (add-when holder)
      (set! when-holders (cons holder when-holders)))
    
    ; The (seconds) method returns the current time in seconds as a float.  This is evaluated (mostly so
    ; that it can be used as an argument to sin).  Note that if used in a constraint, the constraint needs
    ; to use always* rather than always so that (seconds) is re-evaluated each time.
    (define/public (seconds)
      (exact->inexact (/ (evaluate symbolic-time mysolution) 1000.0)))
    ; return milliseconds - inconsistently, this is NOT evaluated.  milliseconds is always an integer.
    (define/public (milliseconds)
      symbolic-time)
    (define/public (milliseconds-evaluated)
      (evaluate symbolic-time mysolution))
    (define/public (image) myimage)
    ; define a get-thread method to make mythread accessible for the send-thing and send-syncd macros
    (define/public (get-thread) mythread)

    ; viewers -- make these into thread messages
    (define/public (watched-by v)
      (set-add! watchers v)
      ; if this is the first watcher set up an alert to do push notification
      (cond [(equal? 1 (set-count watchers)) (set-alert-helper)]))
    (define/public (unwatched-by v)
      (set-remove! watchers v)
      ; if this was the last watcher terminate any alert
      (terminate-old-alert))

    ; helper functions
    (define (current-time)
      (- (current-milliseconds) time-at-startup))
    
    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.  Note that the calls to solve in this function use a
    ; separate assertion stack, leaving alone the global stack and solution.
    (define (find-time cur-time target)
      ; if there aren't any when statements, just return the target time, otherwise solve for the time to jump to
      (cond [(null? when-holders) target]
            [else (assert (> symbolic-time cur-time))
                  (assert (or (equal? symbolic-time target)
                              (and (< symbolic-time target) (ormap (lambda (w) ((when-holder-condition w))) when-holders))))
                  ; in wallingford.rkt we needed to hack around the start condition:
                  ;     (let minimize ([keep-going (<= 0 (abs total-penalty))])
                  (let search ([keep-going #t])
                    (with-handlers ([exn:fail? void])    ; if unsat we are done: (current-solution) holds the minimum value for current-time
                      ; (when debug (printf "in minimize - keep-going: ~a\n" keep-going))
                      ; keep-going is true if we can find a symbolic-time that is less than the one in the current solution
                      ; if the value of this expression is true then one of the 'when' conditions holds
                      ;     (ormap (lambda (w) ((when-holder-condition w))) when-holders)
                      (solve (assert keep-going)) ; the best solution seen so far.
                      (search (< symbolic-time (evaluate symbolic-time)))))
                  (clear-asserts)
                  ; symbolic-time still retains its value in the solution even though we are clearing asserts
                  (evaluate symbolic-time)]))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define (advance-time-helper target)
      (let ([cur-time (evaluate symbolic-time mysolution)])
        ; make sure we haven't gone by the target time already - if we have, don't do anything
        (cond [(< cur-time target)
               (let ([next-time (find-time cur-time target)])
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
                        (for/set ([w watchers]) (send-thing w thing-changed))])
                 ; if we didn't get to the target try again
                 (if (< next-time target) (advance-time-helper target) (void)))])))
    
    ; ** alerts **
    (define (set-alert-helper)
      ; Set an alert (using a new thread) that wakes up at the next time a when condition will hold given 
      ; current knowledge.  If there is a button press or other external event in the meantime, set-alert
      ; will be called again and the event accounted for.  If no interesting times found, wake up in a week.
      ; In practice this means that the user will probably have exited the program in the meantime, but it
      ; would still be ok to wake up then.  If we go to real-valued time, and include +infinity as a real,
      ; we could use that as the first value, and then avoid setting up a thread at all.  (This seems a bit
      ; cleaner and also like it would make zero difference in practice.)
      (terminate-old-alert)
      (let* ([cur-time (evaluate symbolic-time mysolution)]
             [in-a-week (+ cur-time (* 1000 60 60 24 7))] ; a week from now
             [target (find-time cur-time in-a-week)]
             [seconds-to-sleep  (/ (- target (current-time)) 1000.0)])
        ; make a new thread that wakes up at target and advances time to the target, then recursively
        ; sets another alert
        (set! alert (thread (lambda ()
                              ; seconds-to-sleep might be negative, if clock time advanced beyond the target already
                              (cond [(> seconds-to-sleep 0.0) (sleep seconds-to-sleep)])
                              ; we might already have gone by the target -- that's ok, since advance-time-syncd
                              ; won't do anything in that case
                              (send-syncd this advance-time-syncd target)
                              (send-thing this set-alert))))))
    (define (terminate-old-alert) ; disable any existing alerts
      (cond [(not (null? alert))
             (kill-thread alert)
             (set! alert null)]))))
