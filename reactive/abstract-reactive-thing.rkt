#lang s-exp rosette
; superclass for both reactive-thing% (which is interpreted) and compiled-reactive-thing%

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(provide abstract-reactive-thing% send-thing send-syncd
         mouse-event mouse-event-time mouse-event-pt mouse-event-button-state)
; Each reactive thing has its own thread.  Any messages from another thread should use send-thing
; or send-syncd for synchronization, rather than sending an ordinary message to the thing.
; Internally there are some helper functions, and also some methods for use in constraint conditions
; like (seconds) and (milliseconds).  When used in a when constraint they can be called directly,
; since the constraint condition or body will be evaluated in the thing's thread -- otherwise they
; should be called using evaluate-syncd.

; send-thing is like send, except that the message is directed to the thing's thread
(define-syntax-rule (send-thing thing msg args ...)
  (thread-send (send thing get-thread) (list 'msg args ...)))
; send-syncd is similar, but blocks until there is a response on the channel.  It's a way to get
; a value back, but can also be used just for synchronization.
(define-syntax-rule (send-syncd thing msg args ...)
  (let ([c (make-channel)])
    (thread-send (send thing get-thread) (list 'msg c args ...))
    (channel-get c)))

; all times are relative to the time the program is started
(define time-at-startup (current-milliseconds))

; struct to hold information for a mouse or button event
(struct mouse-event (time pt button-state) #:transparent)

(define abstract-reactive-thing%
  (class thing%
    (super-new)
    ; myimage is a graphical object that is returned in response to the 'get-image' message
    (define myimage null)
    ; viewers on this thing
    (define watchers (mutable-set))
    ; alert is either a thread for the current alert, or null if none
    (define alert null)
    ; list of pairs (time,event)
    (define mouse-events '())
   
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
    
    ; Match the message received by mythread and do the appropriate action.  If overridden
    ; in a subclass, the overriding method should also invoke this method using super.
    ; Messages are lists consisting of the message name (a symbol) followed by any arguments.
    (define/public (match-thread-receive r)
      (match r
        [(list 'show dc)
         (send dc clear)
         (showthing (send this concrete-image) dc)]
        [(list 'show-syncd ch dc)
         (send dc clear)
         (showthing (send this concrete-image) dc)
         (channel-put ch null)]
        [(list 'advance-time target)
         (send this advance-time-and-prune-events target)]
        [(list 'advance-time)  ; version with default arg for target
         (send this advance-time-and-prune-events (current-clock-time))]
        [(list 'advance-time-syncd ch target)
         (advance-time-and-prune-events target)
         (channel-put ch null)]
        [(list 'advance-time-syncd ch)
         (advance-time-and-prune-events (current-clock-time))
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
        [(list 'get-sampling-syncd ch)
         (channel-put ch (send this get-sampling))]
        [(list 'mouse-event event-time event-x event-y state)
         ; Got a event from the viewer.  Add it to the list of mouse-events using the mouse-event struct.  To avoid
         ; growing the list of events excessively, if the first event in the list is an 'up or 'down event and the
         ; new event is also an 'up or 'down event respectively, replace the first element with the new event.
         ; Otherwise just cons the new event onto the list.  As a result we always save 'going-up and 'going-down
         ; events, until they are pruned because this thing's time has advanced beyond the event time.
         ; Formerly: to avoid cycles, we assumed the event occurred at least 1 millisecond after the thing's
         ; current internal time.  Doesn't seem necessary any more???
         (let* ([t (if (null? event-time) (current-clock-time) event-time)]
                [e (mouse-event t (point event-x event-y) state)])  ; e is the new event
           (cond [(null? mouse-events) (set! mouse-events (list e))]
                 [(and (or (eq? state 'up) (eq? state 'down)) (eq? state (mouse-event-button-state (car mouse-events))))
                  (set! mouse-events (cons e (cdr mouse-events)))]
                 [else (set! mouse-events (cons e mouse-events))]))
         ; revise when to wake up next if need be (could be wake up right now)
         ; only pay attention to the button press (in terms of setting an alert) if this thing is being observed
         (cond [(not (set-empty? watchers)) (set-alert-helper)])]
        [(list 'milliseconds-syncd ch)
         (channel-put ch (send this milliseconds-evaluated))]
        [(list 'set-alert)
         (set-alert-helper)]
        ; evaluate thunk for side effects only (no syncronization)
        [(list 'do-evaluate thunk)
         (thunk)]
        [(list 'evaluate-syncd ch thunk)
         (channel-put ch (thunk))]
        [_
         (error "thread message not understood: ~a\n" r)]))
    
    ; get-sampling says how to sample.  It should be one of '(push) '(pull) '(push pull) or '()
    (define/public (get-sampling)
      (error "get-sampling -- subclass responsibility\n"))
    
    ; *** button events ***
    ; button-going-down? can test whether the button is going down at the current time, or find a time
    ; the button is going down
    (define/public (button-going-down?)
      (let ([t (send this milliseconds)])
        (not (eq? #f (findf (lambda (e) (and (equal? t (mouse-event-time e)) (eq? 'going-down (mouse-event-button-state e))))
                           mouse-events)))))
    (define/public (button-going-up?)
      (let ([t (send this milliseconds)])
        (not (eq? #f (findf (lambda (e) (and (equal? t (mouse-event-time e)) (eq? 'going-up (mouse-event-button-state e))))
                           mouse-events)))))
    
    ; Return the mouse position for the current time (call it t):
    ;   - if there are no events, return a default position (point 0 0)
    ;   - if there is an event exactly at t, return the position for it
    ;   - otherwise, if there are events after t, return the position for the soonest event after t
    ;   - otherwise, return the position for the most recent event before t
    ; The rationale for this is that we record all 'going-up and 'going-down events, so if we don't have an event
    ; for t, we assume for now that t isn't an interesting time as far as the mouse is concerned.  (Later it might
    ; be interesting if we can have 'when' conditions involving e.g. the mouse position being equal to some value.)
    ; If t isn't an interesting time, we favor beging a little ahead of the actual state at t.
    ; An earlier version of the code - tagged as v1.0 - saved all mouse events, including all move events, and
    ; interpolated if need be.  It had the defect that the list of events got very long.  This new version avoids that,
    ; but doesn't do interpolations and doesn't save so many events.
    (define/public (mouse-position)
      (if (null? mouse-events)
          (point 0 0)   ; formerly gave an erropr
          (mouse-position-helper (send this milliseconds-evaluated) mouse-events)))
    (define (mouse-position-helper t events)
      (cond [(null? (cdr events))
             (mouse-event-pt (car events))]
            [(and (<= t (mouse-event-time (car mouse-events))) (> t (mouse-event-time (cadr mouse-events))))
             (mouse-event-pt (car events))]
            [else (mouse-position-helper t (cdr events))]))
    
    ; return the state of the button at the current time (time is evaluated for this function also)
    ; if we don't have an event for the exact time, use the closest previous time
    (define/public (button-state)
      (button-state-helper mouse-events (send this milliseconds-evaluated)))
    ; convenience function to test for button down or going-up
    (define/public (button-pressed?)
      (let ([s (send this button-state)])
        (or (eq? s 'down) (eq? s 'going-up))))
    (define (button-state-helper events time)
      (if (null? events) 'up  ; assume button starts up
          (let* ([e1 (car events)]
                 [t1 (mouse-event-time e1)]
                 [s1 (mouse-event-button-state e1)])
            (cond  [(= t1 time) s1] ; found exact time
                   [(> t1 time) (button-state-helper (cdr events) time)] ; keep looking
                   ; If we get this far, the time for the most recent event in the list 'events' is before 'time'.
                   ; If the button was going down at some time before 'time', then it is down at 'time'.
                   [(eq? s1 'going-down) 'down]
                   [(eq? s1 'down) 'down]
                   [else 'up]))))
    
    ; Methods to access time
    (define/public-final (current-clock-time)
      (- (current-milliseconds) time-at-startup))
    ; Other time access methods.  These other methods don't directly use the thing's thread -- instead they
    ; are intended to always be called from code the thread is running
    ; The (seconds) method returns the current time in seconds as a float.  This is evaluated (mostly so
    ; that it can be used as an argument to sin).
    ; (milliseconds should be overridden in subclasses to return the thing's current idea of its own time.
    ; It can be symbolic.
    (define/public (milliseconds)
      (error "milliseconds -- subclass responsibility\n"))
    ; If milliseconds is symbolic, this should evaluate it.  Otherwise it just returns milliseconds.
    (define/public (milliseconds-evaluated)
      (send this milliseconds))
    (define/public (seconds)
      (exact->inexact (/ (send this milliseconds-evaluated) 1000.0)))
    
    ; Accessing the thing's image.  concrete-image should be overridden if the image needs to be evaluated before drawing it
    (define/public (image) myimage)
    (define/public (concrete-image) myimage)
    (define/public (set-image! newimage)
      (set! myimage newimage))
    
    (define/public (get-mouse-events)
      mouse-events)
    
    (define/public-final (get-thread) mythread)

    ; viewers -- make these into thread messages
    (define/public (watched-by v)
      (set-add! watchers v)
      ; if this is the first watcher set up an alert to do push notification
      (cond [(equal? 1 (set-count watchers)) (set-alert-helper)]))
    (define/public (unwatched-by v)
      (set-remove! watchers v)
      ; if this was the last watcher terminate any alert
      (terminate-old-alert))
    (define/public (get-watchers)
      watchers)

    (define/public-final (notify-watchers-changed)
      ; Notify any viewers that this thing has changed.  (It might not actually
      ; have changed but that's ok -- we just don't want to miss telling them if it did.)
      (for/set ([w watchers]) (send-thing w thing-changed)))
    (define/public-final (notify-watchers-update-sampling)
      ; Notify any viewers that they should update their sampling regime for this thing.
      (for/set ([w watchers]) (send-thing w update-sampling)))
    
    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.  Note that the calls to solve in this function use a
    ; separate assertion stack, leaving alone the global stack and solution.
    (define/public (find-time mytime target)
      (error "find-time -- subclass responsibility\n"))

    (define/public (advance-time-and-prune-events target)
      (send this advance-time-helper target)
      ; prune all events that occurred before the current time - except leave one older event in the list
      ; (needed for interpolating mouse position)
     (set! mouse-events (prune-event-list mouse-events (send this milliseconds-evaluated))))

    ; helper function to return a new event list that includes only events that occurred at or after time t,
    ; leaving one older event if the list would be otherwise empty (so that we can use its state)
    (define (prune-event-list events t)
      (cond [(null? events) null]
            [(< t (mouse-event-time (car events))) ; first event occurred after t
             (cons (car events) (prune-event-list (cdr events) t))]
            [else (list (car events))]))  ; first event occurred at or before t
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true or is an
    ; interesting time for a 'while'.  Solve all constraints in active when and while constraints.  If this makes
    ; additional 'when' or 'while' constraints active, include those also, and keep iterating until a fixpoint is
    ; reached and no more constraints become active.  It is an error if this would result in an already-active
    ; constraint subsequently becoming inactive - raise an exception if that occurs.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    ; Finally, this method should notify viewers if the sampling regime should be changed, and also
    ; if this thing has potentially changed (so that the viewer should refresh the image).  This is done
    ; using the notify-watchers-update-sampling and notify-watchers-changed messages respectively (both
    ; defined in this class).
    (define/public (advance-time-helper target)
      (error "advance-time-helper -- subclass responsibility\n"))
    
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
      (let* ([mytime (send this milliseconds-evaluated)]
             [in-a-week (+ mytime (* 1000 60 60 24 7))] ; a week from now
             [target (find-time mytime in-a-week)]
             [seconds-to-sleep  (/ (- target (current-clock-time)) 1000.0)])
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
