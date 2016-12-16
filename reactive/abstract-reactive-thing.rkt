#lang s-exp rosette
; superclass for both reactive-thing% (which is interpreted) and compiled-reactive-thing%

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(provide abstract-reactive-thing% send-thing send-syncd define-public-symbolic*
         mouse-event mouse-event-time mouse-event-pt mouse-event-button-state)
; Each reactive thing has its own thread.  Any messages from another thread should use send-thing
; or send-syncd for synchronization, rather than sending an ordinary message to the thing.
; Internally there are some helper functions, and also some methods for use in constraint tests
; like (seconds) and (milliseconds).  When used in a when constraint they can be called directly,
; since the constraint test or body will be evaluated in the thing's thread -- otherwise they
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

; Macro to define a symbolic variable and also a sync'd access method (mostly for writing tests). A call
;         (define-public-symbolic* x y)
; defines two Rosette symbolic variables x and y, and generates access methods get-x and get-y.
; The access methods use evaluate-syncd so that they go via the thing's thread.
; This is for use within a class definition, since it generates a send to 'this' among other things.
(require (for-syntax racket/syntax))
(define-syntax (define-public-symbolic* stx)
  (syntax-case stx ()
    [(_ type)  ; base case for recursion - no variables to define
     #'(void)]
    [(_ v1 vs ... type)
     (with-syntax ([getter (format-id stx "get-~a" #'v1)])
       #'(begin (define-symbolic* v1 type)
                (define/public (getter) (send-syncd this evaluate-syncd (lambda () (send this wally-evaluate v1))))
                (define-public-symbolic* vs ... type)))]))

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
         ; Got a event from the viewer.  Add it to the list of mouse-events using the mouse-event struct.
         ; Formerly: to avoid cycles, we assumed the event occurred at least 1 millisecond after the thing's
         ; current internal time.  Doesn't seem necessary any more -- although we might now end up with two
         ; events with the same time stamp.
         (let* ([tm (if (null? event-time) (current-clock-time) event-time)]
                [ev (mouse-event tm (point event-x event-y) state)])
           (set! mouse-events (cons ev mouse-events)))
         ; If this is a button press event (rather than just a move), revise when to wake up next if need be.
         ; (This could be wake up right now.)  This is a heuristic -- later if user-defined events could be
         ; caused just by mouse moves then we'd need to account for that as well.  In any case only pay attention
         ; to the button press (in terms of setting an alert) if this thing is being observed
         (cond [(and (not (set-empty? watchers)) (or (eq? state 'going-down) (eq? state 'going-up)))
                (set-alert-helper)])]
        [(list 'milliseconds-syncd ch)
         (channel-put ch (send this milliseconds-evaluated))]
        [(list 'set-alert)
         (set-alert-helper)]
        ; evaluate thunk for side effects only (no synchronization)
        [(list 'do-evaluate thunk)
         (thunk)]
        [(list 'evaluate-syncd ch thunk)
         (channel-put ch (thunk))]
        [_
         (error "thread message not understood: ~a\n" r)]))
    
    ; start should be called as the last step for initializing a new thing, in particular after all of the
    ; when and while constraints are added.  It should initialize the time to 0 and solve any active constraints.
    (define/public (start)
      ; default is to not do anything - this is overridden in reactive-thing%
      (void))

    ; get-sampling says how to sample.  It should be one of '(push) '(pull) '(push pull) or '()
    (define/public (get-sampling)
      (error "get-sampling -- subclass responsibility\n"))
    
    ; *** button events ***
    ; button-going-down? can test whether the button is going down at the current time, or find a time
    ; the button is going down -- similarly for button-going-up?
    (define/public (button-going-down?)
      (button-going-up-or-down? 'going-down))
    (define/public (button-going-up?)
      (button-going-up-or-down? 'going-up))
    (define (button-going-up-or-down? direction)
      (let ([t (send this milliseconds)])
        ; findf will return the event if found, but we want this function to return just #t or #f
        (not (eq? #f (findf (lambda (e) (and (equal? t (mouse-event-time e)) (eq? direction (mouse-event-button-state e))))
                            mouse-events)))))
    ; If we don't have a record of mouse-position at the current time, interpolate between the two nearest times.
    ; Note that time is evaluated for this function - we don't try to find a time that corresponds to a position.
    (define/public (mouse-position)
      (interpolate-mouse-position (send this milliseconds-evaluated) mouse-events))
    ; return the state of the button at the current time (time is evaluated for this function also)
    ; if we don't have an event for the exact time, use the closest previous time
    (define/public (button-state)
      (button-state-helper mouse-events (send this milliseconds-evaluated)))
    ; convenience function to test for button down or going-up
    (define/public (button-pressed?)
      (let ([s (send this button-state)])
        (or (eq? s 'down) (eq? s 'going-up))))

    ; Helper function for mouse-position.  The events in the list are with most recent event first -- find
    ; mouse position at the exact time, or else interpolate the mouse position between the two closest events
    (define (interpolate-mouse-position time events)
      (cond [(null? events) (point 0 0)]
            ; formerly: [(null? events) (error 'interpolate-mouse-position "couldn't find a time")]
            [(null? (cdr events)) (mouse-event-pt (car events))] ; just one event in the list
            [(equal? time (mouse-event-time (car events))) (mouse-event-pt (car events))]  ; exact time found
            [(> time (mouse-event-time (cadr events))) ; interpolate between the first two events on the list
             (let* ([e1 (car events)]
                    [e2 (cadr events)]
                    [x1 (point-x (mouse-event-pt e1))]
                    [y1 (point-y (mouse-event-pt e1))]
                    [x2 (point-x (mouse-event-pt e2))]
                    [y2 (point-y (mouse-event-pt e2))]
                    [t1 (mouse-event-time e1)]
                    [t2 (mouse-event-time e2)])
               ; If the two or more events have the same time stamp (which happens occasionally), then use the
               ; position of the most recent event, which will be the first one on the list; and otherwise interpolate.
               (if (= t1 t2)
                   (point x1 y1)
                   (let* ([ratio (/ (exact->inexact (- t1 time)) (exact->inexact (- t1 t2)))]
                          [x (- x1 (* ratio (- x1 x2)))]
                          [y (- y1 (* ratio (- y1 y2)))])
                     (point (round x) (round y)))))]
            [else (interpolate-mouse-position time (cdr events))]))
    
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
      (/ (send this milliseconds-evaluated) 1000))
    
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
    ; 'when' test true or is an interesting time for a 'while'.  If there aren't any such values between the
    ; current and target times, then return the target.  Note that the calls to solve in this function use a
    ; separate assertion stack, leaving alone the global stack and solution.
    ; Return two values: the time to advance to, and either #f or a new target
    ; (the new target is used if we are doing a binary search for a time to advance to)
    (define/public (find-time mytime target)
      (error "find-time -- subclass responsibility\n"))

    (define/public (advance-time-and-prune-events target)
      (send this advance-time-helper target)
      ; prune all events that occurred before the current time - except leave one older event in the list
      ; (needed for interpolating mouse position)
     (set! mouse-events (pruned-event-list mouse-events (send this milliseconds-evaluated))))

    ; Helper function to return a new event list that includes only events that occurred after time t
    ; (leaving one older event, which might be needed for interpolation).  Note that the event list is in
    ; reverse chronological order, i.e. the first event on the list is the most recent one.
    (define (pruned-event-list events t)
      (cond [(null? events) events]
            [(null? (cdr events)) events]
            [(< (mouse-event-time (car events)) t) (list (car events))]
            [else (cons (car events) (pruned-event-list (cdr events) t))]))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' test true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    ; In addition, this method should notify viewers if the sampling regime should be changed, and also
    ; if this thing has potentially changed (so that the viewer should refresh the image).  This is done
    ; using the notify-watchers-update-sampling and notify-watchers-changed messages respectively (both
    ; defined in this class).
    (define/public (advance-time-helper target)
      (error "advance-time-helper -- subclass responsibility\n"))
    
    ; ** alerts **
    (define (set-alert-helper)
      ; Set an alert (using a new thread) that wakes up at the next time a when test will hold given
      ; current knowledge.  If there is a button press or other external event in the meantime, set-alert
      ; will be called again and the event accounted for.  If no interesting times found, wake up in a week.
      ; In practice this means that the user will probably have exited the program in the meantime, but it
      ; would still be ok to wake up then.  (If +infinity is allowed as a real, we could use that as the first
      ; value, and then avoid setting up a thread at all.  This seems a bit cleaner and also like it would
      ; make zero difference in practice.)
      (terminate-old-alert)
      (let*-values ([(mytime) (send this milliseconds-evaluated)]
                    [(in-a-week) (+ mytime (* 1000 60 60 24 7))] ; a week from now
                    [(target ignore) (find-time mytime in-a-week)]
                    [(seconds-to-sleep)  (/ (- target (current-clock-time)) 1000.0)])
        ; make a new thread that wakes up at target and advances time to the target, then recursively
        ; sets another alert
        (set! alert (thread (lambda ()
                              ; seconds-to-sleep might be negative, if clock time advanced beyond the target already
                              (cond [(> seconds-to-sleep 0) (sleep seconds-to-sleep)])
                              ; we might already have gone by the target -- that's ok, since advance-time-syncd
                              ; won't do anything in that case
                              (send-syncd this advance-time-syncd target)
                              (send-thing this set-alert))))))
    (define (terminate-old-alert) ; disable any existing alerts
      (cond [(not (null? alert))
             (kill-thread alert)
             (set! alert null)]))))
