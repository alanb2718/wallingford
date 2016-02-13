#lang s-exp rosette
; superclass for compiled reactive things
; need to override the following methods in subclasses: [list them]

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "reactive-thing.rkt")

(provide compiled-reactive-thing%)

; all times are relative to the time the program is started, to keep the number of bits down
(define time-at-startup (current-milliseconds))

; send-thing and send-syncd are already defined in reactive-thing -- use those

(define compiled-reactive-thing%
  (class object%
    ; initialization argument:
    ;   image is a graphical object that is returned in response to the 'get-image' message
    (init [init-image null])
    (define myimage init-image)
    ; viewers on this thing
    (define watchers (mutable-set))
    ; alert is either a thread for the current alert, or null if none
    (define alert null)
    ; list of times that a button down event occurred, and a matching list of locations
    (define button-down-event-times '())
    (define button-down-locations '())
    ; my-time is the compiled equivalent of symbolic-time
    (define my-time 0)
    
    ; still have a thread per compiled reactive thing
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
         (showthing myimage dc)]
        [(list 'show-syncd ch dc)
         (send dc clear)
         (showthing myimage dc)
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
               (cons (max (+ 1 my-time) (if (null? event-time) (current-time) event-time))
                     button-down-event-times))
         (set! button-down-locations (cons (point mx my) button-down-locations))
         ; revise when to wake up next if need be (could be wake up right now)
         ; only pay attention to the button press (in terms of setting an alert) if this thing is being observed
         (cond [(not (set-empty? watchers)) (set-alert-helper)])]
        [(list 'milliseconds-syncd ch)
         (channel-put ch my-time)]
        [(list 'set-alert)
         (set-alert-helper)]
        ; evaluate thunk for side effects only (no syncronization)
        [(list 'do-evaluate thunk)
         (thunk)]
        [(list 'evaluate-syncd ch thunk)
         (channel-put ch (thunk))]
        [_
         (error "thread message not understood: ~a\n" r)]))
    
    (define/public (get-sampling)
      ; later: use an interface to avoid this
      (error "self subclass responsibility\n"))
    
    ; button events
    (define/public (button-pressed)
      (member my-time button-down-event-times))
    ; if we don't have a record of mouse-position at the current time, return (point 0 0)
    (define/public (mouse-position)
      (or (for/first ([t button-down-event-times]
                      [p button-down-locations]
                      #:when (equal? my-time t))
            p)
          (point 0 0)))
    
    
    ; The (seconds) method returns the current time in seconds as a float.
    (define/public (seconds)
      (exact->inexact (/ my-time 1000.0)))
    ; return milliseconds as an integer.
    (define/public (milliseconds)
      my-time)
    ; for use by subclasses - not for external use really
    (define/public (set-my-time new-time)
      (set! my-time new-time))
    (define/public (image) myimage)
    ; define a get-thread method so that mythread is accessible in subclasses
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
        
    (define/public (get-watchers)
      watchers)
    
    (define (current-time)
      (- (current-milliseconds) time-at-startup))
    
    ; new for compiled thing -- allow subclasses to set a new image
    (define/public (update-myimage newimage)
      (set! myimage newimage))
    ; also new for compiled thing -- give access to button-down-event-times
    (define/public (get-button-down-event-times)
      button-down-event-times)
    
    (define/public (update-mysolution)
      (error "should override in subclasses\n"))

    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.
    (define/public (find-time target)
      (error "should override in subclasses\n"))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define (advance-time-helper target)
      (let ([next-time (send this find-time target)])
        (send this set-my-time next-time)
        (send this update-mysolution)
        ; If any whens were activated tell the viewers that this thing changed.  (It might not actually
        ; have changed but that's ok -- we just don't want to miss telling them if it did.)
        (for/set ([w (send this get-watchers)]) (send-thing w thing-changed))
        (if (< next-time target) (advance-time-helper target) (void))))
    
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
      (let* ([in-a-week (+ my-time (* 1000 60 60 24 7))] ; a week from now
             [target (find-time in-a-week)]
             [seconds-to-sleep  (/ (- target (current-time)) 1000.0)])
        ; make a new thread that wakes up at target and advances time to the target, then recursively
        ; sets another alert
        (set! alert (thread (lambda ()
                              ; seconds-to-sleep might be negative, if clock time advanced beyond the target already
                              (cond [(> seconds-to-sleep 0.0) (sleep seconds-to-sleep)])
                              (send-syncd this advance-time-syncd target)
                              (send-thing this set-alert))))))
    (define (terminate-old-alert) ; disable any existing alerts
      (cond [(not (null? alert))
             (kill-thread alert)
             (set! alert null)]))))
