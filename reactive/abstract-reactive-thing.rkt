#lang s-exp rosette
; superclass for both reactive-thing% (which is interpreted) and compiled-reactive-thing%

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(provide abstract-reactive-thing% send-thing send-syncd)
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

; all times are relative to the time the program is started, to keep the number of bits down
(define time-at-startup (current-milliseconds))

(define abstract-reactive-thing%
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
    ; *** TODO: clean these up to work with compiled version also ***
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
         (send this advance-time-helper target)]
        [(list 'advance-time)  ; version with default arg for target
         (send this advance-time-helper (current-clock-time))]
        [(list 'advance-time-syncd ch target)
         (advance-time-helper target)
         (channel-put ch null)]
        [(list 'advance-time-syncd ch)
         (advance-time-helper (current-clock-time))
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
         ; to avoid cycles, assume the button down event occurred at least 1 millisecond after
         ; the thing's current internal time
         (set! button-down-event-times
               (cons (max (+ 1 (milliseconds-evaluated)) (if (null? event-time) (current-clock-time) event-time))
                     button-down-event-times))
         (set! button-down-locations (cons (point mx my) button-down-locations))
         ; revise when to wake up next if need be (could be wake up right now)
         ; only pay attention to the button press (in terms of setting an alert) if this thing is being observed
         (cond [(not (set-empty? watchers)) (set-alert-helper)])]
        [(list 'milliseconds-syncd ch)
         (channel-put ch (milliseconds-evaluated))]
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
      (error "subclass responsibility\n"))
    
    ; button events
    (define/public (button-pressed)
      (member (send this milliseconds) button-down-event-times))
    ; If we don't have a record of mouse-position at the current time, return (point 0 0)
    ; Maybe fix this later?  Kind of funky ....
    (define/public (mouse-position)
      (or (for/first ([t button-down-event-times]
                      [p button-down-locations]
                      #:when (equal? (send this milliseconds-evaluated) t))
            p)
          (point 0 0)))
    
        
    ; Methods to access time
    (define/public-final (current-clock-time)
      (- (current-milliseconds) time-at-startup))
    ; Other time access methods.  These other methods don't directly use the thing's thread -- instead they
    ; are intended to always be called from code the thread is running
    ; The (seconds) method returns the current time in seconds as a float.  This is evaluated (mostly so
    ; that it can be used as an argument to sin).  Note that if used in a constraint, the constraint needs
    ; to use always* rather than always so that (seconds) is re-evaluated each time.
    ; (milliseconds should be overridden in subclasses to return the thing's current idea of its own time.
    ; It can be symbolic.
    (define/public (milliseconds)
      (error "subclass responsibility\n"))
    ; If milliseconds is symbolic, this should evaluate it.  Otherwise it just returns milliseconds.
    (define/public (milliseconds-evaluated)
      (send this milliseconds))
    (define/public (seconds)
      (exact->inexact (/ (milliseconds-evaluated) 1000.0)))
    
    ; Accessing the thing's image.  concrete-image should be overridden if the image needs to be evaluated before drawing it
    (define/public (image) myimage)
    (define/public (concrete-image) myimage)
    
    ; methods relevant for compiled things -- allow subclasses to set a new image, or access button-down-event-times
    (define/public (update-myimage newimage)
      (set! myimage newimage))
    (define/public (get-button-down-event-times)
      button-down-event-times)
    
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

    (define/public-final (notify-watchers)
      ; Notify any viewers that this thing has changed.  (It might not actually
      ; have changed but that's ok -- we just don't want to miss telling them if it did.)
      (for/set ([w watchers]) (send-thing w thing-changed)))
    
    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.  Note that the calls to solve in this function use a
    ; separate assertion stack, leaving alone the global stack and solution.
    (define/public (find-time mytime target)
      (error "subclass responsibility\n"))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define/public (advance-time-helper target)
      (error "subclass responsibility\n"))
    
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
      (let* ([mytime (milliseconds-evaluated)]
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
