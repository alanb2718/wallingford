#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "reactive-thing.rkt")

(provide viewer% make-viewer)

(define viewer%
  (class reactive-thing%
    (inherit get-thread)
    (init thing dc time-display)
    (define my-thing thing)
    (define my-dc dc)
    (define my-time-display time-display)
    (super-new)
    
    (define/override (match-thread-receive r)
      (match r
        [(list 'update-view-syncd ch)
         (send-syncd my-thing advance-time-syncd)
         (refresh-helper)
         (channel-put ch null)]
        [(list 'thing-changed)   ; not sure if this needs to be synchd ....
         ; This is for push notification.  The thing should already have advanced time to the right time.
         (refresh-helper)]
        [(list 'refresh-syncd ch)
         (refresh-helper)
         (channel-put ch null)]
        [(list 'viewer-button-down-event event-time x y)
         ; for now always send the event along - later only send it if the watched cares about button down events
         (send-thing my-thing button-down-event event-time x y)]
        [_
         (super match-thread-receive r)]))
    
    ; helper functions
    (define (refresh-helper)
      (cond [running
             (send-thing my-thing show my-dc)
             (send my-time-display set-label (seconds->string (send my-thing seconds)))]
            [else
             (send my-dc clear)
             (send my-time-display set-label " ")]))
    ; seconds->string is to get around Racket's seeming inability to print a float formatted to have
    ; exactly one decimal place (?!!)
    (define (seconds->string s)
      (let* ([i (inexact->exact (round (* 10 s)))]
             [whole-part (quotient i 10)]
             [decimal-part (remainder i 10)])
        (string-append "Time: " (number->string whole-part) "." (number->string decimal-part))))
    
    (define running #f)
    (define/public (unwatch)
      (set! running #f)
      (send my-thing unwatched-by this)
      (send-syncd this refresh-syncd))
    (define/public (watch)
      (set! running #t)
      (send my-thing watched-by this)
      ; if my-thing wants pull sampling, set up a thread to poll every 100 ms until the 'stop' button is pushed
      (cond [(member 'pull (send my-thing get-sampling))
             (thread (lambda ()
                       (let loop ()
                         (send-syncd this update-view-syncd)
                         ; later: take account of compute time
                         (sleep 0.1)
                         (if running (loop) (void)))))]))))

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas%
    (define myviewer null)  ; hack - initialize myviewer later to avoid initialization cycle
    (define/public (set-viewer v) 
      (set! myviewer v))
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      ; note we can't call this statement 'when'!
      (cond [(send event button-down?)
             (send-thing myviewer viewer-button-down-event null (send event get-x) (send event get-y))]))
    ; Call the superclass init, passing on all init args
    (super-new)))

; make a viewer on a reactive-thing r
(define (make-viewer r [title "A viewer"])
  (define frame (new frame%
                     [label title]
                     [width 600]
                     [height 600]
                     [x 150]
                     [y 100]))
  (define panel (new vertical-panel% [parent frame]))
  (define canv (new my-canvas% [parent panel]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-pen "black" 1 'solid)
                       (send dc set-brush "black" 'solid)
                       (send-syncd v refresh-syncd))]))
  (define dc (send canv get-dc))
  (define controls (new horizontal-panel% [parent panel] [alignment '(center center)]))
  (send controls stretchable-height #f)
  (new button% [parent controls]
       [label "Watch"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send v watch))])
  (new button% [parent controls]
       [label "Unwatch"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send v unwatch))])
  ; start the label off as blank, with enough blanks to accommodate any reasonable time
  (define td (new message% [parent controls] [label (make-string 30 #\space)]))
  ; make a viewer and start it up
  (define v (new viewer% [thing r] [dc dc] [time-display td]))
  (send canv set-viewer v)
  (send frame show #t)
  (send dc clear)
  (send v watch))
