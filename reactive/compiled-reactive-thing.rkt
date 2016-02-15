#lang s-exp rosette
; superclass for compiled reactive things
; need to override the following methods in subclasses:
;   (get-sampling)
;   (update-mysolution)
;   (find-time mytime target)
; These methods are all called by the thing's thread, so do not need to (and should not)
; use send-thing or send-syncd

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")

(provide compiled-reactive-thing%)

(define compiled-reactive-thing%
  (class abstract-reactive-thing%
    ; my-time is the compiled equivalent of symbolic-time
    (define my-time 0)
    (super-new)
    
    (define/override (milliseconds)
      my-time)
    ; for use by subclasses - not for external use really
    (define/public (set-my-time new-time)
      (set! my-time new-time))
    
    (define/public (update-mysolution)
      (error "should override in subclasses\n"))

    ; Find a time to advance to.  This will be the smaller of the target and the smallest value that makes a
    ; 'when' condition true.  If there aren't any values between the current time and the target that makes
    ; a 'when' condition true, then return the target.
    (define/override (find-time mytime target)
      (error "should override in subclasses\n"))
    
    ; Advance time to the smaller of the target and the smallest value that makes a 'when' condition true.
    ; Solve all constraints in active when constraints.
    ; If we advance time to something less than 'target', call advance-time-helper again.
    (define/override (advance-time-helper target)
      ; make sure we haven't gone by the target time already - if we have, don't do anything
      (cond [(< my-time target)
             (let ([next-time (send this find-time my-time target)])
               (send this set-my-time next-time)
               (send this update-mysolution)
               ; If any whens were activated tell the viewers that this thing changed.  (It might not actually
               ; have changed but that's ok -- we just don't want to miss telling them if it did.)
               (for/set ([w (send this get-watchers)]) (send-thing w thing-changed))
               (if (< next-time target) (advance-time-helper target) (void)))]))))
