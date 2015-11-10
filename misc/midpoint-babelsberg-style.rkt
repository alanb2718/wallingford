#lang s-exp rosette

;; Version of midpoint.rkt expanded into Rosette code, using Babelsberg-style fresh objects
;; for each tick (or at least in the style of the formal semantics for Babelsberg).  This
;; was the original idea for how to do this, but is less in the Rosette style.  This version
;; hasn't been tested or debugged.

(struct point (x y) #:transparent)
(struct line (end1 end2) #:transparent #:mutable)

(define (point-plus p1 p2)
  (point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))
(define (point-scale p1 s)
  (point (* (point-x p1) s) (* (point-y p1) s)))

(define line1 (line (point 10 10) (point 20 50)))
(define midpoint (point 15 30))  

;; factor out advancing time and adding the constraints to the current version.  If we dynamically add variables or
;; constraints we would need new versions of tick.  (Not sure whether or not we want to factor out this tick function
;; in the automatically generated code.)
(define (tick)
  ;; need to define symbolic variables at the top level rather than in an expression
  (define-symbolic* x1 number?)
  (define-symbolic* y1 number?) 
  (define-symbolic* x2 number?)
  (define-symbolic* y2 number?)
  (define-symbolic* xm number?)
  (define-symbolic* ym number?)
  (define old-line1 line1)
  (define old-midpoint midpoint)
  (set! line1 (line (point x1 y1) (point x2 y2)))
  (set! midpoint (point xm ym))
  ;; add the always constraint on the new state of the line
  (assert (equal? midpoint (point-scale (point-plus (line-end1 line1) (line-end2 line1)) 0.5)))
  ;; Stay constraints.  The ? makes the old values read-only.
  ;; An alternative to using read-only annotations would be to have a way to solve for the values
  ;; of line1 and midpoint and then use those as constants when we define old-line1 and old-midpoint
  ;; (although I'm not sure how to do that).
  (assert (equal? line1 (? old-line1) #:priority weak))
  (assert (equal? midpoint (? old-midpoint) #:priority weak)))

;; move one endpoint of the line
; (set-line-end1! line1 (point 0 0))
(tick)
;; we turn the assignment into a constraint
(assert (equal? (line1-end1 line1) (point 0 0)))

; (set-line-end1! line1 (point 20 70))
(tick)
(assert (equal? (line1-end1 line1) (point 20 70)))

; (set! midpoint (point 100 100))
(tick)
(assert (equal? midpoint (point 100 100)))