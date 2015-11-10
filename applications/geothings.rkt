#lang s-exp rosette
; geometric things in Wallingford/Rosette (also an example of handling state and change)
(require "../core/wallingford.rkt")

(provide point point? point-x point-y make-point point-plus point-scale
         line line? line-end1 line-end2 make-line 
         midpointline-line midpointline-midpoint make-midpointline make-midpointline-with-stays
         showthing)

;; structs for points and lines
(struct point (x y) #:transparent)
(struct line (end1 end2) #:transparent)
(struct midpointline (line midpoint) #:transparent)

; functions to make symbolic objects
(define (make-point)
  (define-symbolic* x y number?)
  (point x y))
(define (make-line)
  (line (make-point) (make-point)))

(define (make-midpointline)
  (define line1 (make-line))
  (define midpoint (make-point))
  (always (equal? midpoint (point-scale (point-plus (line-end1 line1) (line-end2 line1)) 0.5)))
  (midpointline line1 midpoint))

(define (make-midpointline-with-stays)
  (define line1 (make-line))
  (define midpoint (make-point))
  ; the midpoint constraint and stays on the endpoints of the line
  ; We want to put the stays on the two endpoints of the line rather than the line as a whole, so 
  ; that we prefer solutions that leave one endpoint where it was even if we need to move the other.
  ; And we don't put the stays all the way down on the x and y values, to avoid the split stay problem.
  (always (equal? midpoint (point-scale (point-plus (line-end1 line1) (line-end2 line1)) 0.5)))
  (stay (line-end1 line1) #:priority low)
  (stay (line-end2 line1) #:priority low)
  ; we could put a stay on the midpoint but it's not actually needed
  (midpointline line1 midpoint))

; show function for all geometric types (oh for objects!!)
(define (showthing g dc)
  (cond ([point? g] (send dc draw-ellipse (point-x g) (point-y g) 5 5))
        ([line? g] (send dc draw-line (point-x (line-end1 g)) (point-y (line-end1 g))
                         (point-x (line-end2 g)) (point-y (line-end2 g))))
        ([midpointline? g] (showthing (midpointline-line g) dc) (showthing (midpointline-midpoint g) dc))
        (else (error "unknown type of thing to show"))))

;; utility functions to operate on points
(define (point-plus p1 p2)
  (point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))
(define (point-scale p1 s)
  (point (* (point-x p1) s) (* (point-y p1) s)))
