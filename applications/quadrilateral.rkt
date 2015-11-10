#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(wally-clear)
(define side1 (make-midpointline))
(define side2 (make-midpointline))
(define side3 (make-midpointline))
(define side4 (make-midpointline))
(define mid1 (make-line))
(define mid2 (make-line))
(define mid3 (make-line))
(define mid4 (make-line))
; connect everything up
(always (equal? (line-end2 (midpointline-line side1)) (line-end1 (midpointline-line side2))))
(always (equal? (line-end2 (midpointline-line side2)) (line-end1 (midpointline-line side3))))
(always (equal? (line-end2 (midpointline-line side3)) (line-end1 (midpointline-line side4))))
(always (equal? (line-end2 (midpointline-line side4)) (line-end1 (midpointline-line side1))))
(always (equal? (midpointline-midpoint side1) (line-end1 mid1)))
(always (equal? (midpointline-midpoint side1) (line-end2 mid4)))
(always (equal? (midpointline-midpoint side2) (line-end1 mid2)))
(always (equal? (midpointline-midpoint side2) (line-end2 mid1)))
(always (equal? (midpointline-midpoint side3) (line-end1 mid3)))
(always (equal? (midpointline-midpoint side3) (line-end2 mid2)))
(always (equal? (midpointline-midpoint side4) (line-end1 mid4)))
(always (equal? (midpointline-midpoint side4) (line-end2 mid3)))

; use explicit stays so that we can provide unambiguous behavior
(stay (line-end1 (midpointline-line side1)) #:priority 1)
(stay (line-end1 (midpointline-line side2)) #:priority 2)
(stay (line-end1 (midpointline-line side3)) #:priority 3)
(stay (line-end1 (midpointline-line side4)) #:priority 4)

; initialize the locations of the sides (the midpoints and parallelogram sides can take care of themselves)
(assert (equal? (line-end1 (midpointline-line side1)) (point 250  50)))
(assert (equal? (line-end1 (midpointline-line side2)) (point 550 250)))
(assert (equal? (line-end1 (midpointline-line side3)) (point 250 550)))
(assert (equal? (line-end1 (midpointline-line side4)) (point  50 250)))
(wally-solve)

; Hack for dragging - make a list of all the corners and midpoints
(define points (list (line-end1 (midpointline-line side1))
                     (line-end1 (midpointline-line side2))
                     (line-end1 (midpointline-line side3))
                     (line-end1 (midpointline-line side4))
                     (midpointline-midpoint side1)
                     (midpointline-midpoint side2)
                     (midpointline-midpoint side3)
                     (midpointline-midpoint side4)))
                     

(define frame (new frame%
                   [label "Interactive quadrilateral demo"]
                   [width 700]
                   [height 700]
                   [x 150]
                   [y 100]))
; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (when (send event button-down?) (select-point event))
      (when (send event button-up?) (unselect-point event))
      (when (send event dragging?) (drag-point event)))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
(define selected-point #f)

(define (select-point event)
  (define ev-points (map evaluate points))
  (define x (send event get-x))
  (define y (send event get-y))
  (set! selected-point (findf (lambda (p) (close x y p)) points)))
                     
(define (close x y p)
  (define gap 10)
  (define ev-p (evaluate p))
  (and (< (abs (- x (point-x ev-p))) gap) (< (abs (- y (point-y ev-p))) gap)))

(define (unselect-point event)
  (set! selected-point null))

(define (drag-point event)
  (when selected-point 
    (assert (equal? selected-point (point (send event get-x) (send event get-y))))
    (wally-solve)
    (showquad)))

(define canv (new my-canvas% [parent frame]
                  [paint-callback
                   (lambda (canvas dc)
                     (send dc set-pen "black" 1 'solid)
                     (send dc set-brush "black" 'solid)
                     (showquad))]))
(define dc (send canv get-dc))

(define (showquad)
  (define s1 (evaluate (midpointline-line side1)))
  (define s2 (evaluate (midpointline-line side2)))
  (define s3 (evaluate (midpointline-line side3)))
  (define s4 (evaluate (midpointline-line side4)))
  (define mp1 (evaluate mid1))
  (define mp2 (evaluate mid2))
  (define mp3 (evaluate mid3))
  (define mp4 (evaluate mid4))
  (send dc clear)
  (showthing s1 dc)
  (showthing s2 dc)
  (showthing s3 dc)
  (showthing s4 dc)
  (showthing mp1 dc)
  (showthing mp2 dc)
  (showthing mp3 dc)
  (showthing mp4 dc))
  
(send frame show #t)
