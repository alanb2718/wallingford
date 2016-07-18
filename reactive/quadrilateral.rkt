#lang s-exp rosette
; quadrilateral demo, using 'when' and 'while' constraints for selecting a point to move and dragging

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "reactive-thing.rkt")

(define quadrilateral%
  (class reactive-thing%
    (inherit button-going-down? button-going-up? button-pressed? mouse-position image seconds)
    (super-new)
    (define side1 (make-midpointline this))
    (define side2 (make-midpointline this))
    (define side3 (make-midpointline this))
    (define side4 (make-midpointline this))
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
    (assert (equal? (line-end1 (midpointline-line side3)) (point 250 500)))
    (assert (equal? (line-end1 (midpointline-line side4)) (point  50 250)))
    
    ; the image for this is just a list of the component parts
    (send this set-image! (list side1 side2 side3 side4 mid1 mid2 mid3 mid4))
    
    ; make a list of all the corners and midpoints for dragging
    (define points (list (line-end1 (midpointline-line side1))
                         (line-end1 (midpointline-line side2))
                         (line-end1 (midpointline-line side3))
                         (line-end1 (midpointline-line side4))
                         (midpointline-midpoint side1)
                         (midpointline-midpoint side2)
                         (midpointline-midpoint side3)
                         (midpointline-midpoint side4)))

    (define selected-point #f)
    (when (button-going-down?)
      (let ([m (send this mouse-position)])
        (set! selected-point (findf (lambda (p) (close? m (send this wally-evaluate p))) points))))
    ; for some reason having this version doesn't work:
    ; (while (and (point? selected-point) (button-pressed?))
    ;       #:interesting-time (button-going-up?)
    ;       (assert (equal? selected-point (mouse-position))))
    (while (button-pressed?)
           (if selected-point (assert (equal? selected-point (mouse-position))) (void)))
    (define (close? p1 p2)
      (define gap 10)
      (and (< (abs (- (point-x p1) (point-x p2))) gap) (< (abs (- (point-y p1) (point-y p2))) gap)))
    
    
    (send this solve)))

(define q (new quadrilateral%))
(make-viewer q)
