#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(wally-clear)
(define side1 (make-midpointline-with-stays))
(define side2 (make-midpointline-with-stays))
(define side3 (make-midpointline-with-stays))
(define side4 (make-midpointline-with-stays))
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

; initialize the locations of the sides (the midpoints and parallelogram sides can take care of themselves)
(assert (equal? (line-end1 (midpointline-line side1)) (point 250  50)))
(assert (equal? (line-end1 (midpointline-line side2)) (point 550 250)))
(assert (equal? (line-end1 (midpointline-line side3)) (point 250 550)))
(assert (equal? (line-end1 (midpointline-line side4)) (point  50 250)))
(wally-solve)

(define frame (new frame%
                   [label "Moving one endpoint of a midpoint line"]
                   [width 700]
                   [height 700]
                   [x 150]
                   [y 100]))
(define canv (new canvas% [parent frame]
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
  (define m1 (evaluate mid1))
  (define m2 (evaluate mid2))
  (define m3 (evaluate mid3))
  (define m4 (evaluate mid4))
  (send dc clear)
  (showthing s1 dc)
  (showthing s2 dc)
  (showthing s3 dc)
  (showthing s4 dc)
  (showthing m1 dc)
  (showthing m2 dc)
  (showthing m3 dc)
  (showthing m4 dc))
  
(send frame show #t)

(for ([i 50])
  (let ((x (+ (* i 4) 250))
        (y (+ (* i 12) 50)))
    (assert (equal? (line-end1 (midpointline-line side1)) (point x y)))
    (wally-solve)
    (showquad)))
