#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(define picture (new thing%))
(define side1 (make-midpointline-with-stays picture))
(define side2 (make-midpointline-with-stays picture))
(define side3 (make-midpointline-with-stays picture))
(define side4 (make-midpointline-with-stays picture))
(define mid1 (make-line))
(define mid2 (make-line))
(define mid3 (make-line))
(define mid4 (make-line))
; connect everything up
(always (equal? (line-end2 (midpointline-line side1)) (line-end1 (midpointline-line side2))) #:owner picture)
(always (equal? (line-end2 (midpointline-line side2)) (line-end1 (midpointline-line side3))) #:owner picture)
(always (equal? (line-end2 (midpointline-line side3)) (line-end1 (midpointline-line side4))) #:owner picture)
(always (equal? (line-end2 (midpointline-line side4)) (line-end1 (midpointline-line side1))) #:owner picture)
(always (equal? (midpointline-midpoint side1) (line-end1 mid1)) #:owner picture)
(always (equal? (midpointline-midpoint side1) (line-end2 mid4)) #:owner picture)
(always (equal? (midpointline-midpoint side2) (line-end1 mid2)) #:owner picture)
(always (equal? (midpointline-midpoint side2) (line-end2 mid1)) #:owner picture)
(always (equal? (midpointline-midpoint side3) (line-end1 mid3)) #:owner picture)
(always (equal? (midpointline-midpoint side3) (line-end2 mid2)) #:owner picture)
(always (equal? (midpointline-midpoint side4) (line-end1 mid4)) #:owner picture)
(always (equal? (midpointline-midpoint side4) (line-end2 mid3)) #:owner picture)

; initialize the locations of the sides (the midpoints and parallelogram sides can take care of themselves)
(assert (equal? (line-end1 (midpointline-line side1)) (point 250  50)))
(assert (equal? (line-end1 (midpointline-line side2)) (point 550 250)))
(assert (equal? (line-end1 (midpointline-line side3)) (point 250 550)))
(assert (equal? (line-end1 (midpointline-line side4)) (point  50 250)))
(send picture solve)

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
  (define s1 (send picture wally-evaluate (midpointline-line side1)))
  (define s2 (send picture wally-evaluate (midpointline-line side2)))
  (define s3 (send picture wally-evaluate (midpointline-line side3)))
  (define s4 (send picture wally-evaluate (midpointline-line side4)))
  (define m1 (send picture wally-evaluate mid1))
  (define m2 (send picture wally-evaluate mid2))
  (define m3 (send picture wally-evaluate mid3))
  (define m4 (send picture wally-evaluate mid4))
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
    (send picture solve)
    (showquad)))
