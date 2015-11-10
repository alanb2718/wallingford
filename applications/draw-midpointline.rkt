#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(wally-clear)
(define mpl (make-midpointline-with-stays))
; initialize the line's location
(assert (equal? (midpointline-line mpl) (line (point 10 10) (point 200 250))))
(wally-solve)

(define frame (new frame%
                   [label "Moving one endpoint of a midpoint line"]
                   [width 600]
                   [height 600]
                   [x 150]
                   [y 100]))
(define canv (new canvas% [parent frame]
                  [paint-callback
                   (lambda (canvas dc)
                     (send dc set-pen "black" 1 'solid)
                     (send dc set-brush "black" 'solid)
                     (showthing (evaluate mpl) dc))]))
(define dc (send canv get-dc))

(send frame show #t)

(for ([i 100])
  (let ((x (+ i 10))
        (y (+ (* i 4) 20)))
    (assert (equal? (line-end1 (midpointline-line mpl)) (point x y)))
    (wally-solve)
    (send dc clear)
    (showthing (evaluate mpl) dc)))
