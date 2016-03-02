#lang s-exp rosette
; Example of selecting objects.  There are three circles and we select one
(require "../applications/geothings.rkt")
(require "reactive.rkt")

; put item as the first element in the list referred to by the box things
(define (put-first item things)
  (let* ([things-list (unbox things)]
         [others (filter (lambda (x) (not (eq? item x))) things-list)])
    (set-box! things (cons item others))))

(define selection-example%
  (class reactive-thing%
    (inherit button-pressed mouse-position image previous)
    (super-new)
    (define c1 (make-circle this (circle (point 150 150) 50 (color "blue"))))
    (define c2 (make-circle this (circle (point 200 150) 50 (color "red"))))
    (define c3 (make-circle this (circle (point 250 150) 50 (color "green"))))
    (define my-image (box (list c1 c2 c3)))
    (define actual-target null)
    (define actual-offset null)
    (send this set-image! my-image)
    (define (potential-targets)
      (printf "computing potential targets\n")
      (filter (lambda (c) (contains-point c (mouse-position)))
              (send this evaluate-with-my-solution (unbox my-image))))
    (when (button-pressed)
      (let ([tgs (potential-targets)])
        (printf "tgs = ~a  mousepos ~a \n" tgs (mouse-position))
        (cond [(pair? tgs)
               (set! actual-target (car tgs))
               (set! actual-offset (point-minus (mouse-position) (circle-center actual-target)))
               (put-first actual-target my-image)]))
      (send this solve)
      )))


(define s (new selection-example%))
(make-viewer s #:title "Selection example")
