#lang s-exp rosette
; Example of selecting objects.  There are three circles and we select one.
; The list of potential targets is stored in a variable, rather than being a procedure call.  
; This results in *non-linear constraints* which causes z3 to return 'unknown' and crash.
(require "reactive.rkt")

; put item as the first element in the list referred to by the box things
(define (put-first item things)
  (let* ([things-list (unbox things)]
         [others (filter (lambda (x) (not (eq? item x))) things-list)])
    (set-box! things (cons item others))))


(define selection-example%
  (class reactive-thing%
    (inherit button-pressed mouse-position image previous)
    (define c1 (make-circle (circle (point 150 150) 50 (color "blue"))))
    (define c2 (make-circle (circle (point 200 150) 50 (color "red"))))
    (define c3 (make-circle (circle (point 250 150) 50 (color "green"))))
    (define my-image (box (list c1 c2 c3)))
    (define actual-target null)
    (define actual-offset null)
    (super-new [init-image my-image])
    (define mp (make-point))
    (always* (equal? mp (mouse-position)))
    (define potential-targets (filter (lambda (c) (contains-point c mp)) (unbox my-image)))
    (when (button-pressed)
      (cond [(pair? potential-targets)
             (set! actual-target (car potential-targets))
             (set! actual-offset (point-minus (mouse-position) (circle-center actual-target)))
             (put-first actual-target my-image)]))
    ))


(define s (new selection-example%))
(make-viewer s #:title "Selection example")
