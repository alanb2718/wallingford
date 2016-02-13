#lang s-exp rosette
; hand compiled code selecting-live3.rkt
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "compiled-reactive-thing.rkt")

; put item as the first element in the list referred to by the box things
(define (put-first item things)
  (let* ([things-list (unbox things)]
         [others (filter (lambda (x) (not (eq? item x))) things-list)])
    (set-box! things (cons item others))))

(define compiled-selecting-live3%
  (class compiled-reactive-thing%
    (inherit button-pressed mouse-position image)
    (define c1 (circle (point 150 150) 50 (color "blue")))
    (define c2 (circle (point 200 150) 50 (color "red")))
    (define c3 (circle (point 250 150) 50 (color "green")))
    (define my-image (box (list c1 c2 c3)))
    (define actual-target null)
    (define actual-offset null)
    (super-new [init-image my-image])
    (define potential-targets (filter (lambda (c) (contains-point c (mouse-position))) (unbox my-image)))

    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      '(push))
    (define/override (update-mysolution)
      ; compiling for these constraints:
      ;   (always* (equal? mp (mouse-position)))
      ;   (define potential-targets (filter (lambda (c) (contains-point c mp)) (unbox my-image)))
      (set! potential-targets 
            (filter (lambda (c) (contains-point c (mouse-position))) (evaluate (unbox my-image))))
      (cond [(and (button-pressed) (pair? potential-targets))
             (set! actual-target (car potential-targets))
             (set! actual-offset (point-minus (mouse-position) (circle-center actual-target)))
             (put-first actual-target my-image)]))
    (define/override (find-time target)
      ; if there is a button press between the current time and target, advance to that, and otherwise to target
      ; get-button-down-event-times
      (let* ([now (send this milliseconds)]
             [potential-times (filter (lambda (t) (and (> t now) (< t target)))
                                      (send this get-button-down-event-times))])
        (if (null? potential-times) target (car potential-times))))))

(make-viewer (new compiled-selecting-live3%))
