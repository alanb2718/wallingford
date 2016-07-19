#lang s-exp rosette
; hand compiled code for quadrilateral.rkt
(require "../applications/geothings.rkt")
(require "reactive.rkt")
(require "abstract-reactive-thing.rkt")
(require "compiled-reactive-thing.rkt")
(require "../rhea/rhea.rkt")   ; this is a symlink in the reactive/ directory to the actual rhea directory

; functions to make symbolic objects using Cassowary variables
(define (make-cassowary-point x y)
  (point (new cassowary-variable% [initial-value x]) (new cassowary-variable% [initial-value y])))

(define (make-cassowary-line x1 y1 x2 y2)
  (line (make-cassowary-point x1 y1) (make-cassowary-point x2 y2)))

(define (make-cassowary-midpointline x1 y1 x2 y2 solver)
  (define myline (make-cassowary-line x1 y1 x2 y2))
  (define midpoint (make-cassowary-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)))
  (let ([vx1 (point-x (line-end1 myline))]
        [vy1 (point-y (line-end1 myline))]
        [vx2 (point-x (line-end2 myline))]
        [vy2 (point-y (line-end2 myline))]
        [vmx (point-x midpoint)]
        [vmy (point-y midpoint)])
    (send solver add-constraint (send vmx = (send (send vx1 + vx2) * 0.5)))
    (send solver add-constraint (send vmy = (send (send vy1 + vy2) * 0.5)))
    (midpointline myline midpoint)))

(define (add-point-stay pt strength solver)
  (send solver add-stay (point-x pt) strength)
  (send solver add-stay (point-y pt) strength))

; hack -- function to compute concrete versions of points, lines, and midpointlines
(define (compute-concrete g)
  (cond [(point? g) (point (send (point-x g) value) (send (point-y g) value))]
        [(line? g) (line (compute-concrete (line-end1 g)) (compute-concrete (line-end2 g)))]
        [(midpointline? g) (midpointline (compute-concrete (midpointline-line g)) (compute-concrete (midpointline-midpoint g)))]
        [else (error "unknown argument type")]))

(define (connect-points p1 p2 solver)
  (send solver add-constraint (send (point-x p1) = (point-x p2)))
  (send solver add-constraint (send (point-y p1) = (point-y p2))))

(define compiled-quadrilateral%
  (class compiled-reactive-thing%
    (inherit seconds image button-pressed? button-going-up? button-going-down? mouse-position)
    (super-new)
    (define solver (new cassowary%))
    
    (define side1 (make-cassowary-midpointline 250 50 550 250 solver))
    (define side2 (make-cassowary-midpointline 550 250 250 500 solver))
    (define side3 (make-cassowary-midpointline 250 500 50 250 solver))
    (define side4 (make-cassowary-midpointline 50 250 250 50 solver))
    ; join the corners of the quadrilateral
    (connect-points (line-end2 (midpointline-line side1)) (line-end1 (midpointline-line side2)) solver)
    (connect-points (line-end2 (midpointline-line side2)) (line-end1 (midpointline-line side3)) solver)
    (connect-points (line-end2 (midpointline-line side3)) (line-end1 (midpointline-line side4)) solver)
    (connect-points (line-end2 (midpointline-line side4)) (line-end1 (midpointline-line side1)) solver)
    ; add stays with explicit priorities to disambiguate behavior
    (add-point-stay (line-end2 (midpointline-line side1)) (send solver medium-strength) solver)
    (add-point-stay (line-end2 (midpointline-line side2)) (send solver weak-strength) solver)
    (add-point-stay (line-end2 (midpointline-line side3)) (send solver weaker-strength) solver)
    (add-point-stay (line-end2 (midpointline-line side4)) (send solver weakest-strength) solver)
    ; semi-hack: just share the endpoints of the midpoint lines (although that's what ThingLab did), rather than using equality constraints
    (define mid1 (line (midpointline-midpoint side1) (midpointline-midpoint side2)))
    (define mid2 (line (midpointline-midpoint side2) (midpointline-midpoint side3)))
    (define mid3 (line (midpointline-midpoint side3) (midpointline-midpoint side4)))
    (define mid4 (line (midpointline-midpoint side4) (midpointline-midpoint side1)))

    ; list of all the corners and midpoints for dragging
    (define points (list (line-end1 (midpointline-line side1))
                         (line-end1 (midpointline-line side2))
                         (line-end1 (midpointline-line side3))
                         (line-end1 (midpointline-line side4))
                         (midpointline-midpoint side1)
                         (midpointline-midpoint side2)
                         (midpointline-midpoint side3)
                         (midpointline-midpoint side4)))
    (define selected-point #f)

    ; the image for this is a list of the current values of the component parts
    ; just hacked together for now -- this makes a copy of the parts with the current values
    (define (compute-image)
      (let ([concrete-side1 (compute-concrete side1)]
            [concrete-side2 (compute-concrete side2)]
            [concrete-side3 (compute-concrete side3)]
            [concrete-side4 (compute-concrete side4)]
            [concrete-mid1 (compute-concrete mid1)]
            [concrete-mid2 (compute-concrete mid2)]
            [concrete-mid3 (compute-concrete mid3)]
            [concrete-mid4 (compute-concrete mid4)])
        (send this set-image! (list concrete-side1 concrete-side2 concrete-side3 concrete-side4
                                    concrete-mid1 concrete-mid2 concrete-mid3 concrete-mid4))))
    ; initialize image
    (compute-image)

    (define (close? p1 p2)
      (define gap 10)
      (and (< (abs (- (point-x p1) (point-x p2))) gap) (< (abs (- (point-y p1) (point-y p2))) gap)))

    ; hand written versions of methods intended to be compiled automatically
    (define/override (get-sampling)
      (let ([s (send this button-state)])
        (if (or (eq? s 'going-down) (eq? s 'down) (eq? s 'going-up)) '(push pull) '(push))))
    
    (define/override (update-mysolution)
      ; compiling for the quadrilateral constraints (see quadrilateral.rkt)
      ; if the button is going down see if a point is being selectd
      (cond [(button-going-down?)
             (let ([m (send this mouse-position)])
               (compute-image)
               (set! selected-point (findf (lambda (p) (close? m (compute-concrete p))) points)))])
      ; while the button is pressed update the selected point to follow the mouse, and also satisfy the constraints
      (cond [(and selected-point (button-pressed?))
             ; no edit constraints for now!
             (let ([cx (send (point-x selected-point) = (point-x (mouse-position)))]
                   [cy (send (point-y selected-point) = (point-y (mouse-position)))])
               (send solver add-constraint cx (send solver strong-strength))
               (send solver add-constraint cy (send solver strong-strength))
               (send solver solve)
               (send solver remove-constraint cx)
               (send solver remove-constraint cy)
               (compute-image))])
      (cond [(button-going-up?)  ; additional check -- button-pressed? will also include the button-going-up? case
             (send this notify-watchers-changed)]))
    
    (define/override (find-time mytime target)
      ; if there is a button press or release between the current time and target, advance to that, and otherwise to target
      (let ([potential-targets (filter (lambda (e) (and (> (mouse-event-time e) mytime)
                                                        (< (mouse-event-time e) target)
                                                        (or (eq? (mouse-event-button-state e) 'going-down)
                                                            (eq? (mouse-event-button-state e) 'going-up))))
                                       (send this get-mouse-events))])
        (if (null? potential-targets) target (apply min (map mouse-event-time potential-targets)))))))

(make-viewer (new compiled-quadrilateral%) #:title "Compiled version of quadrilateral" #:sleep-time 0.01)


