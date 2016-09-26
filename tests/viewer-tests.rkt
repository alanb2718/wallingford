#lang s-exp rosette
;; unit tests for viewer%

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require racket/string)
(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "../reactive/reactive.rkt")
(require "../reactive/viewer.rkt")
(require "../reactive/pulser-class.rkt")


(provide viewer-tests)

; make a test stub for a drawing context
(define dc-stub%
  (class object%
    ; keep track of the actions performed in a list, most recent action first
    (define actions null)
    (define/public (clear)
      (set! actions (cons '(clear) actions)))
    (define/public (set-brush color style)
      (set! actions (cons (list 'set-brush color style) actions)))
    (define/public (set-pen color width style)
      (set! actions (cons (list 'set-pen color width style) actions)))
    (define/public (draw-line x1 y1 x2 y2)
      (set! actions (cons (list 'draw-line x1 y1 x2 y2) actions)))
    (define/public (draw-ellipse x y width height)
      (set! actions (cons (list 'draw-ellipse x y width height) actions)))
    (define/public (get-actions)
      actions)
    (super-new)))

(define display-stub%
  (class object%
    (define labels null)
    (define/public (set-label str)
      (set! labels (cons str labels)))
    (define/public (get-labels)
      labels)
    (super-new)))

(define (pulser-test)
  (test-case
   "test a pulser-like object"
   (define p (new pulser%))
   (define dc (new dc-stub%))
   (define time-display (new display-stub%))
   (define fps-display (new display-stub%))
   (define v (new viewer% [thing p] [dc dc] [time-display time-display] [fps-display fps-display] [sleep-time 0.1]))
   (send v watch)
   (sleep 1)
   (send v unwatch)
   ; (printf "action names ~a \n"  (map car (send dc get-actions)))
   ; (printf "count ~a \n" (count (lambda (a) (eq? (car a) 'draw-ellipse)) (send dc get-actions)))
   (check-true (<= 2 (count (lambda (a) (eq? (car a) 'draw-ellipse)) (send dc get-actions))))
   ; Check that at least two of the labels have the form "Time: 10.2"
   ; with at least one digit left of the decimal and exactly one right of the decimal.
   ; Since this should be using 'pull' we should have several different times displayed.
   ; (printf "displays ~a\n"  (send time-display get-labels))
   ; (printf "count ~a \n" (count (lambda (s) (regexp-match? #px"Time: \\d+\\.\\d" s))
   ;                              (send time-display get-labels)))
   (check-true (<= 2 (count (lambda (s) (regexp-match? #px"Time: \\d+\\.\\d" s))
                            (send time-display get-labels))))
   ))

(define (colored-flipper-test)
  (test-case
   "test a colored flipper object (pulses, changes color on button press)"
   (define (flip c)
     (if (equal? c (color "blue")) (color "red") (color "blue")))
   
   (define colored-flipper%
     (class pulser%
       (inherit button-going-down? image previous)
       (super-new)
       ; this constraint is inherited:
       ; (always (equal? (circle-radius (image)) (+ 60 (* 50 (sin (seconds))))))
       (when (button-going-down?)
         (assert (equal? (circle-color (image))
                         (flip (previous (circle-color (image)))))))))
   ; count how many set-brush actions with the given color are in the action list alist
   ; this is using just the name (as a string) so that we can look it up in the color database
   (define (count-set-brush-actions color-name action-list)
     (count (lambda (a) (and (eq? (car a) 'set-brush) (equal? (cadr a) (send the-color-database find-color color-name))))
            action-list))
   
   (define f (new colored-flipper%))
   (define dc (new dc-stub%))
   (define time-display (new display-stub%))
   (define fps-display (new display-stub%))
   (define v (new viewer% [thing f] [dc dc] [time-display time-display] [fps-display fps-display] [sleep-time 0.1]))
   (send v watch)
   (sleep 1)
   (check-true (<= 2 (count-set-brush-actions "blue" (send dc get-actions))))
   (check-true (zero? (count-set-brush-actions  "red" (send dc get-actions))))
   ; Check that at least two of the labels have the form "Time: 10.2"
   ; with at least one digit left of the decimal and exactly one right of the decimal.
   ; Since this should be using 'pull' we should have several different times displayed.
   ; (printf "displays ~a\n"  (send time-display get-labels))
   ; (printf "count ~a \n" (count (lambda (s) (regexp-match? #px"Time: \\d+\\.\\d" s))
   ;                              (send time-display get-labels)))
   (check-true (<= 2 (count (lambda (s) (regexp-match? #px"Time: \\d+\\.\\d" s))
                            (send time-display get-labels))))
   (send v unwatch)
   ))

(define viewer-tests 
  (test-suite+
   "unit tests for viewer"
   (pulser-test)
   (colored-flipper-test)
   ))

(time (run-tests viewer-tests))
