#lang s-exp rosette

(require racket/gui/base)
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")

(wally-clear)

(define frame (new frame%
                   [label "Binary Tree Example"]
                   [width 800]
                   [height 600]))
(define canv (new canvas% [parent frame]
                  [paint-callback
                   (lambda (canvas dc)
                     (send dc set-pen "black" 1 'solid)
                     (send dc set-brush "black" 'solid)
                     (showtest))]))
(define dc (send canv get-dc))

(define branch-x-spacing 5) ; Space between adjacent points from separate subtrees
(define branch-x-distrib 20) ; Space between adjacent points in same subtree
(define branch-y-spacing 20) ; Vertical space of a single tree "row"

(struct branch (ll ; Left Line
                rl ; Right Line
                lt ; Left subtree
                rt) ; Right Subtree
  #:transparent)


(define tree-points '())

(define (make-tree-point)
  (let ([p (make-point)])
    (set! tree-points (cons p tree-points))
    ;; Dirty Hack to make sure values are nice
    (always (<= 0 (point-x p)))
    (always (<= 0 (point-y p)))
    ;; End Hack
    p))


(define (branch-tp b)
  (if b
      (line-end1 (branch-ll b))
      #f))

(define (branch-leftmost b)
  (if (branch-lt b)
      (branch-leftmost (branch-lt b))
      (line-end2 (branch-ll b))))

(define (branch-rightmost b)
  (if (branch-rt b)
      (branch-rightmost (branch-rt b))
      (line-end2 (branch-rl b))))

(define (make-branch [lt #f] [rt #f])
  (let* ([tp (make-tree-point)]
         ; use top point from left child, if it exists
         [lp (or (branch-tp lt) (make-tree-point))]
         ; use top point from right child, if it exists
         [rp (or (branch-tp rt) (make-tree-point))]
         [ll (line tp lp)]
         [rl (line tp rp)]
         [b (branch ll rl lt rt)])

    ; 1. separation of bottom points (by 2x x-spacing)
    (always (<= (+ branch-x-distrib (point-x lp))
                (point-x rp)))
    (always (equal? (+ branch-x-distrib (point-x lp))
                    (point-x rp))
            #:priority low)

    ; 2. bottom points are equally distributed around top point
    (always (equal? (- (point-x tp) (point-x lp))
                    (- (point-x rp) (point-x tp))))

    ; 3. Top point is always 30 above left and right point
    (always (equal? (+ branch-y-spacing (point-y tp))
                    (point-y lp)))
    (always (equal? (+ branch-y-spacing (point-y tp))
                    (point-y rp)))

    (when lt
      ; 7. Rightmost point in left subtree is left of top point
      (always (<= (+ branch-x-spacing (point-x (branch-rightmost lt)))
                  (point-x tp)))
      (always (equal? (+ branch-x-spacing (point-x (branch-rightmost lt)))
                      (point-x tp))
              #:priority low)
      )


    (when rt
      ; 9. Leftmost point in right subtree is right of top point
      (always (<= (+ branch-x-spacing (point-x tp))
                  (point-x (branch-leftmost rt))))
      (always (equal? (+ branch-x-spacing (point-x tp))
                      (point-x (branch-leftmost rt)))
              #:priority low)
      )

    b))

(define (binary-tree-recur n)
  (cond [(<= n 1) (make-branch)]
        [else (let ([lt (binary-tree-recur (- n 1))]
                    [rt (binary-tree-recur (- n 1))])
                (make-branch lt rt))]))

(define (binary-tree n x y)
  (let* ([bt (binary-tree-recur n)])

    (assert (equal? (point-x (branch-tp bt)) x))
    (assert (equal? (point-y (branch-tp bt)) y))

    bt))


(define (show-tree bt)
  (show-tree-recur (evaluate bt)))

(define (show-tree-recur bt)
  (let ([lt (branch-lt bt)]
        [rt (branch-rt bt)])

    (showthing (branch-ll bt) dc)
    (showthing (branch-rl bt) dc)
    (when lt
      (show-tree-recur lt))
    (when rt
      (show-tree-recur rt))
    ))

(define test (binary-tree 4 300 50))

(printf "solving\n")
(time (wally-solve))
(printf "solved\n")

(define (showtest)
  (show-tree test))

(send frame show #t)

;; This should be the result of (binary-tree 2 300 50)
;;
;; (branch
;;  (line (point 300 50) (point 285 70))
;;  (line (point 300 50) (point 315 70))
;;  (branch
;;   (line (point 285 70) (point 275 90))
;;   (line (point 285 70) (point 295 90))
;;   #f
;;   #f)
;;  (branch
;;   (line (point 315 70) (point 305 90))
;;   (line (point 315 70) (point 325 90))
;;   #f
;;   #f))
