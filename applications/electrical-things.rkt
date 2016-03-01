#lang s-exp rosette
; electrical things in Wallingford/Rosette

(require "../core/wallingford.rkt")

(provide make-battery battery-plus battery-minus battery-internal-voltage
         make-resistor resistor-lead1 resistor-lead2 resistor-resistance
         lead-voltage lead-current
         make-ground connect)

(struct lead (voltage current) #:transparent)
(struct battery (plus minus internal-voltage) #:transparent)
(struct resistor (lead1 lead2 resistance) #:transparent)

(define (make-lead circuit)
  (define-symbolic* v i real?)
  (lead v i))

(define (make-battery circuit [intv null])
  (define-symbolic* internal-voltage real?)
  (define plus (make-lead circuit))
  (define minus (make-lead circuit))
  (always (equal? internal-voltage (- (lead-voltage plus) (lead-voltage minus))) #:owner circuit)
  (always (equal? 0 (+ (lead-current plus) (lead-current minus))) #:owner circuit)
  ; if the intv argument is present, fix the internal voltage
  (unless (null? intv) (always (equal? internal-voltage intv) #:owner circuit))
  (battery plus minus internal-voltage))

(define (make-resistor circuit [r null])
  (define-symbolic* resistance real?)
  (define lead1 (make-lead circuit))
  (define lead2 (make-lead circuit))
  (always (equal? (- (lead-voltage lead2) (lead-voltage lead1)) (* resistance (lead-current lead1))) #:owner circuit)
  (always (equal? 0 (+ (lead-current lead1) (lead-current lead2))) #:owner circuit)
  ; if the resistance argument is present, fix the resistance of this resistor
  (unless (null? r) (always (equal? resistance r) #:owner circuit))
  (resistor lead1 lead2 resistance))

(define (make-ground circuit)
  (define ld (make-lead circuit))
  (always (equal? 0 (lead-voltage ld)) #:owner circuit)
  (always (equal? 0 (lead-current ld)) #:owner circuit)
  ld)

(define (connect circuit leads)
  (let ((lead1 (car leads))
        (others (cdr leads)))
    (for ([ld others])
      (always (equal? (lead-voltage lead1) (lead-voltage ld)) #:owner circuit))
    (always (equal? 0 (foldl + 0 (map lead-current leads))) #:owner circuit)))
