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

(define (make-lead)
  (define-symbolic* v i number?)
  (lead v i))

(define (make-battery [intv null])
  (define-symbolic* internal-voltage number?)
  (define plus (make-lead))
  (define minus (make-lead))
  (always (equal? internal-voltage (- (lead-voltage plus) (lead-voltage minus))))
  (always (equal? 0 (+ (lead-current plus) (lead-current minus))))
  ; if the intv argument is present, fix the internal voltage
  (unless (null? intv) (always (equal? internal-voltage intv)))
  (battery plus minus internal-voltage))

(define (make-resistor [r null])
  (define-symbolic* resistance number?)
  (define lead1 (make-lead))
  (define lead2 (make-lead))
  (always (equal? (- (lead-voltage lead2) (lead-voltage lead1)) (* resistance (lead-current lead1))))
  (always (equal? 0 (+ (lead-current lead1) (lead-current lead2))))
  ; if the resistance argument is present, fix the resistance of this resistor
  (unless (null? r) (always (equal? resistance r)))
  (resistor lead1 lead2 resistance)) 

(define (make-ground)
  (define ld (make-lead))
  (always (equal? 0 (lead-voltage ld)))
  (always (equal? 0 (lead-current ld)))
  ld)

(define (connect leads)
  (let ((lead1 (car leads))
        (others (cdr leads)))
    (for ([ld others])
      (always (equal? (lead-voltage lead1) (lead-voltage ld))))
    (always (equal? 0 (foldl + 0 (map lead-current leads))))))
