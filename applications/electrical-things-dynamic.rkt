#lang s-exp rosette
; Version of electrical things that allows for dynamically adding and removing components by
; changing the set of leads connected to a node.  Uses always* to enable this.

(require "../core/wallingford.rkt")

(provide make-battery battery-plus battery-minus battery-internal-voltage
         make-resistor resistor-lead1 resistor-lead2 resistor-resistance
         lead-node lead-current node-voltage node-leads
         make-ground connect)

(struct node (voltage leads) #:transparent #:mutable) ; mutable so that we can add and remove leads
(struct lead (node current) #:transparent #:mutable)
(struct battery (plus minus internal-voltage) #:transparent)
(struct resistor (lead1 lead2 resistance) #:transparent)

; There is an invariant: for every lead ld, ld is a member of ld.node.leads
; This is currently enforced by the programmer - it would be cool for this to 
; simply be a constraint that gets maintained automatically.

(define (make-node)
  (define-symbolic* v number?)
  (define nd (node v null))
  ; the sum of the currents flowing into this node is 0 (Kirchhoff's first law)
  ; this is a dynamic constraint, so that it will work if leads are connected or disconnected
  ; this constraint will stick around for nodes that are no longer used - this may lead to
  ; efficiency problems after a while.  (Solutions: explicitly remove old constraints, or 
  ; garbage collect ones that aren't applicable to any visible objects.)
  (always* (equal? 0 (foldl + 0 (map lead-current (node-leads nd)))))
  nd)

; make a new lead that connects to the given node, or create a new node if needed
(define (make-lead [nd null])
  (define-symbolic* i number?)
  (define mynode (if (null? nd) (make-node) nd))
  (define ld (lead mynode i))
  (set-node-leads! mynode (cons ld (node-leads mynode)))
  ld)

(define (make-battery [intv null])
  (define-symbolic* internal-voltage number?)
  (define plus (make-lead))
  (define minus (make-lead))
  ; since we are allowing nodes to be changed we need to make the first of these constraints dynamic as well
  ; (the second one doesn't actually need to be dynamic but I just left it that way for consistency)
  (always* (equal? internal-voltage (- (node-voltage (lead-node plus)) (node-voltage (lead-node minus)))))
  (always* (equal? 0 (+ (lead-current plus) (lead-current minus))))
  (unless (null? intv) (always (equal? internal-voltage intv)))
  (battery plus minus internal-voltage))

(define (make-resistor [r null])
  (define-symbolic* resistance number?)
  (define lead1 (make-lead))
  (define lead2 (make-lead))
  ; similarly the first of these constraints needs to be dynamic
  (always* (equal? (- (node-voltage (lead-node lead2)) (node-voltage (lead-node lead1))) (* resistance (lead-current lead1))))
  (always* (equal? 0 (+ (lead-current lead1) (lead-current lead2))))
  (unless (null? r) (always (equal? resistance r)))
  (resistor lead1 lead2 r))

(define (make-ground)
  (define ld (make-lead))
  (always* (equal? 0 (node-voltage (lead-node ld))))
  (always* (equal? 0 (lead-current ld)))
  ld)

; Connect a list of leads together by making a new node and plugging that node into each lead.
; It shouldn't matter if some of the leads are already connected - we make a fresh node
; and use that.  (The old nodes will have always* Kirchhoff's law constraints, which will
; persist.  This should be harmless although could be a cause of inefficiencies.)
(define (connect leads)
  (let ((all-leads null)
        (new-node (make-node)))
    ; find all the existing leads that are connected to leads in 'leads' and put them in all-leads
    (for ([ld1 leads])
      (for ([ld2 (node-leads (lead-node ld1))])
      (unless (memq ld2 all-leads) (set! all-leads (cons ld2 all-leads)))))
    (set-node-leads! new-node all-leads)
    (for ([ld all-leads])
      ; nuke the old set of leads for the node at the end of this lead, so that its always*
      ; constraint on the currents doesn't mess things up
      (set-node-leads! (lead-node ld) null)
      (set-lead-node! ld new-node))))

; disconnect the one lead, leaving othe connections as is
(define (disconnect ld)
  (error "not yet written"))

; disconnect all the leads from the given node (so that we get new nodes for all the leads)
(define (disconnect-all nd)
   (error "not yet written"))
 
