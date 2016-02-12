#lang s-exp rosette
; DSL for handling state and updates in Rosette (also a stepping stone toward Constraint Reactive Programming)

; set debug to #t to print debugging information
(define debug #f)

(provide always always* stay wally-clear wally-solve wally-solve+ always*-code
         required high medium low lowest
         ;; temporarily expose these lists for debugging - remove this later
 required-constraints  required-constraint-procs  soft-constraints  soft-constraint-procs   required-stays   soft-stays)

(require rosette/solver/smt/z3)
(current-solver (new z3%))
(current-bitwidth 32) ; use 32-bit integers to prevent overflow

; symbolic names for priorities (represented as integers here)
; this is a bit hazardous since the update method sums the priority values in
; computing the penalty -- 11 low-priority constraints would win over a medium one
; (since these differ by a factor of 10)
(define required 10000)
(define high 1000)
(define medium 100)
(define low 10)
(define lowest 1)

; struct to represent soft constraints and stays.  For internal use only.
; target is one of the following:
;   a predicate (presumably containing one or more symbolic variables) - from always
;   a procedure that evaluates to a predicate - from always*
;   an expression - from stay
; Which one it is is determined by which list the struct is stored in.
(struct soft (target priority) #:transparent)

; lists of always constraints and stays (separated into required and soft constraints/stays)
; Each list element for the soft things will be a soft-cn of some kind
(define required-constraints '())
(define required-constraint-procs '()) ; procedures that create required constraints (used for always*)
(define soft-constraints '())
(define soft-constraint-procs '())
(define required-stays '())   ; not sure why you'd want a required stay, but for consistency here it is!
(define soft-stays '())
; always*-code is just a list consisting of all of the code for the always* constraints
; (used by reactive-thing% to decide whether there are temporal constraints)
(define always*-code '())

; clear the lists of always constraints and stays, as well as the global assertion store
(define (wally-clear)
  (set! required-constraints '())
  (set! required-constraint-procs '())
  (set! soft-constraints '())
  (set! soft-constraint-procs '())
  (set! required-stays '())
  (set! soft-stays '())
  (set! always*-code '())
  (clear-asserts))

; functions to add an always constraint or a stay
(define (always cn #:priority [p required])
  (if (= p required)
      (set! required-constraints (cons cn required-constraints))
      (set! soft-constraints (cons (soft cn p) soft-constraints))))

; dynamic version of always (supports re-evaluating the expression each time by wrapping it in a lambda)
(define-syntax always*
  (syntax-rules ()
    ((always* e1) (add-always*-helper 'e1 (lambda () e1) #t))
    ((always* e1 #:priority p) (add-always*-helper 'e1 (soft (lambda () e1) p) #f))))
(define (add-always*-helper expr fn required?)
  (if required? 
      (set! required-constraint-procs (cons fn required-constraint-procs))
      (set! soft-constraint-procs (cons fn soft-constraint-procs)))
  (set! always*-code (cons expr always*-code)))

(define (stay obj #:priority [p lowest])
  (if (= p required)
      (set! required-stays (cons obj required-stays))
      (set! soft-stays (cons (soft obj p) soft-stays))))

; Return a solution to the global assertion store plus all constraints declared
; using 'always' and 'stay'.  Required always and stay constraints must be satisfied, while
; soft always and stay constraints should be satisfied if possible, respecting their
; relative priorities.  Stay constraints are considered relative to the (current-solution)
; object at the start of solving.  After finding a solution, clear the global assertion store.
; When we return from calling wally-solve, the solution object that is returned holds a solution.
(define (wally-solve [old-soln (current-solution)])
  (define old-required-stay-vals (map (lambda (s) (evaluate s old-soln)) required-stays))
  (define old-soft-stay-vals (map (lambda (s) (evaluate s old-soln)) (map soft-target soft-stays)))
  ; assert the required always an always* constraints
  (for ([c required-constraints])
    (assert c))
  (for ([p required-constraint-procs])
    (assert (p)))
  ; add the required stays as assertions
  (for ([x required-stays] [old old-required-stay-vals])
    (assert (equal? x old)))
  ; raise an exception if the required constraints and stays aren't satisfiable
  (define soln (solve (assert #t)))
  ; cn-penalties, cn-proc-penalties, and stay-penalties are lists of penalties for the soft constraints,
  ; dynamic soft constraints, and soft stays respectively
  (define cn-penalties (map (lambda (s) (if (soft-target s) 0 (soft-priority s))) soft-constraints))
  (define cn-proc-penalties (map (lambda (s) (if ((soft-target s)) 0 (soft-priority s))) soft-constraint-procs))
  (define stay-penalties (map (lambda (s old)
                                (if (equal? (soft-target s) old) 0 (soft-priority s))) soft-stays old-soft-stay-vals))
  (define total-penalty (+ (foldl + 0 cn-penalties) (foldl + 0 cn-proc-penalties) (foldl + 0 stay-penalties)))
  (when debug
    (printf "cn-penalties: ~a\n" cn-penalties)
    (printf "cn-proc-penalties: ~a\n" cn-proc-penalties)
    (printf "stay-penalties: ~a\n" stay-penalties)
    (printf "total-penalty: ~a\n" total-penalty))
  ; Use iterative deepening to minimize the penalties for the unsatisfied soft constraints and stays.
  ; The parameter keep-going is initially (<= 0 (abs total-penalty)) -- just having it be [keep-going #t]
  ; ought to work, but without it Rosette sometimes doesn't try to come up with initial values for all
  ; relevant variables.  (There are some tests in wallingford-core-tests.rkt that illustrate this.)
  (let minimize ([keep-going (<= 0 (abs total-penalty))])
    (with-handlers ([exn:fail? void])    ; unsat! we are done: (current-solution) holds
      (when debug (printf "in minimize - keep-going: ~a\n" keep-going))
      (set! soln (solve (assert keep-going))) ; the best solution seen so far.
      (when debug (printf "passed the solve call in minimize\n")
        (printf "model: ~a\n" (model (current-solution)))
        (printf "about to call minimize with total-penalty=~a, (evaluate total-penalty)=~a \n" total-penalty (evaluate total-penalty)))
      (minimize (< total-penalty (evaluate total-penalty)))))
  ; Clear the global assertion store.
  (clear-asserts)
  soln)


; An incremental version of 'wally-solve', which uses 'solve+' underneath.
; This may be faster in some cases.
(define (wally-solve+ [old-soln (current-solution)])
  (define old-required-stay-vals (map (lambda (s) (evaluate s old-soln)) required-stays))
  (define old-soft-stay-vals (map (lambda (s) (evaluate s old-soln)) (map soft-target soft-stays)))
  ; clear out current solver's state
  (send (current-solver) clear)

  ; assert the required always an always* constraints
  (for ([c required-constraints])
    (assert c))
  (for ([p required-constraint-procs])
    (assert (p)))
  ; add the required stays as assertions
  (for ([x required-stays] [old old-required-stay-vals])
    (assert (equal? x old)))
  ; raise an exception if the required constraints and stays aren't satisfiable
  (define soln (solve+ (assert #t)))
  ; cn-penalties, cn-proc-penalties, and stay-penalties are lists of penalties for the soft constraints,
  ; dynamic soft constraints, and soft stays respectively
  (define cn-penalties (map (lambda (s) (if (soft-target s) 0 (soft-priority s))) soft-constraints))
  (define cn-proc-penalties (map (lambda (s) (if ((soft-target s)) 0 (soft-priority s))) soft-constraint-procs))
  (define stay-penalties (map (lambda (s old)
                                (if (equal? (soft-target s) old) 0 (soft-priority s))) soft-stays old-soft-stay-vals))
  (define total-penalty (+ (foldl + 0 cn-penalties) (foldl + 0 cn-proc-penalties) (foldl + 0 stay-penalties)))
  (when debug
    (printf "cn-penalties: ~a\n" cn-penalties)
    (printf "cn-proc-penalties: ~a\n" cn-proc-penalties)
    (printf "stay-penalties: ~a\n" stay-penalties)
    (printf "total-penalty: ~a\n" total-penalty))
  ; Use iterative deepening to minimize the penalties for the unsatisfied soft constraints and stays.
  ; The parameter keep-going is initially (<= 0 (abs total-penalty)) -- just having it be [keep-going #t]
  ; ought to work, but without it Rosette sometimes doesn't try to come up with initial values for all
  ; relevant variables.  (There are some tests in wallingford-core-tests.rkt that illustrate this.)
  (let minimize ([keep-going (<= 0 (abs total-penalty))])
    (with-handlers ([exn:fail? void])    ; unsat! we are done: (current-solution) holds
      (when debug (printf "in minimize - keep-going: ~a\n" keep-going))
      (set! soln (solve+ (assert keep-going))) ; the best solution seen so far.
      (when debug (printf "passed the solve call in minimize\n")
        (printf "model: ~a\n" (model (current-solution)))
        (printf "about to call minimize with total-penalty=~a, (evaluate total-penalty)=~a \n" total-penalty (evaluate total-penalty)))
      (minimize (< total-penalty (evaluate total-penalty)))))
  ; Clear the global assertion store.
  (clear-asserts)
  soln)
