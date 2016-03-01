#lang s-exp rosette
; class thing% for handling state and updates in Rosette (also a superclass for reactive-thing%)

; set debug to #t to print debugging information
(define debug #f)

(provide thing% always always* stay required high medium low lowest)

; symbolic names for priorities (represented as integers here)
; this is a bit hazardous since the update method sums the priority values in
; computing the penalty -- 11 low-priority constraints would win over a medium one
; (since these differ by a factor of 10)
(define required 10000)
(define high 1000)
(define medium 100)
(define low 10)
(define lowest 1)

; macros to add an always or always* constraint or a stay
; all of these macros take optional #:priority and #:owner arguments
; (owner is the thing that owns the constraint or stay)
; These are done in a simplistic way rather than trying to figure out how to handle optional
; keyword macro arguments.
(define-syntax always
  (syntax-rules ()
    ((always expr) (always expr #:owner this #:priority required))
    ((always expr #:priority p) (always expr #:owner this #:priority p))
    ((always expr #:owner c) (always expr #:owner c #:priority required))
    ((always expr #:priority p #:owner c) (always expr #:owner c #:priority p))
    ; finally the actual definition ...
    ((always expr #:owner c #:priority p) (send c add-always-helper expr p))))
; dynamic version of always (supports re-evaluating the expression each time by wrapping it in a lambda)
(define-syntax always*
  (syntax-rules ()
    ((always* expr) (always* expr #:owner this #:priority required))
    ((always* expr #:priority p) (always* expr #:owner this #:priority p))
    ((always* expr #:owner c) (always* expr #:owner c #:priority required))
    ((always* expr #:priority p #:owner c) (always* expr #:owner c #:priority p))
    ((always* expr #:owner c #:priority p) (send c add-always*-helper 'expr (lambda () expr) p))))
(define-syntax stay
  (syntax-rules ()
    ((stay expr) (stay expr #:owner this #:priority lowest))
    ((stay expr #:priority p) (stay expr #:owner this #:priority p))
    ((stay expr #:owner c) (stay expr #:owner c #:priority lowest))
    ((stay expr #:priority p #:owner c) (stay expr #:owner c #:priority p))
    ((stay expr #:owner c #:priority p) (send c add-stay-helper expr p))))

(define thing%
  (class object%
    (super-new)
    
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

    ; start current-solution as an empty solution
    (define current-solution (sat))

    (define/public (get-current-solution)
      current-solution)
    (define/public (update-current-solution sol)
      (set! current-solution sol))
    (define/public (wally-evaluate expr [soln current-solution])
      (evaluate expr soln))
    
    ; clear the lists of always constraints and stays, as well as the global assertion store
    (define/public (clear)
      (set! required-constraints '())
      (set! required-constraint-procs '())
      (set! soft-constraints '())
      (set! soft-constraint-procs '())
      (set! required-stays '())
      (set! soft-stays '())
      (set! always*-code '())
      (clear-asserts!))
    
    ; Return a solution to the global assertion store plus all constraints declared
    ; using 'always' and 'stay'.  Required always and stay constraints must be satisfied, while
    ; soft always and stay constraints should be satisfied if possible, respecting their
    ; relative priorities.  Stay constraints are considered relative to the old-soln
    ; object at the start of solving.  After finding a solution, clear the global assertion store.
    ; When we return from calling wally-solve, the solution object that is returned holds a solution.
    ; Also update (current-solution) using the solution that is found.
    ; Optional argument: old-soln is the starting solution.  Defaults to (current-solution).
    (define/public (solve [old-soln current-solution])
      (define old-required-stay-vals (map (lambda (s) (evaluate s old-soln)) required-stays))
      (define old-soft-stay-vals (map (lambda (s) (evaluate s old-soln)) (map soft-target soft-stays)))
      ; get a handle to the current solver: ok to use the solver directly because we aren't doing finitization!
      (define solver (current-solver))
      (solver-clear solver)
      ; obtain the solution to the 
      ; * required always 
      ; * always* constraints
      ; * required stays
      ; * any assertions from Rosette's global store generated by the execution of (p)s
      (solver-add solver (for/list ([c required-constraints]) c))
      (solver-add solver (for/list ([p required-constraint-procs]) (p)))
      (solver-add solver (for/list ([x required-stays] [old old-required-stay-vals])  
                           (equal? x old)))
      (solver-add solver (asserts))
      (define soln (solver-check solver))
      ;  (printf "REQUIRED:\n")
      ;  (for ([c required-constraints])
      ;    (printf " ~a\n" (term->datum c)))
      ;  (printf "ASSERTS ~a\n" (map term->datum (asserts)))
      ; raise an exception if the required constraints and stays aren't satisfiable
      (unless (sat? soln)
        (error 'wally-solve "Required constrants and stays aren't satisfiable."))
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
      ; can't work if no other constraints are added to the solver. If the only added constraint is #t, 
      ; Rosette has no basis for assigning values to any variables. There are some tests in 
      ; wallingford-core-tests.rkt that illustrate this.
      (let minimize ([keep-going (<= 0 (abs total-penalty))])
        (when debug (printf "in minimize - keep-going: ~a\n" keep-going))
        (solver-add solver (list keep-going))
        (set! soln (solver-check solver)) ; the best solution seen so far.
        (when (sat? soln)
          (set! current-solution soln)
          (when debug 
            (printf "passed the solve call in minimize\n")
            (printf "model: ~a\n" (model soln))
            (printf "about to call minimize with total-penalty=~a, (evaluate total-penalty)=~a \n" 
                    total-penalty (evaluate total-penalty soln)))
          (minimize (< total-penalty (evaluate total-penalty soln)))))
      ; Clear the global assertion store.
      (clear-asserts!)
      ; Clear the solver state.
      (solver-clear solver)
      ; Return current-solution
      current-solution)
    
    
    ; get the source code for always* constraints (to use in deciding whether they are temporally dependent)
    (define/public (get-always*-code)
      always*-code)
    
    ; helper methods for adding always and always* constraints and stays
    ; these are declared as public, but should only be used by the macros for always, always*, and stay
    (define/public (add-always-helper cn p)
      (if (= p required) 
          (set! required-constraints (cons cn required-constraints))
          (set! soft-constraints (cons (soft cn p) soft-constraints))))
    (define/public (add-always*-helper expr fn p)
      (if (= p required) 
          (set! required-constraint-procs (cons fn required-constraint-procs))
          (set! soft-constraint-procs (cons (soft fn p) soft-constraint-procs)))
      (set! always*-code (cons expr always*-code)))
    (define/public (add-stay-helper obj p)
      (if (= p required)
          (set! required-stays (cons obj required-stays))
          (set! soft-stays (cons (soft obj p) soft-stays))))
    ; struct to represent soft constraints and stays.  For internal use only.
    ; target is one of the following:
    ;   a predicate (presumably containing one or more symbolic variables) - from always
    ;   a procedure that evaluates to a predicate - from always*
    ;   an expression - from stay
    ; Which one it is is determined by which list the struct is stored in.
    (struct soft (target priority) #:transparent)
    
    ))
