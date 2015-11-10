#lang s-exp rosette

(require rosette/solver/smt/z3)
(current-solver (new z3%))
(current-bitwidth 32) ; use 32-bit integers to prevent overflow

(define (solve-opt hard objective)
  (current-solution (empty-solution))
  ; We'll use iterative deepening to optimize the objective.
  ; Note that every call to solve solves the union of *all* assertions 
  ; in the global assertion store, and *all* assertions emitted by the body of solve.
  ; As a result, the loop below will find the globally maximal solution.  
  (hard)
  ; Note that any assertions emitted by the call to (hard) 
  ; are dropped from the global assertion store after the call 
  ; to solve, as well as from the solver's state.  But the call to 
  ; solve solves all hard constraints and the added soft constraints, 
  ; and then clears the solver's state.
  (let optimize ([weight-constraint #t])
    (with-handlers ([exn:fail? void])      ; Unsat! we are done: (current-solution) holds  
      (solve (assert weight-constraint))  ; the best solution seen so far, which is the 
      (optimize (objective (current-solution))))) ; global maxoptimimumimum.
  
  (printf "assertion store: ~a\n" (asserts))
  (clear-asserts)  
  (current-solution))

(define (solve-opt+ hard objective)
  (current-solution (empty-solution))
  ; We'll use iterative deepening to optimize the objective.
  ; We'll also use incremental solving to make this faster.
  ; Note that every call to solve+ (the incremental version of solve)
  ; solves the union of *all* assertions added to it so far.  
  ; As a result, the loop below will find the lobally maximal solution.  
  (parameterize ([current-solver (new z3%)])  ; Let's get a fresh solver instance.
    ; Add all hard assertions to the solver.  
    ; Note that any assertions emitted by the call to (hard) 
    ; are dropped from the global assertion store after the call 
    ; to solve+.  However, these assertions remain in the solver's state when 
    ; solve+ is used instead of solve.  
    (solve+ (hard))
    (printf "assertion store: ~a\n" (asserts))
    (let optimize ([weight-constraint #t])
      (with-handlers ([exn:fail? void])      ; Unsat! we are done: (current-solution) holds  
        (solve+ (assert weight-constraint))  ; the best solution seen so far, which is the 
        (optimize (objective (current-solution)))))) ; global optimimum.
  
  (printf "assertion store: ~a\n" (asserts))
  (current-solution))

(define-symbolic x y z number?)

(define (test1 solve-fun)
  (solve-fun
   (lambda () ; Hard constraints
     (assert (<= x 10))
     (assert (> (abs (- x 20)) 0))) ; Prevent the trivial solution due to integer overflow.
   (lambda (sol)
     (let ([obj (abs (- x 20))])
       (< obj (evaluate obj sol)))))) ; decrease the objective value


(define (test2 solve-fun)
  (solve-fun
   (lambda () ; Hard constraints
     (assert (= x y))
     (assert (= y z)))
   (lambda (sol)
     (let ([obj (+ (if (= x 2) 1 0)
                   (if (= y 3) 1 0)
                   (if (= z 3) 1 0))])
       (> obj (evaluate obj sol)))))) ; increase the objective value

(define (test3 solve-fun)
  (solve-fun
   ; solve-fun takes two parameters (both other functions)
   ; the first parameter asserts the hard constraints
   (lambda () ; Hard constraints
     (assert (= x y)))
   (lambda (sol)
     (let ([obj (+ (if (= x 2) 1 0)
                   (if (= y 3) 2 0))])
       (> obj (evaluate obj sol)))))) ; increase the objective value

(test1 solve-opt)
(test1 solve-opt+)

(test2 solve-opt)
(test2 solve-opt+)

(test3 solve-opt)
(test3 solve-opt+)
