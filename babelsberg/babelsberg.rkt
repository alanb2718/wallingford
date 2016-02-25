#lang s-exp rosette
; version of Wallingford that attempts to closely follow Babelsberg

; set debug to #t to print debugging information
(define debug #f)

(provide always always* stay wally-clear wally-solve current-solution (rename-out [wally-evaluate evaluate])
         required high medium low lowest
         ; Babelsberg-ish functions and macros follow:
         symbolic-struct wally-define-symbolic wally-define-symbolic* wally-set!
         ; temporarily expose these lists for debugging - remove this later
 required-constraints required-constraint-procs soft-constraints soft-constraint-procs required-stays soft-stays 
 identity-constraints setters)

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

; Hash table for functions to make symbolic structs.  The keys are symbols (e.g., 'point)
; and the values are creators (e.g., make-point) that return a new symbolic struct.
; Using a symbol is a (temporary?) hack - it would be better to use the predicate point?
; but I wasn't sure how to get the predicate point? from the struct.
(define symbolic-struct-creators (make-hasheq))
; add the primitives
(hash-set! symbolic-struct-creators 'integer (lambda () (define-symbolic* x integer?) x))
(hash-set! symbolic-struct-creators 'boolean (lambda () (define-symbolic* x boolean?) x))
; supertype of symbolic structs
(struct symbolicstruct () #:transparent)

; Hash table of already-created symbolic structs, each indexed by a syntax element
; to identify it uniquely
(define symbolic-structs (make-hasheq))

(define-syntax wally-define-symbolic
  (syntax-rules (integer? boolean?)
    ((wally-define-symbolic vs ... integer?) (define-symbolic vs ... integer?))
    ((wally-define-symbolic vs ... boolean?) (define-symbolic vs ... boolean?))
    ((wally-define-symbolic vs ... pred) (define-symbolic-struct vs ... (unquestion 'pred)))))

(define-syntax define-symbolic-struct
  (syntax-rules ()
    ((define-symbolic-struct name) (void))
    ((define-symbolic-struct v1 name) 
     (define v1 (let ((dummy null)) (make-symbolic-struct (syntax dummy) name))))
    ((define-symbolic-struct v1 vs ... name)
     (begin (define-symbolic-struct v1 name) (define-symbolic-struct vs ... name)))))

(define (make-symbolic-struct stx name)
  (hash-ref! symbolic-structs stx (lambda () ((hash-ref symbolic-struct-creators name)))))

; trim the ? off the end of a symbol and return the new one (e.g., point? goes to point)
(define (unquestion symb)
  (string->symbol (string-trim (symbol->string symb) "?")))

(define-syntax wally-define-symbolic*
  (syntax-rules (integer? boolean?)
    ((wally-define-symbolic* vs ... integer?) (define-symbolic* vs ... integer?))
    ((wally-define-symbolic* vs ... boolean?) (define-symbolic* vs ... boolean?))
    ((wally-define-symbolic* vs ... pred) (define-symbolic-struct* vs ... (unquestion 'pred)))))

(define-syntax define-symbolic-struct*
  (syntax-rules ()
    ((define-symbolic-struct* name) (void))
    ((define-symbolic-struct* v1 name) (define v1 ((hash-ref symbolic-struct-creators name))))
    ((define-symbolic-struct* v1 vs ... name)
     (begin (define-symbolic-struct* v1 name) (define-symbolic-struct* vs ... name)))))

; Macro to define a new symbolic structure.  The format is the name of the structure,
; followed by a list of field names and type predicates, e.g.
;   (symbolic-struct point ([x number?] [y number?]))
; this is a subtype of symbolicstruct so that we can test for these
(define-syntax-rule (symbolic-struct name ((n pred) ...))
  (begin
    (struct name symbolicstruct (n ...) #:transparent)  ; define the struct
    ; make a creator function and put it in symbolic-struct-creators
    (let ([creator (lambda () (name ((hash-ref symbolic-struct-creators (unquestion 'pred))) ...))])
      (hash-set! symbolic-struct-creators 'name creator))))

; lists of always constraints and stays (separated into required and soft constraints/stays)
; Each list element for the soft things will be a soft-cn of some kind
(define required-constraints null)
(define required-constraint-procs null) ; procedures that create required constraints (used for always*)
(define soft-constraints null)
(define soft-constraint-procs null)
(define required-stays null)   ; not sure why you'd want a required stay, but for consistency here it is!
(define soft-stays null)
; identity-constraints is a set of lists of variables constrained to be identical, where the lists 
; contain syntax objects for those variables.  We use syntax objects rather than just symbols to
; account for scope.  We will use free-identifier=? to check for the same identifier (respecting scope).
(define identity-constraints (mutable-seteq))
; setters is an association list. The first element in each pair is a syntax object representing
; an identifier used in one or more identity constraints.  The corresponding value is a procedure
; with one parameter to set the identifier.
(define setters null)

; clear the lists of always constraints and stays, as well as the global assertion store
(define (wally-clear)
  (set! required-constraints null)
  (set! required-constraint-procs null)
  (set! soft-constraints null)
  (set! soft-constraint-procs null)
  (set! required-stays null)
  (set! soft-stays null)
  (set-clear! identity-constraints)
  (set! setters null)
  (clear-asserts!))

(define-syntax always
  (syntax-rules (eq?)
    ((always (eq? x y))
     (begin
       (unless (eq? x y) (error "identity constraints must be satisfied at the time they are declared"))
       (add-identity-constraint (syntax x) (syntax y) (lambda (n) (set! x n)) (lambda (n) (set! y n)))))
    ((always cn) (begin (set! required-constraints (cons cn required-constraints)) (wally-solve)))
    ((always (eq? x y) #:priority p)
     (if (= p required) (always (eq? x y)) (error "soft priorities on identity constraints not allowed")))
    ((always cn #:priority p)
     (cond [(= p required) (set! required-constraints (cons cn required-constraints)) (wally-solve)]
           [else (set! soft-constraints (cons (soft cn p) soft-constraints)) (wally-solve)]))))

; dynamic version of always (supports re-evaluating the expression each time by wrapping it in a lambda)
; doesn't support identity constraints (which don't make sense here)
(define-syntax always*
  (syntax-rules ()
    ((always* e1)
     (begin (set! required-constraint-procs (cons (lambda () e1) required-constraint-procs)) (wally-solve)))
    ((always* e1 #:priority p)
     (begin (set! soft-constraint-procs (cons (soft (lambda () e1) p) soft-constraint-procs)) (wally-solve)))))

(define (stay obj #:priority [p lowest])
  (if (= p required)
      (set! required-stays (cons obj required-stays))
      (set! soft-stays (cons (soft obj p) soft-stays))))

(define-syntax wally-set!
  (syntax-rules ()
    ((update var expr)
     (let ([value (wally-evaluate expr)])
       (cond [(or (number? var) (boolean? var) (symbolicstruct? var)) (assert (equal? var value))]
             [else (set! var expr) (solve-id-constraints (syntax var) expr)])
       (wally-solve)))))
  
(define (add-identity-constraint x y set-x! set-y!)
  ; find x-list and y-list (lists of vars constrained to be identical to x and y respectively)
  ; if present delete each from identity-constraints
  ; if x-list is null set it to (x) and similarly for y-list
  ; (define new x-list)
  ; for each var in y-list if it's not free-equal to some var in new, cons it on
  ; add new to identity-constraints
  (define xlist null)
  (define ylist null)
  ; not every efficient, since it keeps going even after you've found it ... but it should work
  (for ([vs identity-constraints])
    (for ([v vs])
      (when (free-identifier=? v x) (set! xlist vs))
      (when (free-identifier=? v y) (set! ylist vs))))
  (if (null? xlist) 
      (set! xlist (list x)) 
      (set-remove! identity-constraints xlist))
  (if (null? ylist) 
      (set! ylist (list y)) 
      (set-remove! identity-constraints ylist))
  (define newlist xlist)
  (for ([v ylist])
    (unless (findf (lambda (s) (free-identifier=? v s)) xlist) (set! newlist (cons v newlist))))
  (set-add! identity-constraints newlist)
  (add-setter x set-x!)
  (add-setter y set-y!))

(define (add-setter v set-v!)
  (unless (findf (lambda (s) (free-identifier=? v (car s))) setters)
    (set! setters (cons (list v set-v!) setters))))

(define (solve-id-constraints x expr)
  (for ([vs identity-constraints])
    ; if x is in xs then call the setter for each var in vars to set it to expr
    ; (later: and break)
    (when (findf (lambda (s) (free-identifier=? s x)) vs)
      (for ([v vs])
        (call-setter v expr)))))

; could augment this with a break but it should work without it
(define (call-setter x expr)
  (for ([s setters])
    (when (free-identifier=? x (car s)) ((cadr s) expr))))


(define current-solution (make-parameter (sat)))

(define (wally-evaluate expr [soln (current-solution)])
  (evaluate expr soln))

; Find a solution to the global assertion store plus all constraints declared 
; using 'always' and 'stay'.  Required always and stay constraints must be satisfied, while
; soft always and stay constraints should be satisfied if possible, respecting their
; relative priorities.  Stay constraints are considered relative to the (current-solution) 
; object at the start of solving.  After finding a solution, clear the global assertion store.  
; When we return from calling wally-solve, the global (current-solution) object holds a 
; solution.  (evaluate v) evaluates the given value wrt to the (current-solution).
(define (wally-solve [old-soln (current-solution)])
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
      (current-solution soln)
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
  (current-solution))
