#lang s-exp rosette

; FRAN-like constraint reactive programming examples in Wallingford
; These examples don't run yet - this is just to explore the language.
; They are based on the examples in the note Babelsberg/CRP.

; The language uses a hybrid automata approach, loosely modeled on that described in
; "Hybrid Automata: An Algorithmic Approach to the Specification and Verification of
; Hybrid Systems", http://pub.ist.ac.at/~tah/Publications/hybrid_automata.pdf
;
; The basic intuition is that we are modeling or simulating a continuous system (or better,
; a set of interacting continuous systems) on a discrete digital computer.  An application
; consists of one or more interacting processes.  Each process has a continuously varying clock,
; and also a program counter, which ranges over a set of control locations.  The state of each
; process can change continuously with time, as specified by the currently active set of constraints.
; The processes can also be reactive -- when some event occurs, or a condition changes its truth value,
; the system can transition to a new state.  This might involve adding or removing constraints, and
; also assigning to variables.

; There is a distinguished, predefined process named 'system', whose clock is the hardware clock, and
; that also can be queried for the state of input devices.  Another predefined process is 'display',
; for output.

; A simpler starting point may be to just have one process initially, and later figure out multiple
; interacting processes.  For now, though, this document uses multiple interacting processes.

; An important concept is that of *observing* or *sampling*.  Semantically, time is varying continuously,
; but we can only observe or sample the state of a process at discrete times.  By defining appropriate
; events (using 'when'), we can avoid missing important states -- things that might be true just for
; an instant.

; The type of a time variable is just Real, but it has the additional semantics that its state 
; increases continously and monotonically.  (At one point in the design there was a separate type
; Continuous, but this is now dropped.)

; An event is a boolean-valued variable that denotes that something happened at a particular instant.
; (Again, at one point in the design there was a separate type Event, but this is also dropped in favor
; of the type just being Boolean.)  An event models some discrete occurrence; there will be only a
; finite number of instants in which the value of the event variable is true in any given time interval.
; (If we define another variable V whose value is 0 or 1 depending on whether the event variable is
; false or true, the integral of V over any finite time interval will be 0.  Not sure about infinite
; intervals, but it probably doesn't matter.  Hopefully someone who is better at theory than me can
; clean this up.)

; New macros: when, while, derivative, integral, detect
; - when: takes an event-valued expression and evaluates some statements at the instant that event
;    occurs.  These can include assignments and assertions of new constraints.  (Note that this is
;    not the same as the built-in when control structure in Racket .... for a prototype implementation,
;    we could call it wallywhen.)
; - while: asserts a set of constraints that should hold as long as the condition has the value true.
; - derivative: evaluates to the derivative of one continuous variable with respect to another (usually time)
; - integral: inverse of derivative
; - detect: takes a boolean-valued expression and returns an event that is true at the instant the
;   value of the expression changes to true

; We'll use objects and messages for requesting the time of a process, and also the input device states
; from the system process, for example:
;   (send system time)
;   (send system left-button-down) 
;   (send system left-button-down-event)
; The left-button-down message returns true if the button is down then; the left-button-down-event
; returns true just at the instant it is pressed.

TODO: note about physical systems

; Now here are a series of examples, mostly adapted from the note "Babelsberg/CRP".

; Babelsberg version:
; c := Circle.new();                                                              
; c.center := point(10,20);                                                       
; /* this causes the circle to keep growing and shrinking */                      
; always c.radius = 100 + 100*sin(system.time) 
;
(define c (make-circle))
; initialize the circle's location
(assert (equal? (circle-center c) (point 10 20)))
(always (equal? (circle-radius c) (+ 100 (* 100 (sin (send system time))))))
(wally-solve)

; Babelsberg version:
; always at5 = (system.time^2 = 5)
;
(define-symbolic at5 event?)
(always (equal? at5 (equal? (expt (send system time) 2) 5)))

; Babelsberg version:
; e1 = first(system.leftButtonPressedEvent, system.time, t0)
; TODO

; Babelsberg version:
; def flip(c)
;   if c=red then black else red;
; end;
;
; c := red;  /* initial value for c */
; when system.leftButtonPressedEvent
;   c := flip(c);
;
(define c 'red)
(define (flip c) (if (eq? c 'red) 'black 'red))
(when leftButtonPressedEvent
  (assert (eq? (next c) (flip c))))

; Babelsberg version:
; always self.vel = deriv(self.pos, self.time);
(define pos vel continuous?)
(always (equal? vel (derivative pos (send system time))))

; Babelsberg version:
; class Wiggler
;  instvars value;
;  def init(lo, hi)
;    -- set up constraints
;    always wiggle = sin(pi*system.time)
;    always self.value = (hi-lo)? * (wiggle+1)/2
;    /* We left lo, hi, and wiggle as just local variables rather than
;       making them be instance variables, although if we wanted access
;       to them later we could make them be instance vars.  wiggle is
;       read-only since system.time is read-only; and the ? on the
;       expression (hi-lo) means that only self.value can change to
;       satisfy the constraint. */
;  end;
(struct wiggler (value))
(define (make-wiggler lo hi)
  (define-symbolic value wiggle continuous?)
  (always (equal? wiggle (sin (* pi (send system time)))))
  ; in this version, lo and hi are concrete values, so we don't need the read-only annotations
  (always (equal? value (* (- hi lo) (+ wiggle 1) 0.5)))
  (wiggler value))

; Babelsberg version:
; pBall := Circle.new;
; pBall.center := point(10,20);
; pBall.color := red;
; /* this causes the circle to keep growing and shrinking */
; wig := Wiggler.new(0.5,1);
; always pBall.radius = wig.value;
;
; /* rBall that tracks the changes in the size of  pBall */
; rBall := Circle.new;
; rBall.center := point(10,50);
; rBall.color := red;
; /* make rBall always be 1/10 the size of pBall
; always rBall.radius = pBall.radius * 0.1;
(struct circle (center radius color))
(define pBall (make-circle (point 10 20) 10 'red))
(define rBall (make-circle (point 10 50) 1 'red))
(define wig (make-wiggler 0.5 1.0))
(always (equal? (circle-radius pBall) (wiggler-value wig)))
(always (equal? (circle-radius rBall) (* 0.1 (circle-radius pBall))))

; Babelsberg version:
; always im.center = system.mousePosition;
(struct image (center bits))
(define im (image (point 100 100) (get-jpg "picture.bits")))
(always (equal? (image-center im) mouse-pos))
; while system.leftButtonPressed 
;  im.center = system.mousePosition;
; end;
(while (system-frame-of-reference-left-button-pressed system)
       (equal? (image-center im) mouse-pos))

; no collision ball example
; class NoCollisionBall
;   def init
;     initialPos := self.position;
;     initialVel := 0;
;     always self.pos = initialPos? + integral(self.vel,self.time);
;     always self.vel = initialVel? + integral(g,time);
;     /* g is the acceleration due to gravity */
;   end;
(define (make-no-collision-ball)
  (define b (make-circle (point 100 100) 10 'black))
  (define initialPos (circle-center b))
  (define initialVel 0.0)
  (define g 32.0)  ; or whatever it is in the appropriate units
  (define-symbolic pos vel continuous?)
  (always (equal? pos (circle-center b)))
  (always (equal? vel (+ initialVel (integral g (send system time)))))
  (always (equal? pos (+ initialPos (integral vel (send system time)))))
  b)

; Bouncing ball example (ball inside a box)
; class Ball
;   def init
;     self.box := .... [the bounding box that it bounces around in];
;     /* the coefficient of restitution */
;     self.cor = 0.8;
;     /* startPos is originally the initial position of the ball, and
;        following the first collision is its position at the last collision */
;     startPos := self.position;
;     /* similarly for startVel */
;     startVel := 0;
;     while not(collision)
;       self.pos = startPos? + integral(self.vel,self.time);
;       self.vel = startVel? + integral(g,time);
;       /* g is the acceleration due to gravity */    
;     end;
;     when collision
;       self.vel := self.compute_bounce_velocity();
;       startVel := self.vel;
;       /* note that we don't need to change self.pos */
;       startPos := self.pos  /* save the current position in startPos */
;     end;
;   end;
; 
;   def collision
;     return not(self.box.contains(self.pos));
;     /* we could instead check whether self.pos is exactly on a side of the box */
;   end;
; 
;   /* reflect the velocity depending on which wall we hit, reducing it
;      by the coefficient of restitution */
;   def compute_bounce_velocity
;     if (self.pos.y = self.box.top) | self.pos.y = self.box.bottom
;       return self.vel * vector(1,-1) * self.cor;
;     else
;       return self.vel * vector(-1,1) * self.cor;
;     end;
;   end;
; 
; end;

; TODO - write this


