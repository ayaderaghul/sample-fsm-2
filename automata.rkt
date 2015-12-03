#lang racket
(provide (all-defined-out))

(define COOPERATE 0)
(define DEFECT    1)
(define actions#  2)
(struct automaton (current initial payoff table) #:transparent)
(struct state (action dispatch) #:transparent)

(define (make-random-automaton states#)
  (define initial-current (random states#))
  (define (states*) (build-vector states# make-state))
  (define (make-state _) (state (random actions#) (transitions)))
  (define (transitions) (build-vector actions# make-transition))
  (define (make-transition _) (random states#))
  (automaton initial-current initial-current 0 (states*)))

(define (defects p0)
  (automaton 0 0 p0 (vector (state DEFECT (vector 0 0)))))

(define (cooperates p0)
  (define t (vector (state COOPERATE (vector COOPERATE COOPERATE))))
  (automaton COOPERATE COOPERATE p0 t))

(define (clone a)
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

(define (interact auto1 auto2 rounds-per-match)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 p1) n2 (+ payoff2 p2) (cons round-result round-results))))
  (values (reverse round-results) (automaton new1 c1 p1 table1) (automaton new2 c2 p2 table2)))

(define PAYOFF-TABLE
  (vector (vector (cons 3 3) (cons 0 4))
          (vector (cons 4 0) (cons 1 1))))

(define (payoff current1 current2)
  (vector-ref (vector-ref PAYOFF-TABLE current1) current2))

(define (flatten-automaton au)
  (match-define (automaton current initial payoff states) au)
  (define l (vector-length states))
  (define body
    (for/list ([i l])
      (match-define (state action dispatch) (vector-ref states i))
      (list action (vector->list dispatch))))
  (flatten (list initial body)))

(define (recover au)
  (define s (/ (- (length au) 1) 3))
  (define body (drop au 1))
  (define recovered-body
    (for/vector ([i s])
      (state (list-ref body (* 3 i)) (list->vector (take (drop body (+ 1 (* 3 i))) 2)))))
  (automaton (first au) (first au) 0 recovered-body))

(define (immutable-set a-list a-posn a-value)
  (define head (take a-list a-posn))
  (define tail (drop a-list (+ 1 a-posn)))
  (append head (list a-value) tail))

(define (mutate au)
  (define a (flatten-automaton au))
  (define r (random (length a)))
  (define s (/ (- (length a) 1) 3))
  (recover
   (cond
    [(zero? r) (immutable-set a 0 (random s))]
    [(= 1 (modulo r 3)) (immutable-set a r (random 2))]
    [else (immutable-set a r (random s))])))
