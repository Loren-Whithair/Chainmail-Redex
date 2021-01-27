#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-states)

(define (test-states)
  
  (display "-------------------------------------")
  (display "\nRunning States Tests:\n")

  (define Machine_state? (redex-match? Loo-Machine state))

  (define true_states (list
                       (term (mt ((() mt) mt)))
                       (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})]) ((() mt) mt)))
                       (term (mt ((((x1 @ f1 := x2) mt) · (() mt)) mt)))
                       ))

  (define false_states (list
                        (term ((mt) ((() mt) mt)))
                        (term (mt ((() (mt)) mt)))
                        (term (([C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C1 [f1 -> 10])]))))
                        (term (mt ((((x1 @ f1 := x2) (mt)) · (() mt)) mt))) 
                        ))

  (for ([states true_states])
    (test-equal (Machine_state? states) #true))

  (for ([states false_states])
    (test-equal (Machine_state? states) #false))

  (test-results)

  (display "-------------------------------------")
  )