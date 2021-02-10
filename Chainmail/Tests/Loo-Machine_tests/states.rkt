#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-states)

(define (test-states)
  
  (display "-------------------------------------")
  (display "\nRunning States Tests:\n")

  (define Machine_state? (redex-match? Loo-Machine state))

  (define true_states (list

                       ;; simple state (every element empty)
                       (term (mt ((() mt) mt)))

                       ;; with non-empty elements
                       (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})]) ((() mt) mt)))  ;; Module
                       (term (mt ((((x1 @ f1 := x2) mt) · (() mt)) mt))) ;; Frame/Stack
                       (term (mt ((() mt) (mt [1 -> (C mt)])))) ;; Heap

                       ))

  (define false_states (list

                        (term ((mt) ((() mt) mt))) ;; brackets around mt
                        (term (mt ((() (mt)) mt))) ;; brackets around mt
                        
                        (term (([C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C1 [f1 -> 10])]))))  ;;invalid Module - missing mt at start
                        (term (mt ((((x1 @ f1 := x2) (mt)) · (() mt)) mt))) ;; brackets around mt in frame
                        ))

  (for ([states true_states])
    (test-equal (Machine_state? states) #true))

  (for ([states false_states])
    (test-equal (Machine_state? states) #false))

  (test-results)

  (display "-------------------------------------")
  )