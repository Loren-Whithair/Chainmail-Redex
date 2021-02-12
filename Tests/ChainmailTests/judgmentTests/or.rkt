#lang racket
(require redex)
(require "../../../Loo.rkt")
(require "../../../Chainmail.rkt")

(provide test-chainmail-or)

(define (test-chainmail-or)

  (display "-------------------------------------")
  (display "\nRunning Chainmail 'or' Tests:\n")

  ; true
  (test-equal #t (judgment-holds
                  (? (mt [C2 -> (clss C2() {})]) ((mt [C1 -> (clss C1() {})]) ((() mt) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]))) ⊨ ((< 1 internal >) ∨ (< 1 external >)))))

  ; false
  (test-equal #f (judgment-holds
                  (? (mt [C2 -> (clss C2() {})]) ((mt [C1 -> (clss C1() {})]) ((() mt) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]))) ⊨ ((< 2 internal >) ∨ (< 1 external >)))))

  (test-results)
  
  (display "-------------------------------------")
  )