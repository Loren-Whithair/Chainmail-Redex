#lang racket
(require redex)
(require "../../../Loo.rkt")
(require "../../../Chainmail.rkt")

(provide test-chainmail-access)

(define (test-chainmail-access)

  (display "-------------------------------------")
  (display "\nRunning Chainmail 'access' Tests:\n")

  ; true
  (test-equal #t (judgment-holds
                  (? mt (mt ((( () ((mt [this -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C (mt [f1 -> 2]))]) [2 -> (C mt)]))) ⊨ (< 1 access 2 >))))

  ; true (because addr_1 is FIELD of addr_0)
  (test-equal #t (judgment-holds
                  (? mt (mt ((( () (mt [x0 -> 1])) · (() mt)) ((mt [1 -> (C (mt [f1 -> 2]))]) [2 -> (C mt)]))) ⊨ (< 1 access 2 >))))

   ; true (1 is 'this' and 2 is in lcl)
  (test-equal #t (judgment-holds
                  (? mt (mt ((( () ((mt [this -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)]))) ⊨ (< 1 access 2 >))))

  ; false (addr_1 is NOT field of addr_0)
  (test-equal #f (judgment-holds
                  (? mt (mt ((( () (mt [x0 -> 1])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)]))) ⊨ (< 1 access 2 >))))

  ; false (1 is not 'this')
  (test-equal #f (judgment-holds
                  (? mt (mt ((( () ((mt [x0 -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)]))) ⊨ (< 1 access 2 >))))

  (test-results)
  
  (display "-------------------------------------")
  )






