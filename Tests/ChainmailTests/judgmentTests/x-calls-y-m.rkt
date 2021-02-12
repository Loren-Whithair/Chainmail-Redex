#lang racket
(require redex)
(require "../../../Loo.rkt")
(require "../../../Chainmail.rkt")

(provide test-chainmail-x-calls-y)

(define (test-chainmail-x-calls-y)

  (display "-------------------------------------")
  (display "\nRunning Chainmail 'x-calls-y' Tests:\n")

  ; true
  (test-equal #t (judgment-holds
                  (? mt (mt (((((x_0 := x_1 @ m_0()) $ ()) ((mt [this -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >))))

; false (no method call)
  (test-equal #f (judgment-holds
                  (? mt (mt (((() ((mt [this -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >))))

; false (addr 1 is not 'this')
  (test-equal #f (judgment-holds
                  (? mt (mt (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_0 -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >))))

  (test-results)

  (display "-------------------------------------")
  )