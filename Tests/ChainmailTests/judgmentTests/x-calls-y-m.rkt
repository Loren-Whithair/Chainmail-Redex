#lang racket
(require redex)
(require "../../../Loo.rkt")

(judgment-holds
 (? mt (mt (((((x_0 := x_1 @ m_0()) $ ()) ((mt [this -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >)))

;; false: no method call
(judgment-holds
 (? mt (mt (((() ((mt [this -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >)))

;; false: addr 1 is not 'this'
(judgment-holds
 (? mt (mt (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_0 -> 1]) [x_1 -> 2])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)])))  ⊨ (< 1 calls 2 @ m_0() >)))


