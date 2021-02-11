#lang racket
(require redex)
(require "../../../Loo.rkt")


;;true
(judgment-holds
 (? (mt [C2 -> (clss C2() {})]) ((mt [C1 -> (clss C1() {})]) ((() mt) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]))) ⊨ ((< 1 internal >) ∧ (< 2 external >))))


;;false
(judgment-holds
 (? (mt [C2 -> (clss C2() {})]) ((mt [C1 -> (clss C1() {})]) ((() mt) ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]))) ⊨ ((< 1 internal >) ∧ (< 1 external >))))

