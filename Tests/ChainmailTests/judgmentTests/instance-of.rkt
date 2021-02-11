#lang racket
(require redex)
(require "../../../Loo.rkt")

;;true
(judgment-holds 
(? mt ((mt [C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 1 : C1 >)))

;;false
(judgment-holds 
(? mt ((mt [C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 1 : C2 >)))