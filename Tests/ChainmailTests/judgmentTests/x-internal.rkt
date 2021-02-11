#lang racket
(require redex)
(require "../../../Loo.rkt")


(judgment-holds 
(? mt ((mt [C1 -> (clss C() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 1 internal >)))

(judgment-holds 
(? mt ((mt [C1 -> (clss C() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 2 internal >)))

(judgment-holds 
(? mt ((mt [C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C2 mt)]))) ⊨ (< 1 internal >)))




;; External tests

(judgment-holds
 (? (mt [C1 -> (clss C1() {})]) (mt ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 1 external >)))


(judgment-holds
 (? (mt [C1 -> (clss C1() {})]) (mt ((() mt) (mt [1 -> (C2 mt)]))) ⊨ (< 1 external >)))


((x_0 := x_1 @ m_0(x ...)) $ Stmts)
(x_0 := x_1 @ m_0(x ...))