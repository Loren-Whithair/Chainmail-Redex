#|
 In this file, we give a "proof" of the correctness of the syntax our reduction rules.
 In particular, we show that both parts of the reduction are states as required, and also show an example of a fully encoded state matching the given patterns.
|#

;;;;;;;;;;;;;;;;;; field assignment ;;;;;;;;;;;;;;;;;;
; 1st part of reduction- correct
(redex-match?
 Loo-Machine
 (M (((((x_0 @ f := x_1) $ Stmts) η) · ψ) χ))
 (term (mt (((((x_0 @ f := x_1) $ ()) mt) · (() mt)) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((((x_0 @ f := x_1) $ ()) mt) · (() mt)) mt))))

; 2nd part of reduction- correct
(redex-match?
 Loo-Machine
 (M (((Stmts η) · ψ) χ_1)) ;; where χ_1 = insert-hextend(χ_0 [f -> y])
 (term (mt (((() mt) · (() mt)) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((() mt) · (() mt)) mt))))

;;;;;;;;;;;;;;;;;; method call ;;;;;;;;;;;;;;;;;;
; 1st part of reduction- correct
(redex-match?
 Loo-Machine
 (M (((((x_0 := x_0 @ m(x ...)) $ Stmts) η) · ψ) χ))
 (term (mt (((((x_0 := x_0 @ m(x y)) $ ()) mt) · (() mt)) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((((x_0 := x_0 @ m(x y)) $ ()) mt) · (() mt)) mt))))
 
; 2nd part of reduction- correct
(redex-match?
 Loo-Machine
 (M ((Φ_1 · (((x := * $ Stmts) η_0) · ψ)) χ))
 (term (mt (((() mt) · (((x := * $ ()) mt) · (() mt))) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((() mt) · (((x := * $ ()) mt) · (() mt))) mt))))


;;;;;;;;;;;;;;;;;; object create ;;;;;;;;;;;;;;;;;;
; 1st part of reduction- correct
(redex-match?
 Loo-Machine
 (M (((((result := new C(x ...)) $ Stmts) η) · ψ) mt))
 (term (mt (((((result := new C(x y)) $ ()) mt) · (() mt)) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((((result := new C(x y)) $ ()) mt) · (() mt)) mt))))

; 2nd part of reduction- correct
(redex-match?
 Loo-Machine
 (M ((Φ_1 · (((x_0 := * $ Stmts) η_0) · ψ)) χ_1)) ;; where χ_1 = add-to-heap(χ_0 [addr_1 -> (C empty)])
 (term (mt (((() mt) · (((x_0 := * $ ()) mt) · (() mt))) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((() mt) · (((x_0 := * $ ()) mt) · (() mt))) mt))))