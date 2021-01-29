#lang racket
(require redex)
(require "Loo.rkt")

#|
 In this file, we give a "proof" of the correctness of the syntax our reduction rules.
 In particular, we show that:
   1. We can create a term that matches the general pattern given
   2. That this term is also a valid Loo-Machine state
|#



;------------------------------------------------
;------------------methCall_OS-------------------
;------------------------------------------------

; Reduction input: correct
(redex-match? Loo-Machine
              (M (((((x_0 := x_0 @ m(x ...)) $ Stmts) η) · ψ) χ))
        (term (mt (((((x_0 := x_0 @ m(x y)) $ ()) mt) · (() mt)) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((((x_0 := x_0 @ m(x y)) $ ()) mt) · (() mt)) mt))))
 
; Reduction output: correct
(redex-match? Loo-Machine
              (M ((Φ_1 · (((x := * $ Stmts) η_0) · ψ)) χ))
        (term (mt (((() mt) · (((x := * $ ()) mt) · (() mt))) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((() mt) · (((x := * $ ()) mt) · (() mt))) mt))))



;------------------------------------------------
;------------------varAssgn_OS-------------------
;------------------------------------------------

; Reduction input: correct

(redex-match? Loo-Machine
               (M  (((((x_0 := x_1 @ f) $ Stmts) η ) · ψ      ) χ ))
         (term (mt (((((x_0 := x_1 @ f) $ ()   ) mt) · (() mt)) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((((x_0 := x_1 @ f) $ ()   ) mt) · (() mt)) mt))))


; Reduction output: correct
(redex-match? Loo-Machine
          (M  (((Stmts η ) · ψ      ) χ ))
    (term (mt (((()    mt) · (() mt)) mt))))

(redex-match? Loo-Machine
             state
             (term (mt (((()    mt) · (() mt)) mt))))



;------------------------------------------------
;------------------fieldAssgn_OS-----------------
;------------------------------------------------

; Reduction input: correct
(redex-match? Loo-Machine
              (M (((((x_0 @ f := x_1) $ Stmts) η) · ψ) χ))
        (term (mt (((((x_0 @ f := x_1) $ ()) mt) · (() mt)) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((((x_0 @ f := x_1) $ ()) mt) · (() mt)) mt))))

; Reduction output: correct
(redex-match? Loo-Machine
              (M (((Stmts η) · ψ) χ_1)) ;; where χ_1 = insert-hextend(χ_0 [f -> y])
        (term (mt (((() mt) · (() mt)) mt))))

(redex-match?
 Loo-Machine
 state
 (term (mt (((() mt) · (() mt)) mt))))




;------------------------------------------------
;------------------objCreate_OS------------------
;------------------------------------------------

; Reduction input: correct
(redex-match? Loo-Machine
              (M (((((result := new C(x ...)) $ Stmts) η) · ψ) mt))
        (term (mt (((((result := new C(x y)) $ ()) mt) · (() mt)) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((((result := new C(x y)) $ ()) mt) · (() mt)) mt))))

; Reduction output: correct
(redex-match? Loo-Machine
              (M ((Φ_1 · (((x_0 := * $ Stmts) η_0) · ψ)) χ_1)) ;; where χ_1 = add-to-heap(χ_0 [addr_1 -> (C empty)])
        (term (mt (((() mt) · (((x_0 := * $ ()) mt) · (() mt))) mt))))

(redex-match? Loo-Machine
              state
              (term (mt (((() mt) · (((x_0 := * $ ()) mt) · (() mt))) mt))))


;------------------------------------------------
;-------------------return_OS--------------------
;------------------------------------------------

; Reduction input: correct
(redex-match? Loo-Machine
               (M  (((((return x_1) $ Stmts_0) η_0) · (((x_2 := * $ Stmts_1) η_1) · ψ      )) χ))
         (term (mt (((((return x_1) $ ()     ) mt ) · (((x_2 := * $ ()     ) mt ) · (() mt))) mt))))

(redex-match? Loo-Machine
               state
               (term (mt (((((return x_1) $ ()     ) mt ) · (((x_2 := * $ ()     ) mt ) · (() mt))) mt))))

; Reduction output: correct
(redex-match? Loo-Machine
             (M  (((Stmts_1 η_2 ) · ψ      ) χ ))
       (term (mt (((()    mt) · (() mt)) mt))))

(redex-match? Loo-Machine
             state
             (term (mt (((()    mt) · (() mt)) mt))))


;------------------------------------------------
;----------------return_OS-noArgs----------------
;------------------------------------------------

; Reduction input: correct
(redex-match? Loo-Machine
                (M  ((((return x ) η_0) ·  (((x_1 := * $ Stmts_1) η_1) · ψ      )) χ ))
          (term (mt ((((return x_2) mt ) · (((x_1 := * $ ()     ) mt ) · (() mt))) mt))))

(redex-match? Loo-Machine
                state
                (term (mt ((((return x_2) mt ) · (((x_1 := * $ ()     ) mt ) · (() mt))) mt))))

; Reduction output: correct
(redex-match? Loo-Machine
                (M  (((Stmts_1 η_2) · ψ      ) χ ))
                (term (mt (((()      mt ) · (() mt)) mt))))

(redex-match? Loo-Machine
                state
                (term (mt (((()      mt ) · (() mt)) mt))))