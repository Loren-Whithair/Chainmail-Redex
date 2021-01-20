#lang racket
(require redex)
(require "../../Loo.rkt")

(display "--------------\n")
(display "Continuations:\n")


(define Machine_Continuation? (redex-match? Loo-Machine Continuation))

(define true_Conts (list
                    (term ())  
                    (term (x := * $ ()))
                    (term (x := * $ (() $ ())))
                    (term (x := * $ (x @ f := y)))
                    (term (x := * $ (z := y @ f)))
                    (term (x := * $ (method_result := x @ m())))
                    (term (x := * $ (method_result := x @ m(arg1))))
                    (term (x1 @ f1 := x2))
                    (term ((x1 @ f1 := x2) $ (x2 := x4 @ mtd())))
                    (term (x := * $ (x2 := x4 @ mtd())))
                    ))

(define false_Conts (list
                     (term (x := * $))
                       (term ((x1 @ f1 := x2) $ (x := * $ ())))
                       (term ((x1 := * $ (x2 := * $ ()))))
                       (term (* := * $ ()))
                         ))


; note: All valid Stmts are also valid Continuations, SO WE SHOULD APPEND THE LISTS AND TEST BOTH
; also note: it might not be best practise to do it like this in code so we might need to change it
(for ([conts true_Conts])
  (test-equal (Machine_Continuation? conts) #true))

; the same applies to false_Conts with false_Stmts
(for ([conts false_Conts])
  (test-equal (Machine_Continuation? conts) #false))


(test-results)