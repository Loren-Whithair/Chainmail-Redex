#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-continuation)

(define (test-continuation)

  (display "-------------------------------------")
  (display "\nRunning Continuation Tests:\n")

  (define Machine_Continuation? (redex-match? Loo-Machine Continuation))

  (define true_Conts (list

                      ;;simple Continuation (empty Stmt)
                      (term ())  

                      ;; with hole, followed by various Stmts
                      (term (x := * $ ()))
                      (term (x := * $ (() $ ())))
                      (term (x := * $ (x @ f := y)))
                      (term (x := * $ (z := y @ f)))
                      (term (x := * $ (method_result := x @ m())))
                      (term (x := * $ (method_result := x @ m(arg1))))

                      ;; without hole, just Stmts
                      (term (x1 @ f1 := x2))
                      (term ((x1 @ f1 := x2) $ (x2 := x4 @ mtd())))
                      (term (x := * $ (x2 := x4 @ mtd())))
                      ))

  (define false_Conts (list
                       (term (x := * $)) ; -------------------- ;; if given $, must be followed by another Stmt
                       (term ((x1 @ f1 := x2) $ (x := * $ ()))) ;; A continuation with a hole cannot come after a regular Stmt
                       (term ((x1 := * $ (x2 := * $ ()))))      ;; A continuation cannot have more than one hole
                       (term (* := * $ ()))  ; ---------------- ;; The hole must come after :=   - that is, the value being assigned to the variable is the hole 
                       ))


  (for ([conts true_Conts])
    (test-equal (Machine_Continuation? conts) #true))

  (for ([conts false_Conts])
    (test-equal (Machine_Continuation? conts) #false))

  (test-results)

  (display "-------------------------------------")
  )
  