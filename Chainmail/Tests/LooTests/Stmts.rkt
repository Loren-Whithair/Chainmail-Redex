#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-Stmts)

(define (test-Stmts)
  (display "-------------------------------------")
  (display "\nRunning Stmts Tests:\n")

  (define Loo_Stmts? (redex-match? Loo Stmts)) ;

  ; true statements
  (define true_Stmts (list

                      ;; no Stmts
                      (term ()) ; ------- ;; testing the empty Stmt
                      (term (() $ ()))    ;; chaining Stmt

                      
                      ;; single Stmt: field Access
                      (term (x @ f := y))

                      ;; single Stmt: variable Assignment
                      (term (z := y @ f))

                      ;; single Stmt: method invocation
                      (term (method_result := x @ m()))
                      (term (method_result := x @ m(arg1)))
                      (term (method_res := x @ m(arg1 arg2)))

                      ;; single Stmt: object creation
                      (term (an_object := new C()))
                      (term (object_result := new C(arg1)))
                      (term ((object_result := new C1(arg1)) $ ())) ;
                      (term (object_result := new C(arg1 arg2)))

                      ;; single Stmt: return
                      (term (return result))
                      (term (return x1))

                      ;; multiple Stmts
                      (term ((x2 @ f1 := x1) $ ()))  ; ---------------------- ;; two stmts, second one empty
                      (term ((x2 @ f1 := x1) $ (x1 := x2 @ m1(arg2)))) ; ---- ;; two stmts, both with values 
                      (term ((x2 @ f1 := x1) $ ((x1 := x2 @ m1(arg2)) $ ()))) ;; three stmts, last one empty

                      ))

  ; false statements
  (define false_Stmts (list
                       (term (x @ 5 := y)) ; ----------------------------------- ;; 5 cannot be a fieldID
                       (term (x2 @ f1 := x1 $ method_result := x @ m()))  ; ---- ;; missing brackets around each Stm
                       (term ((x2 @ f1 := x1) $ (method_result := x @ m()) $ ())) ;; missing brackets around the second pair of Stmt, still valid


                       (term (() $ () $ ()))   ;; incorrect brackets
                       (term ((() $ ()) $ ()))
                       ))

  (for ([statements true_Stmts])
    (test-equal (Loo_Stmts? statements) #true))
  
  (for ([statements false_Stmts])
    (test-equal (Loo_Stmts? statements) #false))
  


  (test-results)
  (display "-------------------------------------")
  )