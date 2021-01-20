#lang racket
(require redex)
(require "../../Loo.rkt")

; Stmts Tests
(module+ test
  (define Loo_Stmts? (redex-match? Loo Stmts)) ;

   ; true statements
  (define true_Stmts (list
                      (term ()) ;; testing the empty statement
                      (term (() $ ())) ;; testing that we can have multiple statments chained together
                      (term (x @ f := y))
                      (term (z := y @ f))
                      (term (method_result := x @ m()))
                      (term (method_result := x @ m(arg1)))
                      (term (method_result := x @ m(arg1 arg2)))
                      (term (object_result := new C()))
                      (term (object_result := new C(arg1)))
                      (term (object_result := new C(arg1 arg2)))
                      (term (return result))
                      ))

  ; false statements
  (define false_Stmts (list
                     ))

  (for ([statements true_Stmts])
    (test-equal (Loo_Stmts? statements) #true))
  
  (for ([statements false_Stmts])
    (test-equal (Loo_Stmts? statements) #false))
  
  ; the next 3 tests show the importance of correctly bracketing our chained statements
  (test-equal (Loo_Stmts? (term (() $ () $ ()))) #false)
  (test-equal (Loo_Stmts? (term ((() $ ()) $ ()))) #false)
  (test-equal (Loo_Stmts? (term (() $ (() $ ())))) #true)
)
