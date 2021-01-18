#lang racket
(require redex)
(require "Loo.rkt")

; -----------------------------------------------------
; ---------------------- Tests ------------------------
; -----------------------------------------------------

; Statement tests
(module+ test
  (test-equal (redex-match? Loo Stmts (term ())) #true) ;; testing the empty statement
  (test-equal (redex-match? Loo Stmts (term (() $ ()))) #true) ;; testing that we can have multiple statments chained together

  ; the next 3 tests show the importance of correctly bracketing our chained statements
  (test-equal (redex-match? Loo Stmts (term (() $ () $ ()))) #false)
  (test-equal (redex-match? Loo Stmts (term ((() $ ()) $ ()))) #false)
  (test-equal (redex-match? Loo Stmts (term (() $ (() $ ())))) #true)
  
  (test-equal (redex-match? Loo Stmts (term (x @ f := y))) #true)
  (test-equal (redex-match? Loo Stmts (term (z := y @ f))) #true)
  (test-equal (redex-match? Loo Stmts (term (method_result := x @ m()))) #true)
  (test-equal (redex-match? Loo Stmts (term (method_result := x @ m(arg1)))) #true)
  (test-equal (redex-match? Loo Stmts (term (method_result := x @ m(arg1 arg2)))) #true)
  (test-equal (redex-match? Loo Stmts (term (object_result := new C()))) #true)
  (test-equal (redex-match? Loo Stmts (term (object_result := new C(arg1)))) #true)
  (test-equal (redex-match? Loo Stmts (term (object_result := new C(arg1 arg2)))) #true)
  (test-equal (redex-match? Loo Stmts (term (return result))) #true)
)

; (GhostDecl ::= ghost f(x ...) { e })

; Ghost Declaration tests
(module+ test
  (test-equal (redex-match? Loo Stmts (term ())) #true) ;; testing the empty statement
  )

(module+ test
  (test-results))
