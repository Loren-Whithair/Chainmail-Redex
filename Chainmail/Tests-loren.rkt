#lang racket
(require redex)
(require "Loo.rkt")

(module+ test

  (define e1 (term ((x_1 = x_4)))
  
  (test-equal (redex-match? Loo e (term (x_1 = x_4))) #true)
  (test-equal (redex-match? Loo e (term (true = false))) #true)
  (test-equal (redex-match? Loo e (term ((true = false) = true))) #true)
  (test-equal (redex-match? Loo e (term (true = false = true))) #false)
  (test-equal (redex-match? Loo e (term (5 = 5))) #false)

  (test-equal (redex-match? Loo e (term (if true then true else true))) #true)
  (test-equal (redex-match? Loo e (term  (if x_5 then null else x_3))) #true)
  (test-equal (redex-match? Loo e (term  (if (x_1 = x_2) then (x_3 = (true = null)) else false))) #true)
  (test-equal (redex-match? Loo e (term (if null then (true) else null))) #false)
  (test-equal (redex-match? Loo e (term (if (true) then x_1 else null))) #false)
  
  (test-equal (redex-match? Loo e (term (x_1 @ fname(true false null)))) #true)
  (test-equal (redex-match? Loo e (term ((true = true) @ fname()))) #true)
  (test-equal (redex-match? Loo e (term (x_1 @ fname ()))) #true)
  (test-equal (redex-match? Loo e (term (true @ fname ()))) #true)
  (test-equal (redex-match? Loo e (term (null @ f(true = true)))) #false)
  (test-equal (redex-match? Loo e (term (null @ f((true = true))))) #true)
  (test-equal (redex-match? Loo e (term (null @ f ((true = true = true))))) #false))

(module+ test
  (test-equal (redex-match? Loo MethDecl (term (method m1(x_1) {()}))) #true)
  
  )

(module+ test
  (test-results))
              
                            