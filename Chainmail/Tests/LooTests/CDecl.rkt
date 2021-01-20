#lang racket
(require redex)
(require "../../Loo.rkt")

; Constructor Declarations
(module+ test
  (define Loo_CDecl? (redex-match? Loo CDecl))

  (define true_Const (list
                      (term (constructor() {()}))
                      (term (constructor(arg1) {()}))
                      (term (constructor(arg1 arg2) {()}))
                      ))

  (define false_Const (list
                       (term (constructor_1(arg1 arg2) {()}))
                       (term (constructor(1 2) {()}))
                       ))
    

  (for ([constructor_declarations true_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #true))
  
  (for ([constructor_declarations false_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #false))
  )
