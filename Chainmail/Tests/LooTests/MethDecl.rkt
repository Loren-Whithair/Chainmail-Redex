#lang racket
(require redex)
(require "../../Loo.rkt")

; Method Declarations
(display "\nRunning Method Declaration Tests:\n")

(define Loo_MethDecl? (redex-match? Loo MethDecl))

(define true_Meths (list
                    (term (method m() {()}))
                    (term (method m(arg1) {()}))
                    (term (method m(arg1 arg2) {()}))
                    ))

(define false_Meths (list
                     (term (method_1 m(arg1 arg2) {()}))
                     (term (method m(1 2) {()}))
                     ))

(for ([method_declarations true_Meths])
  (test-equal (Loo_MethDecl? method_declarations) #true))
  
(for ([method_declarations false_Meths])
  (test-equal (Loo_MethDecl? method_declarations) #false))

(test-results)