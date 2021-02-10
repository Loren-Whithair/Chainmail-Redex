#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-CDecl)

(define (test-CDecl)

  (display "-------------------------------------")
  (display "\nRunning Constructor Declaration Tests:\n")

  (define Loo_CDecl? (redex-match? Loo CDecl))

  (define true_Const (list
                      (term (constructor() {()}))  ; -------------------- ;; A constructor will pattern match without a method body, but will not reduce
                      (term (constructor(arg1) {()})) ; ----------------- ;; Arguments do not have to be used in the constructor body
                      (term (constructor(arg1 arg2) {()})) ; ------------ ;;
                      (term (constructor() {(method_result := x @ m())})) ;; 
                      ))

  (define false_Const (list
                       (term (constructor_1(arg1 arg2) {()}))  ;; constructor is the only correct literal
                       (term (constructor(1 2) {()})) ; ------ ;; arguments must be VarID's, cannot be values
                       (term (constructor(arg1) {(x @ m ())})) ;; body must be a valid Stmts
                       ))
    

  (for ([constructor_declarations true_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #true))
  
  (for ([constructor_declarations false_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #false))

  (test-results)
  (display "-------------------------------------")
  )