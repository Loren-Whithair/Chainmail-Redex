#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-MethDecl)

(define (test-MethDecl)

  (display "-------------------------------------")
  (display "\nRunning Method Declaration Tests:\n")

  (define Loo_MethDecl? (redex-match? Loo MethDecl))

  (define true_Meths (list

                      ;; simple method (no parameters, empty Stmts)
                      (term (method m() {()}))  
                      (term (method myMethod() {()}))  ; ---------- ;; the method name can be any variable not otherwise mentioned

                      
                      ;; with parameters
                      (term (method m(p1) {()}))  ; ------------- ;; a parameter doesn't have to be used in the method body
                      (term (method m(firstArg myOtherArg) {()}))  ; -------- ;;

                      ;; with method body
                      (term (method myM() {(x1 := x2 @ f)}))
                      
                      (term (method m(p1 p2) { (a1 := a2 @ fName)}))
                      (term (method m(p1 p2) { (z := y @ f) })) ;; the method body can refer to arguments that are not in the parameter list

                      ))

  (define false_Meths (list
                       (term (method_1 m(p1 p2) {()}))  ;; method is the only valid keyword
                       (term (method m(1 2) {()}))  ; ----- ;; parameters must be VarIDs
                       ))

  (for ([method_declarations true_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #true))
  
  (for ([method_declarations false_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #false))

  (test-results)
  (display "-------------------------------------")
  )