#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-modules)

(define (test-modules)

  (display "-------------------------------------")
  (display "\nRunning Modules Tests:\n")

  (define Loo_M? (redex-match? Loo M))

  (define true_Modules (list

                        ;; simple Modules
                        (term mt)   ; --------------------------------------------- ;; empty Module
                        (term (mt [C1 -> (clss C1() {})]))  ; --------------------- ;; one ClassDesc
                        (term ((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])) ;; two ClassDesc

                        ;; more complicated ClassDescs inside of Module
                        (term (mt [C1 -> (clss C1(x1) {})])) 
                        (term (mt [C1 -> (clss C(arg1 arg2) { (fld f_1) (fld f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) })]))
                        ))

  (define false_Modules (list
                         (term (mt))  ; ----------------------------------------- ;; no brackets required around mt
                         (term ([C1 -> (clss C1() {})]))  ; --------------------- ;; missing mt
                         (term (([C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])) ;; missing mt
                         ))

  (for ([Modules true_Modules])
    (test-equal (Loo_M? Modules) #true))
  
  (for ([Modules false_Modules])
    (test-equal (Loo_M? Modules) #false))

  (test-results)

  (display "-------------------------------------")
  )