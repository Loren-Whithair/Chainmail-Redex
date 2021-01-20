#lang racket
(require redex)
(require "../../Loo.rkt")

(provide (all-defined-out))

;Modules
(module+ test
  (define Loo_M? (redex-match? Loo M))

  (define true_Modules (list
                        (term mt)
                        (term (mt [C1 -> ('class C1() {})]))
                        (term ((mt [C1 -> ('class C1() {})]) [C2 -> ('class C2() {})]))
                        (term (mt [C1 -> ('class C1(x1) {})]))
                        (term (mt [C1 -> ('class C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) })]))
                      ))

  (define false_Modules (list
                       (term (mt))
                       (term ([C1 -> ('class C1() {})]))
                       (term (([C1 -> ('class C1() {})]) [C2 -> ('class C2() {})]))
                       ))

  (for ([Modules true_Modules])
    (test-equal (Loo_M? Modules) #true))
  
  (for ([Modules false_Modules])
    (test-equal (Loo_M? Modules) #false))
  )