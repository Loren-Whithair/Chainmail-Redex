#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-M-match) ;; for use by the test helper

(define (test-M-match)

  (display "-------------------------------------")
  (display "\nRunning M-match Tests:\n")

  (define module1 (term (mt [C1 -> (clss C1() {})])))
  (define module2 (term ((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])))
  (define module3 (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})]) [C3 -> (clss C3() {})])))
  (define moduleX (term (mt [C1 -> (clss C(arg1 arg2) { (fld f_1) (fld f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) })])))
  
  
  (test-equal (term (mf-apply M-match mt C1)) #false)  ;; empty module has no classes
  ;;one class in module
  (test-equal (term (mf-apply M-match ,module1 C1)) #true) 
  (test-equal (term (mf-apply M-match ,module1 C2)) #false) 

  ;; two classes in module
  (test-equal (term (mf-apply M-match ,module2 C1)) #true)  ;;finding first one
  (test-equal (term (mf-apply M-match ,module2 C2)) #true)  ;;finding last class
  (test-equal (term (mf-apply M-match ,module2 C3)) #false) ;;class not in module

  ;;three classes in module
  (test-equal (term (mf-apply M-match ,module3 C1)) #true)  ;;finding first one
  (test-equal (term (mf-apply M-match ,module3 C2)) #true)  ;;finding middle one
  (test-equal (term (mf-apply M-match ,module3 C3)) #true)  ;;finding last one
  (test-equal (term (mf-apply M-match ,module3 C4)) #false) ;;not in module

  ;;more complicated ClassDesc
  (test-equal (term (mf-apply M-match ,moduleX C1)) #true)  
  (test-equal (term (mf-apply M-match ,moduleX C2)) #false)

 
  (test-results)

  (display "-------------------------------------")
  )