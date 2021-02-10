#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-method-lookup)

(define (test-method-lookup)

  (display "-------------------------------------")
  (display "\nRunning method-params Tests:\n")

  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0() { () }) }) m_0))
              (term (method m_0() { () }))) ;; finds the MethDecl corresponing to method named m_0
  
  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0(x_0) { () }) }) m_0))
              (term (method m_0(x_0) { () }))) ;; finds the MethDecl corresponing to method named m_0
  
  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0(y_0 x_0) { () }) }) m_0))
              (term (method m_0(y_0 x_0) { () }))) ;; finds the MethDecl corresponing to method named m_0
  

  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0() { ((method_result := x @ m(arg1 arg2)) $ (object_result := new C(arg1 arg2))) }) }) m_0))
              (term (method m_0() { ((method_result := x @ m(arg1 arg2)) $ (object_result := new C(arg1 arg2))) }))) ;; finds the MethDecl corresponing to method named m_0

  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0(x_0) { ((x @ f := y) $ (return x)) }) }) m_0))
              (term (method m_0(x_0) { ((x @ f := y) $ (return x)) }))) ;; finds the MethDecl corresponing to method named m_0

  (test-equal (term (mf-apply method-lookup (clss C() { (method m_0(y_0 x_0) { (() $ (() $ ())) }) }) m_0))
              (term (method m_0(y_0 x_0) { (() $ (() $ ())) }))) ;; finds the MethDecl corresponing to method named m_0
  
  (test-results)
  
  (display "-------------------------------------")
  )