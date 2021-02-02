#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-method-Stmts)

(define (test-method-Stmts)

  (display "-------------------------------------")
  (display "\nRunning method-Stmts Tests:\n")

  (test-equal (term (mf-apply method-Stmts (method m() {()}))) (term ()))  ;;empty stmt list
  
  (test-equal (term (mf-apply method-Stmts (method m(arg1 arg2) { (z := y @ f) }))) (term (z := y @ f)))  ;;Stmts == Stmt
  
  (test-equal (term (mf-apply method-Stmts (method m(arg1) { ((x2 @ f1 := x1) $ (x1 := x2 @ m1(arg2))) })))
              (term ((x2 @ f1 := x1) $ (x1 := x2 @ m1(arg2))))) ;;Stmts == (Stmt $ Stmt)
  
  (test-results)
  (display "-------------------------------------")
  )
