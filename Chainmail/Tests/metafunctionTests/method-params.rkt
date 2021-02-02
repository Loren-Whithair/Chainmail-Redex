#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-method-params)

(define (test-method-params)

  (display "-------------------------------------")
  (display "\nRunning method-params Tests:\n")

  (test-equal (term (mf-apply method-params (method m() {()}))) (term ()))   ;;empty method
  (test-equal (term (mf-apply method-params (method m(arg1) {()}))) (term (arg1)))  ;;one arg
  (test-equal (term (mf-apply method-params (method m(arg1 arg2) { (z := y @ f) }))) (term (arg1 arg2))) ;;multiple args
  (test-equal (term (mf-apply method-params (method myMeth(x1 x2 x3 otherparam) {()}))) (term (x1 x2 x3 otherparam)))
                              
  (test-results)
  (display "-------------------------------------")
  )
