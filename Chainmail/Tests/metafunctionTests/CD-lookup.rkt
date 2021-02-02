#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-CD-lookup)

(define (test-CD-lookup)

  (display "-------------------------------------")
  (display "\nRunning CD-lookup Tests:\n")

  (define module1 (term (mt [C1 -> (clss C1() {})])))
  (define module2 (term ((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() { (fld f_1) (fld f_2) })])))
  (define module3 (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})]) [C3 -> (clss C3() {})])))
  
  (test-equal (term (mf-apply CD-lookup ,module1 C1)) (term (clss C1() {}))) ;;looking up the only ClassDesc 

  (test-equal (term (mf-apply CD-lookup ,module2 C1)) (term (clss C1() {}))) ;;lookup up the first of multiple ClassDescs
  (test-equal (term (mf-apply CD-lookup ,module2 C2)) (term (clss C2() { (fld f_1) (fld f_2) }))) ;;lookup up the last of multiple ClassDescs

  (test-equal (term (mf-apply CD-lookup ,module3 C2)) (term (clss C2() {}))) ;;lookup one of multiple ClassDescs, not first nor last


  (test-results)
  (display "-------------------------------------")
  )


