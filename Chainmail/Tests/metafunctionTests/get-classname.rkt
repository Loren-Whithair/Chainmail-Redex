#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-get-classname)

(define (test-get-classname)

  
  (display "-------------------------------------")
  (display "\nRunning get-classname Tests:\n")


  (test-equal (term (mf-apply get-classname (C1 mt))) (term C1))
  (test-equal (term (mf-apply get-classname (C1 (mt [f1 -> 1])))) (term C1))
  (test-equal (term (mf-apply get-classname (random mt))) (term random)) 
  
  (test-results)
  (display "-------------------------------------")

  )

