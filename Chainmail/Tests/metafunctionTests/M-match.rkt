#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-M-match) ;; for use by the test helper

(define (test-M-match)

  (display "-------------------------------------")
  (display "\nRunning M-match Tests:\n")

  (define module (term ((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])))

  (test-equal (term (mf-apply M-match ,module C1)) #true)
 


  (test-results)

  (display "-------------------------------------")
  )