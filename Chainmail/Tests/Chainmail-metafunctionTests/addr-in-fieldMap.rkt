#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../../Chainmail.rkt")

(provide test-addr-in-fieldMap)

(define (test-addr-in-fieldMap)

  (display "-------------------------------------")
  (display "\nRunning addr-in-fieldMap Tests:\n")

  (test-equal (term (mf-apply addr-in-fieldMap (mt [f_0 -> 0]) 0)) #false)
  
  (test-results)
  (display "-------------------------------------")
  )

