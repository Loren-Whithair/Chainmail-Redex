#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../../Chainmail.rkt")

(provide test-addr-in-lcl)

(define (test-addr-in-lcl)

  (display "-------------------------------------")
  (display "\nRunning addr-in-lcl Tests:\n")


  (test-equal (term (mf-apply addr-in-lcl mt 0)) #false)
  (test-equal (term (mf-apply addr-in-lcl (mt [x_0 -> 0]) 1)) #false)
  (test-equal (term (mf-apply addr-in-lcl ((mt [x_0 -> 0]) [f_1 -> 1]) 2)) #false)

  (test-equal (term (mf-apply addr-in-lcl (mt [x_0 -> 0]) 0)) #true)
  (test-equal (term (mf-apply addr-in-lcl ((mt [x_0 -> 0]) [x_1 -> 1]) 1)) #true)
  
  (test-results)
  (display "-------------------------------------")
  )

