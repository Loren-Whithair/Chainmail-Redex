#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-lcl-lookup)

(define (test-lcl-lookup)

  (display "-------------------------------------")
  (display "\nRunning lcl-lookup Tests:\n")

  (test-equal (term (mf-apply η-lookup (mt [x -> 5]) x))
              5) ;; returns the value pointed to by x
  
  (test-equal (term (mf-apply η-lookup ((mt [x -> 5]) [y -> 10]) x))
              5) ;; returns the value pointed to by x
  
  (test-equal (term (mf-apply η-lookup ((mt [x -> 5]) [y -> 10]) y))
              10) ;; returns the value pointed to by y
  
  (test-equal (term (mf-apply η-lookup (((mt [x -> 5]) [x -> 10]) [x -> 15]) x))
              15) ;; most recent local variable mapping will be the correct one (in the case of having multiple)
  
  (test-results)

  (display "-------------------------------------")
  )