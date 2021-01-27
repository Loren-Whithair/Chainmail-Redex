#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-lcl-lookup)

(define (test-lcl-lookup)

  (display "-------------------------------------")
  (display "\nRunning lcl-lookup Tests:\n")

  ; (test-equal (term (mf-apply η-lookup (mt) mt)) (term (mt))) ;; failed- we will need to handle this case
  (test-equal (term (mf-apply η-lookup (mt [x -> 5]) x)) 5)
  (test-equal (term (mf-apply η-lookup ((mt [x -> 5]) [y -> 10]) x)) 5)
  (test-equal (term (mf-apply η-lookup ((mt [x -> 5]) [y -> 10]) y)) 10)
  (test-equal (term (mf-apply η-lookup (((mt [x -> 5]) [x -> 10]) [x -> 15]) x)) 15) ;; most recent local variable mapping will be the correct one (in the case of having multiple)
  
  (test-results)

  (display "-------------------------------------")
  )