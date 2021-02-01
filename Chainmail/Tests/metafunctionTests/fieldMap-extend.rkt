#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-fieldMap-extend)

(define (test-fieldMap-extend)

  (display "-------------------------------------")
  (display "\nRunning fieldMap-extend Tests:\n")

  (test-equal (term (mf-apply fieldMap-extend* mt [f_0 -> null])) (term (mt [f_0 -> null])))
  (test-equal (term (mf-apply fieldMap-extend* (mt [f_0 -> null]) [f_1 -> 5])) (term ((mt [f_0 -> null]) [f_1 -> 5])))
  (test-equal (term (mf-apply fieldMap-extend* ((mt [f_0 -> null]) [f_1 -> 5]) [f_2 -> true])) (term (((mt [f_0 -> null]) [f_1 -> 5]) [f_2 -> true])))
  
  (test-results)

  (display "-------------------------------------")
  )

