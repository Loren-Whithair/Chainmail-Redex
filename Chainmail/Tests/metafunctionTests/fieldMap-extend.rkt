#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-fieldMap-extend)

(define (test-fieldMap-extend)

  (display "-------------------------------------")
  (display "\nRunning fieldMap-extend Tests:\n")

  (test-equal (term (mf-apply fieldMap-extend* mt [f_0 -> null])) ;; extending an empty (mt) fieldMap
              (term (mt [f_0 -> null]))) ;; returns the original fieldMap with the additional mapping as expected
  
  (test-equal (term (mf-apply fieldMap-extend* (mt [f_0 -> null]) [f_1 -> 5])) ;; extending a fieldMap of size once (excluding mt)
              (term ((mt [f_0 -> null]) [f_1 -> 5]))) ;; returns the original fieldMap with the additional mapping as expected
  
  (test-equal (term (mf-apply fieldMap-extend* ((mt [f_0 -> null]) [f_1 -> 5]) [f_2 -> true])) ;; extending a fieldMap of size two (excluding mt)
              (term (((mt [f_0 -> null]) [f_1 -> 5]) [f_2 -> true]))) ;; returns the original fieldMap with the additional mapping as expected
  
  (test-results)

  (display "-------------------------------------")
  )

