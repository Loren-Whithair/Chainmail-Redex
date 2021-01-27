#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-field-lookup)

(define (test-field-lookup)

  (display "-------------------------------------")
  (display "\nRunning field-lookup Tests:\n")

  (test-equal (term (mf-apply field-lookup (C (mt [f1 -> 1])) f1)) (term 1))  ;; lookup the only field
  (test-equal (term (mf-apply field-lookup (C (mt [f1 -> true])) f1)) (term true)) 

  (test-equal (term (mf-apply field-lookup (C ((mt [f1 -> 1]) [f2 -> 2])) f1)) (term 1)) ;; lookup the first field
  (test-equal (term (mf-apply field-lookup (C ((mt [f1 -> 1]) [f2 -> 2])) f2)) (term 2)) ;; lookup the last field
  (test-equal (term (mf-apply field-lookup (C (((mt [f1 -> 1]) [f2 -> 2]) [f3 -> 3])) f2)) (term 2)) ;; lookup one in the middle
  
  (test-equal (term (mf-apply field-lookup (C (((mt [f -> 1]) [f -> 2]) [f -> 3])) f)) (term 3))  ;;when there are multiple fields with the same name, returns the last one in the list
  (test-equal (term (mf-apply field-lookup (C ((((mt [f -> 1]) [f1 -> 2]) [f -> 3]) [f1 -> 4])) f)) (term 3)) ;;returns the last one in the list that has that field name
       
  (test-equal (term (mf-apply field-lookup (C (((mt [f -> 1]) [f -> 2]) [f -> 3])) f)) (term 3)) ;; when there are multiple fields with the same name, returns the last one in the list
  (test-equal (term (mf-apply field-lookup (C ((((mt [f -> 1]) [f1 -> 2]) [f -> 3]) [f1 -> 4])) f)) (term 3)) ;; returns the last one in the list that has that field name
                        
  (test-results)
  (display "-------------------------------------")
  )

