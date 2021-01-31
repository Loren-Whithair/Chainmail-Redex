#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-Object-extend)

(define (test-Object-extend)

  (display "-------------------------------------")
  (display "\nRunning Object-extend* Tests:\n")

  (test-equal (term (mf-apply Object-extend* (C mt) [f1 -> true])) (term (C (mt [f1 -> true])))) ; ------------------------------ ;; empty fieldMap, add one field->var mapping
  (test-equal (term (mf-apply Object-extend* (C (mt [f1 -> true])) [f2 -> false])) (term (C ((mt [f1 -> true]) [f2 -> false]))))  ;; fieldMap with one mapping, add a new field->var mapping
  (test-equal (term (mf-apply Object-extend* (C (mt [f1 -> true])) [f1 -> false])) (term (C (mt [f1 -> false]))))  ; ------------ ;; replacing a value a pre-existing fieldname points to
  (test-equal (term (mf-apply Object-extend* (C ((mt [f1 -> 1]) [f2 -> 2])) [f2 -> false])) (term (C ((mt [f1 -> 1]) [f2 -> false]))))
  (test-equal (term (mf-apply Object-extend* (C ((mt [f1 -> [10]]) [f2 -> 1])) [f1 -> 1])) (term (C ((mt [f1 -> 1]) [f2 -> 1])))) ;; two different fields can map to the same value
  
  (test-results)
  (display "-------------------------------------")
  )

