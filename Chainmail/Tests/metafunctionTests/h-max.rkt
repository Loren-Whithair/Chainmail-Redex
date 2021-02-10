#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-h-max) ;; for use by the test helper

(define (test-h-max)

  (display "-------------------------------------")
  (display "\nRunning h-max Tests:\n")

  ;; empty heap
  (test-equal (term (mf-apply h-max mt)) (term 0))

  ;; heap with ordered elements
  (test-equal (term (mf-apply h-max (mt [1 -> (C mt)]))) (term 1)) ; ----------------------------- ;; one element
  (test-equal (term (mf-apply h-max ((mt [1 -> (C mt)]) [2 -> (C mt)]))) (term 2)) ; ------------- ;; two elements
  (test-equal (term (mf-apply h-max (((mt [1 -> (C mt)]) [2 -> (C mt)]) [3 -> (C mt)]))) (term 3)) ;; three elements

  ;; heap with elements out of order
  (test-equal (term (mf-apply h-max ((mt [2 -> (C mt)]) [1 -> (C mt)]))) (term 2))
  (test-equal (term (mf-apply h-max (((mt [1 -> (C mt)]) [3 -> (C mt)]) [2 -> (C mt)]))) (term 3))
  
  (test-results)

  (display "-------------------------------------")
  )

