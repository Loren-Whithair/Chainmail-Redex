#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-h-extend)

(define (test-h-extend)

  (display "-------------------------------------")
  (display "\nRunning h-extend* Tests:\n")

  ;; h-extend* throws an error if you give it something invalid to add to the heap ("not in my domain")
  
  (test-equal (term (mf-apply h-extend* mt [1 -> (C mt)]))
              (term (mt [1 -> (C mt)]))) ;; empty heap, add one mapping

  (test-equal (term (mf-apply h-extend* (mt [1 -> (C mt)]) [2 -> (C mt)]))
              (term ((mt [1 -> (C mt)]) [2 -> (C mt)]))) ;; adding a new mapping to a heap with items, new name
  
  (test-equal (term (mf-apply h-extend* (mt [1 -> (C mt)]) [2 -> (C (mt [f1 -> null]))]))
              (term ((mt [1 -> (C mt)]) [2 -> (C (mt [f1 -> null]))])))

  (test-equal (term (mf-apply h-extend* (mt [1 -> (C mt)]) [1 -> (C (mt [f1 -> null]))]))
              (term (mt [1 -> (C (mt [f1 -> null]))]))) ;; replacing a mapping that already existed, only pair in map
  
  (test-equal (term (mf-apply h-extend* ((mt [1 -> (C (mt [f1 -> null]))]) [2 -> (C mt)]) [ 1 -> (C mt)]))
              (term ((mt [1 -> (C mt)]) [2 -> (C mt)]))) ;; replacing a mapping that already existed, first pairing in map
  
  (test-equal (term (mf-apply h-extend* ((mt [1 -> (C mt)]) [2 -> (C mt)]) [2 -> (C (mt [f1 -> null]))]))
              (term ((mt [1 -> (C mt)]) [2 -> (C (mt [f1 -> null]))]))) ;; replacing a mapping, last value
  
  (test-equal (term (mf-apply h-extend* (((mt [1 ->(C mt)]) [2 -> (C mt)]) [3 -> (C mt)]) [2 -> (Cname mt)]))
              (term (((mt [1 -> (C mt)]) [2 -> (Cname mt)]) [3 -> (C mt)]))) ;; replacing a mapping, middle value

  (test-equal (term (mf-apply h-extend* ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [2 -> (C1 mt)]))
              (term ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))) ;; we can overwrite mappings so that two addresses map to the same object
  
  (test-results)
  
  (display "-------------------------------------")
  )

