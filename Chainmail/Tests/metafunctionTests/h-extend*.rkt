#lang racket
(require redex)
(require "../../Loo.rkt")
(provide test-h-extend*)


(define (test-h-extend*)

  (display "-------------------------------------")
  (display "\nRunning h-extend* Tests:\n")

  (test-equal (term (mf-apply h-extend* mt [1 -> mt])) (term (mt [1 -> mt]))) ;;empty heap, add one mapping
  (test-equal (term (mf-apply h-extend* mt [1 -> (C mt)])) (term (mt [1 -> (C mt)]))) ;;empty heap, add one mapping with an object


  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [2 -> mt])) (term ((mt [1 -> mt]) [2 -> mt])))  ;;adding a new mapping to a heap with items, new name
  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [2 -> (C mt)])) (term ((mt [1 -> mt]) [2 -> (C mt)])))

  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [1 -> (C mt)])) (term (mt [1 -> (C mt)]))) ;;replacing a mapping that already existed, first mapping in map
  (test-equal (term (mf-apply h-extend* ((mt [1 -> mt]) [2 -> mt]) [2 -> (C mt)])) (term ((mt [1 -> mt]) [2 -> (C mt)]))) ;;replacing a mapping, last value
  (test-equal (term (mf-apply h-extend* (((mt [1 -> mt]) [2 -> mt]) [3 -> mt]) [2 -> (Cname mt)])) (term (((mt [1 -> mt]) [2 -> (Cname mt)]) [3 -> mt]))) ;;replacing a mapping, middle value
  (test-equal (term (mf-apply h-extend* (mt [1 -> (C mt)]) [1 -> mt])) (term (mt [1 -> mt])))

  
  (test-results)
  (display "-------------------------------------")
  )


