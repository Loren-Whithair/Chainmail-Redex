#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-h-extend*)

(define (test-h-extend*)

  (display "-------------------------------------")
  (display "\nRunning h-extend* Tests:\n")


  ;; h-extend* throws an error if you give it something invalid to add to the hepa ("not in my domain") - do we want to test these? Can we catch errors?
  
  (test-equal (term (mf-apply h-extend* mt [1 -> mt])) (term (mt [1 -> mt]))) ;;empty heap, add one mapping
  (test-equal (term (mf-apply h-extend* mt [1 -> (C mt)])) (term (mt [1 -> (C mt)]))) ;;empty heap, add one mapping with an object


  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [2 -> mt])) (term ((mt [1 -> mt]) [2 -> mt])))  ;;adding a new mapping to a heap with items, new name
  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [2 -> (C mt)])) (term ((mt [1 -> mt]) [2 -> (C mt)])))

  (test-equal (term (mf-apply h-extend* (mt [1 -> mt]) [1 -> (C mt)])) (term (mt [1 -> (C mt)]))) ;;replacing a mapping that already existed, only pair in map
  (test-equal (term (mf-apply h-extend* ((mt [1 -> mt]) [2 -> mt]) [ 1 -> (C mt)])) (term ((mt [1 -> (C mt)]) [2 -> mt])))  ;;replacing a mapping that already existed, first pairing in map
  (test-equal (term (mf-apply h-extend* ((mt [1 -> mt]) [2 -> mt]) [2 -> (C mt)])) (term ((mt [1 -> mt]) [2 -> (C mt)]))) ;;replacing a mapping, last value
  (test-equal (term (mf-apply h-extend* (((mt [1 -> mt]) [2 -> mt]) [3 -> mt]) [2 -> (Cname mt)])) (term (((mt [1 -> mt]) [2 -> (Cname mt)]) [3 -> mt]))) ;;replacing a mapping, middle value

  (test-equal (term (mf-apply h-extend* ((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [2 -> (C1 mt)])) (term ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))) ;;we can overwrite mappings so that two addresses map to the same object

  
  (test-results)
  (display "-------------------------------------")
  )

