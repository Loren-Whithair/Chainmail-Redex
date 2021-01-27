#lang racket
(require redex)
(require "../../Loo.rkt")
(provide test-lcl-extend*)

(define (test-lcl-extend*)

  (display "-------------------------------------")
  (display "\nRunning lcl-extend* Tests:\n")

  (test-equal (term (mf-apply η-extend* mt [x -> 1])) (term (mt [x -> 1]))) ;;empty local var map, add one mapping
  (test-equal (term (mf-apply η-extend* mt [xname -> 2])) (term (mt [xname -> 2])))

  (test-equal (term (mf-apply η-extend* (mt [x -> 1]) [x1 -> true])) (term ((mt [x -> 1]) [x1 -> true]))) ;;adding new mapping to local var map, map was not empty to start with
  (test-equal (term (mf-apply η-extend* (mt [x -> 1]) [x -> 2])) (term (mt [x -> 2])))  ;;overwriting the only mapping that existed
  (test-equal (term (mf-apply η-extend* ((mt [x -> 1]) [x1 -> 2]) [x -> 3])) (term ((mt [x -> 3]) [x1 -> 2])))  ;;overwriting the first of multiple mappings
  (test-equal (term (mf-apply η-extend* ((mt [x -> 1]) [x1 -> 2]) [x1 -> 3])) (term ((mt [x -> 1]) [x1 -> 3])))  ;;overwriting the last mapping
  (test-equal (term (mf-apply η-extend* (((mt [x0 -> 1]) [x1 -> 2]) [x2 -> 3]) [x1 -> 4])) (term (((mt [x0 -> 1]) [x1 -> 4]) [x2 -> 3]))) ;;overwriting the middle mapping

  (test-equal (term (mf-apply η-extend* ((mt [x0 -> false]) [x1 -> 2]) [x1 -> 1])) (term ((mt [x0 -> false]) [x1 -> 1]))) ;;setting two x's to map to the same value
  
  (test-results)

  (display "-------------------------------------")
  )

