#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../Loo-Machine_tests/heap.rkt") ;; we could use our true_heaps from here

(provide test-h-lookup) ;; for use by the test helper

(define (test-h-lookup)

  (display "-------------------------------------")
  (display "\nRunning h-lookup Tests:\n")

  ; (display (term (mf-apply h-lookup (mt [1 -> (C mt)]) mt))) ;; doesn't work- will need to 
  (test-equal (term (mf-apply h-lookup (mt [1 -> (C mt)]) 1)) (term (C mt)))
  (test-equal (term (mf-apply h-lookup (mt [1 -> (C1 (mt [f1 -> true]))]) 1)) (term (C1 (mt [f1 -> true]))))
  (test-equal (term (mf-apply h-lookup ((mt [1 -> (C1 (mt [f1 -> 10]))]) [2 -> (C2 mt)]) 1)) (term (C1 (mt [f1 -> 10]))))
  (test-equal (term (mf-apply h-lookup ((mt [1 -> (C1 (mt [f1 -> 10]))]) [2 -> (C2 mt)]) 2)) (term (C2 mt)))
  (test-equal (term (mf-apply h-lookup ((mt [1 -> (C mt)]) [1 -> (C1 mt)]) 1)) (term (C1 mt))) ;; most recent heap  mapping will be the correct one (in the case of having multiple)

  (test-results)

  (display "-------------------------------------")
  )