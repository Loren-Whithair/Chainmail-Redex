#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../Loo-Machine_tests/heap.rkt") ;; we could use our true_heaps from here

(provide test-new-addr) ;; for use by the test helper

(define (test-new-addr)

  (display "-------------------------------------")
  (display "\nRunning new-addr Tests:\n")

  (test-equal (term (mf-apply new-addr mt)) (term 1))
  (test-equal (term (mf-apply new-addr (mt [1 -> (C mt)]))) (term 2))
  
  (test-results)

  (display "-------------------------------------")
  )

(test-new-addr)