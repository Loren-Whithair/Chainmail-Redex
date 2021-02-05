#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-new-addr) ;; for use by the test helper

(define (test-new-addr)

  (display "-------------------------------------")
  (display "\nRunning new-addr Tests:\n")

  (test-equal (term (mf-apply new-addr mt)) (term 1))
  (test-equal (term (mf-apply new-addr (mt [1 -> (C mt)]))) (term 2))
  (test-equal (term (mf-apply new-addr ((mt [2 -> (C mt)]) [1 -> (C mt)]))) (term 3)) ;; shows that heap doesn't haven to be ordered (by address)
  (test-equal (term (mf-apply new-addr ((mt [5 -> (C mt)]) [3 -> (C mt)]))) (term 6)) ;; shows that there can be 'gaps' in the adresses
  (test-equal (term (mf-apply new-addr ((mt [3300 -> (C mt)]) [34000 -> (C mt)]))) (term 34001)) ;; shows that addresses work for more than 32k values
  (test-equal (term (mf-apply new-addr ((mt [65000 -> (C mt)]) [66000 -> (C mt)]))) (term 66001)) ;; shows that addresses work for more than 64k values
  (test-equal (term (mf-apply new-addr ((mt [129000 -> (C mt)]) [130000 -> (C mt)]))) (term 130001)) ;; shows that addresses work for more than 128k values
  (test-equal (term (mf-apply new-addr ((mt [257000 -> (C mt)]) [258000 -> (C mt)]))) (term 258001)) ;; shows that addresses work for more than 256k values
  
  (test-results)

  (display "-------------------------------------")
  )