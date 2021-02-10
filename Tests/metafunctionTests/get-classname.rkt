#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-get-classname)

(define (test-get-classname)

  
  (display "-------------------------------------")
  (display "\nRunning get-classname Tests:\n")

  (test-equal (term (mf-apply get-classname (C1 mt)))
              (term C1)) ;; returns the class name C1 as expected
  
  (test-equal (term (mf-apply get-classname (C1 (mt [f1 -> 1]))))
              (term C1)) ;; same as above but without the empty fieldMap
  
  (test-equal (term (mf-apply get-classname (Random mt)))
              (term Random)) ;; returns the class name Random as expected
  
  (test-results)
  
  (display "-------------------------------------")

  )

