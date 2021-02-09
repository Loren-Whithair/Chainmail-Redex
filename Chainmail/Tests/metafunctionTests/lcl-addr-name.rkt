#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../../Chainmail.rkt")

(provide test-lcl-addr-name)

(define (test-lcl-addr-name)

  (display "-------------------------------------")
  (display "\nRunning lcl-addr-name Tests:\n")

  (define l2 (term ((mt [x1 -> 1]) [x2 -> 2])))
  (define l3 (term (((mt [x1 -> 1]) [x2 -> 2]) [x3 -> 3])))
  
  (test-equal (term (mf-apply lcl-addr-name (mt [x_0 -> 2]) 2)) (term x_0))

  (test-equal (term (mf-apply lcl-addr-name ,l2 1)) (term x1))
  (test-equal (term (mf-apply lcl-addr-name ,l2 2)) (term x2))

  (test-equal (term (mf-apply lcl-addr-name ,l3 1)) (term x1))
  (test-equal (term (mf-apply lcl-addr-name ,l3 2)) (term x2))
  (test-equal (term (mf-apply lcl-addr-name ,l3 3)) (term x3))

  (test-equal (term (mf-apply lcl-addr-name ((mt [this -> 1]) [x1 -> 2]) 1)) (term this))
  
  (test-results)
  (display "-------------------------------------")
  )
