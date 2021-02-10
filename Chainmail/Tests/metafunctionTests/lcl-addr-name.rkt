#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../../Chainmail.rkt")

(provide test-lcl-addr-name)

(define (test-lcl-addr-name)

  (display "-------------------------------------")
  (display "\nRunning lcl-addr-name Tests:\n")

  (define l2 (term ((mt [x1 -> 1]) [x2 -> 2]))) ;; local variable map
  
  (define l3 (term (((mt [x1 -> 1]) [x2 -> 2]) [x3 -> 3]))) ;; local variable map
  
  (test-equal (term (mf-apply lcl-addr-name (mt [x_0 -> 2]) 2))
              (term x_0)) ; returns the variable the points to 2

  (test-equal (term (mf-apply lcl-addr-name ,l2 1))
              (term x1)) ; returns the variable that points to 1 in l2
  
  (test-equal (term (mf-apply lcl-addr-name ,l2 2))
              (term x2)) ; returns the variable that points to 2 in l2

  (test-equal (term (mf-apply lcl-addr-name ,l3 1))
              (term x1)) ; returns the variable that points to 1 in l3
  
  (test-equal (term (mf-apply lcl-addr-name ,l3 2))
              (term x2)) ; returns the variable that points to 2 in l3
  
  (test-equal (term (mf-apply lcl-addr-name ,l3 3))
              (term x3)) ; returns the variable that points to 3 in l3

  (test-equal (term (mf-apply lcl-addr-name ((mt [this -> 1]) [x1 -> 2]) 1))
              (term this)) ; returns the variable the points to 1
  
  (test-results)
  
  (display "-------------------------------------")
  )
