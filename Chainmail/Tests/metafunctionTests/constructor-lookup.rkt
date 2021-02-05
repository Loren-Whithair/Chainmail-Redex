#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../Loo-Machine_tests/heap.rkt") ;; we could use our true_heaps from here

(provide test-constructor-lookup) ;; for use by the test helper

(define (test-constructor-lookup)

  (display "-------------------------------------")
  (display "\nRunning cosntructor-lookup Tests:\n")

  (test-equal (term (mf-apply constructor-lookup (clss C() {}))) (term (constructor() { () })))
  (test-equal (term (mf-apply constructor-lookup (clss C() {(constructor(x_1 x_2) { (x1 := y1 @ f) })}))) (term (constructor(x_1 x_2) { (x1 := y1 @ f) })))
  
  (test-results)

  (display "-------------------------------------")
  )

(test-constructor-lookup)