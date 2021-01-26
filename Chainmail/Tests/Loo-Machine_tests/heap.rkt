#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-heap)

(define (test-heap)

  (display "-------------------------------------")
  (display "\nRunning Heap Tests:\n")

  (define Machine_heap? (redex-match? Loo-Machine χ))

  (define true_heaps (list
                      (term mt)   ;;empty heap
                      (term (mt [1 -> (C mt)]))
                      (term (mt [1 -> (C1 (mt [f1 -> true]))]))
                      (term ((mt [1 -> (C1 (mt [f1 -> 10]))]) [2 -> (C2 mt)]))
                      ))

  (define false_heaps (list
                       (term (mt [a -> (C mt)]))  ;;invalid addr
                       (term ([1 -> (C1 (mt [f1 -> 2]))]))   ;;missing mt
                       (term (mt [1 -> true]))  ;;cannot map to values, as they are stored as immediate references from local variables (i.e. x -> v
                       (term (mt [1 -> (C1 (mt [f1 -> 10]))] [2 -> (C1 mt)]))  ;;incorrect bracketing, require nesting
                       ))

  (for ([heaps true_heaps])
    (test-equal (Machine_heap? heaps) #true))

  (for ([heaps false_heaps])
    (test-equal (Machine_heap? heaps) #false))

  (test-results)

  (display "-------------------------------------")
  )