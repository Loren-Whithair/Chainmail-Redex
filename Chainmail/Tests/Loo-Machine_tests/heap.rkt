#lang racket
(require redex)
(require "../../Loo.rkt")

(display "--------------\n")
(display "Heaps:\n")


(define Machine_heap? (redex-match? Loo-Machine χ))

(define true_heaps (list
                    (term mt)   ;;empty heap
                    (term (mt [1 -> (C)]))
                    (term (mt [1 -> (C1 [f1 -> 10])]))
                    (term ((mt [1 -> (C1 [f1 -> 10])]) [2 -> (C2 [f2 -> 100])]))
                    ))

(define false_heaps (list
                     (term (mt [a -> (C)]))  ;;invalid addr
                     (term ([1 -> (C1 [f1 -> 2])]))   ;;missing mt
                     (term (mt [1 -> true]))  ;;cannot map to values, should this be changed? Ask Julian
                     (term (mt [1 -> (C1 [f1 -> 10])] [2 -> (C1 [f1 -> 30])]))  ;;incorrect bracketing, require nesting
                     ))

(for ([heaps true_heaps])
  (test-equal (Machine_heap? heaps) #true))

(for ([heaps false_heaps])
  (test-equal (Machine_heap? heaps) #false))

(test-results)