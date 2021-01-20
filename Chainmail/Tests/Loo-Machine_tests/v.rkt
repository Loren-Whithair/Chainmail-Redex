#lang racket
(require redex)
(require "../../Loo.rkt")

(display "--------------\n")
(display "Values:\n")


(define Machine_V? (redex-match? Loo-Machine v))

(define true_values (list
                     (term null)
                     (term 5)
                     (term true)
                     (term false)
                     ))

(define false_values (list
                      (term 3.5)
                      (term -2)
                      (term (1 4 6 3.5))
                      (term (1 3 4))
                      ))

(for ([values true_values])
  (test-equal (Machine_V? values) #true))

(for ([values false_values])
  (test-equal (Machine_V? values) #false))

(test-results)