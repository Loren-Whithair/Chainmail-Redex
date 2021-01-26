#lang racket
(require redex)
(require "../../Loo.rkt")
(require "../Loo-Machine_tests/heap.rkt")

(provide test-h-lookup) ;; for use by the test helper

(define (test-h-lookup)

  (display "-------------------------------------")
  (display "\nRunning h-lookup Tests:\n")


  (define true_h-lookup (list

                        ))

  (define false_h-lookup (list

                         ))

  (for ([h-lookup true_h-lookup])
    (test-equal ))
  
  (for ([h-lookup false_h-lookup])
    (test-equal ))

  (test-results)

  (display "-------------------------------------")
  )