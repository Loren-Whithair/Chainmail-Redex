#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-fieldMap)

(define (test-fieldMap)

  (display "-------------------------------------")
  (display "\nRunning fieldMap Tests:\n")

  (define Machine_fieldMaps? (redex-match? Loo-Machine fieldMap))

  (define true_fMs (list
                    (term mt)
                    (term (mt [f1 -> true]))
                    (term ((mt [f1 -> true]) [f2 -> 5]))
                    (term (((mt [f1 -> false]) [f2 -> 10]) [f3 -> null]))
                    (term ((mt [f -> 10]) [f -> 10]))
                    (term ((mt [f1 -> 10]) [f1 -> true]))
                    ))

  (define false_fMs (list
                     (term (mt))
                     (term (mt [f -> v]))
                     (term (mt [f1 -> true] [f2 -> 5]))
                     ))

  (for ([fieldMaps true_fMs])
    (test-equal (Machine_fieldMaps? fieldMaps) #true))

  (for ([fieldMaps false_fMs])
    (test-equal (Machine_fieldMaps? fieldMaps) #false))

  (test-results)

  (display "-------------------------------------")
  )
