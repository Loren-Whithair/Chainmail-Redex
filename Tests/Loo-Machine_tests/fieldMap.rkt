#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-fieldMap)

(define (test-fieldMap)

  (display "-------------------------------------")
  (display "\nRunning fieldMap Tests:\n")

  (define Machine_fieldMaps? (redex-match? Loo-Machine fieldMap))

  (define true_fMs (list

                    ;; simple fieldMap (empty)
                    (term mt)

                    ;; with multiple mappings
                    (term (mt [f1 -> true]))  ; ------------------------- ;; one mapping
                    (term ((mt [f1 -> true]) [f2 -> 5])) ; -------------- ;; two mappings
                    (term (((mt [f1 -> false]) [f2 -> 10]) [f3 -> null])) ;; three mappings

                    (term ((mt [f1 -> 10]) [f2 -> 10]))   ;; two different fields can map to the same value
                    (term ((mt [f1 -> 10]) [f1 -> true])) ;; the same field can map to multiple values
                    (term ((mt [f -> 10]) [f -> 10]))     ;; the same field-value mapping can be listed twice
                    ))

  (define false_fMs (list
                     (term (mt))  ; -------------------- ;; brackets around mt
                     (term (mt [f -> v]))  ; ----------- ;; fields must map to actual values, not non-terminals
                     (term (mt [f1 -> true] [f2 -> 5]))  ;; incorrect brackets, must be nested
                     ))

  (for ([fieldMaps true_fMs])
    (test-equal (Machine_fieldMaps? fieldMaps) #true))

  (for ([fieldMaps false_fMs])
    (test-equal (Machine_fieldMaps? fieldMaps) #false))

  (test-results)

  (display "-------------------------------------")
  )
