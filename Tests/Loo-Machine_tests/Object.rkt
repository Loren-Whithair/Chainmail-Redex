#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-object)

(define (test-object)

  (display "-------------------------------------")
  (display "\nRunning Object Tests:\n")

  (define Machine_Object? (redex-match? Loo-Machine Object))

  (define true_Objects (list

                        ;; simple Objects
                        (term (C mt))   
                        (term (Cname mt)) ;; the ClassID can be any variable not otherwise mentioned

                        ;; with fieldMappings
                        (term (C (mt [f -> 1])))  ; --------------------- ;; one field
                        (term (C ((mt [f1 -> 1]) [f2 -> 2])))  ; -------- ;; two fields
                        (term (C (((mt [f1 -> 1]) [f2 -> 2]) [f3 -> 3]))) ;; three fields

                        (term (C (((mt [f -> 1]) [f -> 2]) [f -> 1])))  ;;one object can map the same field to different values, or the same value, multiple times
                        ))

  (define false_Objects (list
                         
                         (term ((C (mt [f -> 1])))) ; ---------------- ;; extra brackets around Object
                         (term (((clss C() { ('field f) }) [f -> 5]))) ;; a ClassDesc, not an Object

                         ;; invalid fieldMaps
                         (term (C (mt [f1 -> addr]))) ;; wrong v type
                         (term (C [f1 -> 2 -> 5]))    ;; invalid mapping
                         (term (C1 [10 -> 35])) ; --- ;; 10 is not a fieldID
                         (term (C1 [f1 -> 5])) ; ---- ;; missing mt in fieldMap
                         ))

  (for ([Objects true_Objects])
    (test-equal (Machine_Object? Objects) #true))

  (for ([Objects false_Objects])
    (test-equal (Machine_Object? Objects) #false))

  (test-results)

  (display "-------------------------------------")
  )