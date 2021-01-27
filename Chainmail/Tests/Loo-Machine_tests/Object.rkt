#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-object)

(define (test-object)

  (display "-------------------------------------")
  (display "\nRunning Object Tests:\n")

  (define Machine_Object? (redex-match? Loo-Machine Object))

  (define true_Objects (list
                        (term mt) ;; empty machine object- possibly suggests that mt for machine objects is unneccessary
                        (term (C mt))  ; ----------------- ;; object with no fields
                        (term (Cname mt))   ; ------------ ;; ren
                        (term (C (mt [f -> 1])))  ; -------- ;; single field
                        (term (C ((mt [f1 -> 1]) [f2 -> 2])))  ;; multiple fields
                        (term (C (((mt [f1 -> 1]) [f2 -> 2]) [f3 -> 3])))
                        (term (C (((mt [f -> 1]) [f -> 2]) [f -> 1])))  ;;one object can map the same field to different values, or the same value, multiple times
                        ))

  (define false_Objects (list
                         (term ((C (mt [f -> 1])))) ;; showing the importance of correct bracketing
                         (term (((clss C() { ('field f) }) [f -> 5]))) ;; correct bracketing but using ClassDesc instead of class identifier
                         (term (C (mt [f1 -> addr])))     ;;wrong v type
                         (term (C [f1 -> 2 -> 5]))   ;; invalid mapping
                         (term (C1 [10 -> 35]))      ;; invalid mapping
                         (term (C1 [f1 -> 5]))
                         ))

  (for ([Objects true_Objects])
    (test-equal (Machine_Object? Objects) #true))

  (for ([Objects false_Objects])
    (test-equal (Machine_Object? Objects) #false))

  (test-results)

  (display "-------------------------------------")
  )