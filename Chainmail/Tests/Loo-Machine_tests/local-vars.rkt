#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-localVars)

(define (test-localVars)

  (display "-------------------------------------")
  (display "\nRunning Local-Vars Tests:\n")
  
  (define Machine_local-var? (redex-match? Loo-Machine η ))

  (define true_locals (list

                       ;; simple local var map (empty)
                       (term mt)

                       ;; with multiple mappings
                       (term (mt [x -> 5])) ; --------------------- ;; one mapping
                       (term ((mt [x -> 5]) [y -> 10])) ; --------- ;; two mappings
                       (term (((mt [x -> 5]) [y -> 10]) [z -> 15])) ;; three mappings
                       
                       (term (((mt [x -> 5]) [x -> 10]) [x -> 15])) ;; one varID can map to multiple values
                       (term ((mt [x1 -> 5]) [x2 -> 5])) ; -------- ;; two varIDs can map to the same value
                       (term ((mt [x1 -> 5]) [x1 -> 5])) ; -------- ;; the same [varID -> value] can be listed multiple times 
                       
                       ))

  (define false_locals (list
                        
                        (term ma)   ;; final value must be 'mt', cannot be 'ma'
                        (term (mt)) ;; brackets around mt
                      
                        (term (mt [x -> v])) ;; can only map to literal values, not non-terminals
                        
                        (term (mt [x -> 5] [y -> 10])) ; ------- ;; bracketing, needs nesting
                        (term (mt [x -> 5] [y -> 10] [z -> 15])) ;; bracketing, needs nesting

                        ))

  (for ([local-vars true_locals])
    (test-equal (Machine_local-var? local-vars) #true))
  
  (for ([local-vars false_locals])
    (test-equal (Machine_local-var? local-vars) #false))

  (test-results)

  (display "-------------------------------------")
  )