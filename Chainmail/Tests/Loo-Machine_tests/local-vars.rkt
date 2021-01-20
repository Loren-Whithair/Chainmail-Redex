#lang racket
(require redex)
(require "../../Loo.rkt")

(display "--------------\n")
(display "Local Variable Mappings:\n")


(define Machine_local-var? (redex-match? Loo-Machine η ))

(define true_locals (list
                     (term mt)
                     (term (mt [x -> 5]))
                     (term ((mt [x -> 5]) [y -> 10])) ;; shows the importance of correct bracketing
                     (term (((mt [x -> 5]) [y -> 10]) [z -> 15])) ;; shows the importance of correct bracketing
                     (term (((mt [x -> 5]) [x -> 10]) [x -> 15])) ;; shows that it's syntacically valid to have multiple variable mappings of the same variable in one local variable list
                     ))

(define false_locals (list
                      (term ma)
                      (term (mt))
                      
                      (term (mt [x -> v]))
                      (term (mt [x -> 5] [y -> 10])) ;; shows the importance of correct bracketing
                      (term (mt [x -> 5] [y -> 10] [z -> 15])) ;; shows the importance of correct bracketing
                      ))

(for ([local-vars true_locals])
  (test-equal (Machine_local-var? local-vars) #true))
  
(for ([local-vars false_locals])
  (test-equal (Machine_local-var? local-vars) #false))


(test-results)