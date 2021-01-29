#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-varAssgn_OS)

(define (test-varAssgn_OS)

  (display "-------------------------------------")
  (display "\nRunning varAssgn_OS Tests:\n")


  ;;PUT TESTS HERE
  
  (test-results)
  (display "-------------------------------------")
  )



#|

Attempting to see if the reduction works:

(traces
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))])))))

--THIS WORKS

|#



