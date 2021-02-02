#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-methCall_OS)

(define (test-methCall_OS)

  (display "-------------------------------------")
  (display "\nRunning methCall_OS Tests:\n")


  ;;PUT TESTS HERE

  (traces
   expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0() { (return something) }) })]) (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))))
  
  (test-results)
  (display "-------------------------------------")

  )