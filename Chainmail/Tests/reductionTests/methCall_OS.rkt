#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-methCall_OS)

(define (test-methCall_OS)

  (display "-------------------------------------")
  (display "\nRunning methCall_OS Tests:\n")


  ;;PUT TESTS HERE

  (test-->
    expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0() { (return something) }) })]) (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))))
   (term ((mt (C1 -> (clss C1 () ((method m_0 () ((return something))))))) ((((return something) (mt (this -> 2))) · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) · (() mt))) ((mt (1 -> (C1 mt)))(2 -> (C1 mt)))))))


  (traces
   expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0(arg1) { (return something) }) })]) (((((x_0 := x_1 @ m_0(param1)) $ ()) (((mt [x_1 -> 2]) [this -> 1]) [param1 -> null])) · (() mt)) ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))))
  
  (test-results)
  (display "-------------------------------------")

  )