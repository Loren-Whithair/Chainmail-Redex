#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-methCall_OS)

(define (test-methCall_OS)

  (display "-------------------------------------")
  (display "\nRunning methCall_OS Tests:\n")

  ;----------------
  ;---TRUE TESTS---
  ;----------------

  
  (test-->
    expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0() { (return something) }) })])
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) · (() mt))
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))))

   (term ((mt (C1 -> (clss C1 () ((method m_0 () ((return something)))))))
          ((((return something) (mt (this -> 2))) · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) · (() mt)))
           ((mt (1 -> (C1 mt)))(2 -> (C1 mt)))))))

  
  (test-->
   expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0(arg1) { (return something) }) })])
          (((((x_0 := x_1 @ m_0(param1)) $ ()) (((mt [x_1 -> 2]) [this -> 1]) [param1 -> null])) · (() mt))
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))))
   
   (term ((mt (C1 -> (clss C1() ((method m_0(arg1) ((return something)))))))
          ((((return something) ((mt (arg1 -> null)) (this -> 2))) · (((x_0 := * $ ()) (((mt (x_1 -> 2)) (this -> 1)) (param1 -> null))) · (() mt))) ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))))

  
 (test-->
   expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0(arg1 arg2) { (return something) }) })])
          (((((x_0 := x_1 @ m_0(param1 param2)) $ ()) ((((mt [x_1 -> 2]) [this -> 1]) [param1 -> null]) [param2 -> null])) · (() mt))
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))))
   
   (term ((mt (C1 -> (clss C1() ((method m_0(arg1 arg2) ((return something)))))))
          ((((return something) (((mt (arg1 -> null)) (arg2 -> null)) (this -> 2))) · (((x_0 := * $ ()) ((((mt (x_1 -> 2)) (this -> 1)) (param1 -> null)) (param2 -> null))) · (() mt)))
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))))

  

  ;----------------
  ;---FALSE TESTS--
  ;----------------

  (test-->
   expr-reductions
   (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])
          ((() mt)
           mt)))

   ;;doesn't reduce
   )

  
  (test-->  ;; x_1 not an addr (i.e. not an object)
   expr-reductions
   (term ((mt [C1 -> (clss C1() { (method m_0() { (return something) }) })])
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> null]) [this -> 1])) · (() mt))
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)]))))

   ;;doesn't reduce
   )
   
  
  (test-->  ;; (M-Match M C_1) ≠ #t    - the class of x_1 is not defined in M
   expr-reductions
   (term ((mt [C2 -> (clss C2() { (method m_0() { (return something) }) })]) 
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) 
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; 

   ;;doesn't reduce
   )

  
  (test-results)
  (display "-------------------------------------")

  )