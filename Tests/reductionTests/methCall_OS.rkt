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

  ; In this test, we call the method m_0 in class C0 which returns 'something' and save it in local variable x_0.
  ; This works as expected with the next step in the reduction creating a new frame with the statement 'return something',
  ; then there's another frame under this one (the continuation) which waits for the result to store in x_0 (a hole).
  (test-->
    expr-reductions
   (term ((mt [C1 ->
                  (clss C1() { (method m_0() { (return something) }) })]) ;; module
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; heap

   (term ((mt (C1 ->
                  (clss C1 () ((method m_0 () ((return something))))))) ;; module
          ((((return something) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt)))(2 -> (C1 mt))))))) ;; heap

  ; This test is nearly identical to the above except that we pass an argument to the method.
  ; It reduces as expected with the top frame having the argument passed in the local variable maps.
  (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() { (method m_0(arg1) { (return something) }) })]) ;; module
          (((((x_0 := x_1 @ m_0(param1)) $ ()) (((mt [x_1 -> 2]) [this -> 1]) [param1 -> null])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; heap
   
   (term ((mt (C1 ->
                  (clss C1() ((method m_0(arg1) ((return something))))))) ;; module
          ((((return something) ((mt (arg1 -> null)) (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) (((mt (x_1 -> 2)) (this -> 1)) (param1 -> null))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  ; This test is nearly identical to the above except that we pass in two arguments. It also reduces as expected.
 (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() { (method m_0(arg1 arg2) { (return something) }) })]) ;; module
          (((((x_0 := x_1 @ m_0(param1 param2)) $ ()) ((((mt [x_1 -> 2]) [this -> 1]) [param1 -> null]) [param2 -> null])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; heap
   
   (term ((mt (C1 ->
                  (clss C1() ((method m_0(arg1 arg2) ((return something))))))) ;; module
          ((((return something) (((mt (arg1 -> null)) (arg2 -> null)) (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((((mt (x_1 -> 2)) (this -> 1)) (param1 -> null)) (param2 -> null))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  

  ;----------------
  ;---FALSE TESTS--
  ;----------------
  
  (test-->  ;; x_1 not an addr (i.e. not an object)
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() { (method m_0() { (return something) }) })]) ;; module
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> null]) [this -> 1])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; heap

   ;;doesn't reduce
   )
   
  
  (test-->  ;; (M-Match M C_1) ≠ #t    - the class of x_1 is not defined in M
   expr-reductions
   (term ((mt [C2 ->
                  (clss C2() { (method m_0() { (return something) }) })]) ;; module
          (((((x_0 := x_1 @ m_0()) $ ()) ((mt [x_1 -> 2]) [this -> 1])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 mt)]) [2 -> (C1 mt)])))) ;; heap

   ;;doesn't reduce
   )

  
  (test-results)
  (display "-------------------------------------")

  )