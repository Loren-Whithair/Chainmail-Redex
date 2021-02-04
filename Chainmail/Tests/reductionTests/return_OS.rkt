#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-return_OS)

(define (test-return_OS)

  (display "-------------------------------------")
  (display "\nRunning return_OS Tests:\n")
  
  ;----------------
  ;---TRUE TESTS---
  ;----------------

  (test-->
   expr-reductions
   (term ((mt (C1 -> (clss C1() ((method m_0() ((return this))))))) ;; module
          ((((return this) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 -> (clss C1() ((method m_0() ((return this))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_0 -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  (test-->
   expr-reductions
   (term ((mt (C1 -> (clss C1() ( (method m_0() ((return x_0))))))) ;; module
          ((((return x_0) ((mt (this -> 2)) (x_0 -> true))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 -> (clss C1() ((method m_0() ((return x_0))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_0 -> true))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  ;; same tests as above with added Stmts after the return call
  (test-->
   expr-reductions
   (term ((mt (C1 -> (clss C1() ((method m_0() (((return this) $ ()))))))) ;; module
          ((((return this) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 -> (clss C1() ((method m_0() (((return this) $ ()))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_0 -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  (test-->
   expr-reductions
   (term ((mt (C1 -> (clss C1() ( (method m_0() ((return x_0))))))) ;; module
          ((((return x_0) ((mt (this -> 2)) (x_0 -> true))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 -> (clss C1() ((method m_0() ((return x_0))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_0 -> true))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap


  ;----------------
  ;---FALSE TESTS--
  ;----------------

  #|   ;;y not in local variable map: throws error
  (test-->
   expr-reductions
     (term ((mt (C1 -> (clss C1() ((method m_0() ((return y))))))) ;; module
          ((((return y) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

     ;;doesn't reduce
     )
  |#

 
  #|
  ;; Showing that we can't return another method call from a method.
  (traces
   expr-reductions
      (term ((mt (C1 -> (clss C1() ( (fld f_0) (method m_0() ((return m_0()))) (method m_1() ((return this))))))) ;; module
          ((((return f_0) ((mt (this -> 2)) (f_0 -> true))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap
  |#


  (test-results)
  (display "-------------------------------------")

  )