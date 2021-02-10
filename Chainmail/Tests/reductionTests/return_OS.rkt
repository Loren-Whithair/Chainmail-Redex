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

  ; In this test we return the reference to 'this' from top frame where 'this' points to 2 in the local variable map.
  ; In the next frame a hole waits for the value of the above frame to be returned so the value can be assigned to local variable x_0.
  ; This reduces as we would expect with the top frame being popped off and the next frame pointing x_0 to 2.
  ; Note that these tests are identical to the ones in "return_OS-no-args.rkt", except that we have additional statements after the return call (the empty statement).
  (test-->
   expr-reductions
   (term ((mt (C1 ->
                  (clss C1() ((method m_0() (((return this) $ ()))))))) ;; module
          (((((return this) $ ()) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 ->
                  (clss C1() ((method m_0() (((return this) $ ()))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_0 -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  
  ; This test is similar to the above, with the exception that we return a boolean value 'true' instead of a reference to an object.
  ; In the next frame a hole waits for the value of the above frame to be returned so the value can be assigned to local variable x_2.
  ; This again works as expected with the top frame being popped off and the next frame pointing x_2 to 'true'.
  ; Note that these tests are identical to the ones in "return_OS-no-args.rkt", except that we have additional statements after the return call (the empty statement).
  (test-->
   expr-reductions
   (term ((mt (C1 ->
                  (clss C1() ( (method m_0() ((return x_0))))))) ;; module
          (((((return x_0) $ ()) ((mt (this -> 2)) (x_0 -> true))) ;; top frame
            · (((x_2 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

   (term ((mt (C1 ->
                  (clss C1() ((method m_0() ((return x_0))))))) ;; module
          (((() (((mt (x_1 -> 2)) (this -> 1)) (x_2 -> true))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt))))))) ;; heap

  ; NOTE: our reduction rules don't allow us to return a field directly (this is how the return reduction is defined in the Chainmail paper).
  ; However, one can assign the value of a field to a local variable and then return the local variable (this can be seen in program example "Program1.rkt").

  ;----------------
  ;---FALSE TESTS--
  ;----------------

  #|
  
  (test--> ;; y not in local variable map: throws error
   expr-reductions
     (term ((mt (C1 -> (clss C1() ((method m_0() ((return y))))))) ;; module
          (((((return y) $ ()) (mt (this -> 2))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

  ;; doesn't reduce
  )
 
 
  (test--> ;; Showing that we can't return another method call from a method.
   expr-reductions
      (term ((mt (C1 -> (clss C1() ( (fld f_0) (method m_0() ((return m_0()))) (method m_1() ((return this))))))) ;; module
          (((((return f_0) $ ()) ((mt (this -> 2)) (f_0 -> true))) ;; top frame
            · (((x_0 := * $ ()) ((mt (x_1 -> 2)) (this -> 1))) ;; next frame
               · (() mt))) ;; bottom frame
           ((mt (1 -> (C1 mt))) (2 -> (C1 mt)))))) ;; heap

  ;; doesn't reduce
  )
  |#


  (test-results)
  (display "-------------------------------------")

  )