#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-varAssgn_OS)

(define (test-varAssgn_OS)

  (display "-------------------------------------")
  (display "\nRunning varAssgn_OS Tests:\n")

  ;----------------
  ;---TRUE TESTS---
  ;----------------

  ; This test assigns the value of the field f1 of x_1 to x_0.
  ; x_1 points to 2 and 2 points to an object where f1 has the value of 'false'.
  ; This reduces as we expect, with x_0 pointing to false in the top frame after the redction is complete.
  (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() {})]) ;; module
          (((((x_0 := x_1 @ f1) $ ()) ((mt [x_1 -> 2]) [this -> 1])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))])))) ;; heap
   
   (term ((mt (C1
               -> (clss C1 () ()))) ;; module
          (((() (((mt (x_1 -> 2))(this -> 1))(x_0 -> false))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (1 -> (C1 (mt (f1 -> true))))) (2 -> (C1 (mt (f1 -> false))))))))) ;; heap

  ; This test is very comparable to the above, just with more complexity to the code around it.
  ; We have two objects in the heap with fields f_0, this shows that our reduction will choose the correct one.
  (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() {})]) ;; module
          (((((new_local_variable := x_1 @ f_0) $ (x_2 := x_3 @ f_1)) ((mt [x_1 -> 1]) [this -> 2])) ;; top frame
            · (() mt)) ;; bottom frame
           (((mt [1 -> (C1 (mt [f_0 -> 3]))]) [2 -> (C1 (mt [f_0 -> null]))]) [3 -> (C2 mt)])))) ;; heap
   
   (term ((mt (C1 ->
                  (clss C1 () ()))) ;; module
          ((((x_2 := x_3 @ f_1) (((mt (new_local_variable -> 3)) (x_1 -> 1)) (this -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           (((mt (1 -> (C1 (mt (f_0 -> 3))))) (2 -> (C1 (mt (f_0 -> null))))) (3 -> (C2 mt))))))) ;; heap

  ; This is very comparable to the above test (there's only so many interesting ways you can test variable assignment).
  ; The difference with this is that all the objects in the heap have the field 'bar'. Again, this reduces as we would expect.
  (test-->
   expr-reductions
   (term (((mt [C1 ->
                   (clss C1() {})]) [C2 -> (clss C2() {})]) ;; module
          (((((result := foo @ bar) $ (temp := weather @ temperature_at(x y z))) ((mt [foo -> 2]) [this -> 2])) ;; top frame
            · (() mt)) ;; bottom frame
           (((mt [1 -> (C1 (mt [bar -> 3]))]) [2 -> (C2 (mt [bar -> null]))]) [3 -> (C1 (mt [bar -> true]))])))) ;; heap
   
   (term (((mt [C1 ->
                   (clss C1() {})]) [C2 -> (clss C2() {})]) ;; module
          ((((temp := weather @ temperature_at(x y z)) (((mt (foo -> 2)) (result -> null)) (this -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           (((mt (1 -> (C1 (mt (bar -> 3))))) (2 -> (C2 (mt (bar -> null))))) (3 -> (C1 (mt (bar -> true))))))))) ;; heap
   ;; doesn't require field assignment to be from another class

  
  ;----------------
  ;---FALSE TESTS--
  ;----------------

  
  (test-->  ;;Class(this) ≠ Class(x_1)
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) 
          (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) 
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))])))) 

   ;; doesn't reduce
   )

  
  (test-->  ;;x_1 does NOT point to an address, poins to integer: [10]
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) 
          (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> [10]]) [this -> 1])) · (() mt)) 
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))    

   ;; doesn't reduce
   )

  
  (test-results)
  (display "-------------------------------------")
  )



