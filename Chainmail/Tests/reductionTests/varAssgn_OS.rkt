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

  (test-->
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})])
          (((((x_0 := x_1 @ f1) $ ()) ((mt [x_1 -> 2]) [this -> 1])) · (() mt))
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))
   
   (term ((mt (C1 -> (clss C1 () ())))
          (((() (((mt (x_1 -> 2))(this -> 1))(x_0 -> false))) · (() mt))
           ((mt (1 -> (C1 (mt (f1 -> true))))) (2 -> (C1 (mt (f1 -> false)))))))))

  
  (test-->
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})])
          (((((new_local_variable := x_1 @ f_0) $ (x_2 := x_3 @ f_1)) ((mt [x_1 -> 1]) [this -> 2])) · (() mt))
           (((mt [1 -> (C1 (mt [f_0 -> 3]))]) [2 -> (C1 (mt [f_0 -> null]))]) [3 -> (C2 mt)]))))
   
   (term ((mt (C1 -> (clss C1 () ())))
          ((((x_2 := x_3 @ f_1) (((mt (new_local_variable -> 3)) (x_1 -> 1)) (this -> 2))) · (() mt))
           (((mt (1 -> (C1 (mt (f_0 -> 3))))) (2 -> (C1 (mt (f_0 -> null))))) (3 -> (C2 mt)))))))

  
  (test-->
   expr-reductions
   (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])
          (((((result := foo @ bar) $ (temp := weather @ temperature_at(x y z))) ((mt [foo -> 2]) [this -> 2])) · (() mt))
           (((mt [1 -> (C1 (mt [bar -> 3]))]) [2 -> (C2 (mt [bar -> null]))]) [3 -> (C1 (mt [bar -> true]))]))))
   
   (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])
          ((((temp := weather @ temperature_at(x y z)) (((mt (foo -> 2)) (result -> null)) (this -> 2))) · (() mt))
           (((mt (1 -> (C1 (mt (bar -> 3))))) (2 -> (C2 (mt (bar -> null))))) (3 -> (C1 (mt (bar -> true)))))))))
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



