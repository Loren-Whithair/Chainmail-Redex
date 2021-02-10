#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-fieldAssgn_OS)

(define (test-fieldAssgn_OS)

  (display "-------------------------------------")
  (display "\nRunning fieldAssgn_OS Tests:\n")

  
  ;----------------
  ;---TRUE TESTS---
  ;----------------

  ; This test assigns the value of the field f belonging to x_0 to x_1.
  ; This works as we would expect with the field of the object pointed to by x_0 (address 0 in the heap), changing field value from 'null' to 1 (which is the value of x_1 in the local variable map).
  ; Note: that this value doesn't point to anything in the heap (we do no checks for this).
  (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() {})]) ;; module
          (((((x_0 @ f := x_1) $ (x := x @ m(x y))) (((mt [x_0 -> 0]) [x_1 -> 1]) [this -> 0])) ;; top frame
            · (() mt)) ;; bottom frame
           (mt [0 -> (C1 (mt [f -> null]))])))) ;; heap
   
   (term ((mt (C1 ->
                  (clss C1 () ()))) ;; module
          ((((x := x @ m(x y)) (((mt (x_0 -> 0)) (x_1 -> 1)) (this -> 0))) ;; top frame
            · (() mt)) ;; bottom frame
           (mt (0 -> (C1 (mt (f -> 1))))))))) ;; heap

  ; This test is extremely comparable to the above test with the difference being that the value assigned to the field does actually point to an object in the heap.
  (test-->
   expr-reductions
   (term ((mt [C1 ->
                  (clss C1() {})]) ;; module
          (((((x_0 @ f := x_1) $ ()) (((mt [x_0 -> 0]) [x_1 -> 2]) [this -> 2])) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt [0 -> (C1 (mt [f -> null]))]) [2 -> (C1 (mt [f -> true]))])))) ;; heap
   
   (term ((mt (C1 ->
                  (clss C1 () ()))) ;; module
          (((() (((mt (x_0 -> 0)) (x_1 -> 2)) (this -> 2))) ;; top frame
            · (() mt)) ;; bottom frame
           ((mt (0 -> (C1 (mt (f -> 2))))) (2 -> (C1 (mt (f -> true))))))))) ;; heap

  
  ;----------------
  ;---FALSE TESTS--
  ;----------------

  (test--> ;;Class(x_0) ≠ Class(this), with one class not define in Module M
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})])
          (((((x_0 @ f_1 := x_1) $ ()  ) ((mt [x_0 -> 2]) [this -> 1])) · (() mt))
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))]))))

   ;;doesn't reduce
  )

  
  (test-->  ;;Class(x_0) ≠ Class(this)
   expr-reductions
   (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])
          (((((x_0 @ f_1 := x_1) $ ()  ) ((mt [x_0 -> 2]) [this -> 1])) · (() mt))  
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))])))) 

   ;;doesn't reduce
  )

  
  (test-->  ;;x_0 does NOT refer to an object, refers to a primitive 'true'
   expr-reductions
    (term ((mt [C1 -> (clss C1() {})]) 
           (((((x_0 @ f1 := x_1) $ ()  ) ((mt [x_0 -> true]) [this -> 1])) · (() mt)) 
            ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))  

    ;;doesn't reduce
   )

  
  (test-results)
  (display "-------------------------------------")
  )
