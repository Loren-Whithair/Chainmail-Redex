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

  ;;Add tests here



  
  ;----------------
  ;---FALSE TESTS--
  ;----------------

  (test--> ;;Class(x_0) ≠ Clas(this), with one class not define in Module M
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) ;;Module
          (((((x_0 @ f_1 := x_1) $ ()  ) ((mt [x_0 -> 2]) [this -> 1])) · (() mt)) ;;Stack
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))]))))  ;;Heap
   ;;doesn't reduce
  )

  (test-->  ;;Class(x_0) ≠ Class(this)
   expr-reductions
   (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})])   ;;Module
          (((((x_0 @ f_1 := x_1) $ ()  ) ((mt [x_0 -> 2]) [this -> 1])) · (() mt))  ;;Stack
           ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))]))))   ;;Heap
   ;;doesn't reduce
  )

  (test-->  ;;x_0 does NOT refer to an object, refers to a primitive 'true'
   expr-reductions
    (term ((mt [C1 -> (clss C1() {})]) ;;Module
           (((((x_0 @ f1 := x_1) $ ()  ) ((mt [x_0 -> true]) [this -> 1])) · (() mt)) ;;Stack
            ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))    ;;Heap
    ;;doesn't reduce
    )
    
  
  (test-results)
  (display "-------------------------------------")
  )

;(test-fieldAssgn_OS)


;
 ;  (term (((mt [C1 -> (clss C1() {})]) [C2 -> (clss C2() {})]) (((((x_0 @ f_1 := x_1) $ ()  ) ((mt [x_0 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))]))))
