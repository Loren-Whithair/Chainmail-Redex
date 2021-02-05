#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-objCreate_OS)

(define (test-objCreate_OS)
  
  (display "-------------------------------------")
  (display "\nRunning objCreate_OS Tests:\n")


  ;----------------
  ;---TRUE TESTS---
  ;----------------

  (test-->
   expr-reductions
   (term
    ((mt [myClass -> (clss myClass(arg1 arg2) { (constructor(a1 a2) { (return this) } )})])  ;;Module
     (((((x_0 := new myClass(p1 p2)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3]))        ;;Next frame
       · (() mt))  ;;Stack
      (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)])))   ;;Heap
    )

  (term
   ((mt (myClass -> (clss myClass (arg1 arg2) ((constructor (a1 a2) ((return this)))))))  ;;Module
    ((((return this) (((mt (a1 -> 1))(a2 -> 2)) (this -> 4)))  ;New frame 
   · (((x_0 := * $ ()) (((mt (p1 -> 1)) (p2 -> 2)) (this -> 3)))  ;;Continuation frame
    · (() mt)))  ;;Stack
     ((((mt (1 -> (C1 mt))) (2 -> (C2 mt))) (3 -> (C3 mt))) (4 -> (myClass mt)))))))  ;;Heap


  (test-->
   expr-reductions

   (term
    ((mt [myClass -> (clss myClass(arg1) { (fld f1) (constructor(a1) { ((this @ f1 := a1) $ (return this)) } )})])  ;;Module
     (((((x_0 := new myClass(p1)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3]))        ;;Next frame
       · (() mt))  ;;Stack
      (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)]))))   ;;Heap

   (term ((mt (myClass -> (clss myClass(arg1) ((fld f1) (constructor (a1) (((this @ f1 := a1) $ (return this)))))))) ;; Module
          (((((this @ f1 := a1) $ (return this)) ((mt (a1 -> 1)) (this -> 4))) ;; top frame
            · (((x_0 := * $ ()) (((mt (p1 -> 1)) (p2 -> 2)) (this -> 3))) ;; next frame
               · (() mt))) ;; bottom frame
           ((((mt (1 -> (C1 mt))) (2 -> (C2 mt))) (3 -> (C3 mt))) (4 -> (myClass mt)))))) ;; heap
   )

  ;----------------
  ;---FALSE TESTS--
  ;----------------

  
  (test-results)
  (display "-------------------------------------")
  )