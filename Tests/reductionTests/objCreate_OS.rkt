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

  ; TODO: 
  
  ; This test creates a new myClass object and assigns it to local variable x_0.
  ; MyClass takes in two arguments and then returns 'this'.
  ; This reduces as we would expect by adding a new value to the heap which points to our new object and two new frames.
  ; Of these, the top frame is a a (return this) statement, where 'this' points to the newly created object in the heap.
  ; The next frame has a hole (continuation) which waits for the value of the above frame and assign ito to local variable x_0.
  (test-->
   expr-reductions

   (term
    ((mt [MyClass ->
                  (clss MyClass(arg1 arg2) { (constructor(a1 a2) { (return this) } )})]) ;; module
     (((((x_0 := new MyClass(p1 p2)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3])) ;; top frame
       · (() mt)) ;; bottom frame
      (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)])))) ;; heap

  (term
   ((mt (MyClass ->
                 (clss MyClass (arg1 arg2) ((constructor (a1 a2) ((return this))))))) ;; module
    ((((return this) (((mt (a1 -> 1))(a2 -> 2)) (this -> 4))) ;; top frame 
   · (((x_0 := * $ ()) (((mt (p1 -> 1)) (p2 -> 2)) (this -> 3))) ;; Continuation frame
    · (() mt))) ;; bottom frame
     ((((mt (1 -> (C1 mt))) (2 -> (C2 mt))) (3 -> (C3 mt))) (4 -> (MyClass mt))))))) ;; heap


  ; This test creates a new myClass object and assigns it to local variable x_0.
  ; MyClass takes in one argument, assigns the value of the argument to the field f1, and then returns 'this'.
  ; This reduces as we would expect by adding a new value to the heap which points to our new object and two new frames.
  ; Of these, the top frame is a a (return this) statement, where 'this' points to the newly created object in the heap.
  ; The next frame has a hole (continuation) which waits for the value of the above frame and assign ito to local variable x_0.
  
  ; Note the the pointer to the new object in the heap doesn't have the argument that we assigned in the fieldMap.
  ; This is because we have only done one step of the reduction.
  ; To see the full evaulation of this program, see "Program2.rkt" in '../ProgramExamples/'
  (test-->
   expr-reductions

   (term
    ((mt [MyClass ->
                  (clss MyClass(arg1) { (fld f1) (constructor(a1) { ((this @ f1 := a1) $ (return this)) } )})]) ;; module
     (((((x_0 := new MyClass(p1)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3])) ;; top frame
       · (() mt)) ;; bottom frame
      (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)])))) ;; heap

   (term ((mt (MyClass ->
                       (clss MyClass(arg1) ((fld f1) (constructor (a1) (((this @ f1 := a1) $ (return this)))))))) ;; module
          (((((this @ f1 := a1) $ (return this)) ((mt (a1 -> 1)) (this -> 4))) ;; top frame
            · (((x_0 := * $ ()) (((mt (p1 -> 1)) (p2 -> 2)) (this -> 3))) ;; next frame
               · (() mt))) ;; bottom frame
           ((((mt (1 -> (C1 mt))) (2 -> (C2 mt))) (3 -> (C3 mt))) (4 -> (MyClass mt))))))) ;; heap

  ;----------------
  ;---FALSE TESTS--
  ;----------------

  
  (test-results)
  (display "-------------------------------------")
  )