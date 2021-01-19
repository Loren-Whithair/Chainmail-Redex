#lang racket
(require redex)

(provide (all-defined-out))



#| 
 Loo vs Javalite: notes, comparisons


 NOTES, NEXT STEPS, QUESTIONS
 --------------------------------

 An attempt to define Modules is in the grammar, within the grammar for Loo.


 In the paper, Loo has a definition of field and class lookup, but it is not grammatically defined.
 We will need to add a gramatical definition so that we can use it in reductions.

 Loo has a method, ghostfield, and constructor lookup.

 !!!GhostDecl exist in Loo but not in Javalite!!!

 Reductions of a state of Loo will be over #:domain σ    (runtime configs)

 In Loo, the local scope is managed by ϕ (Frame). Instead of having a list of local vars that needs mutating, the vars are assosciated with the frame, new frames are added to the stack
 when we begin execution of a new method call, expr etc.

 Do we only need expression (e) for ghost functions, nothing else (...?)


    GRAMMAR COMPARISON
 Loo       | Javalite
 ----------|-------------
 M         | μ
 ClassDesc | CL
 FieldDecl | !no equivalent! -- used for ClassDesc, declaring class fields
 CDecl     | !no equivalent! -- constructor, not used in Javalite
 MethDecl  | M
 Stmts     | ~~ (begin e ...)
 Stmt      | ~~ e (Javalite e == Loo (Stmt + e))
 GhostDecl | !no equivalent!
 e         | ~~ e (Javalite e == Loo (Stmt + e))
 x         | x
 C, f, m   | ~~C, f, m
 id        | id

 
    OPERATIONAL SEMANTICS COMPARISON
elem      | Loo                | Javalite
----------|--------------------|-----------------------------------------------------------------
frame     | Φ                  | part of state (similar): (η + e)
stack     | ψ                  | (managed by state, not explicitly an element)
heap      | χ                  | h
contin.   | Continuation       | k (but defn. in Loo paper is much less detailed than Javalite) 
class list| (MODULE + σ)       | μ   
address   | addr (Loo Machine) | pointer (Javalite, not JL-Machine)


    REDUCTIONS COMPARISON ~~roughly
 Loo           | Javalite (as it's labellled in Redex reduction)
---------------|--------------
 methcall_OS   | "raw method invocation" + "assign"
 varAssgn_OS   | "field access" + "assign"
 fieldAssgn_OS | "assign field"
 objCreate_OS  | "new"
 return_OS     | "pop η" ~~roughly, missing the actual return

|#

; -----------------------------------------------------
; -------------------- SYNTAX -------------------------
; -----------------------------------------------------

(define-language Loo

  (M ::=  ;;MODULE
     mt
     (M [C -> ClassDesc]))
  ;;TODO: is this the appropriate place to define a Module, or should this be in Loo-Machine, or separate?
     
  (ClassDesc ::= ('class C(x ...) { FieldDecl ... CDecl ... MethDecl ... GhostDecl ... }))
  (FieldDecl ::= ('field f))
  (CDecl ::= (constructor(x ...) { Stmts }))
  (MethDecl ::= (method m(x ...) { Stmts }))
  (Stmts ::= Stmt
             (Stmt $ Stmts))
  (Stmt ::= () ;; empty statement- possbily delete later
            (x @ f := x)
            (x := x @ f)
            (x := x @ m(x ...))
            (x := new C(x ...))
            (return x))
  
  (GhostDecl ::= (ghost f(x ...) { e }))  ;; missing the outermost brackets →(ghost ... e } ))←

  (e ::= true
         false
         null
         x
         (e = e)
         (if e then e else e)
         (e @ f(e ...)))
  
  (identifier ::= x C f m)
  
  (x ::= this id) ;; VarID  (variable name)
  (C ::= id)      ;; ClassID (class name)
  (f ::= id)      ;; FieldID (field name)
  (m ::= id)      ;; MethID  (method name)
  
  (id ::= variable-not-otherwise-mentioned)

  (language ::= M ClassDesc FieldDecl CDecl MethDecl Stmts GhostDecl e identifier)) ;; this is for random testing


; -----------------------------------------------------
; ---------------- MACHINE SYNTAX ---------------------
; -----------------------------------------------------

(define-extended-language Loo-Machine Loo

  (addr ::= natural) ;; addresses

  (v ::= ;;values
     null
     addr
    (addr ...))

  (Object ::=
          mt ;; added for now, maybe remove later (depending on how we use Object
          ((C [f -> v]) ...))

  (Φ ::= ;; Frame
         (Continuation η)) ;; pairs consisting of a continuation, and a mapping from identifiers to values
  
  (η ::= ;; local vars
     mt
     (η [x -> v]))   

  (ψ ::= ;; Stack
          Φ ;; might want an mt here
         (Φ · ψ)) ;; sequences of frames
<<<<<<< HEAD

  (χ ::= ;; Heap
     mt
     (χ [addr -> Object]))

=======
  
  (χ ::= ;; Heap
         mt
         ([addr -> Object] ...))
  
>>>>>>> machine tests
  (σ ::= ;; Runtime Configurations
         (ψ χ)) ;; consist of heaps and stacks of frames

  (state := (M σ))

  (Continuation ::= ;; Continuation: represents the code to be executed next
                Stmts (x := * $ Stmts))

  (machine-language ::= addr v Object Φ η ψ χ σ state Continuation)) ;; used for random testing of reduction rules


; -----------------------------------------------------
; ---------------- REDUCTION RULES --------------------
; -----------------------------------------------------

(define expr-reductions
  (reduction-relation
   Loo-Machine
   #:domain state

   ; methCall_OS
   (--> (M (((((x_0 := x_0 @ m(x ...)) $ Stmts) η) · ψ) χ))
        (M ((Φ_1 · (((x ... := * $ Stmts) η_0) · ψ)) χ))
        "methCall_OS"
        ;; where Φ_1 is the new frame, based on the method we have called
        ;; where η_0 is the new local variable set for the method we called
        
    )

   ; varAssgn_OS
   (--> (M ((((((x := this @ f) $ Stmts) η) · ψ) χ)))
        (M (((Stmts η_0) · ψ) χ))
        "fieldAssgn_OS"
        ;; where η_0 is the updated local vars, based on the assignment

    )
   
   ; fieldAssgn_OS
   (--> (M ((((((this @ f := y) $ Stmts) η) · ψ) χ)))
        (M (((Stmts η) · ψ) heap-change(χ [f -> y])))
        "varAssgn_OS"
        ;; where heap-change is defined appropriately

    )

   ; objCreate_OS
   (--> (M (((((x_0 := new C(x ...) $ Stmts) η) · ψ) χ))) ;; we might need to change (x ...) to limit or ensure that the number of elements is correct
        (M ((Φ_1 · (((x_0 := * $ Stmts) η_0) · ψ)) add-to-heap(χ [addr_1 -> (C, empty)])))   ;; we might need to change (C, empty) based on the metafunction ↓
        ;; where addr_1 is a newly allocated address on the heap, for the new object
        ;; where Φ'' is the new frame, based on the constructor
        ;; where (C, ∅) is an object created of that class, and none of the fields are assigned values
        ;; where η_0 is the new local variable set for the constructor
    )

   ; return_OS
   (--> (M ((((return x $ Stmts_0) η_0) · (((x_1 := * $ Stmts_1) η_1) · ψ)) χ))
        (M ((Stmts_1 η_2) · ψ) χ)
        "return_OS"
        ;; where η_2 is add-to-local-vars(η_1 [x_1 -> x])
        ;; x gets dereferenced with another metafunction (maybe)
        )
   
   ; return_OS
   (--> (M ((((return x) η_0) · (((x_1 := * $ Stmts_1) η_1) · ψ)) χ))
        (M ((Stmts_1 η_2) · ψ) χ)
        "return_OS - no args"
        ;; where η_2 is add-to-local-vars(η_1 [x_1 -> x])
        ;; x gets dereferenced with another metafunction (maybe)
        )
   ))

; -----------------------------------------------------
; ------------------ HELPER FUNCTIONS -----------------
; -----------------------------------------------------


#|

what we need:

heap-change(χ [f -> y]): changes field 'f' to value y

add-to-heap(χ [addr -> (C, F)]): where F is all fields of class C

add-to-local-vars(η [x_1 -> v]): assigns value of x to x_1 and then adds to η

|#

;(define-metafunction Loo-Machine
;  heap-change : χ [f -> y] -> χ
;  [[]])

; (MethDecl ::= (method m(x ...) { Stmts }))










