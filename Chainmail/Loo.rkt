#lang racket
(require redex)

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

(define-language Loo

  (M ::= ([C -> ClassDesc] ...))  ;;MODULE
  ;;TODO: could it be (ClassDesc ...) ?
  ;;TODO: is this the appropriate place to define a Module, or should this be in Loo-Machine, or separate?
     
  (ClassDesc ::= (class C(x ...) { (FieldDecl) ... (CDecl)? (MethDecl) ... (GhostDecl) ... }))
  (FieldDecl ::= (field f))
  (CDecl ::= (constructor(x ...) { Stmts }))
  (MethDecl ::= (method m(x ...) { Stmts }))
  (Stmts ::= Stmt
             (Stmt $ Stmts))
  (Stmt ::= (x @ f := x)
            (x := x @ f)
            (x := x @ m(x ...))
            (x := new C(x ...))
            (return x))
  (GhostDecl ::= ghost f(x ...) { e })
  (e ::= true
         false
         null
         x
         (e = e)
         (if e then e else e)
         (e @ f(e ...)))

  (x ::= this id) ;; VarID  (variable name)
  (C ::= id)      ;; ClassID (class name)
  (f ::= id)      ;; FieldID (field name)
  (m ::= id)      ;; MethID  (method name)
  
  (id ::= variable-not-otherwise-mentioned))


(define-extended-language Loo-Machine Loo
  (addr ::= natural) ;;addresses
  (v ::= ;;values
     null
     addr
    (addr ...))
  (Object ::= ((C [f -> v]) ...))

  (Φ ::= ;; Frame
         (CodeStub ([ident -> v] ...))) ;; pairs consisting of a continuation, and a mapping from identifiers to values  (TODO: idenfitiers, CodeStub?)
  (ψ ::= ;; Stack
          Φ
         (Φ · ψ)) ;; sequences of frames
  (χ ::= ;; Heap
         ([addr -> Object] ...))
  (σ ::= ;; Runtime Configurations
         (ψ χ)) ;; consist of heaps and stacks of frames

  (Continuation ::= ;; represents the code to be executed next
                (Stmts (x := * $ Stmts)))) ;; Continuation ;;TODO: may need more continuation definitions
