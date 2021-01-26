Loo vs Javalite: notes, comparisons


NOTES, NEXT STEPS, QUESTIONS
--------------------------------

An attempt to define Modules is in the grammar, within the grammar for Loo.
TODO: should it be 



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
-----------|-------------
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
frame     | ϕ                  | part of state (similar): (η + e)
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


 
       

  ------------------------------------
  --------------JAVALITE--------------
  ------------------------------------
  
  (define-language Javalite
  (P ::= (μ (C m)))
  (μ ::= (CL ...))   ;; class list   ;;Loo modules? Would need the concept of multiple separate modules
  (T ::=  ;; types   ;; NOT IN LOO
     bool   
     unit
     C)

  (CL ::= (class C extends C ([T f] ...) (M ...)))  ;; class declaration == Loo ClassDesc. very similar, ?no extensions in Loo?
  (M ::= (T m ([T x] ...) e))   ; ----------------- ;; method decl == Loo MethDecl. very similar
 
 (e ::=      ;; ~~ Loo (Stmt + e)
     x       ;; in Loo e
     v       ;; in Loo e (true false null)
     (new C) ;; in Loo Stmt (x := new C(x ...))

     (e $ f) ; ---------- ;; field access ~~ Loo Stmt (x := x @ f)
     (e @ m (e ...)) ; -- ;; method invocation ~~ Loo Stmt (x := x @ m(x...))
     (e == e)  ; -------- ;; equality == Loo e (e = e)
     (C e)  ; ----------- ;; typecast ;; TYPES NOT IN LOO
     (e instanceof C) ; - ;; TYPES NOT IN LOO
     (x := e)  ; -------- ;; ~~ Loo Stmt
     (x $ f := e) ; ----- ;; field setting
     (if e e else e)  ; - ;; == e (if e then e else e)
     (var T x := e in e)  ;; var declaration
     (begin e ...)) ; --- ;; a block of code

  
  (x ::= this id) ; ----- ;; in Loo (x)
  (f ::= id)  ; --------- ;; in Loo (f)
  (m ::= id)  ; --------- ;; in Loo (m)
  (C ::= Object id)  ; -- ;;
  
  (id ::= variable-not-otherwise-mentioned)
  
  (pointer ::= (addr loc C) null)
  
  (v ::= ;; part of Loo e
     pointer
     true
     false
     unit
     error)  ;; variable
  (loc ::= number))   ;; numeric location in heap  ;;not in basic Loo, but will be needed
  

  
  
  OPERATIONAL SEMANTICS
  
  -varAssgn_os ; class type of this must match class type of y
