Loo vs Javalite: notes, comparisons


NOTES, NEXT STEPS, QUESTIONS
--------------------------------

OPERATIONAL SEMANTICS NOTES

Fields don't have to exist when being assigned to, i.e. you can create a new field in an object with an assignment
This is because for the sake of <access> and <authority> of information with respect to Chainmail, being able to create new fields doesn't matter.

Issues:

(where clauses) allow us to create information e.g. where object_1 = myfunc(x)
How do we do this for (side-condition)? We need to lookup a piece of information in a map and check what type it is

If we have a non-terminal in the grammar that may or may not be a certain other non-terminal, how do we check this in a (side-condition)

We need to create a function (Class x σ) that finds out what ClassID is attributed to the object stored in local var x in the current runtime config σ






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
