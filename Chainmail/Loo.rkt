#lang racket
(require redex)

(define-language Loo

  (M ::= ((C -> ClassDesc) ...))  ;;MODULE
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

  (x ::=      ;; VarID  (variable name)
     this
     id)
  (C ::= id)  ;; ClassID (class name)
  (f ::= id)  ;; FieldID (field name)
  (m ::= id)  ;; MethID  (method name)

  (id ::= variable-not-otherwise-mentioned))


(define-extended-language Loo-Machine Loo
  (addr ::= natural)  ;;addresses
  (v ::=    ;;values
     null
     addr
     (addr ...))
  (Object ::= (C (f -> v) ...))

  (Continuation ::=  Stmts (x := * $ Stmts)) ;;Continuation ;;TODO: may need more continuation definitions
  (ϕ ::= (CodeStub ((ident -> v) ...))) ; ---- ;; Frame   (TODO: idenfitiers?)
  (ψ ::=  ϕ  (ϕ · ψ))  ; --------------------- ;; Stack
  (χ ::= ((addr -> Object) ...)) ; ----------- ;; Heap
  (σ ::= (ψ · χ)))  ; ------------------------ ;; Runtime Config
