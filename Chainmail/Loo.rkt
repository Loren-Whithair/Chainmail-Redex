 #lang racket
(require redex)

(provide (all-defined-out))

#| 
NOTES


SUGGESTIONS
--------------
- Reformat to remove some of the parentheses in the reduction rules

- Swap * with ● for context holes


--------------------------------
OPERATIONAL SEMANTICS NOTES
Fields don't have to exist when being assigned to, i.e. you can create a new field in an object with an assignment
This is because for the sake of <access> and <authority> with respect to Chainmail, being able to create new fields doesn't matter.


-------
NEXT STEPS:

- Do we want metafunctions to catch Redex errors?

- Return statements at the end of constructors, should these be enforced? Where?

- Check that there are no weird results when referring to the same address in the heap multiple times, such as modifying the same object twice in one Stmt
(i.e. what does the fieldAssgn and varAssgn reductions do when the variable is itself? e.g. x_0 @ f := x_0)

- Add/edit comments

|#





; -----------------------------------------------------
; -------------------- SYNTAX -------------------------
; -----------------------------------------------------

(define-language Loo

  (M ::=  ;;MODULE
     mt
     (M [C -> ClassDesc]))
     
  (ClassDesc ::= (clss C(x ...) { FieldDecl ... CDecl ... MethDecl ... GhostDecl ... }))
  (FieldDecl ::= (fld f))
  (CDecl ::= (constructor(x ...) { Stmts }))
  (MethDecl ::= (method m(x ...) { Stmts }))
  (Stmts ::= Stmt
             (Stmt $ Stmts))
  (Stmt ::= () ;; empty statement
            (x @ f := x)
            (x := x @ f)
            (x := x @ m(x ...))
            (x := new C(x ...))
            (return x))
  
  (GhostDecl ::= (ghost gf(x ...) { e }))   

  (e ::= true
         false
         null
         x
         (e = e)
         (if e then e else e)
         (e @ gf(e ...)))
  
  (identifier ::= x C f m)
  
  (x ::= this variable-not-otherwise-mentioned) ; ---- ;; VarID  (variable name)
  (C f m gf ::= variable-not-otherwise-mentioned)      ;; ClassID, fieldID, methodID, ghostfieldID
  
  (language ::= M ClassDesc FieldDecl CDecl MethDecl Stmts GhostDecl e identifier)) ;; for random testing

; -----------------------------------------------------
; ---------------- MACHINE SYNTAX ---------------------
; -----------------------------------------------------

(define-extended-language Loo-Machine Loo

  (addr ::= natural) ;; addresses

  (v ::= ;;values
     null
     addr
     true      ;; not in paper 
     false     ;; not in paper
     [integer] ;; not in paper
)
  
  (Object ::=
          (C fieldMap))
  (fieldMap ::=
            mt
            (fieldMap [f -> v]))

  (Φ ::= ;; Frame
         (Continuation η));; Stmts followed by a mapping of VarIDs to values
  
  (η ::= ;; local vars
     mt
     (η [x -> v]))   

  (ψ ::= ;; Stack
          Φ 
         (Φ · ψ)) 

  (χ ::= ;; Heap
     mt
     (χ [addr -> Object]))
  
  (σ ::= ;; Runtime Configurations
         (ψ χ))

  (state := (M σ)) ;; = (M ((Φ · ψ) χ))
   
  (Continuation ::= ;; 
                Stmts
                (x := * $ Stmts)) ;; * is a hole, where the frame on top will return the value to fill it

  (machine-language ::= addr v Object Φ η ψ χ σ state Continuation)) ;; for random testing of reduction rules


(define Module? (redex-match Loo M))
(define Object? (redex-match Loo-Machine Object))
(define Frame? (redex-match Loo-Machine Φ))
(define local? (redex-match Loo-Machine η))
(define stack? (redex-match Loo-Machine ψ))
(define heap? (redex-match Loo-Machine χ))
(define RC? (redex-match Loo-Machine σ))
(define state? (redex-match Loo-Machine state))
(define Cont? (redex-match Loo-Machine Continuation))


; -----------------------------------------------------
; ---------------- REDUCTION RULES --------------------
; -----------------------------------------------------

; (current-traced-metafunctions 'all)

(define expr-reductions
  (reduction-relation
   Loo-Machine
   #:domain state

   ; methCall_OS
   (--> (M (((((x_0 := x_1 @ m(x_2 ...)) $ Stmts) η) · ψ) χ)) 
        (M ((Φ_1 · (((x_0 := * $ Stmts) η) · ψ)) χ)) 
        "methCall_OS"
        (where addr_0 (η-lookup η x_1))
        (where Object_0 (h-lookup χ addr_0))
        (where C_0 (get-classname Object_0))
        (where #t (M-match M C_0))  ;; The class of x_1 must be defined in the Module
        (where ClassDesc_0 (CD-lookup M C_0)) 
        (where MethDecl_0 (method-lookup ClassDesc_0 m))
        (where Stmts_0 (method-Stmts MethDecl_0))
        (where (x_3 ...) (method-params MethDecl_0))
        ;(where _ [(param x) ...])  
        (where η_1 (η-extend* (mt [this -> (η-lookup η x_1)]) [x_3 -> (η-lookup η x_2)] ...))
        (where Φ_1 (Stmts_0 η_1))
        )

   ; varAssgn_OS
   (--> (M (((((x_0 := x_1 @ f) $ Stmts) η ) · ψ) χ)) 
        (M (((Stmts η_0 ) · ψ) χ))
        "varAssgn_OS"
        (where addr (η-lookup η x_1))  ;; x_1 must be an addr (i.e. an object, so that it can contain fields)
        (where addr_0 (η-lookup η x_1))
        (where Object_0 (h-lookup χ addr_0))
        (where addr_1 (η-lookup η this))
        (where Object_1 (h-lookup χ addr_1))
        (where [C C] [(get-classname Object_0) (get-classname Object_1)])  ;;Class(this) must == Class(x_1) for permission to access
        (where v_0 (field-lookup Object_0 f))
        (where η_0 (η-extend* η [x_0 -> v_0]))
    )

   ; fieldAssgn_OS
   (--> (M (((((x_0 @ f := x_1) $ Stmts) η) · ψ) χ)) 
        (M (((Stmts η) · ψ) χ_1)) 
        "fieldAssgn_OS"
        (where addr (η-lookup η x_0)) ;; x_0 must be an addr (i.e. an object, so that it can contain fields)
        (where addr_0 (η-lookup η x_0))
        (where Object_0 (h-lookup χ addr_0))
        (where addr_1 (η-lookup η this))
        (where Object_1 (h-lookup χ addr_1))
        (where [C C] [(get-classname Object_0) (get-classname Object_1)])  ;;Class(this) must == Class(x_1) for permission to access
        (where v_0 (η-lookup η x_1))
        (where Object_2 (Object-extend* Object_0 [f -> v_0]))
        (where χ_1 (h-extend* χ [addr_0 -> Object_2]))
    )

   ; objCreate_OS
   (--> (M (((((x_0 := new C_0(x_1 ...)) $ Stmts) η) · ψ) χ_0)) 
        ;; we might need to change (x ...) to limit or ensure that the number of elements is correct
        (M ((Φ_1 · (((x_0 := * $ Stmts) η) · ψ)) χ_1)) ;; where χ_1 = add-to-heap(χ_0 [addr_1 -> (C empty)])
        ;; we might need to change (C, empty) based on the metafunction ↓

        "objCreate_OS"
        (where addr_0 (new-addr χ_0))
        (where χ_1 (h-extend* χ_0 [addr_0 -> (C_0 mt)]))

        (where ClassDesc_0 (CD-lookup M C_0))
        (where (constructor(x_2 ...) { Stmts_1 }) (constructor-lookup ClassDesc_0))  ;;should the fields be automatically assigned by a constructor???
        ;;do constructors have to have return stmts at the end???

        (where η_1 (η-extend* (mt [this -> addr_0]) [x_2 -> (η-lookup η x_1)] ...))
        (where Φ_1 (Stmts_1 η_1))

    )

   ; return_OS
   (--> (M (((((return x_0) $ Stmts) η_0) · (((x_1 := * $ Stmts_1) η_1) · ψ)) χ))
        (M  (((Stmts_1 η_2) · ψ) χ))
        "return_OS"
        (where v_0 (η-lookup η_0 x_0)) ;; only η-lookup needed based on reduction rules defined in paper (ie: we can't return a field directly)
        (where η_2 (η-extend* η_1 [x_1 -> v_0]))
        )
   
   ; return_OS-noArgs
   (--> (M ((((return x_0) η_0) · (((x_1 := * $ Stmts_1) η_1) · ψ)) χ))
        (M (((Stmts_1 η_2) · ψ) χ))
        "return_OS -noArgs"
        (where v_0 (η-lookup η_0 x_0))
        (where η_2 (η-extend* η_1 [x_1 -> v_0]))        
   ))
  )


; -----------------------------------------------------
; ------------------ HELPER FUNCTIONS -----------------
; -----------------------------------------------------


(define-metafunction Loo-Machine
  h-max : χ -> addr
  [(h-max mt) 0]
  [(h-max (χ [addr -> Object]))
   ,(max (term addr) (term (h-max χ)))])


(define-metafunction Loo-Machine
  new-addr : χ -> addr
  [(new-addr χ)
   ,(add1 (term (h-max χ)))])


;------------------------------
;-------Simple functions-------

(define-metafunction Loo-Machine
  get-classname : Object -> C
  [(get-classname (C fieldMap)) C])

(define-metafunction Loo-Machine
  method-Stmts : MethDecl -> Stmts
  [(method-Stmts (method m_0(x ...) { Stmts })) Stmts])

(define-metafunction Loo-Machine
  method-params : MethDecl -> (x ...)
  [(method-params (method m_0(x ...) { Stmts })) (x ...)])



;------------------------------
;----Search through mappings---

(define-metafunction Loo-Machine
  h-lookup : χ addr -> Object
  [(h-lookup χ addr)
   (storelike-lookup χ addr)])

(define-metafunction Loo-Machine
  η-lookup : η x -> v
  [(η-lookup η x)
   (storelike-lookup η x)])

(define-metafunction Loo-Machine
  field-lookup : Object f -> v
  [(field-lookup (C fieldMap) f)
   (storelike-lookup fieldMap f)])

(define-metafunction Loo-Machine
  CD-lookup : M C -> ClassDesc
  [(CD-lookup M C)
   (storelike-lookup M C)])

(define-metafunction Loo-Machine
  method-lookup : ClassDesc m -> MethDecl
  [(method-lookup (clss C(x ...) { FieldDecl ... CDecl ... MethDecl ... (method m_0(x_0 ...) { Stmts }) MethDecl ...  GhostDecl ... }) m_0)
   (method m_0(x_0 ...) { Stmts })])

(define-metafunction Loo-Machine
  constructor-lookup : ClassDesc -> CDecl
  [(constructor-lookup (clss C(x_0 ...) { FieldDecl ... (constructor(x_1 ...) { Stmts_0 }) MethDecl ... GhostDecl ... }))
   (constructor(x_1 ...) { Stmts_0 })]
  [(constructor-lookup (clss C(x ...) { FieldDecl ... MethDecl ... GhostDecl ... }))
   (constructor() { (return this) })])  ;;maybe remove return this later



(define-metafunction Loo-Machine
  M-match : M C -> boolean
  [(M-match mt any_0) #false]
  [(M-match (M [C_1 -> any_1]) C_1) #true]
  [(M-match (M_1 [C_1 -> ClassDesc_1]) C_2)
   (M-match M_1 C_2)
   (side-condition (not (equal? (term C_1) (term C_2))))])



;------------------------------
;------Adding to mappings------

(define-metafunction Loo-Machine
  h-extend* : χ [addr -> Object] ... -> χ ;; takes in a arbitrary number of mappings
  [(h-extend* χ [addr -> Object] ...)
   ,(storelike-extend* <= (term χ) (term ([addr -> Object] ...)))])

(define-metafunction Loo-Machine
  η-extend* : η [x -> v] ... -> η
  [(η-extend* η [x -> v] ...)
   ,(storelike-extend* id-<= (term η) (term ([x -> v] ...)))])

(define-metafunction Loo-Machine
  Object-extend* : Object [f -> v] ... -> Object
  [(Object-extend* (C_0 fieldMap) [f -> v] ...)
   (C_0 (mf-apply fieldMap-extend* fieldMap [f -> v] ...))]) 

(define-metafunction Loo-Machine
  fieldMap-extend* : fieldMap [f -> v] ... -> fieldMap
  [(fieldMap-extend* fieldMap [f -> v] ...)
   ,(storelike-extend* id-<= (term fieldMap) (term ([f -> v] ...)))])



;------------------------------
;------Auxiliary functions-----
 
(define-metafunction Loo-Machine
  storelike-lookup : any any -> any
  ; [(storelike-lookup mt any_0) #false] ;; unable to find anything in an empty 'any' (for example, an object)
  [(storelike-lookup (any_0 [any_t -> any_ans]) any_t)
   any_ans] ;; if any_t points to any_ans in any_0, we return any_ans
  [(storelike-lookup (any_0 [any_k -> any_v]) any_t)
   (storelike-lookup any_0 any_t)
   (side-condition (not (equal? (term any_k) (term any_t))))]) ;; ensures any_k != any_t (otherwise we would match the previous condition)



(define (id-<= a b)
  (string<=? (symbol->string a) (symbol->string b)))

;; if 'storelike' is empty, return just the new mapping
; else insert the new mapping [k -> hv] while keeping the ordering of the keys
(define (storelike-extend <= storelike k hv)  ;;storelike: the map we're extending, k: the key, hv: the value
  (match storelike
    ['mt `(mt [,k -> ,hv])]
    [`(,storelike [,ki -> ,hvi])
     (cond
       [(equal? k ki)   ;; if the key is already in the mapping, just replace the value it maps to 
        `(,storelike [,ki -> ,hv])]
       [(<= k ki)  ;;otherwise, if k <= ki, recursively call one item back in the list to check if that one is equal to k
        `(,(storelike-extend <= storelike k hv) [,ki -> ,hvi])]
       [else
        `((,storelike [,ki -> ,hvi]) [,k -> ,hv])])]))     ;;if you either get to the end or find the spot where [k -> hv] fits in the ordering, put it there

(define (storelike-extend* <= storelike extend*)
  (match extend*
    ['() storelike]
    [`([,k -> ,hv] . ,extend*)
     (storelike-extend* <= (storelike-extend <= storelike k hv) extend*)]))



   