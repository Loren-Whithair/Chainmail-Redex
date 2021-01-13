#lang racket
(require redex)

; -----------------------------------------------------
; -------------------- SYNTAX -------------------------
; -----------------------------------------------------

(define-language Javalite
  (P ::= (μ (C m)))
  (μ ::= (CL ...))   ;; class list
  (T ::=  ;; types
     bool
     unit
     C)

  (CL ::= (class C extends C ([T f] ...) (M ...)))  ;; class declaration
  (M ::= (T m ([T x] ...) e))   ;; method declaration
  (e ::=
     x
     v
     (new C)

     (e $ f) ; ---------- ;; field access
     (e @ m (e ...)) ; -- ;; method invocation, with first e as the object the method is being invoked on and (e ...) the args
     (e == e)
     (C e)  ; ----------- ;; typecast
     (e instanceof C)
     (x := e)
     (x $ f := e) ; ----- ;; field setting
     (if e e else e)
     (var T x := e in e)  ;; var declaration, where x is set to the first e, and the second e is the scope in which the var is used
     (begin e ...)) ; --- ;; a block of code
  (x ::= this id)
  (f ::= id)
  (m ::= id)
  (C ::= Object id)
  (id ::= variable-not-otherwise-mentioned)
  (pointer ::= (addr loc C) null)
  (v ::=
     pointer
     true
     false
     unit
     error)  ;; variable
  (loc ::= number))   ;; numeric location in heap


; -----------------------------------------------------
; ---------------- MACHINE SYNTAX ---------------------
; -----------------------------------------------------

(define-extended-language
  JL-Machine Javalite
  

  (e ::=  ;; expression, (i.e. control string)
     ....
     (raw v @ m (v ...)))   ;; a method call once all of the expressions (both the subject and the args) have been swapped out for their evaluated values from the heap

  
  (object ::= ((C [f loc] ...) ...)) ;; a list of classes (super + self), tupled, with their list of field names and the locations of the fields in the heap


  (hv ::= v object) ;; objects that can be stored in the heap
  
  (h ::=   ;; heap
     mt ; ------------ ;; empty
     (h [loc -> hv]))  ;; mapping of locations (numbers) to objects

  (η ::=   ;; local environment-mapping from local variables to heap locations
     mt
     (η [x -> loc]))

  (state ::= (μ h η e k)) ;; Javalites operational semantics are based on the concept of evolving program state

  (k ::=  ;; continuation
     ret
     (* $ f -> k)                    ;; reducing current expression to and object in prep for a field access, before continuing with k
     (* @ m (e ...) -> k)            ;; reducing current expression to the object on which method invocation will be performed
     (v @ m (v ...) * (e ...) -> k)  ;; evaluating the arguments of the method invocation
     (* == e -> k)                   ;; reducing left operand of equality operator to a value
     (v == * -> k)                   ;; reducing right operand of equality operator to a value
     (C * -> k)                      ;; reducing current expression to an object for casting to instance of C
     (* instanceof C -> k)           ;; reducing current expression to an object on which to check membership in the class heirarchy of C
     (x := * -> k)                   ;; evaluating an expression for assignment to a variable
     (x $ f := * -> k)               ;; reducing the expression for assignment to a field
     (if * e else e -> k)            ;; reducing thhe expresion to a value, which is the predicate of an if statement
     (var T x := * in e -> k)        ;; reducing an expression for assignment to a local variable
     (begin * (e ...) -> k)          ;; reducing an expression in a list of expression to be reduced
     (pop η k)))                     ;; restoring the local environment η before continuing with k


; -----------------------------------------------------
; ---------------- REDUCTION RULES --------------------
; -----------------------------------------------------

(define expr-reductions
  (reduction-relation
   JL-Machine
   #:domain state

   ; variable access
   (--> (μ h η x k)
        (μ h η v k)   ;; x becomes v
        "Variable access"
        (where v (h-lookup h (η-lookup η x))))
   
 ; new
   (--> (μ h η (new C) k)
        (μ h_1 η (addr loc_1 C) k)   ;; new heap, (new C) becomes a pointer to the created obj
        "new"
        (where (([T_0 f_0] ...) ...) (fields-parents+self μ C))  ;; list of fields in super classes and this class
        (where (C_0 ...) (class-parents+self μ C))  ; ---------- ;; 
        (where ((v_0 ...) ...) ((default-value* (T_0 ...)) ...)) ;;
        (where (number_0 ...) ((get-length (T_0 ...)) ...))  ; - ;; finds the number of fields for each class...
        (where ((loc_0 ...) ...) (h-malloc-n* h number_0 ...))   ;; allocates heap locations for all of said fields...
        (where object ((C_0 [f_0 loc_0] ...) ...))  ; ---------- ;;  
        (where h_0 (h-extend* h [loc_0 -> v_0] ... ...))  ; ---- ;;
        (where loc_1 (h-malloc h_0))   ; ----------------------- ;;
        (where h_1 (h-extend* h_0 [loc_1 -> object])))  ; ------ ;;

   
   ; field access
   (--> (μ h η (e $ f) k)   
        (μ h η e (* $ f -> k))  ;; the object the field is being extracted from becomes the expression (in preparation for fetching the field value)
        "field access - object eval")
   
   (--> (μ h η (addr loc C) (* $ f -> k))
        (μ h η v k)  ;; the expression goes from pointer to field value, field access command removed from continuation
        "field access"
        (where object (cast (h-lookup h loc) C))  ;; retrieves the object at location 'loc'
        (where loc_0 (field-lookup object f C))   ;; retrieves the location of the field 'f'
        (where v (h-lookup h loc_0))) ; --------- ;; retrieves the value stored at the field's location

   
    ; method invocation (reducing expressions inside the method before we can actually reduce the method itself)
   (--> (μ h η (e_0 @ m (e_1 ...)) k)
        (μ h η e_0 (* @ m (e_1 ...) -> k))  ;;object that the method is being called on becomes the expr to be evaluated
        "method invocation - object eval")

   (--> (μ h η v (* @ m (e_0 e_1 ...) -> k))
        (μ h η e_0 (v @ m () * (e_1 ...) -> k))  ;;the first argument becomes the expr to be evaluated
        "method invocation - arg0 eval")

   (--> (μ h η v_i (v_o @ m (v_a ...) * (e_0 e_1 ...) -> k))
        (μ h η e_0 (v_o @ m (v_a ... v_i) * (e_1 ...) -> k))  ;; the next not-evaluated argument becomes the expr to be evaluated
        "method invocation - argi eval")
   
   (--> (μ h η v_o (* @ m () -> k))   ;; takes the method call with all the e's evaluated...
        (μ h η (raw v_o @ m ()) k)    ;; ...and sets the next expr to be evaluated to a 'raw' method invocation. Also removes the method call from continuation 'k'
        "method invocation - no args")

   (--> (μ h η v_1 (v_o @ m (v_0 ...) * () -> k))  
        (μ h η (raw v_o @ m (v_0 ... v_1)) k)
        "method invocation - args")  ;; same as above, but with arguments 

   ; raw method invocation  (the actual evaluation of the method)
   (--> (μ h η (raw (addr loc C) @ m (v_x ...)) k)  ;; takes a state where the next expression is a raw method invocation...
        (μ h_0 η_0 e_m (pop η k)) ; --------------- ;; ...and 
        "raw method invocation"
        (where (C_0 C_p ...) (class-list-from-object (h-lookup h loc))) ; --------- ;;
        (where (any_0 ... (C_t (x_m ...) e_m) error ...)  ; ----------------------- ;;
               ((method-lookup (class-lookup μ C_p) m) ...))  ; ------------------- ;;
        (where (loc_o loc_x ...) (h-malloc-n h ,(add1 (length (term (v_x ...))))))  ;;
        (where h_0 (h-extend* h [loc_o -> (addr loc C_t)] [loc_x -> v_x] ...)) ; -- ;;
        (where η_0 (η-extend* η [this -> loc_o] [x_m -> loc_x] ...)))  ; ---------- ;;


   ; equals '==' 
   (--> (μ h η (e_0 == e) k)       ;; in preparation
        (μ h η e_0 (* == e -> k))  ;; makes LHS the expr to be reduced, updates k
        "equals - l-operand eval")
   
   (--> (μ h η v (* == e -> k))    ;; in preparation
        (μ h η e (v == * -> k))    ;; makes RHS the expr to be reduced, updates k
        "equals - r-operand eval")

   (--> (μ h η v_0 (v_1 == * -> k))  ;; actually calculates the equality
        (μ h η v_res k)
        "equals"
        (where v_res ,(->bool (equal? (term v_0) (term v_1)))))  ;;finds the answer using Racket equal?, converts to Javalite bool

   

   ;FOR TYPES
      ; typecast
   (--> (μ h η (C e) k)
        (μ h η e (C * -> k))
        "typecast - object eval")
   (--> (μ h η (addr loc C_0) (C_1 * -> k))
        (μ h η (addr loc C_1) k)
        "typecast"
        (where object (h-lookup h loc))
        (side-condition (cast? (term object) (term C_1))))

   ;FOR TYPES
   ; instanceof
   (--> (μ h η (e instanceof C) k)
        (μ h η e (* instanceof C -> k))
        "instanceof - object eval")
   (--> (μ h η (addr loc C_0) (* instanceof C_1 -> k))
        (μ h η v_res k)
        "instanceof"
        (where object (h-lookup h loc))
        (where v_res ,(cast?/->bool (term object) (term C_1))))


   
   ; assign to variable
   (--> (μ h η (x := e) k)  ; --- ;; in preparation
        (μ h η e (x := * -> k))   ;; sets the e in x := e as the expr to be evaluated 
        "assign -- object eval")  

   (--> (μ h η v (x := * -> k))  ;; 
        (μ h_0 η v k)  ; ------  ;; alters the value that x points to in the heap:
        "assign"
        (where loc (η-lookup η x))  ; -------- ;; finds the location number in the heap of the local variable x
        (where h_0 (h-extend* h [loc -> v])))  ;; sets the value at 'loc' in the heap to the new value
   

   ; assign Field
   (--> (μ h η (x $ f := e) k)
        (μ h η e (x $ f := * -> k))
        "assign field -- object eval")
   
   (--> (μ h η v (x $ f := * -> k))
        (μ h_0 η v k)
        "assign field"
        (where loc_0 (η-lookup η x))
        (where (addr loc_1 C) (h-lookup h loc_0))
        (where object (cast (h-lookup h loc_1) C))
        (where loc_2 (field-lookup object f C))
        (where h_0 (h-extend* h [loc_2 -> v])))
   
; -----------------------------------------------------
; ------------------ HELPER FUNCTIONS -----------------
; -----------------------------------------------------
; copied from metafunctions.rkt

(define-metafunction JL-Machine
  get-length : (any ...) -> number  ;; finds the length of the field list of a class
  [(get-length (any_0 ...))
   ,(length (term (any_0 ...)))])
  
(define-metafunction JL-Machine
  default-value : T -> v     ;; generates the default value corresponding to the given type
  [(default-value bool)
   false]
  [(default-value unit)
   unit]
  [(default-value C)
   null])

(define-metafunction JL-Machine
  default-value* : (T ...) -> (v ...)  ;; generates list of default values (v ...) corresponding to the given list of types (T ...)
  [(default-value* ())
   ()]
  [(default-value* (T_0 T_1 ...))
   ((default-value T_0) (default-value T_1) ...)])
  
(define-metafunction JL-Machine
  h-max : h -> number
  [(h-max mt) -1]
  [(h-max (h [loc -> hv]))
   ,(max (term loc) (term (h-max h)))])

(define-metafunction JL-Machine
  h-malloc : h -> number   ;; allocates a location to an object in the heap
  [(h-malloc h)
   ,(add1 (term (h-max h)))])

(define-metafunction JL-Machine
  h-malloc-n-helper : number number -> (loc ...)
  [(h-malloc-n-helper number_b number_c)
   ,(let ([z (term number_b)]) (build-list (term number_c) (lambda (y) (+ y z))))])

(define-metafunction JL-Machine
  h-malloc-n : h number -> (loc ...)  ;; returns a list of new heap locations for n values
  [(h-malloc-n h number)
   (loc_0 ...)
   (where ((loc_0 ...)) (h-malloc-n* h number))])

(define-metafunction JL-Machine
  internal-h-malloc-n* : number (number ...) -> (number (loc ...) ...)
  [(internal-h-malloc-n* number_b (number_0))
   (number_t (loc_1 ...))
   (where (loc_1 ...) (h-malloc-n-helper number_b number_0))
   (where number_t ,(if (empty? (term (loc_1 ...))) (term number_b) (add1 (apply max (term (loc_1 ...))))))]
  [(internal-h-malloc-n* number_b (number_0 number_1 number_2 ...))
   (number_t (loc_0 ...) (loc_1 ...) ...)
   (where (loc_0 ...) (h-malloc-n-helper number_b number_0))
   (where number_i ,(if (empty? (term (loc_0 ...))) (term number_b) (add1 (apply max (term (loc_0 ...))))))
   (where (number_t (loc_1 ...) ...) (internal-h-malloc-n* number_i (number_1 number_2 ...)))])

(define-metafunction JL-Machine
  h-malloc-n* : h number ... -> ((loc ...) ...)  ;; allocates space in the heap for n objects (in format ((loc ...) ...) to correspond to heirarchy of class fields (([T f] ...) ...)
  [(h-malloc-n* h number_0 ...)
   ((loc_0 ...) ...)
   (where (number (loc_0 ...) ...) (internal-h-malloc-n* (h-malloc h) (number_0 ...)))])

(define-metafunction JL-Machine
  storelike-lookup : any any -> any
  [(storelike-lookup mt any_0)
   ,(error 'storelike-loopup "~e not found in ~e" (term any_0) (term mt))]
  [(storelike-lookup (any_0 [any_t -> any_ans]) any_t)
   any_ans]
  [(storelike-lookup (any_0 [any_k -> any_v]) any_t)
   (storelike-lookup any_0 any_t)
   (side-condition (not (equal? (term any_k) (term any_t))))])

(define (id-<= a b)
  (string<=? (symbol->string a) (symbol->string b)))
(define (storelike-extend <= storelike k hv)
  (match storelike
    ['mt `(mt [,k -> ,hv])]
    [`(,storelike [,ki -> ,hvi])
     (cond
       [(equal? k ki)
        `(,storelike [,ki -> ,hv])]
       [(<= k ki)
        `(,(storelike-extend <= storelike k hv) [,ki -> ,hvi])]
       [else
        `((,storelike [,ki -> ,hvi]) [,k -> ,hv])])]))     
  
(define (storelike-extend* <= storelike extend*)
  (match extend*
    ['() storelike]
    [`([,k -> ,hv] . ,extend*)
     (storelike-extend* <= (storelike-extend <= storelike k hv) extend*)]))

(define-metafunction JL-Machine
  h-lookup : h loc -> hv     ;; lookup the value stored in the location 'loc' in the heap
  [(h-lookup h loc)
   (storelike-lookup h loc)])

(define-metafunction JL-Machine
  h-extend* : h [loc -> hv] ... -> h   ;; adds new location-value pairing to existing heap
  [(h-extend* h [loc -> hv] ...)
   ,(storelike-extend* <= (term h) (term ([loc -> hv] ...)))])

(define-metafunction JL-Machine
  η-lookup : η x -> loc   ;; gets location in heap of the variable locally named x
  [(η-lookup η x)
   (storelike-lookup η x)])

(define-metafunction JL-Machine
  η-extend* : η [x -> loc] ... -> η   ;; extends the local variable list to include x
  [(η-extend* η [x -> loc] ...)
   ,(storelike-extend* id-<= (term η) (term ([x -> loc] ...)))])

(define-metafunction JL-Machine
  restricted-field-lookup : object f -> loc
  [(restricted-field-lookup (
                  (C_0 [f_0 loc_0] ...) ...
                  (C_t [f_t0 loc_t0] ...
                       [f_target loc_target]
                       [f_t1 loc_t1] ...)
                  (C_1 [f_1 loc_1] ...) ...)
                 f_target)
   loc_target
   ;; Allows for redefinition and restricts matching
   ;; to be the most recent definition by current cast.
   (side-condition
    (not (member (term f_target)
                 (term (f_t1 ... f_1 ... ...)))))])

(define-metafunction JL-Machine
  field-lookup : object f C -> loc  ;; finds the location in the heap of field f, a field of 'object'
  [(field-lookup object f_target C)
   (restricted-field-lookup (restrict-object object C) f_target)])

(define-metafunction JL-Machine
  restrict-object : object C -> object
  [(restrict-object (    (C_0 [f_0 loc_0] ...) ...
                         (C_c [f_c loc_c] ...)
                         (C_1 [f_1 loc_1] ...) ...) C)
   (    (C_0 [f_0 loc_0] ...) ...
        (C_c [f_c loc_c] ...))
   (side-condition (equal? (term C) (term C_c)))])

(define-metafunction JL-Machine
  class-name : CL -> C
  [(class-name (class C_t extends C ([T f] ...) (M ...)))
   C_t])

(define-metafunction JL-Machine
  parent-name : CL -> C
  [(parent-name (class C extends C_p ([T f] ...) (M ...)))
   C_p])

(define-metafunction JL-Machine
  field-list : CL -> ([T f] ...)
  [(field-list (class C extends C_p ([T f] ...) (M ...)))
   ([T f] ...)])

(define-metafunction JL-Machine
  class-list-extend : (C ...) C -> (C ...)
  [(class-list-extend (C_0 ...) C_1)
   (C_0 ... C_1)])

(define-metafunction JL-Machine
  class-lookup : μ C -> CL   ;; gets a class definition: (CL ::= (class C extends C ([T f] ...) (M ...))) - useful when wanting to get method definitions from a class
  [(class-lookup (CL_0 ... CL_1 CL_2 ...) C)
   CL_1 
   (side-condition (equal? (term (class-name CL_1)) (term C)))])

(define-metafunction JL-Machine
  class-list-from-object : object -> (C ...)   ;; gets the list of classes (super classes + self class) from a 'raw' object (no e's, all v's)
  [(class-list-from-object ((C_1 [f_1 loc_1] ...) ...)) 
   (C_1 ...)])

(define-metafunction JL-Machine
  class-parents+self : μ C -> (C ...)  ;; generates a list including the super classes of C and the class C itself
  [(class-parents+self μ Object)
   (class-list-extend () Object)]
  ; id retricts out the base case above
  [(class-parents+self μ id)
   (class-list-extend (class-parents+self μ C_p) id)
   (where CL (class-lookup μ id))
   (where C_p (parent-name CL))])

(define-metafunction JL-Machine
  field-lists-extend : (([T f] ...) ...) ([T f] ...) -> (([T f] ...) ...)
  [(field-lists-extend  (([T_0 f_0] ...) ...) ([T_1 f_1] ...))
   (([T_0 f_0] ...) ... ([T_1 f_1] ...))])
  
(define-metafunction JL-Machine
  fields-parents+self : μ C -> (([T f] ...) ...)  ;; returns a ([T f] ...) for every class included in list of super classes and self
  [(fields-parents+self μ Object)
   (field-lists-extend () ())]
  ; id restricts out the base case above
  [(fields-parents+self μ id)
   (field-lists-extend (fields-parents+self μ C_p) ([T f] ...))
   (where CL (class-lookup μ id))
   (where C_p (parent-name CL))
   (where ([T f] ...) (field-list CL))])

(define-metafunction JL-Machine
  method-name : M -> m
  [(method-name (T_0 m ([T_1 x] ...) e))
   m])

(define-metafunction JL-Machine
  method-expression : M -> e
  [(method-expression (T_0 m ([T_1 x] ...) e))
   e])

(define-metafunction JL-Machine
  method-args : M -> (x ...)
  [(method-args (T_0 m ([T_1 x] ...) e))
   (x ...)])

(define-metafunction JL-Machine
  method-lookup : CL m -> any    ;; gets the specification of the method identified as 'm' from the class description 'CL'
  [(method-lookup (class C_0 extends C_1 ([T x] ...) (M_0 ... M_t M_1 ...)) m)
   (C_0 (method-args M_t) (method-expression M_t))
   (side-condition (equal? (term (method-name M_t)) (term m)))]
  [(method-lookup (class C_0 extends C_1 ([T x] ...) (M ...)) m)
   error
   (side-condition (equal? (findf (λ (i) (equal? (term (method-name ,i)) (term m)))
                                   (term (M ...))) #f))])

(define (->bool v)
    (if v
        'true
        'false))

(define-metafunction JL-Machine
  cast : object C -> object   ;; cast to a new type ;FOR TYPES
  [(cast (    (C_2 [f_2 loc_2] ...) ... 
              (C_3 [f_3 loc_3] ...) 
              (C_4 [f_4 loc_4] ...) ...) C_3)
   (    (C_2 [f_2 loc_2] ...) ... 
        (C_3 [f_3 loc_3] ...) 
        (C_4 [f_4 loc_4] ...) ...)])

(define (cast? object C_t)  ;; checks if a cast can be done to that type with that class, returns a Racket bool ;FOR TYPES
  (define inner-cast?
    (term-match/single
     JL-Machine
     [((C_2 [f_2 loc_2] ...) ...)
      (term (C_2 ...))]))
  (if (member C_t (inner-cast? object)) #t #f))

(define (cast?/->bool object C_t)   ;; checks if the object can be cast to that type, returns a JL-Machine bool  ;FOR TYPES 
  (->bool (cast? object C_t)))

(define-metafunction JL-Machine
     instanceof* : object C -> v
     [(instanceof* ((C_2 [f_2 loc_2] ...) ...) C_t)
      ,(->bool (member (term C_t) (term (C_2 ...))))])

(define-metafunction JL-Machine
  inject : P -> state
  [(inject (μ (C m)))
   (μ mt mt ((new C) @ m ()) ret)])

(define-metafunction JL-Machine
  inject/with-state : state m -> state
  [(inject/with-state (μ h η e k) m)
   (μ h η (e @ m ()) ret)])
