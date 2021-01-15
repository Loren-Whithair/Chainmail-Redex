#lang racket
(require redex)

; -----------------------------------------------------
; -------------------- SYNTAX -------------------------
; -----------------------------------------------------

(define-language Javalite
  (P ::= (μ (C m))) ;; program
  (μ ::= (CL ...)) ;; class list
  (T ::= ;; types
     bool
     unit ;; equivalent to Java's void type
     C) ;; class type

  (CL ::= (class C extends C ([T f] ...) (M ...))) ;; class declaration
  (M ::= (T m ([T x] ...) e)) ;; method declaration
  (e ::=
     x
     v
     (new C) ;; creation of an object of type C

     (e $ f) ;; field access
     (e @ m (e ...)) ;; method invocation, with first e as the object the method is being invoked on and (e ...) the args
     (e == e)
     (C e) ;; typecast (of expression e to type C)
     (e instanceof C)
     (x := e)
     (x $ f := e) ;; field setting
     (if e e else e)
     (var T x := e in e) ;; var declaration, where x is set to the first e, and the second e is the scope in which the var is used
     (begin e ...)) ;; a block of code
  (x ::= this id)
  (f ::= id)
  (m ::= id)
  (C ::= Object id)
  (id ::= variable-not-otherwise-mentioned)
  (pointer ::= (addr loc C) null)
  (v ::= ;; kinds of variables
     pointer
     true
     false
     unit ;; used as a method return type, for examples
     error)
  (loc ::= number)) ;; numeric location in heap


; -----------------------------------------------------
; ---------------- MACHINE SYNTAX ---------------------
; -----------------------------------------------------

(define-extended-language
  JL-Machine Javalite
  

  (e ::=  ;; expression (i.e. control string)
     ....
     (raw v @ m (v ...))) ;; a method call once all of the expressions (both the subject and the args) have been swapped out for their evaluated values from the heap

  
  (object ::= ((C [f loc] ...) ...)) ;; a list of classes (super + self), tupled, with their list of field names and the locations of the fields in the heap


  (hv ::= v object) ;; objects that can be stored in the heap
  
  (h ::= ;; heap
     mt ;; empty
     (h [loc -> hv])) ;; mapping of locations (numbers) to objects

  (η ::= ;; local environment
     mt
     (η [x -> loc])) ;; mapping from local variable values to heap locations

  (state ::= (μ h η e k)) ;; Javalites operational semantics are based on the concept of evolving program state

  (k ::=  ;; continuation- a continuation in a Javalite program state represents the computation to be performed after the control string has been evaluated
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
        (μ h_1 η (addr loc_1 C) k) ;; new heap, (new C) becomes a pointer to the created obj
        "new"
        (where (([T_0 f_0] ...) ...) (fields-parents+self μ C)) ;; list of fields in super classes and this class
        (where (C_0 ...) (class-parents+self μ C))
        (where ((v_0 ...) ...) ((default-value* (T_0 ...)) ...))
        (where (number_0 ...) ((get-length (T_0 ...)) ...)) ;; finds the number of fields for each class...
        (where ((loc_0 ...) ...) (h-malloc-n* h number_0 ...)) ;; allocates heap locations for all of said fields...
        (where object ((C_0 [f_0 loc_0] ...) ...))
        (where h_0 (h-extend* h [loc_0 -> v_0] ... ...))
        (where loc_1 (h-malloc h_0))
        (where h_1 (h-extend* h_0 [loc_1 -> object])))

   
   ; field access
   (--> (μ h η (e $ f) k)   
        (μ h η e (* $ f -> k)) ;; the object the field is being extracted from becomes the expression (in preparation for fetching the field value)
        "field access - object eval")
   
   (--> (μ h η (addr loc C) (* $ f -> k))
        (μ h η v k) ;; the expression goes from pointer to field value, field access command removed from continuation
        "field access"
        (where object (cast (h-lookup h loc) C)) ;; retrieves the object at location 'loc'
        (where loc_0 (field-lookup object f C))  ;; retrieves the location of the field 'f'
        (where v (h-lookup h loc_0)))            ;; retrieves the value stored at the field's location

   
   ; method invocation (reducing expressions inside the method before we can actually reduce the method itself)
   (--> (μ h η (e_0 @ m (e_1 ...)) k)
        (μ h η e_0 (* @ m (e_1 ...) -> k)) ;;object that the method is being called on becomes the expr to be evaluated
        "method invocation - object eval")

   (--> (μ h η v (* @ m (e_0 e_1 ...) -> k))
        (μ h η e_0 (v @ m () * (e_1 ...) -> k)) ;;the first argument becomes the expr to be evaluated
        "method invocation - arg0 eval")

   (--> (μ h η v_i (v_o @ m (v_a ...) * (e_0 e_1 ...) -> k))
        (μ h η e_0 (v_o @ m (v_a ... v_i) * (e_1 ...) -> k))  ;; the next not-evaluated argument becomes the expr to be evaluated
        "method invocation - argi eval")
   
   (--> (μ h η v_o (* @ m () -> k))   ;; takes the method call with all the e's evaluated...
        (μ h η (raw v_o @ m ()) k)    ;; ...and sets the next expr to be evaluated to a 'raw' method invocation. Also removes the method call from continuation 'k'
        "method invocation - no args")

   (--> (μ h η v_1 (v_o @ m (v_0 ...) * () -> k))  
        (μ h η (raw v_o @ m (v_0 ... v_1)) k)
        "method invocation - args") ;; same as above, but with arguments 

   ; raw method invocation  (the actual evaluation of the method)
   (--> (μ h η (raw (addr loc C) @ m (v_x ...)) k)  ;; takes a state where the next expression is a raw method invocation...
        (μ h_0 η_0 e_m (pop η k)) ;; ...and 
        "raw method invocation"
        (where (C_0 C_p ...) (class-list-from-object (h-lookup h loc)))
        (where (any_0 ... (C_t (x_m ...) e_m) error ...)
               ((method-lookup (class-lookup μ C_p) m) ...))
        (where (loc_o loc_x ...) (h-malloc-n h ,(add1 (length (term (v_x ...))))))
        (where h_0 (h-extend* h [loc_o -> (addr loc C_t)] [loc_x -> v_x] ...))
        (where η_0 (η-extend* η [this -> loc_o] [x_m -> loc_x] ...)))


   ; equals '==' 
   (--> (μ h η (e_0 == e) k) ;; in preparation
        (μ h η e_0 (* == e -> k)) ;; makes LHS the expr to be reduced, updates k
        "equals - l-operand eval")
   
   (--> (μ h η v (* == e -> k)) ;; in preparation
        (μ h η e (v == * -> k)) ;; makes RHS the expr to be reduced, updates k
        "equals - r-operand eval")

   (--> (μ h η v_0 (v_1 == * -> k)) ;; actually calculates the equality
        (μ h η v_res k)
        "equals"
        (where v_res ,(->bool (equal? (term v_0) (term v_1))))) ;; finds the answer using Racket equal?, converts to Javalite bool

   

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
   (--> (μ h η (x := e) k) ;; in preparation
        (μ h η e (x := * -> k)) ;; sets the e in x := e as the expr to be evaluated 
        "assign -- object eval")  

   (--> (μ h η v (x := * -> k))
        (μ h_0 η v k) ;; alters the value that x points to in the heap:
        "assign"
        (where loc (η-lookup η x)) ;; finds the location number in the heap of the local variable x
        (where h_0 (h-extend* h [loc -> v]))) ;; sets the value at position 'loc' in the heap to the value being assigned
   

   ; assign Field
   (--> (μ h η (x $ f := e) k) ;; in preparation
        (μ h η e (x $ f := * -> k)) ;; sets the thing being assigned as the expression to be evaluated, updates continuation
        "assign field -- object eval")
   
   (--> (μ h η v (x $ f := * -> k))
        (μ h_0 η v k) ;; alters the value that x $ f points to in the heap:
        "assign field"
        (where loc_0 (η-lookup η x))               ;; finds the location in the heap of the variable x
        (where (addr loc_1 C) (h-lookup h loc_0))  ;; uses the location to get the pointer to x
        (where object (cast (h-lookup h loc_1) C)) ;; uses the pointer to get the object assigned to x
        (where loc_2 (field-lookup object f C))    ;; uses the object to get the field's location 
        (where h_0 (h-extend* h [loc_2 -> v])))    ;; sets the value at position 'loc' in the heap to the field value being assigned


   ; if-then-else
   (--> (μ h η (if e_p e_t else e_f) k)      ;; in preparation
        (μ h η e_p (if * e_t else e_f -> k)) ;; sets the predicate 'e' as the next expression to be evaluated
        "if-then-else -- object eval")
   
   (--> (μ h η v (if * e_t else e_f -> k))
        (μ h η ,(if (equal? (term v) (term true)) (term e_t) (term e_f)) k)
        "if-then-else")  ;; uses Racket's 'if-then-else' function to set either e_t or e_f as the next expression to be evaluated

   
   ; variable declaration
   (--> (μ h η (var T x := e_0 in e_1) k)      ;; in preparation
        (μ h η e_0 (var T x := * in e_1 -> k)) ;; sets the expression we are assigning to the new variable as the expression to next be evaluated
        "variable declaration -- object eval")
   
   (--> (μ h η v (var T x := * in e_1 -> k))
        (μ h_0 η_0 e_1 (pop η k))               ;; updates the heap to contain the new var, the local scope to contain the new var, and the continuation to be prepared to remove the local var
        "variable declaration"
        (where loc_x (h-malloc h))              ;; allocates a new location value for the variable 'x'
        (where h_0 (h-extend* h [loc_x -> v]))  ;; sets the value of said location to the desired value 'v'
        (where η_0 (η-extend* η [x -> loc_x]))) ;; updates the local scope with 'x' having the specified location

   
   ; begin
   (--> (μ h η (begin) k)
        (μ h η unit k)
        "begin -- empty expression list")  ;; an empty set of commands just evaluates to the default 'unit'

   (--> (μ h η (begin e_0 e_1 ...) k)        
        (μ h η e_0 (begin * (e_1 ...) -> k)) ;; sets the first expression in the 'begin' sequence as the expression to be evaluated, updates continuation 
        "begin -- e_0 evaluation")

   (--> (μ h η v (begin * (e_i e_i+1 ...) -> k)) ;; once the previous expression has been reduced to a value 'v'... 
        (μ h η e_i (begin * (e_i+1 ...) -> k))   ;; ... sets the next expression in the 'begin' sequence as the expression to be evaluated, updates continuation
        "begin -- e_i evaluation")

   #;(--> (μ h η v (begin * (e_n) -> k)) ;; once all but the last expression have been reduced to some values...
        (μ h η e_n (begin * () -> k))    ;; ... sets the last expression in the 'begin' sequence as the expression to be evaluated, updates continuation 
        "begin -- e_n evaluation")

   (--> (μ h η v (begin * () -> k)) ;; once all the expressions have been reduced to values (which also means that the heap and local vars have been altered)...
        (μ h η v k)                 ;; ... remove the 'begin' sequence from the continuation 
        "begin -- complete")
   

   ; Pop η (close scope)
   (--> (μ h η v (pop η_0 k)) 
        (μ h η_0 v k) ;; replaces the local scope with the previous one from one level out (meaning the most local variables are removed)
        "pop η")
   ))

; -----------------------------------------------------
; ------------------ HELPER FUNCTIONS -----------------
; -----------------------------------------------------
; copied from metafunctions.rkt

; finds the length of the field list of a class
(define-metafunction JL-Machine
  get-length : (any ...) -> number
  [(get-length (any_0 ...))
   ,(length (term (any_0 ...)))]) ;; evaluates with built in racket function

; generates the default value corresponding to the given type
(define-metafunction JL-Machine
  default-value : T -> v
  [(default-value bool)
   false] ; default value of boolean is false (like java)
  [(default-value unit)
   unit]
  [(default-value C)
   null]) ;; if a value isn't assigned to a class variable, it's default value is null (like java)

; generates list of default values (v ...) corresponding to the given list of types (T ...)
(define-metafunction JL-Machine
  default-value* : (T ...) -> (v ...)
  [(default-value* ())
   ()] ;; empty list default value is the empty list
  [(default-value* (T_0 T_1 ...))
   ((default-value T_0) (default-value T_1) ...)]) ;; otherwise we apply the default-value function defined above to each term in the list

; returns the max location address used in the heap
(define-metafunction JL-Machine
  h-max : h -> number
  [(h-max mt) -1] ;; empty heap returns value of -1
  [(h-max (h [loc -> hv]))
   ,(max (term loc) (term (h-max h)))]) ;; otherwise we use the built in racket function max

; returns a heap address for a new object to be added to the heap
(define-metafunction JL-Machine
  h-malloc : h -> number
  [(h-malloc h)
   ,(add1 (term (h-max h)))]) ;; uses the h-max function defined above to find the max heap value in use, then uses racket's built in add1 function

; helper function for the h-malloc-n function defined below
(define-metafunction JL-Machine
  h-malloc-n-helper : number number -> (loc ...)
  [(h-malloc-n-helper number_b number_c)
   ,(let ([z (term number_b)]) (build-list (term number_c) (lambda (y) (+ y z))))])

; returns a list of new heap locations for n values
(define-metafunction JL-Machine
  h-malloc-n : h number -> (loc ...)
  [(h-malloc-n h number)
   (loc_0 ...)
   (where ((loc_0 ...)) (h-malloc-n* h number))])

; takes in number (corresponding to the next heap value) and an n, converts it into list (of size n) of heap locations
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

; allocates space in the heap for n objects (in format ((loc ...) ...) to correspond to heirarchy of class fields (([T f] ...) ...)
(define-metafunction JL-Machine
  h-malloc-n* : h number ... -> ((loc ...) ...) ;; takes in heap (h) as argument parameter
  [(h-malloc-n* h number_0 ...)
   ((loc_0 ...) ...)
   (where (number (loc_0 ...) ...) (internal-h-malloc-n* (h-malloc h) (number_0 ...)))]) ;; uses h-malloc to get next heap value

; used for looking up something inside something else (very general because of the functions use of 'any')
; ex: if A is a set, (storelike-lookup A x) will test if x ∈ A and return the value associated with x
(define-metafunction JL-Machine
  storelike-lookup : any any -> any
  [(storelike-lookup mt any_0)
   ,(error 'storelike-loopup "~e not found in ~e" (term any_0) (term mt))] ;; unable to find anything in an empty 'any' (for example, an object)
  [(storelike-lookup (any_0 [any_t -> any_ans]) any_t)
   any_ans] ;; if any_t points to any_ans in any_0, we return any_ans
  [(storelike-lookup (any_0 [any_k -> any_v]) any_t)
   (storelike-lookup any_0 any_t)
   (side-condition (not (equal? (term any_k) (term any_t))))]) ;; ensures any_k != any_t (otherwise we would match the previous condition)

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

; extends the storelike
; for example, adding a new variable to the heap or η
(define (storelike-extend* <= storelike extend*)
  (match extend*
    ['() storelike]
    [`([,k -> ,hv] . ,extend*)
     (storelike-extend* <= (storelike-extend <= storelike k hv) extend*)]))

; lookup the value stored in the location 'loc' in the heap
(define-metafunction JL-Machine
  h-lookup : h loc -> hv
  [(h-lookup h loc)
   (storelike-lookup h loc)]) ;; uses the storelike-lookup metafunction

; adds new location-value pairing to existing heap
(define-metafunction JL-Machine
  h-extend* : h [loc -> hv] ... -> h ;; takes in a arbitrary number of mappings
  [(h-extend* h [loc -> hv] ...)
   ,(storelike-extend* <= (term h) (term ([loc -> hv] ...)))])

; gets location in heap of the local variable called x
(define-metafunction JL-Machine
  η-lookup : η x -> loc
  [(η-lookup η x)
   (storelike-lookup η x)]) ;; uses the storelike-lookup metafunction

; extends the local variable list to include x
(define-metafunction JL-Machine
  η-extend* : η [x -> loc] ... -> η
  [(η-extend* η [x -> loc] ...)
   ,(storelike-extend* id-<= (term η) (term ([x -> loc] ...)))])

; finds the location in heap of a field 'f' assocaited with 'object'
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

; finds the location in the heap of field f, a field of 'object' which is of type C
(define-metafunction JL-Machine
  field-lookup : object f C -> loc
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

; returns the name of a class
; ex: (parent-name (class A extends B {...})) will return 'A'
(define-metafunction JL-Machine
  class-name : CL -> C ;; seems to be defined for a class list input but the class list doesn't seem to be used
  [(class-name (class C_t extends C ([T f] ...) (M ...))) ;; just works on the head of the list
   C_t])

; returns the name of the parent of a class (ie: the class the input class extends from)
; (recall- every class extends from something in Javalite so this is a well defined operation)
; ex: (parent-name (class A extends B {...})) will return 'B'
(define-metafunction JL-Machine
  parent-name : CL -> C
  [(parent-name (class C extends C_p ([T f] ...) (M ...)))
   C_p])

; returns a list of fields (and associated types) associated with a class
(define-metafunction JL-Machine
  field-list : CL -> ([T f] ...)
  [(field-list (class C extends C_p ([T f] ...) (M ...)))
   ([T f] ...)])

; extends a list of classes with an additional class
; adds the new class to the end of the list
(define-metafunction JL-Machine
  class-list-extend : (C ...) C -> (C ...)
  [(class-list-extend (C_0 ...) C_1)
   (C_0 ... C_1)])

; gets a class definition: (CL ::= (class C extends C ([T f] ...) (M ...)))
; useful when wanting to get method definitions from a class
(define-metafunction JL-Machine
  class-lookup : μ C -> CL
  [(class-lookup (CL_0 ... CL_1 CL_2 ...) C)
   CL_1 
   (side-condition (equal? (term (class-name CL_1)) (term C)))])

; gets the list of classes (super classes + self class) from a 'raw' object (no e's, all v's)
(define-metafunction JL-Machine
  class-list-from-object : object -> (C ...)
  [(class-list-from-object ((C_1 [f_1 loc_1] ...) ...)) 
   (C_1 ...)])

; generates a list including the super classes of C and the class C itself
(define-metafunction JL-Machine
  class-parents+self : μ C -> (C ...)
  [(class-parents+self μ Object)
   (class-list-extend () Object)]
  [(class-parents+self μ id) ;; id retricts out the base case above
   (class-list-extend (class-parents+self μ C_p) id)
   (where CL (class-lookup μ id))
   (where C_p (parent-name CL))])

; extends a list of fields by appending another list of fields to the end
(define-metafunction JL-Machine
  field-lists-extend : (([T f] ...) ...) ([T f] ...) -> (([T f] ...) ...)
  [(field-lists-extend  (([T_0 f_0] ...) ...) ([T_1 f_1] ...))
   (([T_0 f_0] ...) ... ([T_1 f_1] ...))])

; returns a ([T f] ...) for every class included in list of super classes and self
(define-metafunction JL-Machine
  fields-parents+self : μ C -> (([T f] ...) ...)
  [(fields-parents+self μ Object)
   (field-lists-extend () ())]
  [(fields-parents+self μ id) ;; id restricts out the base case above
   (field-lists-extend (fields-parents+self μ C_p) ([T f] ...))
   (where CL (class-lookup μ id))
   (where C_p (parent-name CL))
   (where ([T f] ...) (field-list CL))])

; takes a method declaration and returns the name of the method
(define-metafunction JL-Machine
  method-name : M -> m
  [(method-name (T_0 m ([T_1 x] ...) e))
   m])

; takes a method declaration and returns the expression of the method
(define-metafunction JL-Machine
  method-expression : M -> e
  [(method-expression (T_0 m ([T_1 x] ...) e))
   e])

; takes a method declaration and returns the arguments of the method
(define-metafunction JL-Machine
  method-args : M -> (x ...)
  [(method-args (T_0 m ([T_1 x] ...) e))
   (x ...)])

; gets the specification of the method identified as 'm' from the class description 'CL'
(define-metafunction JL-Machine
  method-lookup : CL m -> any
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

; cast to a new type ;FOR TYPES
(define-metafunction JL-Machine
  cast : object C -> object
  [(cast (    (C_2 [f_2 loc_2] ...) ... 
              (C_3 [f_3 loc_3] ...) 
              (C_4 [f_4 loc_4] ...) ...) C_3)
   (    (C_2 [f_2 loc_2] ...) ... 
        (C_3 [f_3 loc_3] ...) 
        (C_4 [f_4 loc_4] ...) ...)])

; checks if a cast can be done to that type with that class, returns a Racket bool ;FOR TYPES
(define (cast? object C_t)
  (define inner-cast?
    (term-match/single
     JL-Machine
     [((C_2 [f_2 loc_2] ...) ...)
      (term (C_2 ...))]))
  (if (member C_t (inner-cast? object)) #t #f))

; checks if the object can be cast to that type, returns a JL-Machine bool ;FOR TYPES 
(define (cast?/->bool object C_t)
  (->bool (cast? object C_t)))

; checks if a object of type C is a member of another object
(define-metafunction JL-Machine
     instanceof* : object C -> v
     [(instanceof* ((C_2 [f_2 loc_2] ...) ...) C_t)
      ,(->bool (member (term C_t) (term (C_2 ...))))])

; used for initialising a program
; turns our program P into a state
; program starts from method 'm' in program 'P'
(define-metafunction JL-Machine
  inject : P -> state
  [(inject (μ (C m)))
   (μ mt mt ((new C) @ m ()) ret)])

; used for initialising a program but preserving a pre-existing state
(define-metafunction JL-Machine
  inject/with-state : state m -> state
  [(inject/with-state (μ h η e k) m)
   (μ h η (e @ m ()) ret)])
