#lang racket
(require redex)


(define-language Javalite
  (P ::= (μ (C m)))
  (μ ::= (CL ...)) ;; class list
  (T ::=  ;; types
     bool
     unit
     C)
  (CL ::= (class C extends C ([T f] ...) (M ...))) ;; class decl
  (M ::= (T m ([T x] ...) e)) ;; method decl
  (e ::=
     x
     v
     (new C)
     (e $ f) ;; field access
     (e @ m (e ...)) ;; method invocation, with first e as the object the method is being invoked on and (e ...) the args
     (e == e)
     (C e) ;; typecast
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
  (v ::=
     pointer
     true
     false
     unit
     error) ;; variable
  (loc ::= number)) ;; numeric location in heap


(define-extended-language
  JL-Machine Javalite
  
  (e ::= ;; expression, (i.e. control string)
     ....
     (raw v @ m (v ...)))

  (object ::= ((C [f loc] ...) ...)) ;; storage format for Objects in the heap ↓↓↓
  (hv ::= v object)
  
  (h ::=   ;; heap
     mt   ;; empty
     (h [loc -> hv]))  ;; mapping of locations (numbers) to objects

  (η ::=   ;; local environment- mapping from local variables to heap locations
     mt
     (η [x -> loc]))  ;

  (state ::= (μ h η e k)) ;; Javalites operational semantics are based on the concept of evolving program state

  (k ::= ;; continuations
     ret
     (* $ f -> k) ;; reducing the current expression to an object in preparation for a field access before continuing with k
     (* @ m (e ...) -> k) ;; reducing the current expression to the object on which method invocation will be performed
     (v @ m (v ...) * (e ...) -> k) ;; evaluating the current expression as an argument to a method invocation
     (* == e -> k) ;; reducing the left operand of the equality operator to a value
     (v == * -> k) ;; reducing the right operand of the equality operator to a value
     (C * -> k) ;; reducing the current expression to an object for casting to an instance of C
     (* instanceof C -> k) ;; reducing the current expression to an object on which to check membership in the class hierarchy of C
     (x := * -> k) ;; evaluating an expression for assignment to a variable
     (x $ f := * -> k) ;; reducing the expression for assignment to a field
     (if * e else e -> k) ;; reducing the expression to a boolean value which is the predicate of an if statement
     (var T x := * in e -> k) ;; reducing an expression for assignment to a local variable
     (begin * (e ...) -> k) ;; reducing an expression in a list of expressions to be reduced
     (pop η k))) ;; restoring the local environment η before continuing with k
 
