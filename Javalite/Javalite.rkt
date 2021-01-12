#lang racket
(require redex)


(define-language Javalite
  (P ::= (μ (C m)))
  (μ ::= (CL ...))   ;class list
  (T ::=  ;types
     bool
     unit
     C)
  (CL ::= (class C extends C ([T f] ...) (M ...)))  ;class decl
  (M ::= (T m ([T x] ...) e))   ;method decl
  (e ::=
     x
     v
     (new C)
     (e $ f)
     (e @ m (e ...))  ;method invoc, with first e as the object the method is being invoked on and (e ...) the args
     (e == e)
     (C e)  ;typecast
     (e instanceof C)
     (x := e)
     (x $ f := e)  ;field setting
     (if e e else e)
     (var T x := e in e)  ;var decl, where x is set to the first e, and the second e is the scope in which the var is used
     (begin e ...))  ;a block of code
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
     error)  ;variable
  (loc ::= number))   ;numeric location in heap


(define-extended-language
  JL-Machine Javalite
  
  (e ::=  ;expression, (i.e. control string)
     ....
     (raw v @ m (v ...)))   ; a method call once all of the expressions (both the subject and the args) have been swapped out for their evaluated values from the heap

  (object ::= ((C [f loc] ...) ...)) ; a list of classes (super + self), tupled, with their list of field names and the locations of the fields in the heap
  ;this ↑↑↑ is the  storage format for Objects in the heap ↓↓↓
  (hv ::= v object)
  
  (h ::=   ;heap
     mt   ;empty
     (h [loc -> hv]))  ;mapping of locations (numbers) to objects

  (η ::=   ;local env
     mt
     (η [x -> loc]))  ;

  (state ::= (μ h η e k))

  (k ::=
     ret
     (* $ f -> k)
     (* @ m (e ...) -> k)
     (v @ m (v ...) * (e ...) -> k)
     (* == e -> k)
     (v == * -> k)
     (C * -> k)
     (* instanceof C -> k)
     (x := * -> k)
     (x $ f := * -> k)
     (if * e else e -> k)
     (var T x := * in e -> k)
     (begin * (e ...) -> k)
     (pop η k)))