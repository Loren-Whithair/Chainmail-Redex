#lang racket
(require redex)

(define-language FJ
  (e ::=    
     x
     e.f
     e.m(e ...) ;method
     (new C(e ...))
     ((C)e))
  (L ::= (class C 'extends C {(C f) ... // K M ...})) ;semi-colons are replaced with //   and extends is replaced with 'extends (it's a keyword)
  (K ::= (C ((C f) ...) (super (f_1 ..._1) // (this. f_1 = f_1) ..._1)))
  (M ::= (C m (C x) ... (return e //)))

  (f ::= variable-not-otherwise-mentioned) ; field Identifier
  (x ::= variable-not-otherwise-mentioned) ; variable Identifier
  (C ::= variable-not-otherwise-mentioned)) ;Class Identifier

; TODO
; define a program execution (define-extended-language prog FJ)

; give a judgment-form for subtypes



(define-language testing
  (te :=
      ((f ..._1) (ta. f = f) ..._1))
  (f ::= variable-not-otherwise-mentioned))

