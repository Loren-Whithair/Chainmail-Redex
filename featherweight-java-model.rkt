#lang racket
(require redex)

; e: expression
; L: class declaration
;    (class [c_name] extends [c_super] {(list of field decls) // [Constructor] [Method decl...])

; K: constructor declaration
;    ([c_name] (([type] [field-name]) 
;    
; M: method declaration
; C: class name
; x: variable
; f: field
; 


;semi-colons are replaced with // and extends is replaced with 'extends (it's a keyword)

(define-language FJ
  (e ::=
     x
     (e · f)
     (e · m(e ...)) ;method
     ('new C(e ...))
     ((C) e))
  (L ::= (class C 'extends C {(C f) ..._1 (C ((C f) ..._1) {super (f ...) // (this · f = f) ...}) M ...})) 
;  (L ::= (class C 'extends C {(C f) ..._1 K M ...}))
  (K ::= (C ((C f) ..._1) {super (f ...) // (this · f = f) ...}))   ;NB: subbed into L in order to restrict the number of params to the number of fields
  (M ::= (C m ((C x) ...) {return e}))

  (f ::= variable-not-otherwise-mentioned) ; fieldId
  (x ::= variable-not-otherwise-mentioned
     this) ; varId
  (C ::= variable-not-otherwise-mentioned) ; classId
  (m ::= variable-not-otherwise-mentioned))



; S: class suite. All of the class definitions
; P: full program that consists of class defn and ONE expression to be evaluated

(define-extended-language
  FJProg   FJ
  (S ::=
     none
     (L S)) 
  (P ::= (S 'MAIN{e})) ;TODO: change the e to an E, define it with a hole probably?  

(define-judgment-form
  FJProg
  #:mode (
  



  
;(define eval
 ; (reduction-relation
  ; FJProg
   ;#:domain P?
   
   

; Problems to solve:
; - how to restrict the NAME of the constructor (i.e. we want the C in K to be the same as the class we are defining
; - - judgment declaration, or reduction?
; - how to get the information of the class defns and use it to reduce E 
; - restrict the number of constructor params to the number of fields in the class (this requires subbing K for it's decl in the decl of L)


; TODO
; define a program execution (define-extended-language prog FJ)
; give a judgment-form for subtypes





(define e? (redex-match FJ e))
(define L? (redex-match FJ L))
(define K? (redex-match FJ K))
(define M? (redex-match FJ M))
