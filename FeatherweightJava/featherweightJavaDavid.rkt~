#lang racket
(require redex)

; e: expression
; L: class declaration
;    (class [c_name] extends [c_super] {(list of field decls) // [Constructor] [Method decl...])

; K: constructor declaration
;    ([c_name] (([type] [field-name])


; x: variable
; f: field
; M: method declaration
; C: class name

;semi-colons are replaced with // and extends is replaced with 'extends (it's a keyword)

(define-language FJ
  (e (e @ f) ;; field access
     (e @ m(e ...)) ;; method access
     (new C(e ...)) ;; new class
     ((C) e)
     x)
  (L (class C 'extends C {(C f) ..._1 (C ((C f) ..._1) {super (f ...) \\ (this @ f = f) ...}) M ...}))
  ;(L (class C 'extends C {(C f) ..._1 K M...}))
  (K (C ((C f) ..._1) {super (f ...) // (this @ f = f) ...}))
  (M (C m ((C x) ...) {return e}))
  
  (x variable-not-otherwise-mentioned this)    ;; varID
  (f variable-not-otherwise-mentioned)         ;; fieldID
  (m variable-not-otherwise-mentioned)         ;; methodID
  (C variable-not-otherwise-mentioned))        ;; classID







