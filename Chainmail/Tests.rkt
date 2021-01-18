#lang racket
(require redex)
(require "Loo.rkt")

; -----------------------------------------------------
; ---------------------- Tests ------------------------
; -----------------------------------------------------

; 
(module+ test
  (test-equal (redex-match? Loo FieldDecl (term (field f))) #true))

(module+ test
  (test-equal (redex-match? Loo CDecl (term (constructor() { () }))) #true) ;; test empty constructor
  (test-equal (redex-match? Loo CDecl (term (constructor(x y) { (y := z @ f) }))) #true))

;; Statement tests
(module+ test
  (test-equal (redex-match? Loo CDecl (term (constructor(x y) { (y := z @ f) }))) #true))
  )

(module+ test
  (test-results))
