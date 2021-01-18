#lang racket
(require redex)
(require "Loo.rkt")

; -----------------------------------------------------
; ---------------------- Tests ------------------------
; -----------------------------------------------------

; ClassDesc
; 

; e (expression) Tests
(module+ test

  (define Loo_expression? (redex-match? Loo e))

  ; true expressions
  (define true_expressions (list
                            (term (x_1 = x_4))
                            (term (true = false))
                            (term ((true = false) = true))
                            (term (if true then true else true))
                            (term  (if x_5 then null else x_3))
                            (term  (if (x_1 = x_2) then (x_3 = (true = null)) else false))
                            (term (x_1 @ fname(true false null)))
                            (term ((true = true) @ fname()))
                            (term (x_1 @ fname ()))
                            (term (true @ fname ()))
                            (term (null @ f((true = true))))
                            ))

  ; false expressions
  (define false_expressions (list
                             (term (true = false = true))
                             (term (5 = 5))
                             (term (if null then (true) else null))
                             (term (if (true) then x_1 else null))
                             (term (null @ f(true = true)))
                             (term (null @ f ((true = true = true))))
                            ))
  
  (for ([expression true_expressions])
    (test-equal (Loo_expression? expression) #true))
  
  (for ([expression false_expressions])
    (test-equal (Loo_expression? expression) #false))
)

; Stmts Tests
(module+ test
  (define Loo_Stmts? (redex-match? Loo Stmts))

   ; true statements
  (define true_Stmts (list
                      (term ()) ;; testing the empty statement
                      (term (() $ ())) ;; testing that we can have multiple statments chained together
                      (term (x @ f := y))
                      (term (z := y @ f))
                      (term (method_result := x @ m()))
                      (term (method_result := x @ m(arg1)))
                      (term (method_result := x @ m(arg1 arg2)))
                      (term (object_result := new C()))
                      (term (object_result := new C(arg1)))
                      (term (object_result := new C(arg1 arg2)))
                      (term (return result))
                      ))

  ; false statements
  (define false_Stmts (list
                     ))

  (for ([statements true_Stmts])
    (test-equal (Loo_Stmts? statements) #true))
  
  (for ([statements false_Stmts])
    (test-equal (Loo_Stmts? statements) #false))
  
  ; the next 3 tests show the importance of correctly bracketing our chained statements
  (test-equal (Loo_Stmts? (term (() $ () $ ()))) #false)
  (test-equal (Loo_Stmts? (term ((() $ ()) $ ()))) #false)
  (test-equal (Loo_Stmts? (term (() $ (() $ ())))) #true)
)

; Ghost fields
(module+ test
  (define Loo_GhostDecl? (redex-match? Loo GhostDecl))

  (define true_Ghosts (list
                          (term (ghost f(x y) { x }))
                          (term (ghost f_1(x_1 x_2 x_3) { true}))
                          (term (ghost f_1() {x_1}))
                          (term (ghost x_1() {x_2})))
                          )

  (define false_Ghosts (list
                        (term (ghost f_1(true) {x_1}))
                        (term (ghost1 f_1(x_1) {x_1})))
                        )

  (for ([ghost_declarations true_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #true))
  
  (for ([ghost_declarations false_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #false))
  )

; Method Declarations
(module+ test
  (define Loo_MethDecl? (redex-match? Loo MethDecl))

  (define true_Meths (list
                      (term (method m() {()}))
                      (term (method m(arg1) {()}))
                      (term (method m(arg1 arg2) {()}))
                      ))

  (define false_Meths (list
                       (term (method_1 m(arg1 arg2) {()}))
                       (term (method m(1 2) {()}))
                       ))

  (for ([method_declarations true_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #true))
  
  (for ([method_declarations false_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #false))
  )


; Constructor Declarations
(module+ test
  (define Loo_CDecl? (redex-match? Loo CDecl))

  (define true_Const (list
                      (term (constructor() {()}))
                      (term (constructor(arg1) {()}))
                      (term (constructor(arg1 arg2) {()}))
                      ))

  (define false_Const (list
                       (term (constructor_1(arg1 arg2) {()}))
                       (term (constructor(1 2) {()}))
                       ))
    

  (for ([constructor_declarations true_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #true))
  
  (for ([constructor_declarations false_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #false))
  )

 
;Class Description Declarations
(module+ test
  (define Loo_ClassDesc? (redex-match? Loo ClassDesc))

  (define true_Class (list
                      (term ('class C() {}))
                      (term ('class C() { ('field f) }))
                      (term ('class C() { ('field f_1) ('field f_2) }))
                      (term ('class C() { (constructor() { () }) }))
                      (term ('class C() { (constructor(arg1) { () }) }))
                      (term ('class C() { (constructor(arg1 arg2) { () }) }))
                      (term ('class C() { (method m() { () }) }))
                      (term ('class C() { (method m(arg1) { () }) }))
                      (term ('class C() { (method m(arg1 arg2) { () }) }))
                      (term ('class C() { (ghost f(x y) { x }) }))
                      (term ('class C() { ('field f) (constructor() { () }) }))
                      (term ('class C() { ('field f) (method m() { () }) }))
                      (term ('class C() { ('field f) (ghost f(x y) { x }) }))
                      (term ('class C() { ('field f) (constructor() { () }) (method m() { () }) }))
                      (term ('class C() { (constructor() { () }) (method m() { () }) }))
                      (term ('class C() { (constructor() { () }) (ghost f(x y) { x }) }))
                      (term ('class C() { (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term ('class C() { ('field f) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term ('class C() { ('field f_1) ('field f_2) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term ('class C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term ('class C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m(arg1 arg2) { () }) (ghost f(x y) { x }) }))
                      ))

  (define false_Class (list
                       (term ('class C() { (constructor1() { () }) (constructor2() { () })}))
                       (term ('class C() { (constructor() { () }) ('field f)  (method m() { () }) (ghost f(x y) { x }) }))
                       (term ('class C() { ('field f) (method m() { () }) (constructor() { () }) (ghost f(x y) { x }) }))
                       (term ('class C() { ('field f) (constructor() { () }) (ghost f(x y) { x }) (method m() { () }) }))
                        ))
    

  (for ([class_descriptions true_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #true))
  
  (for ([class_descriptions false_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #false))
  )

;Modules
(module+ test
  (define Loo_M? (redex-match? Loo M))

  (define true_Modules (list

                      ))

  (define false_Modules (list
                       
                       ))

  (for ([Modules true_Modules])
    (test-equal (Loo_M? Modules) #true))
  
  (for ([Modules false_Modules])
    (test-equal (Loo_M? Modules) #false))
  )



; -----------------------------------------------------
; ------------------ Random Testing -------------------
; -----------------------------------------------------

(module+ test
  ;(define syntax_correct? (redex-match Loo language))
  ;(redex-check Loo language (syntax_correct? (term e)))
  )


(module+ test
  (test-results))