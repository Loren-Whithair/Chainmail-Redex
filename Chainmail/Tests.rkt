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
                          ((term (ghost f(x y) { x }))
                           (term (ghost f_1(x_1 x_2 x_3) { true}))
                           (term (ghost f_1() {x_1}))
                           (term (ghost x_1() {x_2})))
                          ))

  (define false_Ghosts (list
                        ((term (ghost f_1(true) {x_1}))
                         (term (ghost1 f_1(x_1) {x_1})))
                        ))

  (for ([ghost_declarations true_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #true))
  
  (for ([ghost_declarations false_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #false))
  )

; Method Declarations
(module+ test
  (define Loo_MethDecl? (redex-match? Loo MethDecl))

  (define true_Meths (list
                      (;terms here
                      )
                      ))

  (define false_Meths (list
                       (;terms here
                        ))
    )

  (for ([method_declarations true_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #true))
  
  (for ([method_declarations false_Meths])
    (test-equal (Loo_MethDecl? method_declarations) #false))
  )


;Constructor Declarations
(module+ test
  (define Loo_CDecl? (redex-match? Loo CDecl))

  (define true_Const (list
                      (;terms here
                      )
                      ))

  (define false_Const (list
                       (;terms here
                        ))
    )

  (for ([constructor_declarations true_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #true))
  
  (for ([constructor_declarations false_Const])
    (test-equal (Loo_CDecl? constructor_declarations) #false))
  )

 
;Class Description Declarations
(module+ test
  (define Loo_ClassDesc? (redex-match? Loo ClassDesc))

  (define true_Class (list
                      (;terms here
                      )
                      ))

  (define false_Class (list
                       (;terms here
                        ))
    )

  (for ([class_descriptions true_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #true))
  
  (for ([class_descriptions false_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #false))
  )


;Modules
(module+ test
  (define Loo_MethDecl? (redex-match? Loo MethDecl))

  (define true_Meths (list
                      (;terms here
                      )
                      ))

  (define false_Meths (list
                       (;terms here
                        ))
    )

  (for ([method_declarations true_Meths])
    (test-equal (Loo_GhostField? method_declarations) #true))
  
  (for ([method_declarations false_Meths])
    (test-equal (Loo_GhostField? method_declarations) #false))
  )


(module+ test
  (test-results))