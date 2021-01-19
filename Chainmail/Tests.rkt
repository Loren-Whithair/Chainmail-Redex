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
  (define Loo_Stmts? (redex-match? Loo Stmts)) ;

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
                        (term mt)
                        (term (mt [C1 -> ('class C1() {})]))
                        (term ((mt [C1 -> ('class C1() {})]) [C2 -> ('class C2() {})]))
                        (term (mt [C1 -> ('class C1(x1) {})]))
                        (term (mt [C1 -> ('class C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) })]))
                      ))

  (define false_Modules (list
                       (term (mt))
                       (term ([C1 -> ('class C1() {})]))
                       (term (([C1 -> ('class C1() {})]) [C2 -> ('class C2() {})]))
                       ))

  (for ([Modules true_Modules])
    (test-equal (Loo_M? Modules) #true))
  
  (for ([Modules false_Modules])
    (test-equal (Loo_M? Modules) #false))
  )


; -----------------------------------------------------
; ----------------- Machine Testing -------------------
; -----------------------------------------------------

;Values
(module+ test
  (define Machine_V? (redex-match? Loo-Machine v))

  (define true_values (list
                        (term null)
                        (term 5)
                        (term (2 5 5))
                        ))

  (define false_values (list
                        (term 3.5)
                        (term -2)
                        (term (1 4 6 3.5))
                         ))

  (for ([values true_values])
    (test-equal (Machine_V? values) #true))
  
  (for ([values false_values])
    (test-equal (Machine_V? values) #false))
  )


;Objects
(module+ test
  (define Machine_Object? (redex-match? Loo-Machine Object))

  (define true_Objects (list
                        (term ()) ;; empty machine object
                        (term mt) ;; empty machine object- possibly suggests that mt for machine objects is unneccessary
                        (term ((C [f -> 1])))
                        (term ((C1 [f1 -> 1]) (C2 [f2 -> 2])))
                        (term ((C1 [f1 -> 1]) (C2 [f2 -> 2]) (C3 [f3 -> 3])))
                        (term ((C [f -> 1]) (C [f -> 2]) (C [f -> 1]) (C [f -> 2]))) ;; showing that one machine object can map the same field value of the same class multiple times
                        ))

  (define false_Objects (list
                         (term (C [f -> 1])) ;; showing the importance of correct bracketing
                         (term ((('class C() { ('field f) }) [f -> 5]))) ;; correct bracketing but using ClassDesc instead of class identifier
                         ))

  (for ([Objects true_Objects])
    (test-equal (Machine_Object? Objects) #true))
  
  (for ([Objects false_Objects])
    (test-equal (Machine_Object? Objects) #false))
  )

;Continuation
(module+ test
  (define Machine_Continuation? (redex-match? Loo-Machine Continuation))

  ; because I've appended all true Stmts to the list before testing (see below, I will only add Continuations of the form (x := * $ Stmts) to this list (but both forms will be tested) 
  (define true_Conts (list
                        (term (x := * $ ()))
                        (term (x := * $ (() $ ())))
                        (term (x := * $ (x @ f := y)))
                        (term (x := * $ (z := y @ f)))
                        (term (x := * $ (method_result := x @ m())))
                        (term (x := * $ (method_result := x @ m(arg1))))
                        ))

  (define false_Conts (list
                         ;terms here
                         ))

  ; note: Stmts by themselves are a valid continuation so I'm testing all the true_Stmts here as part of true_Conts and they should pass
  ; also note: it might not be best practise to do it like this in code so we might need to change it
  (for ([conts (append true_Conts true_Stmts)])
    (test-equal (Machine_Continuation? conts) #true))

  ; the same applies to false_Conts with false_Stmts
  (for ([conts (append false_Conts false_Stmts)])
    (test-equal (Machine_Continuation? conts) #false))
  )


;Local vars
(module+ test
  (define Machine_local-var? (redex-match? Loo-Machine η ))

  (define true_locals (list
                        (term mt)
                        (term (mt [x -> 5]))
                        (term ((mt [x -> 5]) [y -> 10])) ;; shows the importance of correct bracketing
                        (term (((mt [x -> 5]) [y -> 10]) [z -> 15])) ;; shows the importance of correct bracketing
                        (term (((mt [x -> 5]) [x -> 10]) [x -> 15])) ;; shows that it's syntacically valid to have multiple variable mappings of the same variable in one local variable list
                        ))

  (define false_locals (list
                        (term (mt [x -> v]))
                        (term (mt [x -> 5] [y -> 10])) ;; shows the importance of correct bracketing
                        (term (mt [x -> 5] [y -> 10] [z -> 15])) ;; shows the importance of correct bracketing
                        ))

  (for ([local-vars true_locals])
    (test-equal (Machine_local-var? local-vars) #true))
  
  (for ([local-vars false_locals])
    (test-equal (Machine_local-var? local-vars) #false))
  )

;Frame
(module+ test
  (define Machine_Frame? (redex-match? Loo-Machine Φ))

  (define true_Frames (list
                        (term ((x := * $ ()) mt))
                        (term ((x := * $ (x @ f := y)) (mt [x -> 5])))
                        (term ((x := * $ (method_result := x @ m(arg1))) ((mt [x -> 5]) [y -> 10])))
                        ))

  (define false_Frames (list
                         ;terms here
                         ))

  (for ([frames true_Frames])
    (test-equal (Machine_Frame? frames) #true))
  
  (for ([frames false_Frames])
    (test-equal (Machine_Frame? frames) #false))
  )

;Stack
(module+ test
  (define Machine_stack? (redex-match? Loo-Machine ψ))

  (define true_stacks (list
                        (term ((x := * $ ()) mt))
                        (term (((x := * $ ()) mt) · ((x := * $ ()) mt)))
                        ;; shows the importance of correctly bracketing- note that the bracketing is opposite to usual because we need the new frame to be at the head of the stack
                        (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt))))
                        (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt)))))
                        ))

  (define false_stacks (list
                        (term ((((x := * $ ()) mt) · ((x := * $ ()) mt)) · ((x := * $ ()) mt))) ;; bracketing like we usually would fails
                         ))

  (for ([stacks true_stacks])
    (test-equal (Machine_stack? stacks) #true))
  
  (for ([stacks false_stacks])
    (test-equal (Machine_stack? stacks) #false))
  )

;Heap
(module+ test
  (define Machine_heap? (redex-match? Loo-Machine χ))

  (define true_heaps (list
                       ;(term ())
                       ;(term ([0 -> mt]))
                       ;(term ([0 -> mt] [1 -> mt]))
                       ;(term ([0 -> mt] [1 -> mt] [2 -> mt]))
                        ))

  (define false_heaps (list
                         ;terms here
                         ))

  (for ([heaps true_heaps])
    (test-equal (Machine_heap? heaps) #true))
  
  (for ([heaps false_heaps])
    (test-equal (Machine_heap? heaps) #false))
  )

;Runtime configs
(module+ test
  (define Machine_runtime-config? (redex-match? Loo-Machine σ))

  (define true_runtime-configs (list
                        (term (((x := * $ ()) mt) ()))
                        (term (((x := * $ ()) mt) ([0 -> mt])))
                        (term (((x := * $ ()) mt) ([0 -> mt] [1 -> mt])))
                        (term (((x := * $ ()) mt) ([0 -> mt] [1 -> mt] [2 -> mt])))
                        ))

  (define false_runtime-configs (list
                         ;terms here
                         ))

  (for ([runtime-configs true_runtime-configs])
    (test-equal (Machine_runtime-config? runtime-configs) #true))
  
  (for ([runtime-configs false_runtime-configs])
    (test-equal (Machine_runtime-config? runtime-configs) #false))
  )

;(term mt)
;(term (mt [C1 -> ('class C1() {})]))
;(term ((mt [C1 -> ('class C1() {})]) [C2 -> ('class C2() {})]))
                        
                        

; States
(module+ test
  (define Machine_state? (redex-match? Loo-Machine state))

  (define true_states (list
                        
                        ))

  (define false_states (list
                         ;terms here
                         ))

  (for ([states true_states])
    (test-equal (Machine_state? states) #true))
  
  (for ([states false_states])
    (test-equal (Machine_state? states) #false))
  )

(module+ test
  (display "Hand written test results:\n")
  (test-results))

; -----------------------------------------------------
; ------------------ Random Testing -------------------
; -----------------------------------------------------

;; random testing of syntax
(module+ test
  (display "\nThen we do random testing of Loo syntax:\n")
  (define syntax_correct? (redex-match Loo language))
  (redex-check Loo language (syntax_correct? (term e)))
  )

;; random testing of reduction rules
;; none will work at this stage because no reduction rules are full defined
(module+ test
  (display "\nThen random testing of Loo reduction rules:\n")
  (define (reduces? e) (not (null? (apply-reduction-relation expr-reductions (term (e))))))
  ;(redex-check Loo-Machine machine-language (reduces? (term e)))
  )