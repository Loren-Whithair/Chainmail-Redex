#lang racket
(require redex)
(require "../../Loo.rkt")

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