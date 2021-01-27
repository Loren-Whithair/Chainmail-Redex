#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-ClassDesc)

(define (test-ClassDesc)
  (display "-------------------------------------")
  (display "\nRunning Class Declarations Tests:\n")

  (define Loo_ClassDesc? (redex-match? Loo ClassDesc))

  (define true_Class (list
                      (term (clss C() {}))
                      (term (clss C() { ('field f) }))
                      (term (clss C() { ('field f_1) ('field f_2) }))
                      (term (clss C() { (constructor() { () }) }))
                      (term (clss C() { (constructor(arg1) { () }) }))
                      (term (clss C() { (constructor(arg1 arg2) { () }) }))
                      (term (clss C() { (method m() { () }) }))
                      (term (clss C() { (method m(arg1) { () }) }))
                      (term (clss C() { (method m(arg1 arg2) { () }) }))
                      (term (clss C() { (ghost f(x y) { x }) }))
                      (term (clss C() { ('field f) (constructor() { () }) }))
                      (term (clss C() { ('field f) (method m() { () }) }))
                      (term (clss C() { ('field f) (ghost f(x y) { x }) }))
                      (term (clss C() { ('field f) (constructor() { () }) (method m() { () }) }))
                      (term (clss C() { (constructor() { () }) (method m() { () }) }))
                      (term (clss C() { (constructor() { () }) (ghost f(x y) { x }) }))
                      (term (clss C() { (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C() { ('field f) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C() { ('field f_1) ('field f_2) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C(arg1 arg2) { ('field f_1) ('field f_2) (constructor(arg1 arg2) { () }) (method m(arg1 arg2) { () }) (ghost f(x y) { x }) }))
                      ))

  (define false_Class (list
                       (term (clss C() { (constructor1() { () }) (constructor2() { () })}))
                       (term (clss C() { (constructor() { () }) ('field f)  (method m() { () }) (ghost f(x y) { x }) }))
                       (term (clss C() { ('field f) (method m() { () }) (constructor() { () }) (ghost f(x y) { x }) }))
                       (term (clss C() { ('field f) (constructor() { () }) (ghost f(x y) { x }) (method m() { () }) }))
                       ))
    

  (for ([class_descriptions true_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #true))
  
  (for ([class_descriptions false_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #false))

  (test-results)
  (display "-------------------------------------")
  )