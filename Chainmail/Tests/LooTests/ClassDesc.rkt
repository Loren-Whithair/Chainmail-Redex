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
                      
                      (term (clss C() { (fld f) }))
                      (term (clss C() { (fld f_1) (fld f_2) }))
                      
                      (term (clss C() { (constructor() { () }) }))
                      (term (clss C() { (constructor(arg1) { () }) }))
                      (term (clss C() { (constructor(arg1 arg2) { () }) }))
                      
                      (term (clss C() { (method m() { () }) }))
                      (term (clss C() { (method m(arg1) { () }) }))
                      (term (clss C() { (method m(arg1 arg2) { () }) }))
                      
                      (term (clss C() { (ghost gf1(x y) { x }) }))

                      (term (clss C() { (fld f) (constructor() { () }) }))
                      (term (clss C() { (fld f) (method m() { () }) }))
                      (term (clss C() { (fld f) (ghost f(x y) { x }) }))
                      (term (clss C() { (fld f) (constructor() { () }) (method m() { () }) }))

                      (term (clss C() { (constructor() { () }) (method m() { () }) }))
                      (term (clss C() { (constructor() { () }) (ghost f(x y) { x }) }))
                      (term (clss C() { (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))

                      (term (clss C() { (fld f) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C() { (fld f_1) (fld f_2) (constructor() { () }) (method m() { () }) (ghost f(x y) { x }) }))

                      (term (clss C(arg1 arg2) { (fld f_1) (fld f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) }))
                      (term (clss C(arg1 arg2) { (fld f_1) (fld f_2) (constructor(arg1 arg2) { () }) (method m(arg1 arg2) { () }) (ghost f(x y) { x }) }))
                      ))

  (define false_Class (list
                       (term (clss C() { (constructor1() { () }) (constructor2() { () })}))  ; ----------------------- ;; two invalid constructors
                       (term (clss C() { (constructor() { () }) (fld f)  (method m() { () }) (ghost gf(x y) { x }) })) ;; constructor and fields in wrong order
                       (term (clss C() { (fld f) (method m() { () }) (constructor() { () }) (ghost gf(x y) { x }) }))  ;; constructor and methods in wrong order 
                       (term (clss C() { (fld f) (constructor() { () }) (ghost f(x y) { x }) (method m() { () }) }))   ;; ghost fields and methods in wrong order
                       ))
    

  (for ([class_descriptions true_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #true))
  
  (for ([class_descriptions false_Class])
    (test-equal (Loo_ClassDesc? class_descriptions) #false))

  (test-results)
  (display "-------------------------------------")
  )