#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-constructor-lookup) ;; for use by the test helper

(define (test-constructor-lookup)

  (display "-------------------------------------")
  (display "\nRunning cosntructor-lookup Tests:\n")

  (test-equal (term (mf-apply constructor-lookup (clss C() {})));; class declaration
              (term (constructor() { (return this) }))) ;; we expect the default constructor if none is specified

  (test-equal (term (mf-apply constructor-lookup (clss C(arg1 arg2) { (fld f_1) (fld f_2) (method m(arg1 arg2) { () }) (ghost f(x y) { x }) }))) ;; class declaration
              (term (constructor() { (return this) }))) ;; we expect the default constructor if none is specified

  ; next we test constructors with different kinds of statements
  
  (test-equal (term (mf-apply constructor-lookup (clss C() {(constructor(x_1 x_2) { (x1 := y1 @ f) })}))) ;; class declaration
              (term (constructor(x_1 x_2) { (x1 := y1 @ f) }))) ;; returns the defined constructor as expected

  (test-equal (term (mf-apply constructor-lookup (clss C() { (fld f) (constructor() { (x1 @ f1 := y1) }) (method m() { () }) }))) ;; class declaration
              (term (constructor() { (x1 @ f1 := y1) }))) ;; returns the defined constructor as expected
  
  (test-equal (term (mf-apply constructor-lookup (clss C() { (fld f) (constructor() { (x1 := y1 @ f1) }) (method m() { () }) }))) ;; class declaration
              (term (constructor() { (x1 := y1 @ f1) }))) ;; returns the defined constructor as expected

  (test-equal (term (mf-apply constructor-lookup  (clss C(arg1 arg2) { (fld f_1) (fld f_2) (constructor(arg1 arg2) { () }) (method m() { () }) (ghost f(x y) { x }) }))) ;; class declaration
              (term (constructor(arg1 arg2) { () }))) ;; returns the defined constructor as expected

  (test-results)

  (display "-------------------------------------")
  )
