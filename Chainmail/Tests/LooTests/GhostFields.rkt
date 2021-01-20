#lang racket
(require redex)
(require "../../Loo.rkt")

; Ghost fields
(display "-------------------------------------")
(display "\nRunning Ghost Field Tests:\n")

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

(test-results)
(display "-------------------------------------")