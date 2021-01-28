#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-GhostFields)

(define (test-GhostFields)
  (display "-------------------------------------")
  (display "\nRunning Ghost Field Tests:\n")

  (define Loo_GhostDecl? (redex-match? Loo GhostDecl))

  (define true_Ghosts (list
                       (term (ghost gf(x y) { x }))
                       (term (ghost gf_1(x_1 x_2 x_3) { true}))
                       (term (ghost gf_1() {x_1}))
                       (term (ghost x_1() {x_2})))
    )

  (define false_Ghosts (list
                        (term (ghost gf_1(true) {x_1}))
                        (term (ghost1 gf_1(x_1) {x_1})))
    )

  (for ([ghost_declarations true_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #true))
  
  (for ([ghost_declarations false_Ghosts])
    (test-equal (Loo_GhostDecl? ghost_declarations) #false))

  (test-results)
  (display "-------------------------------------")
  )