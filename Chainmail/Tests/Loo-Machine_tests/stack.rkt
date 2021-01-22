#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-stack)

(define (test-stack)

  (display "-------------------------------------")
  (display "\nRunning Stack Tests:\n")

  (define Machine_stack? (redex-match? Loo-Machine ψ))

  (define true_stacks (list
                       (term (() mt))
                       (term ((() mt) · (() mt)))
                       (term ((() mt) · ((() mt) · (() mt))))
                     
                       (term (((x1 @ f1 := x2) mt) · (() mt)))
                       (term ((x := * $ ()) mt))
                       (term (((x := * $ ()) mt) · ((x := * $ ()) mt)))
                       ;; shows the importance of correctly bracketing- note that the bracketing is opposite to usual because we need the new frame to be at the head of the stack
                       (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt))))
                       (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt)))))
                       (term ((() ((mt [x1 -> 10]) [x2 -> 20])) · ((x := * $ (x2 := x4 @ mtd())) ((mt [x1 -> 10]) [x2 -> 20]))))
                       ))

  (define false_stacks (list
                        (term (() (mt)))
                        (term ((() (mt)) · (() mt)))
                        (term ((() mt) · (() (mt))))
                        (term ((((x1 @ f1 := x2) $ (x := * $ ())) mt) · (() mt)))
                        (term ((((x := * $ ()) mt) · ((x := * $ ()) mt)) · ((x := * $ ()) mt))) ;; bracketing like we usually would fails
                        ))

  (for ([stacks true_stacks])
    (test-equal (Machine_stack? stacks) #true))

  (for ([stacks false_stacks])
    (test-equal (Machine_stack? stacks) #false))

  (test-results)

  (display "-------------------------------------")
  )