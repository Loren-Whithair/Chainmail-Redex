#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-stack)

(define (test-stack)

  (display "-------------------------------------")
  (display "\nRunning Stack Tests:\n")

  (define Machine_stack? (redex-match? Loo-Machine ψ))

  (define true_stacks (list

                       ;; simple stack (empty Frame, empty η)
                       (term (() mt))

                       ;; chaining simple stacks
                       (term ((() mt) · (() mt)))
                       (term ((() mt) · ((() mt) · (() mt))))

                       ;; with frames containing Stmts
                       (term (((x1 @ f1 := x2) mt) · (() mt)))
                       (term ((x := * $ ()) mt))
                       (term (((x := * $ ()) mt) · ((x := * $ ()) mt)))

                       ;; with frames containing non-empty η
                       (term ((() (mt [x1 -> 1])) · (() mt)))
                       (term ((() (mt [x1 -> 1])) · (() (mt [myVar -> true]))))
                       
                       ;; with frames containing both
                       (term (((x1 @ f1 := x2) (mt [x1 -> 1])) · (() mt)))
                       (term ((() (mt [x1 -> 1])) · ((x1 @ f1 := x2) (mt [myVar -> true]))))

                       ;; note that the bracketing for Stacks is different to other elements such as fieldMap, χ, η etc. 
                       (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt))))
                       (term (((x := * $ ()) mt) · (((x := * $ ()) mt) · (((x := * $ ()) mt) · ((x := * $ ()) mt)))))

                       (term ((() ((mt [x1 -> 10]) [x2 -> 20])) · ((x := * $ (x2 := x4 @ mtd())) ((mt [x1 -> 10]) [x2 -> 20]))))

                       ))

  (define false_stacks (list
                        (term (() (mt))) ; --------- ;; brackets around mt
                        (term ((() (mt)) · (() mt))) ;; brackets around mt in top frame
                        (term ((() mt) · (() (mt)))) ;; brackets around mt in second frame
                        
                        (term ((((x1 @ f1 := x2) $ (x := * $ ())) mt) · (() mt))) ; ----------- ;; brackets
                        (term ((((x := * $ ()) mt) · ((x := * $ ()) mt)) · ((x := * $ ()) mt))) ;; brackets
                        ))

  (for ([stacks true_stacks])
    (test-equal (Machine_stack? stacks) #true))

  (for ([stacks false_stacks])
    (test-equal (Machine_stack? stacks) #false))

  (test-results)

  (display "-------------------------------------")
  )