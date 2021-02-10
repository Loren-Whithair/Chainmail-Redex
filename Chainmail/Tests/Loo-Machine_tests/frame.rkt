#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-frame)

(define (test-frame)

  (display "-------------------------------------")
  (display "\nRunning Frame Tests:\n")

  (define Machine_Frame? (redex-match? Loo-Machine Φ))

  (define true_Frames (list

                       ;; simple frame (empty continuation, empty η)
                       (term (() mt))

                       ;; with non-empty continuation
                       (term ((x := * $ ()) mt))
                       (term ((x1 @ f1 := x2) mt))  ;; First Stmt doesn't have to be a hole

                       ;; with non-empty η
                       (term (() ((mt [x1 -> 10]) [x2 -> 20])))

                       ;; with both non-empty
                       (term ((x := * $ (x @ f := y)) (mt [x -> 5])))                       
                       (term ((x := * $ (method_result := x @ m(arg1))) ((mt [x -> 5]) [y -> 10])))
                       (term ((x := * $ (x2 := x4 @ mtd())) ((mt [x1 -> 10]) [x2 -> 20]))) 
                       ))

  (define false_Frames (list
                        (term (() (mt)))  ; ------------------------- ;; brackets around mt
                        (term (() (mt [x1 -> 10] [x2 -> 20])))  ; --- ;; invalid local var map
                        (term (((x1 @ f1 := x2) $ (x := * $ ())) mt)) ;; invalid Continuation
                        ))

  (for ([frames true_Frames])
    (test-equal (Machine_Frame? frames) #true))

  (for ([frames false_Frames])
    (test-equal (Machine_Frame? frames) #false))


  (test-results)

  (display "-------------------------------------")
  )