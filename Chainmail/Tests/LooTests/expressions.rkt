#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-expressions)

(define (test-expressions)
  (display "-------------------------------------")
  (display "\nRunning expression Tests:\n")

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

  (test-results)
  (display "-------------------------------------")
  )