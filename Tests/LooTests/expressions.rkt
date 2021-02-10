#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-expressions)

(define (test-expressions)

  (display "-------------------------------------")
  (display "\nRunning expression Tests:\n")

  (define Loo_expression? (redex-match? Loo e))

  
  (define true_expressions (list

                            ;; equalities
                            (term (x_1 = x_4))
                            (term (true = false))
                            (term ((true = false) = true))  ;; must nest equalities with brackets

                            ;; if then else
                            (term (if true then true else true))
                            (term  (if x_5 then null else x_3))
                            (term  (if (x_1 = x_2) then (x_3 = (true = null)) else false))

                            ;; application
                            (term (x_1 @ fname(true false null)))
                            (term ((true = true) @ fname()))
                            (term (x_1 @ fname ()))
                            (term (true @ fname ()))
                            (term (null @ f((true = true))))  ;; application takes a list of 'e's, so we need to put brackets around true = true to show it is one expression
                            ))

  (define false_expressions (list
                             (term (true = false = true)) ; --------- ;; cannot chain equalities without nesting brackets
                             (term (5 = 5))  ; ---------------------- ;; expresssions can't be numbers
                             (term (if null then (true) else null))   ;; true doesn't need brackets
                             (term (if (true) then x_1 else null))    ;; true doesn't need brackets
                             (term (null @ f(true = true)))  ; ------ ;; application takes a list of 'e's - true = true is not a valid LIST of 'e's, (true = true) is ONE 'e'
                             (term (null @ f ((true = true = true)))) ;; invalid e inside list 
                             ))
  
  (for ([expression true_expressions])
    (test-equal (Loo_expression? expression) #true))
  
  (for ([expression false_expressions])
    (test-equal (Loo_expression? expression) #false))

  (test-results)
  (display "-------------------------------------")
  )