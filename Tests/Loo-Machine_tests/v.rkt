#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-v)

(define (test-v)

  (display "-------------------------------------")
  (display "\nRunning v (value) Tests:\n")

  (define Machine_V? (redex-match? Loo-Machine v))

  (define true_values (list
                       
                       (term null)
                       (term 5)    ;; addr
                       (term [-2]) ;; integers are wrapped in [] to distinguish between them and addrs
                       (term true)
                       (term false)

                       ))

  (define false_values (list
                        (term 3.5)
                        (term -2)  ;; not an integer (missing []), and not an addr (-2 < 0)
                        (term (1 4 6 3.5))
                        (term (1 3 4))
                        ))

  (for ([values true_values])
    (test-equal (Machine_V? values) #true))

  (for ([values false_values])
    (test-equal (Machine_V? values) #false))

  (test-results)

  (display "-------------------------------------")
  )
