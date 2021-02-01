#lang racket
(require redex)
(require "../../Loo.rkt")

(provide random-test-syntax)
;(provide random-test-semantics)

;; random testing of syntax
(define (random-test-syntax)
  (define syntax_correct? (redex-match Loo language))
  (redex-check Loo language (syntax_correct? (term e))))

;; random testing of reduction rules
;; none will work at this stage because no reduction rules are full defined
#|
(define (random-test-semantics)
  (display "\nThen random testing of Loo reduction rules:\n")
  (define (reduces? e) (not (null? (apply-reduction-relation expr-reductions (term (e))))))
  ;(redex-check Loo-Machine machine-language (reduces? (term e)))
  ) |#
  