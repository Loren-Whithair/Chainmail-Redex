#lang racket
(require redex)
(require "../../../Loo.rkt")
(require "../../../Chainmail.rkt")

(provide test-chainmail-x-internal)

(define (test-chainmail-x-internal)

  (display "-------------------------------------")
  (display "\nRunning Chainmail 'x-internal' Tests:\n")

  ; true
  (test-equal #t (judgment-holds
                  (? mt ((mt [C1 -> (clss C() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 1 internal >))))

  ; false
  (test-equal #f (judgment-holds 
                  (? mt ((mt [C1 -> (clss C1() {})]) ((() mt) (mt [1 -> (C2 mt)]))) ⊨ (< 1 internal >))))

  ; false (throws (program terminating) error)
  ; (test-equal #f (judgment-holds 
  ;                (? mt ((mt [C1 -> (clss C() {})]) ((() mt) (mt [1 -> (C1 mt)]))) ⊨ (< 2 internal >))))

  (test-results)

  (display "-------------------------------------")
  )


;((x_0 := x_1 @ m_0(x ...)) $ Stmts)
;(x_0 := x_1 @ m_0(x ...))