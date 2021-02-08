#lang racket
(require redex)
(require "Loo.rkt")

(define-extended-language Chainmail Loo-Machine
  (chainmailConfiguration ::= (M ! state)) ;; where M is the external module
  (A ::=
     (<obj access obj>)
     (<obj internal>)
     (<obj external>)
     (<obj calls obj @ m(args ...)>)
  )

  (obj ::= addr))

(define-judgment-form
  Chainmail
  #:mode (I I ⊨ O)
  #:contract (M state ⊨ A)
  
  )
  
