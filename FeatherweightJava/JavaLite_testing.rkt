#lang racket
(require redex)

(define language JL
  P ::= (mu (C m)
            