#lang racket
(require redex)


(define-language TestLang
  (e :=
     (if e then e else e)
     v
     x)
     
  (v := 
     true
     false
     integer)
  (x := null))

(define v? (redex-match TestLang v))
(define e? (redex-match TestLang e))

(define rr
  (reduction-relation
   TestLang
;   #:domain e

   (--> (if true then e_0 else e_1)
        (e_0)
        (side-condition (equal? (redex-match? TestLang v (term e_0)) #t))   ;;how to test if a non-term is of a particular instance

        )
   ))
   