#lang racket
(require redex)

(define-language Lambda
  (e ::= x
     (lambda (x_!_ ...) e)  ;; x_!_ means x must differ from all other elems in sequence
     (e e ...))
  (x ::= variable-not-otherwise-mentioned))


; are the identifiers in a given sequence unique?

(define-metafunction Lambda
  unique-vars : x ... -> boolean   ;; the contract of the metafunction
  [(unique-vars) #true] ; ------------------------ ;; if empty, then true
  [(unique-vars x x_1 ... x x_2 ...) #false] ; --- ;; if they contain x, followed by some stuff, followed by x again, followed by other stuff, false
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)]) ;; if we can't find a duplicate of x, remove x and recursively try again


(module+ test
  (test-equal (term (unique-vars x y)) #true)
  (test-equal (term (unique-vars x y x)) #false))



; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
(define-metafunction Lambda
  subtract : (x ...) x ... -> (x ...)  ; contract: takes two lists of x's and returns one
  [(subtract (x ...)) (x ...)] 
  [(subtract (x ...) x_1 x_2 ...) 
   (subtract (subtract1 (x ...) x_1) x_2 ...)]) 

(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))



; (subtract1 (x ...) x_1) removes x_1 from (x ...)
(define-metafunction Lambda
  subtract1: (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)  ;;removes the first found instance of x
       (where (x_2new ...) (subtract1 (x_2 ...) x))  ;;removes the rest of them
       (where #false (in x x_1 ...))] 
  [(subtract1 (x ...) x_1) (x ...)])

(module+ test
  (test-equal (term subtract1 (x y z x) x)) (term (y z)))


(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])



; (fv e) computes the sequence of free vars in e
; a variable occurrence of x is free in e
; if there is no (lambda (... x ...) to bind it

(module+ test
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (lambda (x) x))) (term ()))
  (test-equal (term (fv (lambda (x) (y z x)))) (term (y z))))


(define-metafunction Lambda
  fv : e -> (x ...)
  [(fv x) (x)]  ; no lambda bindings
  [(fv (lambda (x ...) e))
   (subtract (x_e ...) x ...) ; get the free vars from e and remove the (x ...)
   (where (x_e ...) (fv e))]
  [(fv (e_f e_a ...))  ;seq of exprs
   (x_f ... x_a ... ...)
   (where (x_f ...) (fv e_f))  ;reduce e_f...
   (where ((x_a ...) ...) ((fv e_a) ...))])  ; then reduce the rest in the seq



; (sd e) computes the "static distance" version of e
(define-extended-language SD Lambda
  (e ::= .... (K n))
  (n ::= natural))

(define sd1 (term (K 1)))
(define sd2 (term 1))

(define SD? (redex-match? SD e))

(module+ test
  (test-equal (SD? sd1) #true))


(define-metafunction SD
  sd : e -> e
  [(sd e_1) (sd/a e_1 ())])

(module+ test
  (test-equal (term (sd/a x ())) (term x))
  (test-equal (term (sd/a x ((y) (z) (x)))) (term (K 2 0)))
  (test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(define-metafunction SD
  sd/a: e ((x ...) ...) -> e
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ;bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  