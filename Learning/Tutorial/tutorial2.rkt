#lang racket
(require redex)

(define-language Lambda
  (e ::= x
         (lambda (x_!_ ...) e)
         (e e ...))
  (x ::= variable-not-otherwise-mentioned))

; are the identifiers in the given sequence unique?
 
(module+ test
  (test-equal (term (unique-vars x y)) #true)
  (test-equal (term (unique-vars x y x)) #false))
 
(define-metafunction Lambda
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x_1 ... x x_2 ...) #false]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])
 
(module+ test
  (test-results))

; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
 
(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))
 
(define-metafunction Lambda
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)]
  [(subtract (x ...) x_1 x_2 ...)
   (subtract (subtract1 (x ...) x_1) x_2 ...)])
 
; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(module+ test
  (test-equal (term (subtract1 (x y z x) x)) (term (y z))))
 
(define-metafunction Lambda
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])
 
(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])