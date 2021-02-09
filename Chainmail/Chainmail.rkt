#lang racket
(require redex)
(require "Loo.rkt")

(define-extended-language Chainmail Loo-Machine
  (A ::=
     (< addr access addr >)
     (< addr internal >)
     (< addr external >)
     (< addr calls addr @ m(x ...) >)
  )
)




(define-judgment-form
  Chainmail
  #:mode     (? I I I I)
  #:contract (? M state ⊨ A)



  ;; addr_1 is a FIELD of addr_0
  [(where (C fieldMap_0) (h-lookup χ addr_0))
   (side-condition (val-in-fieldMap fieldMap_0 addr_1))
   ---------------
   (? M_0 (M_1 (((Continuation_0 η_0) · Ψ) χ))  ⊨ (< addr_0 access addr_1 >))]

  
  ;; addr_1 is pointed to in local var map, and addr_0 is this (in η_0)
  [(side-condition (addr-in-lcl η_0 addr_1))
   (side-condition (addr-in-lcl η_0 addr_0))
   (where this (lcl-addr-name η_0 addr_0))
   ---------------
   (? M_0 (M_1 (((Continuation_0 η_0) · Ψ) χ))  ⊨ (< addr_0 access addr_1 >))]

  )
  



; -----------------------------------------------------
; ------------------ HELPER FUNCTIONS -----------------
; -----------------------------------------------------

    
;NEEDS TESTING
(define-metafunction Loo-Machine
  addr-in-fieldMap : fieldMap addr -> bool
  [(addr-in-fieldMap mt addr) #false]
  [(addr-in-fieldMap (fieldMap [f_0 -> addr_0]) addr_0) #true]
  [(addr-in-fieldMap (fieldMap_0 [f_0 -> addr_0]) addr_1) (addr-in-fieldMap fieldMap_0 addr_1)])

(define-metafunction Loo-Machine
  addr-in-lcl : η addr -> bool
  [(addr-in-lcl mt addr) #false]
  [(addr-in-lcl (η [x_0 -> addr_0]) addr_0) #true]
  [(addr-in-lcl (η_0 [x_0 -> addr_0]) addr_1) (addr-in-lcl η_0 addr_1)])


(define-metafunction Loo-Machine
  lcl-addr-name : η addr -> x
;  [(lcl-addr-name mt addr) #false]
  [(lcl-addr-name (η [x_0 -> addr_0]) addr_0) x_0]
  [(lcl-addr-name (η_0 [x_0 -> addr_0]) addr_1) (lcl-addr-name η_0 addr_1)])

