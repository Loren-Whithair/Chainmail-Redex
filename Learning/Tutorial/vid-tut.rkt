; TUTORIAL: https://www.youtube.com/watch?v=ktNrRSAjyzQ&list=PL0DsGHMPLUWXFt7slbLYijUBFSJzsPVze&index=2&ab_channel=OPLSS


#lang racket
(require redex)

(define-language PCF
  (e ::=
     v
     x
     (e e)
     (μ x τ e)
     (op e)
     (e op e)
     (if e e e))
  (v ::=
     b
     (λ x τ e))
  (b ::= true false n) ;boolean
  (n ::= number) 
  (τ ::= B N (τ -> τ)) ; type 

  (x ::= variable-not-otherwise-mentioned)

  (op ::= + is-zero is-odd)
  
  (Γ ::= ((x : τ) ...))

  (E ::= hole (E e) (v E) (if E e e) (op E) (E op e) (v op E)))




(define-metafunction PCF
  [(ext ((x_0 : τ_0) ...) (x : τ)) ; ext: takes ((x_0 : τ_0) ...) (x : τ)  ;note that ((x_0 : τ_0) ...) == Γ
   ((x : τ) (x_0 : τ_0) ...)]) ; returns them inside another tuple

   

(define-metafunction PCF  ;order matters in a metafunction
  [(lookup ((x : τ) (x_0 : τ_0) ...) x) τ ]
  [(lookup ((x_0 : τ_0) (x_1 : τ_1) ...) x)
   (lookup ((x_1 : τ_1) ...) x)])

; HOW TO CALL A METAFUNCTION by example:
; > (term (lookup ((x : N) (x : B)) x))



; the judgment form below explicitly states the rules/patterns for
; declaring types in this language

(define-judgment-form PCF
  #:mode (⊢ I I I O) 
  #:contract (⊢ Γ e : τ)  ;the inputs and outputs specifically defined

  [(⊢ (ext Γ (x : τ)) e : τ_n) ; if we assume these inputs give this output...
   -------------------------
   (⊢ Γ (λ x τ e) : (τ → τ_n))] ;... then these inputs gives this output

  [(where τ (lookup Γ x))
   ----------------------
   (⊢ Γ x : τ)]

  [(⊢ Γ e_1 : (τ → τ_n))
   (⊢ Γ e_2 : τ)
   ---------------------
   (⊢ Γ (e_1 e_2) : τ_n)] ; note: using a symbol in the conclusion of the inference-rule gives you the ability to use it in the predicate
  
  [--------------
   (⊢ Γ n : N)]
  
  [-----------------
   (⊢ Γ true : B)]
  
  [-----------------
   (⊢ Γ false : B)]

  [(⊢ Γ e_1 : B)
   (⊢ Γ e_2 : τ)
   (⊢ Γ e_3 : τ)
   ---------------
   (⊢ Γ (if e_1 e_2 e_3) : τ)]

  [(⊢ Γ e : N)
   --------------
   (⊢ Γ (is-zero e) : B)]

  [( ⊢ Γ e : N)
   ---------------
   (⊢ Γ (is-odd e) : B)]

  [( ⊢ Γ e_1 : N)
   (⊢ Γ e_2 : N)
   -----------------
   (⊢ Γ (e_1 + e_2) : N)])
  
; HOW TO TEST SOMETHING AGAINST A JUDGMENT by example:

; >(judgment-holds (⊢ ((x : N)) x : N))    do the I I I relate to the O given?
; #t

;> (judgment-holds
;   (⊢ ((x : N)) x : τ) τ)
; '(N)

(define r
  (reduction-relation
   PCF
   #:domain e ;the Reduction Relation is defined on e (given in the language PCF)
   (--> ((λ x τ e) v) (subst x v e) β) ;case called β
   (--> (μ x τ e) (subst x (\,u x τ e) e) μ)
   (--> (if true e_1 e_2) e_1 ift)
   (--> (if false e_1 e_2) e_2 iff)
   (--> (n_1 + n_2) ,(+ (term n_1) (term n_2)) plus)
   (--> (is-zero n) ,(if (zero? (term n)) (term true) (term false)))
   (--> (is-odd n) ,(if (odd? (term n)) (term true) (term false)))))
   
(define -->r
  (context-closure r PCF E)) ; a RedRel defined by contextual closure
  ;; e r e'
  ;;------
  ;; E[e] -->r E[e']
  
  





(define-extended-language CPCF PCF
  (e ::= ....
     (mon l k e)
     (check l e v)
     (error l))
  (τ ::= ....
     (con τ))
  (E ::= ....
     (mon l k E)
     (check l E v))
  (k ::=
     (flat e
           (k → k)
           (k → λ x τ k)))
  (l ::= (any any any)))
