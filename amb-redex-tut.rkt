#lang racket
(require redex)
(require redex/tut-subst)

(define-language L
  (e (e e)
     (λ (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ t t) num)
  (x variable-not-otherwise-mentioned))

(define-extended-language L+Γ L
(Γ (x : t Γ)·))
  ;  [Γ · (x : t Γ)])
; Γ ::= [(x t) ...]
; Γ ::= [(x t) [(x2 t2) Γ]]

; [(foo a b rest ...) ([a b] (mf-apply foo rest ...))

(define-extended-language Ev L+Γ
  (p (e ...))
  (P (e ... E e ...))
  (E (v E)
     (E e)
     (+ v ... E e ...)
     (if0 E e e)
     (fix E)
     hole)
  (v (λ (x t) e)
     (fix v)
     number))




(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole P (if0 0 e_1 e_2)) ; take a P with the in-hole fill if0 0 e_1 e_2  ; THIS IS THE DEFINITION OF THE IF - it is syntax until we define it as a reduction
        (in-hole P e_1) ;... and then change the fill with e_1 
        "if0t")
   (--> (in-hole P (if0 v e_1 e_2))
        (in-hole P e_2)
        (side-condition (not (equal? 0 (term v)))) ; side-condition: something that must also be true for this reduction to be valid
        "if0f")
   ; ALSO NOTE: 'not' and 'equal' are understood by Redex/Racket
   (--> (in-hole P ((fix (λ (x t) e)) v))
        (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
        "fix")
   (--> (in-hole P ((λ (x t) e) v))
        (in-hole P (subst x v e))
        "βv")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...)
        (e_1 ... (in-hole E e_2) ... e_3 ...)
        "amb")))


(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e t)
 
  [(types Γ e_1 (→ t_2 t_3))
   (types Γ e_2 t_2)
   -------------------------
   (types Γ (e_1 e_2) t_3)]
 
  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]
 
  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   ---------------------------------------
   (types Γ (fix e) (→ t_1 t_2))]

  [---------------------
   (types (x : t Γ) x t)]
 
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]
 
  [(types Γ e num) ...
   -----------------------
   (types Γ (+ e ...) num)]
 
  [--------------------
   (types Γ number num)]
 
  [(types Γ e_1 num)
   (types Γ e_2 t)
   (types Γ e_3 t)
   -----------------------------
   (types Γ (if0 e_1 e_2 e_3) t)]
 
  [(types Γ e num) ...
   --------------------------
   (types Γ (amb e ...) num)])

(test-equal
   (judgment-holds
    (types · (λ (x num) x) t)
    t)
   (list (term (→ num num))))
(test-equal
   (judgment-holds
    (types · (amb 1 2 3) t)
    t)
   (list (term num)))
(test-equal
   (judgment-holds
    (types · (+ 1 2) t)
    t)
   (term (num))) ; in tutorial was (list (term (→ num num)) intentionally incorrect




(define-metafunction L+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])


(define-metafunction Ev
  Σ : number ... -> number ;QUESTION: Giving the type definition???
  [(Σ number ...)
   ,(apply + (term (number ...)))])  ; apply: a Racket feature that gives the function the list of parameters

(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])


(define x? (redex-match Ev x)) ; redex-match with only two args returns a function 


(define (progress-holds? e)
  (if (types? e)
      (or (v? e)
          (reduces? e))
      #t))

(define (types? e)
  (not (null? (judgment-holds (types · ,e t)
                              t))))

(define v? (redex-match Ev v))

(define (reduces? e)
  (not (null? (apply-reduction-relation
               red
               (term (,e))))))




;Exercises:

; e3: (_ ...e_1 e_2 _ ...)
; e4: 
