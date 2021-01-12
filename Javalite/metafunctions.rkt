#lang racket
(require redex)

(define-metafunction Javalite
  get-length : (any ...) -> number
  [(get-length (any_0 ...))
   ,(length (term (any_0 ...)))])

(define-metafunction Javalite
  default-value : T -> v
  [(default-value bool)
   false]
  [(default-value unit)
   unit]
  [(default-value C)
   null ])

(define-metafunction Javalite
  default-value* : (T ...) -> (v ...)
  [(default-value* ())
   ()]
  [(default-value* (T_0 T_1 ...))
   ((default-value T_0) (default-value T_1) ...)])

(define-metafunction Javalite
  h-max : h -> number
  [(h-max mt) -1]
  [(h-max (h [loc -> hv]))
   ,(max (term loc) (term (h-max h)))])

(define-metafunction Javalite
  h-malloc : h -> number
  [(h-malloc h)
   ,(add1 (term (h-max h)))])

(define-metafunction Javalite
  h-malloc-n-helper : number number -> (loc ...)
  [(h-malloc-n-helper number_b number_c)
   ,(let ([z (term number_b)]) (build-list (term number_c) (lambda (y) (+ y z))))])

(define-metafunction Javalite
  h-malloc-n : h number -> (loc ...)
  [(h-malloc-n h number)
   (loc_0 ...)
   (where ((loc_0 ...)) (h-malloc-n* h number))])

(define-metafunction Javalite
  internal-h-malloc-n* : number (number ...) -> (number (loc ...) ...)
  [(internal-h-malloc-n* number_b (number_0))
   (number_t (loc_1 ...))
   (where (loc_1 ...) (h-malloc-n-helper number_b number_0))
   (where number_t ,(if (empty? (term (loc_1 ...)))
                        (term number_b)
                        (add1 (apply max (term (loc_1 ...))))))]
  [(internal-h-malloc-n* number_b (number_0 number_1 number_2 ...))
   (number_t (loc_0 ...) (loc_1 ...) ...)
   (where (loc_0 ...) (h-malloc-n-helper number_b number_0))
   (where number_i ,(if (empty? (term (loc_0 ...)))
                        (term number_b)
                        (add1 (apply max (term (loc_0 ...))))))
   (where (number_t (loc_1 ...)
                    (internal-h-malloc-n* number_i (number_1 number_2 ...))))])

(define-metafunction Javalite h-malloc-n* : h number ... -> ((loc ...) ...)
  [(h-malloc-n* h number_0 ...)
   (( loc_0 ...) ...)
   (where (number (loc_0 ...) ...) (internal-h-malloc-n* (h-malloc h) (number_0 ...)))])
;   ...)



(define-metafunction Javalite
  storelike-lookup : any any -> any
  [(storelike-lookup mt any_0)
   ,(error ’storelike-loopup "~e␣not␣found␣in␣~e" (term any_0) (term mt))]
  [(storelike-lookup (any_0 [any_t -> any_ans]) any_t)
any_ans]
  [(storelike-lookup (any_0 [any_k -> any_v]) any_t)
   (storelike-lookup any_0 any_t)
   (side-condition (not (equal? (term any_k) (term any_t))))])

(define (id-<= a b)
(string <=? (symbol- >string a) (symbol- >string b)))

(define (storelike-extend <= storelike k hv)
  (match storelike
    [’mt ‘(mt [,k -> ,hv])]
    [‘(,storelike [,ki -> ,hvi])
      (cond
        [(equal? k ki)
         ‘(,storelike [,ki -> ,hv])]
        [(<= k ki)
         ‘(,(storelike-extend <= storelike k hv) [,ki -> ,hvi])]
        [else
         ‘((,storelike [,ki -> ,hvi]) [,k -> ,hv])])]))

(define (storelike-extend* <= storelike extend*)
  (match extend*
    [’() storelike]
    [‘([,k -> ,hv] . ,extend*)
      (storelike-extend* <= (storelike-extend <= storelike k hv) extend*)]))

(define-metafunction Javalite
  h-lookup : h loc -> hv
  [(h-lookup h loc)
   (storelike-lookup h loc)])

(define-metafunction Javalite
  h-extend* : h [loc -> hv] ... -> h
  [(h-extend* h [loc -> hv] ...)
   ,(storelike-extend* <= (term h) (term ([loc -> hv] ...)))])

(define-metafunction Javalite
  ?-lookup : ? x -> loc
  [(?- lookup ? x)
   (storelike-lookup ? x)])

(define-metafunction Javalite
  ?-extend* : ? [x -> loc] ... -> ?
  [(?-extend* ? [x -> loc] ...)
   ,(storelike-extend* id-<= (term ?) (term ([x -> loc] ...)))])

(define-metafunction Javalite
  restricted-field-lookup : object f -> loc
  [(restricted-field-lookup (C_c
                             ( C_0 [ f_0 loc_0 ] ...) ...
                             ( C_t [ f_t0 loc_t0 ] ...
                                   [ f_target loc_target ]
                                   [ f_t1 loc_t1 ] ...)
                             ( C_1 [ f_1 loc_1 ] ...) ...)
                            f_target )                          
   loc_target
   ; ; Allows for redefinition and restricts matching
   ; ; to be the most recent definition by current cast .
   ( side-condition
     ( not ( member ( term f_target )
                    ( term ( f_t1 ... f_1 ... ...)))))])

(define-metafunction Javalite
  field-lookup : object f -> loc
  [(field-lookup object f_target)
   (restricted-field-lookup (restrict-object object) f_target)])

( define-metafunction javalite
restrict-object : object -> object
   [( restrict-object ( C_c ( C_0 [ f_0 loc_0 ] ...) ...
                            ( C_c [ f_c loc_c ] ...)
                            ( C_1 [ f_1 loc_1 ] ...) ...))
    ( C_c ( C_0 [ f_0 loc_0 ] ...) ...
          ( C_c [ f_c loc_c ] ...))])

;;;;;;;;;NEW COPY PASTE

( define-metafunction javalite
class-name : CL -> C
   [( class-name ( class C_t extends C ([ T f] ...) (M ...)))
    C_t ])

( define-metafunction javalite
   parent-name : CL - > C
   [( parent-name ( class C extends C_p ([ T f ] ...) (M ...)))
    C_p ])

( define-metafunction javalite
   field-list : CL -> ([ T f] ...)
   [( field-list ( class C extends C_p ([ T f] ...) (M ...)))
    ([ T f ] ...)])

( define-metafunction javalite
   class-list-extend : (C ...) C -> (C ...)
   [( class-list-extend ( C_0 ...) C_1 )
    ( C_0 ... C_1 )])

( define-metafunction javalite
   class-lookup : C -> CL
   [( class-lookup ( CL_0 ... CL_1 CL_2 ...) C)
    CL_1
    ( side-condition ( equal ? ( term ( class-name CL_1 )) ( term C )))])

( define-metafunction javalite
   class-list-from-object : object -> (C ...)
   [( class-list-from-object ( C_0 ( C_1 [ f_1 loc_1 ] ...) ...))
    ; Restrict out the current cast -- Object will be first class
    ( C_1 ...)])

( define-metafunction javalite
   class-parents + self : C -> (C ...)
   [( class-parents + self Object )
    ( class-list-extend () Object )]
   ; id retricts out the base case above
   [( class-parents + self id )
    ( class-list-extend ( class-parents + self C_p ) id )
    ( where CL ( class-lookup id ))
    ( where C_p ( parent-name CL ))])

( define-metafunction javalite
   field-lists-extend : (([ T f] ...) ...) ([ T f] ...) -> (([ T f ] ...) ...)
   [( field-lists-extend (([ T_0 f_0 ] ...) ...) ([ T_1 f_1 ] ...))
    (([ T_0 f_0 ] ...) ... ([ T_1 f_1 ] ...))])

( define-metafunction javalite
   fields-parents + self : C -> (([ T f ] ...) ...)
   [( fields-parents + self Object )
   ( field-lists-extend () ())]
   ; id restricts out the base case above
   [( fields-parents + self id )
    ( field-lists-extend ( fields-parents + self C_p ) ([ T f] ...))
    ( where CL ( class-lookup id ))
    ( where C_p ( parent-name CL ))
    ( where ([ T f] ...) ( field-list CL ))])

( define-metafunction javalite
   method-name : M -> m
   [( method-name ( T_0 m ([ T_1 x] ...) e ))
    m ])

( define-metafunction javalite
   method-expression : M - > e
   [( method-expression ( T_0 m ([ T_1 x] ...) e ))
    e ])

( define-metafunction javalite
   method-args : M -> (x ...)
   [( method-args ( T_0 m ([ T_1 x] ...) e ))
    (x ...)])

( define-metafunction javalite
   method-lookup : CL m -> any
   [( method-lookup ( class C_0 extends C_1 ([ T x ] ...) ( M_0 ... M_t M_1 ...)) m)
    ( C_0 ( method-args M_t ) ( method-expression M_t ))
    ( side-condition ( equal ? ( term ( method-name M_t )) ( term m )))]
   [( method-lookup ( class C_0 extends C_1 ([ T x ] ...) (M ...)) m)
    error
    ( side-condition ( equal ? ( findf (? ( i) ( equal ? ( term ( method-name ,i )) ( term m )))
                                       ( term (M ...))) #f ))])

( define (- > bool v)
   ( if v
        ’ true
        ’ false ))

( define-metafunction javalite
   cast : object C -> object
   [( cast ( C_1 ( C_2 [ f_2 loc_2 ] ...) ...
                 ( C_3 [ f_3 loc_3 ] ...)
                 ( C_4 [ f_4 loc_4 ] ...) ...) C_3 )
    ( C_3 ( C_2 [ f_2 loc_2 ] ...) ...
          ( C_3 [ f_3 loc_3 ] ...)
          ( C_4 [ f_4 loc_4 ] ...) ...)])

( define ( cast ? object C_t )

( define inner-cast ?
   ( term-match / single
                javalite
                [( C_1 ( C_2 [ f_2 loc_2 ] ...) ...)
                 ( term ( C_1 C_2 ...))]))
   ( if ( member C_t ( inner-cast ? object )) #t #f ))

( define ( cast ?/- > bool object C_t )
   (- > bool ( cast ? object C_t )))

( define-metafunction javalite
   instanceof * : object C -> v
   [( instanceof * ( C_1 ( C_2 [ f_2 loc_2 ] ...) ...) C_t )
    ,(- > bool ( member ( term C_t ) ( term ( C_2 ...))))])

( define-metafunction javalite
   inject : P - > state
   [( inject ( (C m )))
    ( mt mt (( new C) @ m ()) ret )])

( define-metafunction javalite
   inject / with-state : state m -> state
   [( inject / with-state ( h ? e k ) m)
    ( h ? (e @ m ()) ret )])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OLD COPY PASTE
(define-metafunction Javalite
  restrict-object : object ->
object
[f_0 loc_0] ...) ... [f_c loc_c] ...)
[f_1 loc_1] ...) ...)) ...
[(restrict-object (C_c
(C_0 (C_c (C_1 ...)
(C_0 [f_0 loc_0] ...) ... (C_t [f_t0 loc_t0] ...field
[f_target loc_target]
[f_t1 loc_t1] ...) (C_1 [f_1 loc_1] ...) ...)
f_target)
;; Allows for redefinition and restricts matching
;; to be the most recent definition by current cast. (side-condition
(not (member (term f_target)
(term (f_t1 ... f_1 ... ...)))))])

(C_c (C_0 [f_0 loc_0]
(C_c [f_c loc_c] ...))])

(define-metafunction Javalite
class-name : CL -> C
[(class-name (class C_t extends C ([T f] ...) (M ...)))
C_t ])

(define-metafunction Javalite
parent-name : CL -> C
[(parent-name (class C extends C_p ([T f] ...) (M ...)))
C_p ])

(define-metafunction Javalite
field-list : CL -> ([T f] ...)
[(field-list (class C extends C_p ([T f] ...) (M ...)))
([T f] ...)])

(define-metafunction Javalite class-list-extend : (C ...) C -> (C ...) [(class-list-extend (C_0 ...) C_1)
(C_0 ... C_1)])

(define-metafunction Javalite
class-lookup : C -> CL
[(class-lookup (CL_0 ... CL_1 CL_2 ...) C)
CL_1
(side-condition (equal? (term (class-name CL_1)) (term C)))])

(define-metafunction Javalite
class-list-from-object : object -> (C ...) [(class-list-from-object (C_0 (C_1 [f_1 loc_1] ...) ...))
; Restrict out the current cast -- Object will be first class
(C_1 ...)])

(define-metafunction Javalite class-parents+self : C -> (C ...) [(class-parents+self Object)
(class-list-extend () Object)]
; id retricts out the base case above
[(class-parents+self id) (class-list-extend (class-parents+self (where CL (class-lookup id))
(where C_p (parent-name CL))])
C_p) id)
(define-metafunction Javalite
field-lists-extend : (([T f] ...) ...) ([T f] ...) -> (([T f] ...) ...) [(field-lists-extend (([T_0 f_0] ...) ...) ([T_1 f_1] ...))
(([T_0 f_0] ...) ... ([T_1 f_1] ...))])
(define-metafunction Javalite fields-parents+self : C -> (([T f] ...) ...) [(fields-parents+self Object)
(field-lists-extend () ())]

; id restricts out the base case above
[(fields-parents+self id) (field-lists-extend (fields-parents+self (where CL (class-lookup id))
(where C_p (parent-name CL))
(where ([T f] ...) (field-list CL))])
(define-metafunction Javalite method-name : M -> m
[(method-name (T_0 m ([T_1 x] ...) e))
m])
(define-metafunction Javalite method-expression : M -> e [(method-expression (T_0 m ([T_1 x] ...) e))
e])
(define-metafunction Javalite method-args : M -> (x ...) [(method-args (T_0 m ([T_1 x] ...) e))
(x ...)])
C_p) ([T f] ...))
(define-metafunction Javalite
method-lookup : CL m -> any
[(method-lookup (class C_0 extends C_1 ([T x] ...) (M_0 ... M_t M_1 ...)) m)
(C_0 (method-args M_t) (method-expression M_t))
(side-condition (equal? (term (method-name M_t)) (term m)))] [(method-lookup (class C_0 extends C_1 ([T x] ...) (M ...)) m)
error
(side-condition (equal? (findf (? (i) (equal? (term (method-name ,i)) (term m)))
(define (->bool v) (if v
’true ’false))
(define-metafunction Javalite
cast : object C -> object
[(cast (C_1 (C_2 [f_2 loc_2] ...) ...
(C_3 [f_3 loc_3] ...)
(term (M ...))) #f))])


(C_4 [f_4 loc_4] ...) ...) C_3) (C_3 (C_2 [f_2 loc_2] ...) ...
(C_3 [f_3 loc_3] ...)
(C_4 [f_4 loc_4] ...) ...)])
(define (cast? object C_t) (define inner-cast?
(term-match/single
Javalite
[(C_1 (C_2 [f_2 loc_2] ...) ...)
(term (C_1 C_2 ...))]))
(if (member C_t (inner-cast? object)) #t #f))
(define (cast?/->bool object C_t) (->bool (cast? object C_t)))
(define-metafunction Javalite
instanceof* : object C -> v
[(instanceof* (C_1 (C_2 [f_2 loc_2] ...) ...) C_t)
,(->bool (member (term C_t) (term (C_2 ...))))])
(define-metafunction Javalite inject : P -> state [(inject ( (C m)))
( mt mt ((new C) @ m ()) ret)])
(define-metafunction Javalite inject/with-state : state m -> state [(inject/with-state ( h ? e k) m)
( h?(e@m())ret)])