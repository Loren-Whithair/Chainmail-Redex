#lang racket
(require redex)
(require "../../Loo.rkt")

(provide test-varAssgn_OS)

(define (test-varAssgn_OS)

  (display "-------------------------------------")
  (display "\nRunning varAssgn_OS Tests:\n")

  ;----------------
  ;---TRUE TESTS---
  ;----------------

  
  (test-->
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))
   (term ((mt (C1 -> (clss C1 () ())))(((() (((mt (x_1 -> 2))(this -> 1))(x_0 -> false))) · (() mt)) ((mt (1 -> (C1 (mt (f1 -> true))))) (2 -> (C1 (mt (f1 -> false)))))))))


  ;----------------
  ;---FALSE TESTS--
  ;----------------


  ;(test-->  ;;looking for a field that isn't there, can't reduce
  ; expr-reductions
  ; (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f_no) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))
  ; (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f_no) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))])))))
  ;;NOTE: runs into an error because the field-lookup metafunction relies on the fact that the field you are searching for EXISTS IN THE OBJECT

  
  (test-->  ;;Class(this) ≠ Class(x_1)
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C2 (mt [f1 -> false]))]))))
   ;nothing
   )
  
  (test-->  ;;x_1 does NOT point to an address, poins to integer: [10]
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> [10]]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))]))))
   ;nothing
   )

  
  (test-results)
  (display "-------------------------------------")
  )



#|

Attempting to see if the reduction works:

(traces
   expr-reductions
   (term ((mt [C1 -> (clss C1() {})]) (((((x_0 := x_1 @ f1) $ ()  ) ((mt [x_1 -> 2]) [this -> 1])) · (() mt)) ((mt [1 -> (C1 (mt [f1 -> true]))]) [2 -> (C1 (mt [f1 -> false]))])))))

--THIS WORKS

|#



