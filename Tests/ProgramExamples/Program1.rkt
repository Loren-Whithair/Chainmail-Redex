#lang racket
(require redex)
(require "../../Loo.rkt")

; This is a Loo program example that uses all six reduction rules.
;
; The program starts with a call to create a new myClass object, passing in p1 (which has the value of 1) as a parameter, this is then assigned tio the field f1.
; We then do a method call to the internal method m_0 and assign the result to local variable (x_0). This method returns the value assigned to the field f1.
; We then return 'this' from the constructor which returns a new reference to the location in the heap of our object.
; Since it is a new object, a new pointer in the heap will be assigned which points to our object (value of 4 in this case).
; After evaluation, we expect p1, f1 and x_0 to all point to the same value of 1. Running the program shows that this is in fact what happens.
(traces
 expr-reductions
 (term
  ((mt [myClass ->
                (clss myClass(arg1) {
                                     (fld f1)

                                     (constructor(a1) {
                                                       ((this @ f1 := a1) $ ((x_0 := this @ m_0()) $ (return this)))
                                                       })

                                     (method m_0() {
                                                    ((x_1 := this @ f1) $ ((return x_1) $ ()))
                                                    }) })]) ;; Module
   
   (((((x_0 := new myClass(p1)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3])) ;; Top frame
     · (() mt)) ;; Bottom frame
    (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)]))))) ;; Heap