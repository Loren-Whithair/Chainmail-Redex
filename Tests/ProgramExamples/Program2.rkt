#lang racket
(require redex)
(require "../../Loo.rkt")

(traces
   expr-reductions
       (term ((mt [MyClass ->
                  (clss MyClass(arg1) { (fld f1) (constructor(a1) { ((this @ f1 := a1) $ (return this)) } )})]) ;; module
     (((((x_0 := new MyClass(p1)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3])) ;; top frame
       · (() mt)) ;; bottom frame
      (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)]))))) ;; heap