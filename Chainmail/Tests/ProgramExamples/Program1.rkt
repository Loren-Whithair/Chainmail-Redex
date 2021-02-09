#lang racket
(require redex)
(require "../../Loo.rkt")

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
                                                    }) })])  ;;Module
   
   (((((x_0 := new myClass(p1)) $ ()) (((mt [p1 -> 1]) [p2 -> 2]) [this -> 3]))        ;;Next frame
     · (() mt))  ;;Stack
    (((mt [1 -> (C1 mt)]) [2 -> (C2 mt)]) [3 -> (C3 mt)])))))   ;;Heap