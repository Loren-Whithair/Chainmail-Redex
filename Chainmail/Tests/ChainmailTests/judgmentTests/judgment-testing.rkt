#lang racket
(require-redex)

;;true
(judgment-holds
 (?
  mt
  (mt ((( () ((mt [this -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C (mt [f1 -> 2]))]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))


;;true based on addr_1 is FIELD of addr_0
(judgment-holds
 (?
  mt
  (mt ((( () (mt [x0 -> 1])) · (() mt)) ((mt [1 -> (C (mt [f1 -> 2]))]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))

;;false, addr_1 is NOT field of addr_0
(judgment-holds
 (?
  mt
  (mt ((( () (mt [x0 -> 1])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))




;;true, 1 is 'this' and 2 is in lcl
(judgment-holds
 (?
  mt
  (mt ((( () ((mt [this -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))

(current-traced-metafunctions 'all)

;;false: 1 is not 'this'
(judgment-holds
 (?
  mt
  (mt ((( () ((mt [x0 -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))

(term x)
(judgment-holds
 (?
  mt
  (mt ((( () ((mt [this -> 1]) [x1 -> 2])) · (() mt)) ((mt [1 -> (C mt)]) [2 -> (C mt)])))
  ⊨
  (< 1 access 2 >)))

