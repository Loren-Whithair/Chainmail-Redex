#lang racket
(require redex)

; Tests in files get run automatically with each call to require

; syntax tests:
(require "LooTests/Module.rkt")
(require "LooTests/ClassDesc.rkt")
(require "LooTests/CDecl.rkt")
(require "LooTests/MethDecl.rkt")
(require "LooTests/GhostFields.rkt")
(require "LooTests/Stmts.rkt")
(require "LooTests/expressions.rkt")

(display "-------------------------------------")

; machine syntax tests:
(require "Loo-Machine_tests/v.rkt")
(require "Loo-Machine_tests/states.rkt")
(require "Loo-Machine_tests/stack.rkt")
(require "Loo-Machine_tests/runtime-config.rkt")
(require "Loo-Machine_tests/local-vars.rkt")
(require "Loo-Machine_tests/heap.rkt")
(require "Loo-Machine_tests/frame.rkt")
(require "Loo-Machine_tests/Object.rkt")
(require "Loo-Machine_tests/Continuation.rkt")
