#lang racket
(require redex)

(require "LooTests/Module.rkt")
(require "LooTests/ClassDesc.rkt")
(require "LooTests/CDecl.rkt")
(require "LooTests/MethDecl.rkt")
(require "LooTests/GhostFields.rkt")
(require "LooTests/Stmts.rkt")
(require "LooTests/expressions.rkt")
(require "Loo-Machine_tests/v.rkt")
(require "Loo-Machine_tests/states.rkt")
(require "Loo-Machine_tests/stack.rkt")
(require "Loo-Machine_tests/runtime-config.rkt")
(require "Loo-Machine_tests/local-vars.rkt")
(require "Loo-Machine_tests/heap.rkt")
(require "Loo-Machine_tests/frame.rkt")
(require "Loo-Machine_tests/Object.rkt")
(require "Loo-Machine_tests/Continuation.rkt")


; syntax tests:

(display "\n--------------------------\n")
(display "----- Syntax Tests -------\n")
(display "--------------------------\n")

(test-modules)
(test-ClassDesc)
(test-CDecl)
(test-MethDecl)
(test-GhostFields)
(test-Stmts)
(test-expressions)


; machine syntax tests:
(display "\n--------------------------\n")
(display "-- Machine Syntax Tests---\n")
(display "--------------------------\n")

(test-states)
(test-runtimeConfig)
(test-continuation)
(test-v)
(test-stack)
(test-localVars)
(test-heap)
(test-frame)
(test-object)
