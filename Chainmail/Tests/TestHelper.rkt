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

(require "RandomTests/RandomTesting.rkt")

(require "metafunctionTests/lcl-lookup.rkt")
(require "metafunctionTests/field-lookup.rkt")
(require "metafunctionTests/h-lookup.rkt")
(require "metafunctionTests/lcl-extend.rkt")
(require "metafunctionTests/h-extend.rkt")
(require "metafunctionTests/fieldMap-extend.rkt")
(require "metafunctionTests/Object-extend.rkt")
(require "metafunctionTests/M-match.rkt")
(require "metafunctionTests/CD-lookup.rkt")
(require "metafunctionTests/method-lookup.rkt")
(require "metafunctionTests/method-params.rkt")
(require "metafunctionTests/method-Stmts.rkt")
(require "metafunctionTests/new-addr.rkt")


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


; random testing:
(display "\n--------------------------\n")
(display "------- Random Tests-------\n")
(display "--------------------------\n")

(random-test-syntax)
;(random-test-semantics)


; machine syntax tests:
(display "\n--------------------------\n")
(display "---- Metafunction Tests----\n")
(display "--------------------------\n")

(test-lcl-lookup)
(test-field-lookup)
(test-h-lookup)
(test-lcl-extend)
(test-h-extend)
(test-fieldMap-extend)
(test-Object-extend)
(test-M-match)
(test-CD-lookup)
(test-method-lookup)
(test-method-Stmts)
(test-method-params)
(test-new-addr)
