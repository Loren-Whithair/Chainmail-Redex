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

; machine syntax tests: