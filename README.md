# Chainmail-Redex

This is a model of the Chainmail language in Redex, which is written about in this paper: https://arxiv.org/abs/2002.08334
This is for the purpose of proving basic Chainmail assertions on programs written in Loo: a java-like object-oriented programming language.

All of the work relating to Chainmail is in the **Chainmail** folder

### Key files/folders:

**Chainmail/Loo.rkt** contains the majority of the completed work. It contains:
- the language "Loo" consisting of the fundamental elements of the language 
- the extended language "Loo-Machine" which adds the elements required to create a program state (heap, stack etc)
- "expr-reductions" which are the operational semantics of Loo implemented as reduction rules on "Loo-Machine"
- helper functions used in "expr-reductions"

**Chainmail/Tests/TestHelper.rkt** will execute every test on elements in Loo.rkt.

**Chainmail/Tests/ProgramExamples** contains examples of valid, reducible Loo programs.

Tests for each of the elements of Loo.rkt (described above) can be found in the "Tests" folder
- "Loo": **Chainmail/LooTests/...**
- "Loo-Machine": **Chainmail/Loo-Machine_tests/...**
- "expr-reductions": **Chainmail/reductionTests/...**
- helper functions: **Chainmail/metafunctionTests/...**


Javalite: https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=4375&context=etd&httpsredir=1&referer=.
A good reference for our project has been Javalite, which is modelled in PLT Redex.
Our approach for Loo shares a lot of similarities with Javalite's approach.

Featherweight Java: https://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf
Featherweight Java is another reference we used to help build Loo and Chainmail.
