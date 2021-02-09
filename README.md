# Chainmail-Redex

This is a model of the Chainmail language in Redex, which is written about in this paper: https://arxiv.org/abs/2002.08334
This is for the purpose of proving basic Chainmail assertions on programs written in Loo: a java-like object-oriented programming language.

Loo.rkt contains the majority of the completed work. It contains:
- the language "Loo" consisting of the fundamental elements of the language 
- the extended language "Loo-Machine" which adds the elements required to create a program state (heap, stack etc)
- "expr-reductions" which are the operational semantics of Loo implemented as reduction rules on "Loo-Machine"
- helper functions used in "expr-reductions"

Tests for each of the above elements of Loo.rkt can be found in the "Tests" folder
- "Loo": LooTests/...
- "Loo-Machine": Loo-Machine_tests/...
- "expr-reductions": reductionTests/...
- helper functions: metafunctionTets/...



A good reference for our project has been Javalite which is modelled in PLT Redex: https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=4375&context=etd&httpsredir=1&referer=.
Our approach for Loo shares a lot of similarities with Javalite's approach.

Featherweight Java is another reference we used to help build Loo and Chainmail: https://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf
