Introduction
------------

This is an implemenation of the Cocke-Younger-Kasami algorithm parsing algorithm in Racket.  It is meant to be a
fun, relatively small project that can be done to get more familiarity
with Racket. Implemented by Siddharth Karamcheti as part of the Par Lab at UC Berkeley.

The goal of this project is to take as input a grammar, such as:

        Expr -> Var OpExpr
        Expr -> Var
        OpExpr -> Op Expr
        Var -> "x"
        Var -> "y"
        Op -> "+"
        Op -> "*"

Or, a natural language grammar like:

        S -> NP VP
        NP -> ART ADJNOUN
        ADJNOUN -> ADJ NOUN
        VP -> V NP
        ART -> "a"
        ART -> "the"
        ADJ -> "red"
        ADJ -> "green"
        ADJ -> "smelly"
        NOUN -> "dog"
        NOUN -> "cat"
        NOUN -> "book"
        NOUN -> "martian"
        V -> "saw"
        V -> "killed"
        V -> "ate"

and a list of tokens, such as:

        ("x" "*" "y" "+" "y")

and use this to produce a parse tree, using the CYK algorithm:

        Expr (x + y * y)
          Var (x)
          OpExpr (+ y * y)
             Op (+)
             Expr (y * y)
                Var (y)
                OpExpr (* y)
                   Op (*)
                   Expr (y)

        S (the smelly dog ate the red book)
            NP (the smelly dog)
               ART (the)
               ADJNOUN (smelly dog)
                  ADJ (smelly)
                  NOUN (dog)
            VP (ate the red book)
               V (ate)
               NP (the red book)
                  ART (the)
                  ADJNOUN (red book)
                     ADJ (red)
                     NOUN (book)

Architecture
------------

The general structure of the system is to create a grammar using
`define-grammar` (from `grammar.rkt`), and then parse a list of tokens
with that grammar using `parse` (from `parser.rkt`), which will create
a parse tree, that can be printed in a reasonable format using
`parse-tree->list` (from `parse-tree.rkt`).  You can see examples of
this by looking at the tests in `tests.rkt`.


Necessary Steps
---------------

The following steps describe the process I used to complete the parser generator:

1. Come up with a representation for grammars (see `grammar.rkt`)
2. Come up with a representation for parse trees (see
   `parse-tree.rkt`)
3. Implement the CYK algorithm for parsing.
4. Write a macro to make it very easy for the user to create grammars
   (see `grammar.rkt`, and examples in `tests.rkt`)


Resources
---------

1. The Racket Guide: http://docs.racket-lang.org/guide/  This is a
   guide to Racket that is targeted at beginners.  This is the main
   resource I used when learning Racket (having already learned
   Scheme). If you also come from a Scheme background, I especially
   recommend:
    + Section 5 (programmer-defined datatypes)
    + Section 11 (iterations and comprehensions)

   You can ignore the other sections initially and read those as you
   happen to need them.  Some sections which you'll need:
    + Section 6 (modules), especially 6.4 and 6.5 (require/provide)
    + Section 16 (macros)

2. The Racket Reference:  This is a manual.  Only refer to it if for
   some reason the Guide is not sufficient.

3. For the CYK algorithm, Google has all the necessary tutorials
   and guides. One thing to keep in mind - probably all of the
   results will show you a dynamic programming approach, but it can
   also be done with memoized recursion.  (This is true of almost all
   dynamic programs.)
