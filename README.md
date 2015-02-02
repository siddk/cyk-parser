Introduction
------------

This is skeleton code for a parser generator.  It is meant to be a
fun, relatively small project that can be done to get more familiarity
with Racket.

The goal of this project is to take as input a grammar, such as:

        Expr -> Var OpExpr
        Expr -> Var
        OpExpr -> Op Expr
        Var -> "x"
        Var -> "y"
        Op -> "+"
        Op -> "*"

and a list of tokens, such as:

        ("x" "*" "y" "+" "y")

and use this to produce a parse tree, using the CYK algorithm:

        (Expr (Var "x")
              (OpExpr (Op "*")
                      (Expr (Var "y")
                            (OpExpr (Op "+")
                                    (Expr (Var "y"))))))))


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

To complete the parser generator, you will need to:

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
   Scheme).  If you also come from a Scheme background, I especially
   recommend:
    + Section 5 (programmer-defined datatypes)
    + Section 11 (iterations and comprehensions)

   You can ignore the other sections initially and read those as you
   happen to need them.  Some sections which you'll need:
    + Section 6 (modules), especially 6.4 and 6.5 (require/provide)
    + Section 16 (macros)

2. The Racket Reference:  This is a manual.  Only refer to it if for
   some reason the Guide is not sufficient.

3. For the CYK algorithm, Google should be able to give you tutorials
   and guides.  One thing to keep in mind - probably all of the
   results will show you a dynamic programming approach, but it can
   also be done with memoized recursion.  (This is true of almost all
   dynamic programs.) (If this doesn't make sense, you can just follow
   the pseudocode from Wikipedia or other sources.)