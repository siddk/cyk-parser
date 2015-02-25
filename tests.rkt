#lang racket

(require "grammar.rkt" "parse-tree.rkt" "parser.rkt")

;; Test with an arithmetic expression grammar.
(define (arith-test)
  (define-grammar arith
    ((Expr -> Var OpExpr)
     (OpExpr -> Op Expr))
    ((Expr -> Var)
     (Var -> "x")
     (Var -> "y")
     (Op -> "+")
     (Op -> "*")))
  (parse arith  '("x" "+" "y" "*" "y")))


(define (nlp-test)
  (define-grammar nlp
    ((S -> NP VP)
     (NP -> ART ADJNOUN)
     (ADJNOUN -> ADJ NOUN)
     (VP -> V NP))
    ((ART -> "a")
     (ART -> "the")
     (ADJ -> "red")
     (ADJ -> "green")
     (ADJ -> "smelly")
     (NOUN -> "dog")
     (NOUN -> "cat")
     (NOUN -> "book")
     (NOUN -> "martian")
     (V -> "saw")
     (V -> "killed")
     (V -> "ate")))
  (parse nlp '("the" "smelly" "dog" "ate" "the" "red" "book")))

(arith-test)
(nlp-test)