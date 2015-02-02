#lang racket

;; TODO: Provide other functions needed to work with grammars.
(provide define-grammar)

;; Definitions of the grammar data structure, as well as a macro to
;; make it easy to define grammars.

;; TODO: Create a representation for grammars.

;; TODO: Write "define-grammar" that makes it easy to define
;; grammars. Here is a start, which you may want to use (but feel free
;; to ignore it).
;; As currently written, it only supports rules of the form A -> B C
;; and A -> foo (this is called Chomsky Normal Form).
;; An example:
;; (define-grammar arith
;;   ((Expr -> Var OpExpr)
;;    (OpExpr -> Op Expr))
;;   ((Expr -> Var)
;;    (Var -> "x")
;;    (Op -> "+")
;;    (Op -> "*")))
;; Some notes:
;; 1) A grammar contains non-terminals (such as Expr and Var) and
;;    terminals (such as "x" and "+").
;; 2) The left hand side of any rule must be a non-terminal.
;; 3) A non-terminal can be the left hand side of many rules (see Expr
;;    and Op for example).
;; 4) Grammars can contain cycles.  For example, in the above grammar,
;;    an Expr can have an OpExpr as a child, which can have another
;;    Expr as a grandchild.
;; As a bonus, have it support more types of rules.  For example, I
;; would like to write something like:
;; (define-better-grammar arith
;;   (Expr -> Var Op Expr)
;;   (Var -> ("a" "b" "x" "y" "z"))
;;   (Op -> ("+" "-" "*" "/")))
;; You can transform these rules into Chomsky Normal Form, which the
;; CYK parser can understand.
(define-syntax define-grammar
  (syntax-rules (->)
    [(define-grammar name ((binary-nonterminal -> left right) ...) ((unary-nonterminal -> terminal) ...))
        (begin (display "Define-grammar is not implemented yet!\n")
               (define name 'stub))]))
