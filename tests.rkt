#lang racket

(require "grammar.rkt" "parse-tree.rkt" "parser.rkt")

;; Testing function.
(define (assert-equal actual-result expected-result)
  (if (not (equal? expected-result actual-result))
      (display (format "Failed.~%Expected: ~A~%Got: ~A~%~%"
                       expected-result actual-result))
      (display (format "Passed!~%"))))



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
  ;; Use the grammar to parse the input "x"
  ;; Then convert it to a list representation
  (assert-equal (parse-tree->list
                 (parse arith '("x")))
                '(Expr (Var "x")))
  (assert-equal (parse-tree->list
                 (parse arith '("x" "*" "y" "+" "y")))
                '(Expr (Var "x")
                       (OpExpr (Op "*")
                               (Expr (Var "y")
                                     (OpExpr (Op "+")
                                             (Expr (Var "y"))))))))

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
  (assert-equal (parse-tree->list
                 (parse nlp '("the" "smelly" "dog" "ate" "the" "red" "book")))
                '(S (NP (ART "the")
                        (ADJNOUN (ADJ "smelly")
                                 (NOUN "dog")))
                    (VP (V "ate")
                        (NP (ART "the")
                            (ADJNOUN (ADJ "red")
                                     (NOUN "book"))))))
  (assert-equal (parse-tree->list
                 (parse nlp '("the" "red" "cat" "saw" "a" "green" "martian")))
                '(S (NP (ART "the")
                        (ADJNOUN (ADJ "red")
                                 (NOUN "cat")))
                    (VP (V "saw")
                        (NP (ART "a")
                            (ADJNOUN (ADJ "green")
                                     (NOUN "martian")))))))

(arith-test)
(nlp-test)

;; Tests for the extra part - a better syntax for defining grammars.
;; Such a grammar will have to be desugared into a grammar with
;; only unary and binary rules (this can be done by creating new,
;; intermediate non-terminals).
;; In addition, the resulting parse tree will have to be
;; postprocessed to remove the intermediate non-terminals.
;; Commented out so that Racket's module system does not
;; complain that "define-better-grammar" does not exist.
;;(define (extra-test)
;;  (define-better-grammar arith
;;    (Expr -> Var Op Expr)
;;    (Var -> ("a" "b" "x" "y" "z"))
;;    (Op -> ("+" "-" "*" "/")))
;;  (assert-equal (parse-tree->list
;;                 (parse arith '("b")))
;;                '(Expr (Var "b")))
;;  (assert-equal (parse-tree->list
;;                 (parse arith '("z" "*" "y" "-" "y")))
;;                '(Expr (Var "z")
;;                       (Op "*")
;;                       (Expr (Var "y")
;;                             (Op "-")
;;                             (Expr (Var "y"))))))

;; (extra-test)