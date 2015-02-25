#lang racket

;; Provide other functions needed to work with grammars.
(provide define-grammar)
(provide (struct-out production))
(provide (struct-out grammar))

;; Definitions of the grammar data structure, as well as a macro to
;; make it easy to define grammars.
(struct grammar (production-list rule-hash))
(struct production (is-unit index left right-first right-second))

;; Write "define-grammar" that makes it easy to define
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
;; You can transform these rules into Chomsky Normal Form, which the
;; CYK parser can understand.
(define-syntax define-grammar
  (syntax-rules (->)
    [(define-grammar name ((binary-nonterminal -> left right) ...) ((unary-nonterminal -> terminal) ...))
        (define name
            (let ()
                (define counter 0)
                (define (count)
                    (set! counter (+ 1 counter))
                    counter)

                (define g-hash (make-hash))
                (begin (hash-set! g-hash (symbol->string 'binary-nonterminal) '()) ...)
                (begin (hash-set! g-hash (symbol->string 'unary-nonterminal) '()) ...)

                (define prod-list '())
                (begin
                    (let ((next (count)))
                        (hash-set! g-hash (symbol->string 'binary-nonterminal)
                                          (cons next (hash-ref g-hash (symbol->string 'binary-nonterminal))))
                    (set! prod-list (cons (production false next (symbol->string 'binary-nonterminal)
                                                                 (symbol->string 'left) (symbol->string 'right))
                                                                                                    prod-list)))
                ...)
                (begin
                    (let ((next (count)))
                        (hash-set! g-hash (symbol->string 'unary-nonterminal)
                                          (cons next (hash-ref g-hash (symbol->string 'unary-nonterminal))))
                    (set! prod-list (cons (production (string? 'terminal) next (symbol->string 'unary-nonterminal)
                                                      (if (symbol? 'terminal) (symbol->string 'terminal) 'terminal) "")
                                                                                                           prod-list)))
                ...)
                (grammar (reverse prod-list) g-hash)))]))
