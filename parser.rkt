#lang racket

(require "grammar.rkt" "parse-tree.rkt")

(provide parse)
; (struct production (is-unit left right-first right-second))
; (struct parse-node (boolean nodeA nodeB))

;; CYK parser generator.
;; Input:  Grammar created according to the guidelines in grammar.rkt,
;;         List of tokens to parse
;; Output: Any valid parse tree of the tokens using the grammar, or
;;         #f if no such parse tree exists.
(define (parse grammar tokens)
  ;; Let P[n,n,r] be an array of booleans. Initialize all elements of P to false.
  (define parse-hash (make-hash))
  ;; for each i = 1 to n
  ;;   for each unit production Rj -> ai
  ;;     set P[i,1,j] = true
  (let ([i 1])
    (for/list ([ai tokens])
        (let ([j 1])
            (for/list ([pj grammar])
                ;; Check if unit production
                (if (production-is-unit pj)
                    (hash-set! parse-hash '(i 1 j) (parse-node true pj ai))
                    '()) ;; Side-effect free else clause.
                (set! j (+ j 1))))
        (set! i (+ i 1))))
  (printf "~a\n" (parse-node-nodeB (hash-ref parse-hash '(i 1 j)))))

;; Test parser with simple lists
(define m1 (production true "Var" "x" '()))
(define m2 (production false "Expr" "Var" "OpExpr"))
(parse (list m1 m2) (list "a" "b" "c"))

