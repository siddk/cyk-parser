#lang racket

(require "grammar.rkt" "parse-tree.rkt")
(provide parse)

;; CYK parser generator.
;; Input:  Grammar created according to the guidelines in grammar.rkt,
;;         List of tokens to parse
;; Output: Any valid parse tree of the tokens using the grammar, or
;;         #f if no such parse tree exists.
(define (parse basic-grammar tokens)
    ;; Let P[n,n,r] be an array of booleans. Initialize all elements of P to false.
    (define parse-hash (make-hash))
    (parse-unit-production basic-grammar tokens parse-hash)
    (for ([j (in-range 1 (+ 1 (length tokens)))])
        (unary-parse 1 j basic-grammar tokens parse-hash))
    (parse-production basic-grammar tokens parse-hash)
    (parse-tree->list tokens (valid-parse basic-grammar tokens parse-hash) ""))


(define (parse-unit-production basic-grammar tokens parse-hash)
    ;; for each i = 1 to n
    ;;   for each unit production Rj -> ai
    ;;     set P[i,1,j] = true
    (let ([i 1])
        (for/list ([ai tokens])
            (let ([j 1])
                (for/list ([pj (grammar-production-list basic-grammar)])
                    ; (printf "Entering grammar/token loop ~a, ~a\n" (production-left pj) ai)
                    ;; Check if unit production
                    (if (and (production-is-unit pj) (eq? (production-right-first pj) ai))
                        (hash-set! parse-hash (list i 1 j) (parse-node true pj i 1 "" ""))
                        '()) ;; Side-effect free else clause.
                    (set! j (+ j 1))))
            (set! i (+ i 1)))))

(define (unary-parse i j basic-grammar tokens parse-hash)
    ;; Handle RA -> RB style rules.
    (for/list ([pj (grammar-production-list basic-grammar)])
        (if (and (not (production-is-unit pj)) (eq? (production-right-second pj) ""))
            (for/list ([right-first-index (lookup-in-hash basic-grammar (production-right-first pj))])
                (let ([first-node (hash-ref parse-hash (list j i right-first-index) default-false-node)])
                    (if (parse-node-boolean first-node)
                        (hash-set! parse-hash (list j i (production-index pj))
                                              (parse-node true pj j i first-node ""))
                        '())))      ;; Side-effect free else clause.
        '())))                      ;; Side-effect free else clause.

(define (parse-production basic-grammar tokens parse-hash)
    ; for each i = 2 to n -- Length of span
    ;   for each j = 1 to n-i+1 -- Start of span
    ;     for each k = 1 to i-1 -- Partition of span
    (let ([n (length tokens)])
        (for ([i (in-range 2 (+ n 1))])
            (for ([j (in-range 1 (+ (- n i) 2))])
                (for ([k (in-range 1 i)])
                    (binary-parse i j k basic-grammar tokens parse-hash)
                (unary-parse i j basic-grammar tokens parse-hash))))))

(define (binary-parse i j k basic-grammar tokens parse-hash)
    ;; for each production RA -> RB RC
    (for/list ([pj (grammar-production-list basic-grammar)])
        (if (and (not (production-is-unit pj)) (not (eq? (production-right-second pj) "")))
            (for/list ([right-first-index (lookup-in-hash basic-grammar (production-right-first pj))])
                (for/list ([right-second-index (lookup-in-hash basic-grammar (production-right-second pj))])
                    ;; if P[j,k,B] and P[j+k,i-k,C] then set P[j,i,A] = true
                    (let ([first-node (hash-ref parse-hash (list j k right-first-index)
                                                            default-false-node)]
                          [second-node (hash-ref parse-hash (list (+ j k) (- i k) right-second-index)
                                                             default-false-node)])
                        (if (and (parse-node-boolean first-node) (parse-node-boolean second-node))
                            (hash-set! parse-hash (list j i (production-index pj))
                                      (parse-node true pj j i first-node second-node))
                            '()))))    ;; Side-effect free else clause.
            '())))                     ;; Side-effect free else clause.

(define (valid-parse basic-grammar tokens parse-hash)
    ;; If any of P[1,n,x] is true (x is iterated over the set s, where s are all the indices for Rs)
    (let ([root-node default-false-node])
        (for ([(key value) (in-hash parse-hash)] #:when (= (length tokens) (car (cdr key))))
            (set! root-node value))
        root-node))

(define (lookup-in-hash basic-grammar production-element)
    (hash-ref (grammar-rule-hash basic-grammar) production-element '()))

(define (default-false-node) (parse-node false "" "" "" "" ""))

;; Test parser with simple grammar
; (define g1 (production false 1 "Expr" "Var" "OpExpr"))
; (define g2 (production false 2 "OpExpr" "Op" "Expr"))
; (define g3 (production false 3 "Expr" "Var" ""))
; (define g4 (production true 4 "Var" "x" ""))
; (define g5 (production true 5 "Var" "y" ""))
; (define g6 (production true 6 "Op" "+" ""))
; (define g7 (production true 7 "Op" "*" ""))
; (define g-hash (make-hash))
; (hash-set! g-hash "Expr" (list 1 3))
; (hash-set! g-hash "OpExpr" (list 2))
; (hash-set! g-hash "Var" (list 4 5))
; (hash-set! g-hash "Op" (list 6 7))
; (define basic-grammar (grammar (list g1 g2 g3 g4 g5 g6 g7) g-hash))

; (parse basic-grammar (list "x" "+" "y" "*" "y"))
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

(arith-test)

