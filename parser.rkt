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
    (valid-parse basic-grammar tokens parse-hash))


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
                        (hash-set! parse-hash (list i 1 j) (parse-node true pj ai))
                        '()) ;; Side-effect free else clause.
                    (set! j (+ j 1))))
            (set! i (+ i 1)))))

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
                    (if (and (parse-node-boolean (hash-ref parse-hash (list j k right-first-index)
                                                                      (parse-node false "" ""))) ; Default false
                             (parse-node-boolean (hash-ref parse-hash (list (+ j k) (- i k) right-second-index)
                                                                      (parse-node false "" "")))) ; Default false
                        (hash-set! parse-hash (list j i (production-index pj)) (parse-node true pj (list-ref tokens (- j 1))))
                        '())))    ;; Side-effect free else clause.
            '())))                ;; Side-effect free else clause.

(define (unary-parse i j basic-grammar tokens parse-hash)
    ;; Handle RA -> RB style rules.
    (for/list ([pj (grammar-production-list basic-grammar)])
        (if (and (not (production-is-unit pj)) (eq? (production-right-second pj) ""))
            (for/list ([right-first-index (lookup-in-hash basic-grammar (production-right-first pj))])
                (if (parse-node-boolean (hash-ref parse-hash (list j i right-first-index) (parse-node false "" "")))
                    (hash-set! parse-hash (list j i (production-index pj)) (parse-node true pj (list-ref tokens (- j 1))))
                    '()))      ;; Side-effect free else clause.
        '())))                 ;; Side-effect free else clause.

(define (valid-parse basic-grammar tokens parse-hash)
    ;; If any of P[1,n,x] is true (x is iterated over the set s, where s are all the indices for Rs)
    (let ([is-member false] [start-index 0])
        (for ([i (in-range 1 (length (grammar-production-list basic-grammar)))])
            (set! is-member (or is-member (hash-ref parse-hash (list 1 (length tokens) i) false)))
            (if (and (not (= start-index 0)) is-member)
                (set! start-index i)
                '()))
        (if is-member
            (list start-index parse-hash)
            false)))

(define (lookup-in-hash basic-grammar production-element)
    (hash-ref (grammar-rule-hash basic-grammar) production-element '()))

(define (print-hash hash)
    (for ([(key value) (in-hash hash)])
        (printf "Key: ~a\n" key)))



;; Test parser with simple grammar
(define g1 (production false 1 "Expr" "Var" "OpExpr"))
(define g2 (production false 2 "OpExpr" "Op" "Expr"))
(define g3 (production false 3 "Expr" "Var" ""))
(define g4 (production true 4 "Var" "x" ""))
(define g5 (production true 5 "Var" "y" ""))
(define g6 (production true 6 "Op" "+" ""))
(define g7 (production true 7 "Op" "*" ""))
(define g-hash (make-hash))
(hash-set! g-hash "Expr" (list 1 3))
(hash-set! g-hash "OpExpr" (list 2))
(hash-set! g-hash "Var" (list 4 5))
(hash-set! g-hash "Op" (list 6 7))
(define basic-grammar (grammar (list g1 g2 g3 g4 g5 g6 g7) g-hash))

(parse basic-grammar (list "x" "+" "y" "*" "y"))


