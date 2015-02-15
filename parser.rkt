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
                    (printf "Entering grammar/token loop ~a, ~a\n" (production-left pj) ai)
                    ;; Check if unit production
                    (if (production-is-unit pj)
                        (hash-set! parse-hash '(i 1 j) (parse-node true pj ai))
                        '()) ;; Side-effect free else clause.
                    (set! j (+ j 1))))
            (set! i (+ i 1))))
    ;; for each i = 2 to n -- Length of span
    ;;   for each j = 1 to n-i+1 -- Start of span
    ;;     for each k = 1 to i-1 -- Partition of span
    (let ([n (length tokens)])
        (for ([i (in-range 2 n)])
            (printf "Entering i loop ~a\n" i)
            (for ([j (in-range 1 (+ (- n i) 1))])
                (printf "Entering j loop ~a\n" j)
                (for ([k (in-range 1 (- i 1))])
                    (printf "Entering k loop ~a\n" k)
                    ;; for each production RA -> RB RC
                    (for/list ([pj grammar])
                        (if (not (production-is-unit pj))
                            ;; if P[j,k,B] and P[j+k,i-k,C] then set P[j,i,A] = true
                            (if (and (hash-ref parse-hash '(j k (production-right-first-index pj)) false)
                                     (hash-ref parse-hash '((+ j k) (- i k) (production-right-second-index pj)) false))
                                (hash-set! parse-hash '(j i (production-left-index pj)) (parse-node true pj (list-ref tokens i)))
                                '())   ;; Side-effect free else clause.
                            '()))      ;; Side-effect free else clause.
                    (printf "Exiting k loop ~a\n" k))
                ;; Handle RA -> RB style rules.

                (printf "Exiting j loop ~a\n" j))
            (printf "Exiting i loop ~a\n" i)))
    ;; if any of P[1,n,x] is true (x is iterated over the set s, where s are all the indices for Rs)
    (let ([is-member false] [start-index 0])
        (for ([i (in-range 1 (length grammar))])
            (printf "Entering final loop ~a\n" i)
            (set! is-member (or is-member (hash-ref parse-hash '(1 n i) false)))
            (printf "Set is-member to boolean value: ~a \n" is-member)
            (if (and (not (= start-index 0)) is-member)
                (set! start-index i)
                '()))
        (if is-member
            (list start-index parse-hash)
            false)))


;; Test parser with simple lists
(define g1 (production false "Expr" 0 "Var" 1 "" -1))
(define g2 (production true "Var" 1 "x" -1 "" -1))
(parse (list g1 g2) (list "x"))

