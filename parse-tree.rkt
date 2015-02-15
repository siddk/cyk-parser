#lang racket
(require "grammar.rkt")

;; Provide other functions needed to work with parse trees.
(provide (struct-out parse-node))
(provide parse-tree->list)
(provide print-hash)
(provide tokens->substring)

;; Definitions of the parse tree data structure.

;; Create a representation for parse trees.
(struct parse-node (boolean production start-token num-tokens right-first right-second))

;; Implement parse-tree->list.  An example of its use can
;; be found in tests.rkt.
(define (parse-tree->list tokens node prefix)
    (let ([new-prefix (string-append prefix "   ")])
        (printf "~a ~a ~a\n" prefix (production-left (parse-node-production node)) (tokens->substring tokens node))
        (if (> (parse-node-num-tokens node) 1)
            (begin
                (parse-tree->list tokens (parse-node-right-first node) new-prefix)
                (if (not (eq? (parse-node-right-second node) ""))
                    (parse-tree->list tokens (parse-node-right-second node) new-prefix)
                    ""))
            "")))

(define (print-hash hash)
    (for ([(key value) (in-hash hash)])
        (printf "Key: ~a\n" key)))

(define (tokens->substring tokens node)
    (let ([start-token (- (parse-node-start-token node) 1)] [num-tokens (parse-node-num-tokens node)])
        (take (list-tail tokens start-token) num-tokens)))