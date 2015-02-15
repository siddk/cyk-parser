#lang racket

;; Provide other functions needed to work with parse trees.
(provide parse-tree->list)
(provide (struct-out parse-node))

;; Definitions of the parse tree data structure.

;; Create a representation for parse trees.
(struct parse-node (boolean nodeA nodeB))

;; Implement parse-tree->list.  An example of its use can
;; be found in tests.rkt.
(define (parse-tree->list hash)
  '())