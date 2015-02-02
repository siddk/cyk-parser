#lang racket

(require "grammar.rkt" "parse-tree.rkt")

(provide parse)

;; CYK parser generator.
;; Input:  Grammar created according to the guidelines in grammar.rkt,
;;         List of tokens to parse
;; Output: Any valid parse tree of the tokens using the grammar, or
;;         #f if no such parse tree exists.
(define (parse grammar tokens)
  ;; TODO: Implement using CYK algorithm
  #f)