#lang racket

(provide solve)

(require "utils.rkt")

(struct Tree
  (x y presents)
  #:transparent)

(define (solve in)
  (define lines (port->lines in))
  (define parts (list-split lines ""))
  (define presents (map rest (drop-right parts 1)))
  (define trees (map parse-tree (last parts)))
  (define ans1
    (for/sum ([tree trees])
      (define need-space (apply + (Tree-presents tree)))
      (define space (* (quotient (Tree-x tree) 3)
                       (quotient (Tree-y tree) 3)))
      (if (>= space need-space) 1 0)))
  (values ans1 (void)))

(define (parse-tree str)
  (match-define (list x y presents ...)
                (read-sep-numbers str #px"[x: ]"))
  (Tree x y presents))
