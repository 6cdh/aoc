#lang racket

(require "lib.rkt")

(define (read-input)
  (define lines (read-lines))

  (for/fold ([lefts (hash)]
             [rights (hash)]
             #:result (list (sequence->stream (in-cycle (first lines)))
                            lefts
                            rights))
            ([line (cddr lines)])
    (match (regexp-match #px"(...) = \\((...), (...)\\)" line)
      [(list _full-match node left right)
       (values (hash-set lefts node left)
               (hash-set rights node right))])))

(define (ends-with? char)
  (λ (node) (string-suffix? node (string char))))

(define (count-steps instructions lefts rights node end?)
  (if (end? node)
      0
      (add1 (count-steps (stream-rest instructions)
                         lefts rights
                         (match (stream-first instructions)
                           [#\L (hash-ref lefts node)]
                           [#\R (hash-ref rights node)]
                           [inst (error "invalid instruction:" inst)])
                         end?))))

(define (main)
  (match-define (list instructions lefts rights) (read-input))

  ;; assume: ZZZ is always reachable from AAA by following the instructions.
  (println (count-steps instructions lefts rights "AAA" (λ (node) (string=? node "ZZZ"))))

  ;; assume: a A node can always reach a Z node by following the instructions.
  ;; def: a path is a sequence of nodes that start from a A node by following the instructions.
  ;; assume: the paths never cross.
  ;; observe and assume: For a path, the next node of the Z node is same as
  ;; the next node of the A node.
  ;; So a path is always a loop:
  ;; A-node -> k steps -> Z-node -> k steps -> Z-node, ...
  ;; this code won't work if the assumption is false.
  (println (apply lcm
                  (for/list ([from (filter (ends-with? #\A) (hash-keys lefts))])
                    (count-steps instructions lefts rights from (ends-with? #\Z))))))

(main)
