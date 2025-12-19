#lang racket

(provide solve)

(require "utils.rkt"
         data/heap
         data/union-find)

(struct Box
  (x y z uf)
  #:transparent)

(struct Pair
  (b1 b2 dist)
  #:transparent)

;; omit sqrt for performance, and it does not affect the result
(define (pair-dist b1 b2)
  (+ (sqr (- (Box-x b1) (Box-x b2)))
     (sqr (- (Box-y b1) (Box-y b2)))
     (sqr (- (Box-z b1) (Box-z b2)))))

(define (solve in)
  (define boxes
    (for/vector ([line (in-list (port->lines in))]
                 [i (in-naturals)])
      (match-define (list x y z) (read-sep-numbers line ","))
      (Box x y z (uf-new i))))
  (define n (vector-length boxes))

  (define sorted-pairs
    (for*/vector ([j (in-range n)]
                  [i (in-range j)])
      (define b1 (vector-ref boxes i))
      (define b2 (vector-ref boxes j))
      (Pair b1 b2 (pair-dist b1 b2))))
  (vector-parallel-sort! sorted-pairs < #:key Pair-dist)

  ; In part 1, the sample connects 10 times, but the real input connects 1000 times.
  ; try to determine it from the size of the input.
  ; it would be wrong for hand-crafted data.
  (define part1-connects
    (if (<= n 100)
        ; for sample
        10
        ; for real input
        1000))

  (for ([i (in-range part1-connects)]
        [p (in-vector sorted-pairs)])
    (uf-union! (Box-uf (Pair-b1 p)) (Box-uf (Pair-b2 p))))

  (define-values (biggest3-sizes circuits-count) (current-circuits-statistics boxes))
  (define ans1 (apply * biggest3-sizes))
  (define ans2 (continue-connect part1-connects circuits-count sorted-pairs))
  (values ans1 ans2))

(define (current-circuits-statistics boxes)
  (define counter
    (for/fold ([counter (hash)])
              ([box (in-vector boxes)])
      (hash-update counter (uf-find (Box-uf box)) add1 0)))
  (define sizes (map cdr (hash->list counter)))
  (define sorted-sizes (sort sizes >))
  (values (take sorted-sizes 3) (hash-count counter)))

;; continue connect from the pair at index `i`
(define (continue-connect i circuit-count sorted-pairs)
  (match-define (Pair b1 b2 dist) (vector-ref sorted-pairs i))
  (cond [(uf-same-set? (Box-uf b1) (Box-uf b2))
         (continue-connect (add1 i) circuit-count sorted-pairs)]
        [else
         (uf-union! (Box-uf b1) (Box-uf b2))
         (if (= circuit-count 2)
             (* (Box-x b1) (Box-x b2))
             (continue-connect (add1 i) (sub1 circuit-count) sorted-pairs))]))

