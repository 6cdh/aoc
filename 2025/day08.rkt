#lang racket

(provide solve)

(require "utils.rkt"
         data/heap
         data/union-find)

(struct Box
  (x y z)
  #:transparent)

(struct Pair
  (b1 b2 dist)
  #:transparent)

(define (solve in)
  (define boxes
    (for/vector ([line (in-list (port->lines in))])
      (define nums (map string->number (string-split line ",")))
      (match-define (list x y z) nums)
      (Box x y z)))
  (define n (vector-length boxes))

  (define (pair-dist i j)
    (define b1 (vector-ref boxes i))
    (define b2 (vector-ref boxes j))
    (+ (sqr (- (Box-x b1) (Box-x b2)))
       (sqr (- (Box-y b1) (Box-y b2)))
       (sqr (- (Box-z b1) (Box-z b2)))))

  (define pairs
    (for*/vector ([j (in-range n)]
                  [i (in-range j)])
      (Pair i j (pair-dist i j))))
  (define sorted-pairs (vector-parallel-sort pairs < #:key Pair-dist))

  (define circuits (build-vector n uf-new))
  ; In part 1, the sample connects 10 times, but the real input connects 1000 times.
  ; try to determine it from the size of the input.
  ; it would be wrong for hand-crafted data.
  (define try-connect
    (if (<= n 100)
        ; for sample
        10
        ; for real input
        1000))

  (for ([i (in-range try-connect)])
    (define p (vector-ref sorted-pairs i))
    (uf-union! (vector-ref circuits (Pair-b1 p)) (vector-ref circuits (Pair-b2 p))))

  (define circuit-counter (make-hash))
  (for ([uf (in-vector circuits)])
    (hash-update! circuit-counter (uf-find uf) add1 0))
  (define sizes (map cdr (hash->list circuit-counter)))
  (define sorted-sizes (sort sizes >))
  (define ans1 (apply * (take sorted-sizes 3)))

  (define ans2
    (let connect ([pi try-connect]
                  [circuit-count (length sizes)])
      (match-define (Pair i j dist) (vector-ref sorted-pairs pi))
      (define ufi (vector-ref circuits i))
      (define ufj (vector-ref circuits j))
      (cond [(not (uf-same-set? ufi ufj))
             (uf-union! ufi ufj)
             (if (= circuit-count 2)
                 (* (Box-x (vector-ref boxes i))
                    (Box-x (vector-ref boxes j)))
                 (connect (add1 pi) (sub1 circuit-count)))]
            [else
             (connect (add1 pi) circuit-count)])))

  (values ans1 ans2))

