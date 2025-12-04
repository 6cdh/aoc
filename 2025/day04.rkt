#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define diagram (read-vector2d in))
  (define-values (m n) (vector2d-size diagram))
  (define init-papers
    (for*/list ([i (in-range m)]
                [j (in-range n)]
                [pos (in-value (Position i j))]
                #:when (removable? diagram pos))
      pos))
  (define ans1 (length init-papers))
  (define ans2 (bfs init-papers diagram))
  (values ans1 ans2))

(define (paper? diagram pos)
  (eq? #\@ (vector2d-ref diagram pos)))

(define (removable? diagram pos)
  (and (paper? diagram pos)
       (< (count-papers-around diagram pos) 4)))

(define (count-papers-around diagram pos)
  (for/sum ([adj (in-list (positions-around pos diagram))]
            #:when (paper? diagram adj))
    1))

(define (positions-around pos diagram)
  (define-values (m n) (vector2d-size diagram))
  (match-define (Position i j) pos)
  (for*/list ([di (in-inclusive-range -1 1)]
              [dj (in-inclusive-range -1 1)]
              [i1 (in-value (+ i di))]
              [j1 (in-value (+ j dj))]
              #:when (and (< -1 i1 m)
                          (< -1 j1 n)
                          (not (and (= i1 i) (= j1 j)))))
    (Position i1 j1)))

(define (bfs init-papers diagram)
  (define removed-count 0)

  (define (remove! pos)
    (set! removed-count (add1 removed-count))
    (vector2d-set! diagram pos #\.))

  (define (bfs-rec papers)
    (when (not (empty? papers))
      (define next-round
        (for*/list ([paper (in-list papers)]
                    [adj (in-list (positions-around paper diagram))]
                    #:when (removable? diagram adj))
          (remove! adj)
          adj))
      (bfs-rec next-round)))

  (for-each remove! init-papers)
  (bfs-rec init-papers)
  removed-count)

