#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define grid (read-vector2d in))
  (define-values (rows cols) (vector2d-size grid))
  (define init-papers
    (for*/list ([r (in-range rows)]
                [c (in-range cols)]
                [pos (in-value (Position r c))]
                #:when (removable? grid pos))
      pos))
  (define ans1 (length init-papers))
  (define ans2 (simulate init-papers grid))
  (values ans1 ans2))

(define (paper? grid pos)
  (eq? #\@ (vector2d-ref grid pos)))

(define (removable? grid pos)
  (and (paper? grid pos)
       (< (count-papers-around grid pos) 4)))

(define (count-papers-around grid pos)
  (length (papers-around grid pos)))

(define (papers-around grid pos)
  (define-values (rows cols) (vector2d-size grid))
  (for*/list ([dr (in-inclusive-range -1 1)]
              [dc (in-inclusive-range -1 1)]
              #:when (not (= dr dc 0))
              [nr (in-value (+ (Position-row pos) dr))]
              [nc (in-value (+ (Position-col pos) dc))]
              #:when (and (< -1 nr rows) (< -1 nc cols))
              [npos (in-value (Position nr nc))]
              #:when (paper? grid npos))
    npos))

(define (simulate init-papers grid)
  (define total-removed 0)

  (define (remove! pos)
    (set! total-removed (add1 total-removed))
    (vector2d-set! grid pos #\.))

  (define (bfs papers)
    (when (not (empty? papers))
      (define next-round
        (for*/list ([paper (in-list papers)]
                    [adj (in-list (papers-around grid paper))]
                    #:when (removable? grid adj))
          (remove! adj)
          adj))
      (bfs next-round)))

  (for-each remove! init-papers)
  (bfs init-papers)
  total-removed)

