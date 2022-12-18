#lang racket

(require "lib.rkt")

(define (read-input)
  (let ([lines (read-lines)])
    (for/set ([line lines])
      (~> (string-split line ",")
          (map string->number %)))))

(define (adjacent-cubes cube)
  (for/list ([dir '((0 0 1) (0 0 -1) (0 1 0) (0 -1 0) (1 0 0) (-1 0 0))])
    (map + cube dir)))

(define (dfs cube mins maxs cubes externals)
  (when (and (<= (first mins) (first cube) (first maxs))
             (<= (second mins) (second cube) (second maxs))
             (<= (third mins) (third cube) (third maxs))
             (not (set-member? cubes cube))
             (not (set-member? externals cube)))
    (set-add! externals cube)
    (for ([adj (adjacent-cubes cube)])
      (dfs adj mins maxs cubes externals))))

(define (day18)
  (let* ([cubes (read-input)]
         [adjs (append* (map adjacent-cubes (set->list cubes)))])
    ;; part 1
    (println (count (λ (adj) (not (set-member? cubes adj))) adjs))

    ;; part 2
    (let ([externals (mutable-set)]
          [mins (map sub1 (apply map min (set->list cubes)))]
          [maxs (map add1 (apply map max (set->list cubes)))])
      (dfs mins mins maxs cubes externals)
      (println (count (λ (adj) (set-member? externals adj)) adjs)))))

(time (day18))
