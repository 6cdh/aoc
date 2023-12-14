#lang racket

(require "lib.rkt")

(define (read-input)
  (for/list ([mirror (read-and-split-in-empty-line)])
    (map string->list (read-lines (open-input-string mirror)))))

(define (reflect? rows above diffs)
  (define below (- (length rows) above))
  (define min-len (min above below))
  (define skip-above (- above min-len))
  (= diffs
     (for/sum ([above-rows (sublist rows skip-above min-len)]
               [below-rows (reverse (sublist rows above min-len))])
       (for/count ([v1 above-rows]
                   [v2 below-rows])
         (not (char=? v1 v2))))))

(define (find-reflect-line-rows name rows diffs)
  (for/first ([above-rows (in-range 1 (length rows))]
              #:when (reflect? rows above-rows diffs))
    (list name above-rows)))

(define (find-reflect-line rows diffs)
  (or (find-reflect-line-rows 'row rows diffs)
      (find-reflect-line-rows 'col (reverse-2d-list rows) diffs)))

(define (solve mirrors diffs)
  (for/sum ([mirror mirrors])
    (match (find-reflect-line mirror diffs)
      [(list 'row above) (* 100 above)]
      [(list 'col left) left])))

(define (main)
  (define mirrors (read-input))
  (println (solve mirrors 0))
  (println (solve mirrors 1)))

(main)
