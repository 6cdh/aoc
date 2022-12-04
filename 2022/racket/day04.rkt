#lang racket

(require "lib.rkt")

(define (includes pairs)
  (define (inc p1 p2)
    (and (<= (first p1) (first p2))
         (<= (second p2) (second p1))))

  (let ([p1 (take pairs 2)]
        [p2 (drop pairs 2)])
    (or (inc p1 p2) (inc p2 p1))))

(define (overlaps pairs)
  (let ([p1 (take pairs 2)]
        [p2 (drop pairs 2)])
    (not (or (< (second p1) (first p2))
             (< (second p2) (first p1))))))

(define (day04)
  (let* ([lines (read-lines)]
         [pairs (map (λ (line) (~> (string-split line #px"[,-]")
                                   (map string->number %)))
                     lines)])
    (println (count includes pairs))
    (println (count overlaps pairs))))

(time (day04))


