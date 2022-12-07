#lang racket

(require "lib.rkt")

(define (priority c)
  (if (char-lower-case? c)
      ($ (char->integer c) - (char->integer #\a) + 1)
      ($ (char->integer c) - (char->integer #\A) + 27)))

(define (puzzle1 line)
  (let* ([n (string-length line)]
         [half1 (substring line 0 (/ n 2))]
         [half2 (substring line (/ n 2))])
    (~> (set-intersect (string->set half1) (string->set half2))
        (set->list %)
        (car %)
        (priority %))))

(define (puzzle2 group)
  (~> (map string->set group)
      (apply set-intersect %)
      (set->list %)
      (car %)
      (priority %)))

(define (day03)
  (let ([lines (read-lines)])
    (println (sum (map puzzle1 lines)))
    (println (sum (map puzzle2 (chunks 3 lines))))))

(time (day03))



