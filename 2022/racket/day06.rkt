#lang racket

(require "lib.rkt")

(define (detect line k)
  (for/first ([i (range k (add1 (string-length line)))]
              #:when (= k (~> (substring line (- i k) i)
                              (string->set %)
                              (set-count %))))
    i))

(define (day06)
  (let ([line (read-line)])
    (println (detect line 4))
    (println (detect line 14))))

(time (day06))

