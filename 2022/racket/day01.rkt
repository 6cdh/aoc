#lang racket

(require "lib.rkt")

(define (day01)
  (let ([lines (read-lines)]
        [calories '(0)])
    (for ([line lines])
      (if (equal? line "")
          (set! calories (cons 0 calories))
          (set! calories (cons (+ (car calories)
                                  (string->number line))
                               (cdr calories)))))
    (let ([sorted (sort calories >)])
      (println (car sorted))
      (println (sum (take sorted 3))))))

(time (day01))

