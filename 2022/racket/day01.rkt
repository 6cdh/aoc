#lang racket

(require "lib.rkt")

(define (day01)
  (let* ([input (read-string #e1e6)]
         [calories (~> (string-split input "\n\n")
                       (map (λ (pkg) (string-split pkg "\n")) %)
                       (map (λ (pkg) (map string->number pkg)) %)
                       (map sum %))]
         [sorted (sort calories >)])
    (println (car sorted))
    (println (sum (take sorted 3)))))

(time (day01))

