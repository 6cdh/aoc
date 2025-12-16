#lang racket

(provide aoc-check)

(require rackunit)

(define-syntax-rule (aoc-check input solve exp-ans1 exp-ans2)
    (let ()
      (define-values (ans1 ans2) (solve (open-input-string input)))
      (check-equal? ans1 exp-ans1 "Part 1 failed.")
      (check-equal? ans2 exp-ans2 "Part 2 failed.")))
