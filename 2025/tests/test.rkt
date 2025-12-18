#lang racket

(require rackunit
         syntax/parse
         syntax/parse/define)

(define-syntax-parser aoc-check
  [(aoc-check file solve exp-ans1 exp-ans2)
   #`(with-input-from-file file
       (λ ()
         (define-values (ans1 ans2) (solve (current-input-port)))
         #,(syntax/loc #'exp-ans1 (check-equal? ans1 exp-ans1 "Part 1 failed."))
         #,(syntax/loc #'exp-ans2 (check-equal? ans2 exp-ans2 "Part 2 failed."))))])

(module+ test
  (require (prefix-in day01: "../day01.rkt")
           (prefix-in day02: "../day02.rkt")
           )

  (test-case "day01"
    (aoc-check "day01/example.txt" day01:solve 3 6)
    (aoc-check "day01/input.txt" day01:solve 997 5978))

  (test-case "day02"
    (aoc-check "day02/example.txt" day02:solve 1227775554 4174379265)
    (aoc-check "day02/input.txt" day02:solve 23039913998 35950619148))
  )

