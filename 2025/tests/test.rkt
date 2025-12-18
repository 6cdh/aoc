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
           (prefix-in day03: "../day03.rkt")
           (prefix-in day04: "../day04.rkt")
           (prefix-in day05: "../day05.rkt")
           (prefix-in day06: "../day06.rkt")
           )

  (test-case "day01"
    (aoc-check "day01/example.txt" day01:solve 3 6)
    (aoc-check "day01/input.txt" day01:solve 997 5978))

  (test-case "day02"
    (aoc-check "day02/example.txt" day02:solve 1227775554 4174379265)
    (aoc-check "day02/input.txt" day02:solve 23039913998 35950619148))

  (test-case "day03"
    (aoc-check "day03/example.txt" day03:solve 357 3121910778619)
    (aoc-check "day03/input.txt" day03:solve 17095 168794698570517))

  (test-case "day04"
    (aoc-check "day04/example.txt" day04:solve 13 43)
    (aoc-check "day04/input.txt" day04:solve 1533 9206))

  (test-case "day05"
    (aoc-check "day05/example.txt" day05:solve 3 14)
    (aoc-check "day05/input.txt" day05:solve 773 332067203034711))

  (test-case "day06"
    (aoc-check "day06/example.txt" day06:solve 4277556 3263827)
    (aoc-check "day06/input.txt" day06:solve 3785892992137 7669802156452))
  )

