#lang racket

(module+ test
  (require "../day01.rkt"
           "macro.rkt")

  (define example
#<<END
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
END
    )

  (aoc-check example solve 3 6)
  (aoc-check (file->string "day01.txt") solve 997 5978))
