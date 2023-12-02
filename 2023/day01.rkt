#lang racket

(require "lib.rkt")

(define (string-search-all-matches str search-set)
  (define n (string-length str))
  (for*/list ([i (in-range 0 n)]
              [substr (in-value (substring str i))]
              [search-str search-set]
              #:when (string-prefix? substr search-str))
    search-str))

(define (solve strs spells)
  (for/sum ([str strs])
    (define all-matches (string-search-all-matches str (hash-keys spells)))
    (+ (* 10 (hash-ref spells (first all-matches)))
       (hash-ref spells (last all-matches)))))

(define (main)
  (define lines (read-lines))
  (println (solve lines
                  (hash "0" 0
                        "1" 1 "2" 2 "3" 3
                        "4" 4 "5" 5 "6" 6
                        "7" 7 "8" 8 "9" 9)))
  (println (solve lines
                  (hash "one" 1 "two" 2 "three" 3
                        "four" 4 "five" 5 "six" 6
                        "seven" 7 "eight" 8 "nine" 9
                        "0" 0
                        "1" 1 "2" 2 "3" 3
                        "4" 4 "5" 5 "6" 6
                        "7" 7 "8" 8 "9" 9))))

(main)
