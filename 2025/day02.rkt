#lang racket

(provide solve)

(require "utils.rkt")

(struct Range
  (start end)
  #:transparent)

(define (solve in)
  (define input-line (first (port->lines in)))
  (define str-range-list (string-split input-line ","))
  (define range-list
    (for/list ([str-range str-range-list])
      (match-define (list start end) (string-split str-range "-"))
      (Range (string->number start) (string->number end))))
  ; (define ans1 (parallel-run (sum-ranges range-list (invalid-id/regex? #px"^(.+?)(\\1)$"))))
  ; (define ans2 (parallel-run (sum-ranges range-list (invalid-id/regex? #px"^(.+?)(\\1)+$"))))
  (define ans1 (parallel-run (sum-ranges range-list (invalid-id/string? even?))))
  (define ans2 (parallel-run (sum-ranges range-list (invalid-id/string? (curryr >= 2)))))
  (values (thread-wait ans1) (thread-wait ans2)))

;; using regular expressions.
;; slower but simpler
(define ((invalid-id/regex? pattern) id)
  (regexp-match-exact? pattern (number->string id)))

;; using manual string matching.
;; complex but faster (10x)
(define ((invalid-id/string? pred) id)
  (pred (max-repeat-times id)))

(define (sum-ranges range-list invalid-id?)
  (for/sum ([range (in-list range-list)])
    (sum-range range invalid-id?)))

(define (sum-range range invalid-id?)
  (for/sum ([id (in-inclusive-range (Range-start range) (Range-end range))]
            #:when (invalid-id? id))
    id))

(define (max-repeat-times id)
  (define str (number->string id))
  (define n (string-length str))
  (or (for/first ([pat-size (in-inclusive-range 1 (quotient n 2))]
                  #:when (and (= 0 (modulo n pat-size))
                              (repeat? str pat-size)))
        (quotient n pat-size))
      1))

(define (repeat? str pat-size)
  (define n (string-length str))
  (for*/and ([i (in-range 0 pat-size)]
             [j (in-range i n pat-size)])
    (char=? (string-ref str i) (string-ref str j))))

