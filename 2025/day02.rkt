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
  ; (define ans1 (parallel-run (sum-ranges range-list (invalid-id/string? even?))))
  ; (define ans2 (parallel-run (sum-ranges range-list (invalid-id/string? (curryr >= 2)))))
  (define ans1 (parallel-run (count-invalid-ids-in-ranges range-list 2)))
  (define ans2 (parallel-run (count-invalid-ids-in-ranges range-list +inf.0)))
  (values (thread-wait ans1) (thread-wait ans2)))

;; == regular expressions (2 seconds) ==

;; using regular expressions.
;; slower but simpler
(define ((invalid-id/regex? pattern) id)
  (regexp-match-exact? pattern (number->string id)))

;; == manual string matching (200 ms) ==

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

;; == generate all invalid ids and query (40 ms) ==

(define (generate-all-invalid-ids digits max-repeat)
  (for*/list ([width (in-inclusive-range 1 (quotient digits 2))]
              [repeat (in-inclusive-range 2 (min max-repeat (quotient digits width)))]
              [num (in-range (expt 10 (sub1 width)) (expt 10 width))])
    (compose-id num width repeat)))

(define (compose-id num width repeat)
  (for/fold ([id num])
            ([_ (in-range 1 repeat)])
    (+ (* id (expt 10 width)) num)))

(define (count-invalid-ids-in-ranges range-list max-repeat)
  (define max-digits
    (for/max 0 ([range (in-list range-list)])
      (max (decimal-digits (Range-start range)) (decimal-digits (Range-end range)))))
  (define all-invalid-ids
    (~> ids (generate-all-invalid-ids max-digits max-repeat)
        (remove-duplicates ids)
        (sort ids <)
        (list->vector ids)))
  (define range-sum (vector-range-sum-querier all-invalid-ids))
  (define n (vector-length all-invalid-ids))
  (for/sum ([range (in-list range-list)])
    (define start (vector-binary-search-first all-invalid-ids (curryr >= (Range-start range))))
    (define end (vector-binary-search-last all-invalid-ids (curryr <= (Range-end range))))
    ;; throw an error if `start` or `end` not found instead of return wrong answer.
    (range-sum start end)))

