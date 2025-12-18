#lang racket

(provide solve)

(require "utils.rkt")

(struct Range
  (start end)
  #:transparent)

(define (solve in)
  (define input-line (first (port->lines in)))
  (define ranges
    (for/list ([str-range (string-split input-line ",")])
      (match-define (list start end) (string-split str-range "-"))
      (Range (string->number start) (string->number end))))
  ; (define ans1 (parallel-run (sum-ranges ranges (invalid-id/regex? #px"^(.+?)(\\1)$"))))
  ; (define ans2 (parallel-run (sum-ranges ranges (invalid-id/regex? #px"^(.+?)(\\1)+$"))))
  ; (values (thread-wait ans1) (thread-wait ans2))

  ; (define ans1 (parallel-run (sum-ranges ranges (invalid-id/string? even?))))
  ; (define ans2 (parallel-run (sum-ranges ranges (invalid-id/string? (curryr >= 2)))))
  ; (values (thread-wait ans1) (thread-wait ans2))

  ; (define ans1 (parallel-run (generate-and-query-ids ranges 2)))
  ; (define ans2 (parallel-run (generate-and-query-ids ranges +inf.0)))
  ; (values (thread-wait ans1) (thread-wait ans2))

  (define ans1 (sum-ranges/math ranges 2))
  (define ans2 (sum-ranges/math ranges +inf.0))
  (values ans1 ans2))

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

(define (sum-ranges ranges invalid-id?)
  (for/sum ([range (in-list ranges)])
    (sum-range range invalid-id?)))

(define (sum-range range invalid-id?)
  (for/sum ([id (in-inclusive-range (Range-start range) (Range-end range))]
            #:when (invalid-id? id))
    id))

(define (max-repeat-times id)
  (define str (number->string id))
  (define str-len (string-length str))
  (or (for/first ([prefix-len (in-inclusive-range 1 (quotient str-len 2))]
                  #:when (and (zero? (modulo str-len prefix-len))
                              (string-repeating? str prefix-len)))
        (quotient str-len prefix-len))
      1))

(define (string-repeating? str prefix-len)
  (define str-len (string-length str))
  (for*/and ([i (in-range 0 prefix-len)]
             [j (in-range i str-len prefix-len)])
    (char=? (string-ref str i) (string-ref str j))))

;; == generate all invalid ids and query (15 ms) ==

(define (generate-repeating-ids max-digits max-repeat)
  (for*/list ([base-width (in-inclusive-range 1 (quotient max-digits 2))]
              [repeat-count (in-inclusive-range 2 (min max-repeat (quotient max-digits base-width)))]
              [base-num (in-range (expt 10 (sub1 base-width)) (expt 10 base-width))])
    (compose-id base-num base-width repeat-count)))

(define (compose-id base-num base-width repeat-count)
  (for/fold ([result-id base-num])
            ([_ (in-range 1 repeat-count)])
    (+ (* result-id (expt 10 base-width)) base-num)))

(define (generate-and-query-ids ranges max-repeat)
  (define max-digits
    (for/max 0 ([range (in-list ranges)])
      (max (decimal-digits-count (Range-start range))
           (decimal-digits-count (Range-end range)))))
  (define all-invalid-ids
    (~> ids (generate-repeating-ids max-digits max-repeat)
        (sort ids <)
        (remove-duplicates-in-sorted-list ids)
        (list->vector ids)))
  (define range-sum-querier (vector-range-sum-querier all-invalid-ids))
  (for/sum ([range (in-list ranges)])
    (define start-idx (vector-binary-search-first all-invalid-ids (curryr >= (Range-start range))))
    (define end-idx (vector-binary-search-last all-invalid-ids (curryr <= (Range-end range))))
    ;; throw an error if `start-idx` or `end-idx` not found instead of return wrong answer.
    (range-sum-querier start-idx end-idx)))

;; == Closed-Form (0 ms) ==

(define (sum-ranges/math ranges max-repeats)
  (for/sum ([range (in-list ranges)])
    (sum-gauss-k range max-repeats)))

;; For part 1,
;; each invalid id `n` can be expressed as
;; n = u * 10^d + u = u * (10^d + 1)
;; where `u` is its unit number, `d` is the decimal digits count of `n`.
;;
;; And the relation between `n` and `d` is: 10^(d-1) <= u <= 10^d - 1 [equation 1].
;;
;; For a given range [start, end], we enumerate all digits `d` that
;; digits(start) <= 2 * d <= digits(end)
;; So, `d`, `start`, `end` is known, we need to know all `u`
;; that makes `n` in the range [start, end], then sum all `n`.
;;
;; Let m = 10^d + 1, then n = u * m, and
;; start <= u * m <= end
;; =>
;; start / m <= u <= end / m [equation 2]
;;
;; Using [equation 1] and [equation 2], we have
;; max(start / m, 10^(d-1)) <= u <= min(end / m, 10^d - 1)
;; This is the only limit of `u`.
;;
;; So we can calculate the smallest u `u_low` and the largest u `u_high` in O(1) time.
;; All sum of `n` is (u_low + (u_low + 1) + (u_low + 2) + ... + u_high) * m
(define (sum-gauss-2 range)
  (match-define (Range start end) range)
  (define min-digits (quotient (decimal-digits-count start) 2))
  (define max-digits (quotient (decimal-digits-count end) 2))
  (for/sum ([d (in-inclusive-range min-digits max-digits)])
    (define m (+ (expt 10 d) 1))
    (define u-low (max (exact-ceiling (/ start m)) (expt 10 (sub1 d))))
    (define u-high (min (exact-floor (/ end m)) (sub1 (expt 10 d))))
    (* m (sum-ints u-low u-high))))

(define (sum-ints from to)
  (/ (* (+ from to) (max 0 (- to from -1))) 2))

;; Part 2 is a general version of part 1,
;; the repeating times is a variable `k` and k >= 2.
;; In part 1, k = 2.
;;
;; In this case,
;; n = u + u * 10^d + u * 10^(2 * d) + u * 10^(3 * d) + ... u * 10^(p * d)
;; =>
;; n = u * (1 + 10^d + 10^(2 * d) + ... + 10^(p * d))
;; =>
;; n = u * (10^(0 * d) + 10^(1 * d) + ... + 10^(p * d))
;; =>
;; n = u * (1 - 10^(d * (p + 1))) / (1 - 10^d)
;; here p >= 1
;;
;; Transform this formula a bit, we have
;; n = u * (10^(d * k) - 1) / (10^d - 1)
;; for the unit `u` and its digits `d`, and repeats `k` times, k >= 2.
;; The total digits of `n` is d * k.
;;
;; Like part 1, for a range [start, end], we scan each combinations of `d` and `k`.
;; So `d` and `k` is known.
;; Let m = (10^(d * k) - 1) / (10^d - 1), then m is known too.
;; Then we have the limit of `u`:
;; start / m <= u <= end / m
;; and
;; 10^(d-1) <= u <= 10^d - 1
;;
;; The later steps are the same as part 1.
;; To avoid count a number multiple times, for example, 222222,
;; use a hashset to record seen numbers.
;;
;; Finally, the solution of part 2 also works for part 1.
(define (sum-gauss-k range max-repeats)
  (match-define (Range start end) range)
  (define min-digits (decimal-digits-count start))
  (define max-digits (decimal-digits-count end))
  (for*/sum ([l (in-inclusive-range min-digits max-digits)]
             [seen (in-value (mutable-set))]
             [k (in-inclusive-range 2 (min max-repeats l))]
             #:when (zero? (modulo l k))
             [d (in-value (quotient l k))])
    (define m (/ (- (expt 10 l) 1)
                 (- (expt 10 d) 1)))
    (define u-low (max (exact-ceiling (/ start m)) (expt 10 (sub1 d))))
    (define u-high (min (exact-floor (/ end m)) (sub1 (expt 10 d))))

    (for*/sum ([u (in-inclusive-range u-low u-high)]
               [n (in-value (* u m))]
               #:when (not (set-member? seen n)))
      (set-add! seen n)
      n)))

