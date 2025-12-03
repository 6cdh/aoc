#lang racket

(provide parallel-run
         ~>
         binary-search-first
         vector-binary-search-first
         binary-search-last
         vector-binary-search-last
         vector-range-sum-querier
         vector-range-sum
         decimal-digits
         for/max)

(define-syntax-rule (parallel-run expr)
  (thread #:pool 'own #:keep 'results
          (lambda () expr)))

(define-syntax-rule (~> name exprs ...)
  (let* ([name exprs] ...)
    name))

;; for a range [beg ... end],
;; apply `pred` to get a list of booleans:
;; [#f ... #t ...]
;; binary-search-first returns the first index that is #t,
;; if not found, return #f.
;; It won't work if the range does not match this pattern.
;; Require beg <= end.
(define (binary-search-first beg end pred)
  (let loop ([from beg] [to end])
    (define mid (quotient (+ from to) 2))
    (cond [(= from to) (if (pred from) from #f)]
          [(pred mid) (loop from mid)]
          [else (loop (add1 mid) to)])))

(define (vector-binary-search-first vec pred)
  (define n (vector-length vec))
  (if (= n 0)
      #f
      (binary-search-first 0 (sub1 n) (λ (i) (pred (vector-ref vec i))))))

;; for a range [beg ... end],
;; apply `pred` to get a list of booleans:
;; [#t ... #f ...]
;; binary-search-last returns the last index that is #t,
;; if not found, return #f.
;; It won't work if the range does not match this pattern.
;; Require beg <= end.
(define (binary-search-last from to pred)
  (define first-not (binary-search-first from to (compose not pred)))
  (cond [(false? first-not) to]
        [(= first-not from) #f]
        [else (sub1 first-not)]))

(define (vector-binary-search-last vec pred)
  (define n (vector-length vec))
  (if (= n 0)
      #f
      (binary-search-last 0 (sub1 n) (λ (i) (pred (vector-ref vec i))))))

;; Return a function that calculates range sum of `vec` in range `[start, end]`.
(define (vector-range-sum-querier vec)
  (define n (vector-length vec))
  (define prefix-sum (make-vector n 0))
  (vector-set! prefix-sum 0 (vector-ref vec 0))
  (for ([i (in-range 1 n)])
    (vector-set! prefix-sum i (+ (vector-ref vec i) (vector-ref prefix-sum (sub1 i)))))
  (λ (start end)
    (- (vector-ref prefix-sum end)
       (if (= start 0)
           0
           (vector-ref prefix-sum (sub1 start))))))

(define (vector-range-sum vec start end)
  (for/sum ([i (in-inclusive-range start end)])
    (vector-ref vec i)))

(define (decimal-digits d)
  (ceiling (log d 10)))

(define-syntax-rule
  (for/max init clauses
           body ...
           last-expr)
  (for/fold ([ans init])
            clauses
    body ...
    (max ans last-expr)))
