#lang racket

(provide D
         parallel-run
         ~>
         binary-search-first
         vector-binary-search-first
         binary-search-last
         vector-binary-search-last
         vector-range-sum-querier
         vector-range-sum
         decimal-digits
         for/max
         remove-duplicates-in-sorted-list
         digit-char->number
         define/cache
         define/cache-vec
         vector-argmax-index)

(require syntax/parse/define)

(define-syntax-rule (D expr)
  (let ([res expr])
    (displayln (format "~a: ~v" 'expr res))
    res))

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

;; for some reason, `remove-duplicates` is slow.
;; `remove-duplicates-in-sorted-list` is much faster for sorted list.
(define (remove-duplicates-in-sorted-list lst [same? equal?])
  (if (empty? lst)
      lst
      (cons (car lst)
            (for/list ([leftv (in-list lst)]
                       [v (in-list (cdr lst))]
                       #:when (not (same? leftv v)))
              v))))

(define (digit-char->number c)
  (- (char->integer c) (char->integer #\0)))

(define-syntax make-array
  (syntax-rules ()
    [(_ n init)
     (build-vector n (λ _ init))]
    [(_ n args ...)
     (build-vector n (λ _ (make-array args ...)))]))

(define-syntax aref
  (syntax-rules ()
    [(_ arr) arr]
    [(_ arr i dims ...)
     (aref (vector-ref arr i) dims ...)]))

(define-syntax aset!
  (syntax-rules ()
    [(_ arr dim new-val)
     (vector-set! arr dim new-val)]
    [(_ arr dim1 dims ... new-val)
     (aset! (vector-ref arr dim1) dims ... new-val)]))

;; cache the procedure using a hash table
;; usage: just replace (define something ...) with (define/cache something ...)
(define-syntax-parser define/cache
  [(_ (fname:id args:id ...)
      body ...)
   #'(define fname
       (letrec ([cache (make-hash)]
                [fn
                 (λ (args ...)
                   body ...)]
                [fname
                 (λ (args ...)
                   (define key (list args ...))
                   (cond [(hash-has-key? cache key)
                          (hash-ref cache key)]
                         [else
                          (define val (fn args ...))
                          (hash-set! cache key val)
                          val]))])
         fname))])

;; cache the procedure using a vector
(define-syntax-parser define/cache-vec
  [(_ (fname:id args:id ...) #:vector (dims ... init)
      body ...)
   (with-syntax ([(inames ...) (generate-temporaries #'(dims ...))])
     #'(define fname
         (letrec ([cache (make-array dims ... init)]
                  [to-index (λ (args ...) (values args ...))]
                  [fn
                   (λ (args ...)
                     body ...)]
                  [fname
                   (λ (args ...)
                     (define-values (inames ...) (to-index args ...))
                     (when (equal? init (aref cache inames ...))
                       (aset! cache inames ... (fn args ...)))
                     (aref cache inames ...))])
           fname)))])

(define (vector-argmax-index proc vec [start 0] [end (vector-length vec)])
  (for/fold ([best-i #f]
             [best-v #f]
             #:result best-i)
            ([i (in-range start end)])
    (define v (proc (vector-ref vec i)))
    (if (or (false? best-v) (> v best-v))
        (values i v)
        (values best-i best-v))))
