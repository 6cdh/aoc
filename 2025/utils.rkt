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
         make-array
         aref
         aset!
         define/cache
         define/cache-vec
         vector-argmax-index
         string->vector

         (struct-out Position)
         aref
         aset!
         read-vector2d
         vector2d-size
         vector2d-ref
         vector2d-set!
         vector2d-update!
         vector2d-copy

         string-empty?
         list-splitf
         list-split
         list-transpose
         vector2d-find
         move
         move-left
         move-right
         move-up
         move-down)

(require syntax/parse/define
         racket/generator)

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
  [(_ (fname:id args:id ...)
      #:vector (dims ... init)
      #:cache (vals-to-cache ...)
      body ...)
   (with-syntax ([(inames ...) (generate-temporaries #'(dims ...))])
     #'(define fname
         (letrec ([cache (make-array dims ... init)]
                  [to-index (λ (args ...) (values vals-to-cache ...))]
                  [fn
                   (λ (args ...)
                     body ...)]
                  [fname
                   (λ (args ...)
                     (define-values (inames ...) (to-index args ...))
                     (when (equal? init (aref cache inames ...))
                       (aset! cache inames ... (fn args ...)))
                     (aref cache inames ...))])
           fname)))]
  [(_ (fname:id args:id ...)
      #:vector (dims ... init)
      body ...)
   #'(define/cache-vec (fname args ...)
       #:vector (dims ... init)
       #:cache (args ...)
       body ...)])

(define (vector-argmax-index proc vec [start 0] [end (vector-length vec)])
  (for/fold ([best-i #f]
             [best-v #f]
             #:result best-i)
            ([i (in-range start end)])
    (define v (proc (vector-ref vec i)))
    (if (or (false? best-v) (> v best-v))
        (values i v)
        (values best-i best-v))))

(define (string->vector str)
  (list->vector (string->list str)))

(struct Position
  (row col)
  #:transparent)

(define (read-vector2d in)
  (~> lines (port->lines in)
      (map string->vector lines)
      (list->vector lines)))

(define (vector2d-size vec2)
  (values (vector-length vec2)
          (vector-length (vector-ref vec2 0))))

(define (vector2d-ref vec2 pos)
  (aref vec2 (Position-row pos) (Position-col pos)))

(define (vector2d-set! vec2 pos val)
  (aset! vec2 (Position-row pos) (Position-col pos) val))

(define (vector2d-update! vec2 pos f)
  (vector2d-set! vec2 pos (f (vector2d-ref vec2 pos))))

(define (vector2d-copy vec2)
  (define-values (m n) (vector2d-size vec2))
  (for/vector ([vec (in-vector vec2)])
    (vector-copy vec)))

(define (string-empty? str)
  (= 0 (string-length str)))

;; like string-split but for list
(define (list-splitf lst pred)
  (define res
    (let loop ([cur '()]
               [lst lst])
      (match lst
        ['() (list (reverse cur))]
        [(cons x xs)
         (if (pred x)
             (cons (reverse cur) (loop '() xs))
             (loop (cons x cur) xs))])))
  (filter-not empty? res))

(define (list-split lst v)
  (list-splitf lst (λ (x) (equal? x v))))

;; transpose a list of lists
;; convert
;; [[a b]
;;  [c d]]
;; to
;; [[a c]
;;  [b d]]
(define (list-transpose lstlst)
  (apply map list lstlst))

(define (vector2d-find vec2 x)
  (define-values (m n) (vector2d-size vec2))
  (for*/first ([i (in-range m)]
               [j (in-range n)]
               #:when (equal? x (aref vec2 i j)))
    (Position i j)))

(define (move pos dir)
  (Position (+ (Position-row pos) (Position-row dir))
            (+ (Position-col pos) (Position-col dir))))

(define (move-left pos)
  (move pos (Position 0 -1)))

(define (move-right pos)
  (move pos (Position 0 1)))

(define (move-down pos)
  (move pos (Position 1 0)))

(define (move-up pos)
  (move pos (Position -1 0)))
