#lang racket

(require "lib.rkt")

;; terms:
;; input: the 2d input array
;; loc: (list x y) represent location of a character in input
;; locseq: list of `loc` represent locations of a number string

(define (get-all-loc input ok?)
  (for*/list ([(line x) (in-indexed input)]
              [(c y) (in-indexed line)]
              #:when (ok? c))
    (list x y)))

;; group constinuous loc to locseq
(define (loc-list->locseq-list loc-list)
  (for/fold ([locseq-list '()]
             #:result (map reverse locseq-list))
            ([loc loc-list])
    (match* (locseq-list loc)
      [('() loc)
       (list (list loc))]
      [((list (list (list lastx lasty) _ ...) _ ...)
        (list x y))

       (if (and (= x lastx) (= y (add1 lasty)))
           (cons (cons loc (car locseq-list)) (cdr locseq-list))
           (cons (list loc) locseq-list))])))

(define (engine-schematic-symbol? char)
  (not (or (char=? char #\.) (char-numeric? char))))

(define (adjacent? loc1 loc2)
  (= 1 (max (absdiff (first loc1) (first loc2))
            (absdiff (second loc1) (second loc2)))))

(define (neighbors input loc)
  (define m (vector-length input))
  (define n (vector-length (aref input 0)))

  (for*/list ([dx (list 0 1 -1)]
              [dy (list 0 1 -1)]
              [x1 (in-value (+ (first loc) dx))]
              [y1 (in-value (+ (second loc) dy))]
              [new-loc (in-value (list x1 y1))]
              #:when (and (< -1 x1 m) (< -1 y1 n) (adjacent? new-loc loc)))
    new-loc))

(define (locseq-adjacent-symbol? input)
  (λ (locseq)
    (for*/or ([loc locseq]
              [nei (neighbors input loc)])
      (engine-schematic-symbol? (aref input (first nei) (second nei))))))

(define (locseq->number input)
  (λ (locseq)
    (list->number
     (for/list ([loc locseq])
       (aref input (first loc) (second loc))))))

(define (locseq-adjacent-loc? loc)
  (λ (locseq)
    (for/or ([loc2 locseq])
      (adjacent? loc loc2))))

(define (main)
  (define input (read-lines-as-vector2d))
  (define num-str-loc-list (get-all-loc input char-numeric?))
  (define locseq-list (loc-list->locseq-list num-str-loc-list))
  (define locseq-of-answer1 (filter (locseq-adjacent-symbol? input) locseq-list))
  (println (sum (map (locseq->number input) locseq-of-answer1)))

  (define gear-loces (get-all-loc input (λ (c) (char=? c #\*))))
  (println
   (for*/sum ([gear-loc gear-loces]
              [adj-numstr-locseq-list
               (in-value (filter (locseq-adjacent-loc? gear-loc) locseq-of-answer1))]
              #:when (= 2 (length adj-numstr-locseq-list)))
     (product (map (locseq->number input) adj-numstr-locseq-list)))))

(main)

