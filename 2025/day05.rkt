#lang racket

(provide solve)

(require data/integer-set
         "utils.rkt")

(struct Range
  (start end)
  #:transparent)

(define (solve in)
  (define-values (id-ranges ids) (parse in))
  (define ans1
    (for/sum ([id (in-list ids)]
              #:when (in-ranges? id id-ranges))
      1))
  (define ans2 (integer-set-method id-ranges))
  (values ans1 ans2))

(define (in-ranges? id id-ranges)
  (for/or ([r id-ranges])
    (<= (Range-start r) id (Range-end r))))

(define (parse in)
  (define lines (port->lines in))
  (match-define (list ranges-str ids-str) (list-split lines ""))
  (define id-ranges
    (for/list ([r ranges-str])
      (match-define (pregexp #px"(\\d+)-(\\d+)" (list _ start end)) r)
      (Range (string->number start) (string->number end))))
  (define ids (map string->number ids-str))
  (values id-ranges ids))

;; == part2: integer set ==

(define (integer-set-method id-ranges)
  (for/fold ([iset (make-integer-set '())]
             #:result (count iset))
            ([r (in-list id-ranges)])
    (union iset (make-range (Range-start r) (Range-end r)))))

;; == part2: merge ranges ==

(define (merge-ranges-method id-ranges)
  (define sorted (sort id-ranges < #:key Range-start))
  (define dedup-ranges
    (for/fold ([dedup-ranges '()])
              ([r (in-list sorted)])
      (match dedup-ranges
        ['() (list r)]
        [(cons last rest)
         (if (intersect? r last)
             (cons (merge-range last r) rest)
             (cons r dedup-ranges))])))

  (for/sum ([r (in-list dedup-ranges)])
    (- (Range-end r) (Range-start r) -1)))

(define (intersect? r1 r2)
  (not (or (< (Range-end r1) (Range-start r2))
           (< (Range-end r2) (Range-start r1)))))

(define (merge-range r1 r2)
  (Range (min (Range-start r1) (Range-start r2))
         (max (Range-end r1) (Range-end r2))))

