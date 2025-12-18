#lang racket

(provide solve)

(require (prefix-in iset: data/integer-set)
         "utils.rkt")

(struct Range
  (start end)
  #:transparent)

(define (solve in)
  (define-values (id-ranges ids) (parse in))
  (define ans1
    (for/count ([id (in-list ids)])
      (id-in-ranges? id id-ranges)))
  (define ans2 (merge-ranges-method id-ranges))
  (values ans1 ans2))

(define (id-in-ranges? id id-ranges)
  (for/or ([r (in-list id-ranges)])
    (<= (Range-start r) id (Range-end r))))

(define (parse in)
  (define lines (port->lines in))
  (match-define (list ranges-lines ids-lines) (list-split lines ""))
  (define id-ranges
    (for/list ([r (in-list ranges-lines)])
      (match-define (list start end) (read-sep-numbers r "-"))
      (Range start end)))
  (define ids (map string->number ids-lines))
  (values id-ranges ids))

;; == part2: integer set ==

(define (integer-set-method id-ranges)
  (for/fold ([iset (iset:make-integer-set '())]
             #:result (iset:count iset))
            ([r (in-list id-ranges)])
    (iset:union iset (iset:make-range (Range-start r) (Range-end r)))))

;; == part2: merge ranges ==

(define (merge-ranges-method id-ranges)
  (define sorted-ranges (sort id-ranges < #:key Range-start))
  (define no-intersect-ranges
    (for/fold ([no-intersect-ranges '()])
              ([r (in-list sorted-ranges)])
      (match no-intersect-ranges
        ['() (list r)]
        [(cons last rest)
         (if (intersect? r last)
             (cons (merge-range last r) rest)
             (cons r no-intersect-ranges))])))

  (for/sum ([r (in-list no-intersect-ranges)])
    (- (Range-end r) (Range-start r) -1)))

(define (intersect? r1 r2)
  (not (or (< (Range-end r1) (Range-start r2))
           (< (Range-end r2) (Range-start r1)))))

(define (merge-range r1 r2)
  (Range (min (Range-start r1) (Range-start r2))
         (max (Range-end r1) (Range-end r2))))

