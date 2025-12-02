#lang racket

(provide solve)

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
  (define ans1 (sum-invalid-ids-for-ranges range-list #px"^(.+?)(\\1)$"))
  (define ans2 (sum-invalid-ids-for-ranges range-list #px"^(.+?)(\\1)+$"))
  (values ans1 ans2))

(define (sum-invalid-ids-for-ranges range-list pattern)
  (for/sum ([range (in-list range-list)])
    (sum-invalid-ids-for-range range pattern)))

(define (sum-invalid-ids-for-range range pattern)
  (for/sum ([id (in-range (Range-start range) (add1 (Range-end range)))]
            #:when (invalid-id? id pattern))
    id))

(define (invalid-id? id pattern)
  (regexp-match-exact? pattern (number->string id)))

