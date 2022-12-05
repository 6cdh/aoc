#lang racket

(require "lib.rkt")

(define/contract (parse-stacks lines)
  (-> (listof string?)
      (vectorof (listof char?)))

  (let* ([strs (takef lines (λ (line) (not (equal? line ""))))]
         [n (length (string-split (last strs)))]
         [stacks (make-vector n '())])
    (for* ([line strs]
           [i (range 1 (string-length line) 4)])
      (let ([c (string-ref line i)])
        (when (char-upper-case? c)
          (vector-update! stacks (quotient i 4)
                          (λ (old) (cons c old))))))
    (vector-map! reverse stacks)
    stacks))

(define/contract (parse-proc lines)
  (-> (listof string?)
      (listof (listof integer?)))

  (let* ([strs (cdr (member "" lines))])
    (for/list ([line strs])
      (let ([words (string-split line)])
        (list (string->number (list-ref words 1))
              (sub1 (string->number (list-ref words 3)))
              (sub1 (string->number (list-ref words 5))))))))

(define/contract (puzzle stacks procedure transformer)
  (-> (vectorof (listof char?))
      (listof (listof integer?))
      (-> (listof char?) (listof char?))
      string?)

  (for ([p procedure])
    (match-let ([(list cnt from to) p])
      (let* ([moved (take (vector-ref stacks from) cnt)]
             [transformed (transformer moved)])
        (vector-update! stacks from (λ (old) (drop old cnt)))
        (vector-update! stacks to (λ (old) (append transformed old))))))

  (apply string (map car (vector->list stacks))))

(define (day05)
  (let* ([lines (read-lines)]
         [procedure (parse-proc lines)])
    (displayln (puzzle (parse-stacks lines) procedure reverse))
    (displayln (puzzle (parse-stacks lines) procedure identity))))

(time (day05))


