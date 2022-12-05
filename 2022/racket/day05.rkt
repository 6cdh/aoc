#lang racket

(require "lib.rkt")

(define/contract (parse-stacks lines)
  (-> (listof string?)
      (vectorof (listof char?)))

  (let* ([strs (takef lines (λ (line) (not (equal? line ""))))]
         [n (length (string-split (last strs)))]
         [stacks (make-vector n '())])
    (for* ([line (take strs (sub1 (length strs)))]
           [i (range 1 (string-length line) 4)])
      (when (char-upper-case? (string-ref line i))
        (vector-update! stacks (quotient i 4)
                        (λ (old) (cons (string-ref line i) old)))))
    (vector-map! reverse stacks)
    stacks))

(define/contract (parse-proc lines)
  (-> (listof string?)
      (listof (listof number?)))

  (let* ([strs (cdr (member "" lines))])
    (for/list ([line strs])
      (let ([words (string-split line)])
        (list (string->number (list-ref words 1))
              (sub1 (string->number (list-ref words 3)))
              (sub1 (string->number (list-ref words 5))))))))

(define/contract (puzzle stacks procedure #:pack pack)
  (-> (vectorof (listof char?))
      (listof (listof number?))
      #:pack boolean?
      string?)

  (for ([p procedure])
    (match-let ([(list cnt from to) p])
      (for ([_ cnt])
        (let ([v (car (vector-ref stacks from))])
          (vector-update! stacks from cdr)
          (vector-update! stacks to (λ (old) (cons v old)))))
      (when pack
        (vector-update! stacks to
                        (λ (old) (append (reverse (take old cnt))
                                         (drop old cnt)))))))

  (apply string (map car (vector->list stacks))))

(define (day05)
  (let* ([lines (read-lines)]
         [procedure (parse-proc lines)])
    (displayln (puzzle (parse-stacks lines) procedure #:pack false))
    (displayln (puzzle (parse-stacks lines) procedure #:pack true))))

(time (day05))


