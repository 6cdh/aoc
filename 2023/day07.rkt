#lang racket

(require "lib.rkt")

(define (read-hands)
  (for/list ([line (read-lines)])
    (match-define (list cards bid) (string-split line " "))
    (list cards (string->number bid))))

(define (sorted-freq-counter hand)
  (sort (counter-as-list hand) > #:key cdr))

(define (hand-strength hand)
  (match (map cdr (sorted-freq-counter hand))
    ['(1 1 1 1 1) 1]
    ['(2 1 1 1) 2]
    ['(2 2 1) 3]
    ['(3 1 1) 4]
    ['(3 2) 5]
    ['(4 1) 6]
    ['(5) 7]
    [hand (error "impossible hands kind" hand)]))

(define (J-transform hands)
  (define sorted-non-J-groups
    (filter-not (λ (c) (char=? #\J (car c)))
                (sorted-freq-counter hands)))
  (define most-common-card
    (if (null? sorted-non-J-groups)
        #\J
        (car (first sorted-non-J-groups))))
  (string-replace-char hands #\J most-common-card))

(define (cards< hand1 hand2 card-order)
  (define (card-strength c)
    (string-find-index card-order c))

  (sequence<? hand1 hand2 char=? card-strength))

(define (hand< transformer card-order)
  (λ (h1 h2)
    (let ([strength1 (hand-strength (transformer h1))]
          [strength2 (hand-strength (transformer h2))])
      (or (< strength1 strength2)
          (and (= strength1 strength2) (cards< h1 h2 card-order))))))

(define (sort-hands hands transformer card-order)
  (sort hands (hand< transformer card-order) #:key first))

(define (solve hands transformer card-order)
  (for/sum ([hand (sort-hands hands transformer card-order)]
            [rank (in-naturals 1)])
    (* rank (second hand))))

(define (main)
  (define hands (read-hands))
  (println (solve hands identity "23456789TJQKA"))
  (println (solve hands J-transform "J23456789TQKA")))

(main)
