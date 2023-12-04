#lang racket

(require "lib.rkt")

(define (read-cards)
  (for/list ([line (read-lines)])
    (match (string-split line #px"[:|]")
      [(list _id str1 str2)
       (list (map string->number (string-split str1 #px" +"))
             (map string->number (string-split str2 #px" +")))]
      [_ (error "invalid input format")])))

(define (get-win-count cards)
  (for/list ([card cards])
    (match card
      [(list win-nums have)
       (length (set-intersect win-nums have))])))

(define (main)
  (define cards (read-cards))
  (define win-count (get-win-count cards))
  (println
   (for/sum ([win win-count]
             #:when (positive? win))
     (expt 2 (sub1 win))))

  (define card-counter (make-vector (length win-count) 1))
  (for/sum ([(win id) (in-indexed win-count)]
            [cnt card-counter])
    (for ([Δ win])
      (vector-update! card-counter (+ id Δ 1)
                      (λ (old) (+ old cnt))))
    cnt))

(main)
