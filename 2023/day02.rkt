#lang racket

(require "lib.rkt")

(define (read-record str)
  (for/fold ([cube-count '(0 0 0)])
            ([record-str (string-split str ", ")])
    (map + cube-count
         (match (string-split record-str " ")
           [(list k "red") (list (string->number k) 0 0)]
           [(list k "green") (list 0 (string->number k) 0)]
           [(list k "blue") (list 0 0 (string->number k))]
           [_ (error "invalid record format")]))))

(define (read-games)
  (for/list ([line (read-lines)])
    (match (string-split line #px"[:;] ")
      [(list _gameid records ...)
       (map read-record records)]
      [_ (error "invalid game format")])))

(define (main)
  (define games (read-games))
  (define have '(12 13 14))
  (println (for/sum ([game games]
                     [id (in-naturals 1)]
                     #:when (for/and ([record game])
                              (and (<= (first record) (first have))
                                   (<= (second record) (second have))
                                   (<= (third record) (third have)))))
             id))
  (println (for/sum ([game games])
             (product (apply map max game)))))

(main)
