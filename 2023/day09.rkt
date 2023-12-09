#lang racket

(require "lib.rkt")

(define (read-history)
  (for/list ([line (read-lines)])
    (map string->number (string-split-in-spaces line))))

(define (diff-seq seq)
  (for/list ([a seq]
             [b (cdr seq)])
    (- b a)))

(define (predict seq picker summary)
  (define (summary-diff-seqs seq)
    (if (andmap zero? seq)
        0
        (summary (picker seq)
                 (summary-diff-seqs (diff-seq seq)))))

  (summary-diff-seqs seq))

(define (main)
  (define history (read-history))
  (println (for/sum ([seq history])
             (predict seq last +)))
  (println (for/sum ([seq history])
             (predict seq first -))))

(main)

