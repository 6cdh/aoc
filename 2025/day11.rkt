#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define graph (make-hash))
  (for ([line (in-list (port->lines in))])
    (match-define (list from tos ...)
                  (string-split line #px"[: ]" #:repeat? #t))
    (hash-set! graph from tos))
  (define ans1 (count-paths "you" graph))
  (define ans2 (count-paths-dacfft "svr" graph))
  (values ans1 ans2))

(define (count-paths from graph)
  (if (equal? from "out")
      1
      (for/sum ([to (hash-ref graph from '())])
        (count-paths to graph))))

(define (count-paths-dacfft from graph)
  (define/cache (count-rec from dac? fft?)
    (if (equal? from "out")
        (if (and dac? fft?) 1 0)
        (for/sum ([to (hash-ref graph from '())])
          (count-rec to
                     (or dac? (equal? from "dac"))
                     (or fft? (equal? from "fft"))))))

  (count-rec from #f #f))

