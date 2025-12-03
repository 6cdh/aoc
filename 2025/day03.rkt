#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define banks
    (for/list ([line (in-list (port->lines in))])
      (~> line (string->list line)
          (map digit-char->number line)
          (list->vector line))))
  (define ans1 (total-output-joltage banks 2))
  (define ans2 (total-output-joltage banks 12))
  (values ans1 ans2))

(define (total-output-joltage banks digits)
  (for/sum ([bank (in-list banks)])
    (best-jolt-solver bank digits)))

(define (best-jolt-solver bank total-digits)
  (define/cache-vec (find-best-jolt bat digits)
                    #:vector ((vector-length bank) (add1 total-digits) #f)
    (cond [(= bat 0)
           (vector-ref bank 0)]
          [(= digits 1)
           (max (vector-ref bank bat)
                (find-best-jolt (sub1 bat) 1))]
          [else
           (max (+ (* 10 (find-best-jolt (sub1 bat) (sub1 digits)))
                   (vector-ref bank bat))
                (find-best-jolt (sub1 bat) digits))]))

  (find-best-jolt (sub1 (vector-length bank)) total-digits))

