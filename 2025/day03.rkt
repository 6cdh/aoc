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
    (best-jolt-greedy bank digits)))

;; == dynamic programming method (top down style and use vector cache) ==

(define (best-jolt-dp bank total-digits)
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

;; == greedy method (faster) ==

(define (best-jolt-greedy bank digits)
  (define n (vector-length bank))
  (for/fold ([start 0]
             [end (- n (- digits 1))]
             [sum 0]
             #:result sum)
            ([_ (in-range digits)])
    (define choose (choose-best-battery bank start end))
    (values (add1 choose) (add1 end) (+ (* 10 sum) (vector-ref bank choose)))))

;; choose the best battery between the range [start, end)
(define (choose-best-battery vec start end)
  (vector-argmax-index identity vec start end))

