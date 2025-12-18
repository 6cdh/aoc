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
    (max-jolt-greedy bank digits)))

;; == dynamic programming method (top down style and use vector cache) ==

(define (max-jolt-dp bank total-digits)
  (define/cache-vec (find-max-jolt bat-idx digits)
    #:vector ((vector-length bank) (add1 total-digits) #f)
    (cond [(= bat-idx 0)
           (vector-ref bank 0)]
          [(= digits 1)
           (max (vector-ref bank bat-idx)
                (find-max-jolt (sub1 bat-idx) 1))]
          [else
           (max (+ (* 10 (find-max-jolt (sub1 bat-idx) (sub1 digits)))
                   (vector-ref bank bat-idx))
                (find-max-jolt (sub1 bat-idx) digits))]))

  (find-max-jolt (sub1 (vector-length bank)) total-digits))

;; == greedy method (faster) ==

;; For choose k digits from a bank, let's start with a random solution,
;; and try to improve it by applying some transformation, to get some insight.
;;
;; Let's start with the first k digits. Try to move the rightmost digit
;; (k-th digit) to the right to see if it makes the total number larger.
;; For convenient, call these chosen positions `d_1`, `d_2`, ..., `d_k`.
;;
;; Oh no, let's start with the last k digits, and move the leftmost chosen digit `d_1`
;; to left. It is easier to get a better solution.
;; We choose the position with the maximum number in the range [0, d_1],
;; this seems the best choice. We call this new position of the first chosen digit `p_1`.
;;
;; Then we try to move the second chosen digit `d_2` to left. It needs to
;; be in this range [p_1 + 1, d_2]. So we choose the maximum number in this range.
;; Here you can realize, to make `d_2` have more choice, in the process of move of `d_1`,
;; if there are multiple candidates they all are maximum numbers in the range [0, d_1],
;; we can choose the leftmost one.
;;
;; Then for each `d_i`, we always choose the maximum number in the range [p_i-1, d_i].
;; For multiple candidates maximum numbers, we choose the leftmost one.
;;
;; Intuitively, for any solution, we can apply this transformation to get a better solution,
;; unless it's already the best solution. And we can only apply this transformation once for
;; any solution.
;;
;; So this greedy transformation always produce the best solution.
(define (max-jolt-greedy bank digits)
  (define n (vector-length bank))
  (for/fold ([window-start 0]
             [window-end (- n (- digits 1))]
             [jolt 0]
             #:result jolt)
            ([_ (in-range digits)])
    (define choose (choose-max-battery bank window-start window-end))
    (values (add1 choose) (add1 window-end) (+ (* 10 jolt) (vector-ref bank choose)))))

;; choose the max battery between the range [start, end)
(define (choose-max-battery vec start end)
  (vector-argmax-index identity vec start end))

