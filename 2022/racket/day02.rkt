#lang racket

(require "lib.rkt")

(define rock 0)
(define paper 1)
(define scissor 2)

(define (opp-char->shape c)
  (match c
    [#\A rock]
    [#\B paper]
    [#\C scissor]))

(define (my-char->shape c)
  (match c
    [#\X rock]
    [#\Y paper]
    [#\Z scissor]))

(define (which-win shape) (modulo (add1 shape) 3))
(define (which-loss shape) (modulo ($ shape + 3 - 1) 3))
(define (score-of shape) (add1 shape))

(define (outcome opp my)
  (cond [(= opp my) 3]
        [(= opp (which-loss my)) 6]
        [else 0]))

(define (which-shape opp result)
  (match result
    [#\X (which-loss opp)]
    [#\Y opp]
    [#\Z (which-win opp)]))

(define (round1 line)
  (let ([opp (opp-char->shape (string-ref line 0))]
        [my (my-char->shape (string-ref line 2))])
    (+ (score-of my) (outcome opp my))))

(define (round2 line)
  (let* ([opp (opp-char->shape (string-ref line 0))]
         [my (which-shape opp (string-ref line 2))])
    (+ (score-of my) (outcome opp my))))

(define (day02)
  (let ([lines (read-lines)])
    (println (sum (map round1 lines)))
    (println (sum (map round2 lines)))))

(time (day02))


