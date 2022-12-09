#lang racket

(require "lib.rkt")

;; move 1 step
(define (move1 old dir)
  (cons (+ (car old) (car dir))
        (+ (cdr old) (cdr dir))))

;; return the new position of `cur`
;; prev: new position of the previous knots
;; cur: old position of the current knots
(define (adjust prev cur)
  (let ([Δx (- (car prev) (car cur))]
        [Δy (- (cdr prev) (cdr cur))])
    (cond [(and (<= (abs Δx) 1)
                (<= (abs Δy) 1))
           cur]
          [else (move1 cur (cons (sgn Δx) (sgn Δy)))])))

;; move k steps
(define (movek rope Δ k visited)
  (let ([n (vector-length rope)])
    (for ([_ k])
      (aupd! rope 0 (λ (old) (move1 old Δ)))
      ;; update the whole rope
      (for ([prev n]
            [i (range 1 n)])
        (aupd! rope i (λ (old) (adjust (aref rope prev) old))))
      (set-add! visited (aref rope (sub1 n))))
    rope))

(define (emulate lines n)
  (let ([visited (mutable-set '(0 . 0))])
    (for/fold ([rope (make-array n '(0 . 0))])
              ([line lines])
      (let* ([ws (string-split line)]
             [dir (first ws)]
             [k (string->number (second ws))])
        (match dir
          ["L" (movek rope '(-1 . 0) k visited)]
          ["R" (movek rope '(1 . 0) k visited)]
          ["U" (movek rope '(0 . 1) k visited)]
          ["D" (movek rope '(0 . -1) k visited)])))
    (set-count visited)))

(define (day09)
  (let ([lines (read-lines)])
    (println (emulate lines 2))
    (println (emulate lines 10))))

(time (day09))

