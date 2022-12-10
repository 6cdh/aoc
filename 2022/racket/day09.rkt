#lang racket

(require "lib.rkt")

;; move 1 step
(define (move-knot old-pos dir)
  (cons (+ (car old-pos) (car dir))
        (+ (cdr old-pos) (cdr dir))))

;; return the new position of `cur`
;; prev: new position of the previous knot
;; cur: old position of the current knot
(define (adjust-knot prev cur)
  (let ([Δx (- (car prev) (car cur))]
        [Δy (- (cdr prev) (cdr cur))])
    (cond [(and (<= (abs Δx) 1)
                (<= (abs Δy) 1))
           cur]
          [else (move-knot cur (cons (sgn Δx) (sgn Δy)))])))

;; move k steps
(define (move-rope rope Δ k visited)
  (let ([n (vector-length rope)])
    (for ([_ k])
      (aupd! rope 0 (λ (old) (move-knot old Δ)))
      ;; update the whole rope
      (for ([prev n]
            [i (range 1 n)])
        (aupd! rope i (λ (old) (adjust-knot (aref rope prev) old))))
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
          ["L" (move-rope rope '(-1 . 0) k visited)]
          ["R" (move-rope rope '(1 . 0) k visited)]
          ["U" (move-rope rope '(0 . 1) k visited)]
          ["D" (move-rope rope '(0 . -1) k visited)])))
    (set-count visited)))

(define (day09)
  (let ([lines (read-lines)])
    (println (emulate lines 2))
    (println (emulate lines 10))))

(time (day09))


