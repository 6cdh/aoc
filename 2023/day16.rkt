#lang racket

(require "lib.rkt")

(define Pos cons)
(define Pos-x car)
(define Pos-y cdr)
(define Dir cons)
(define Dir-x car)
(define Dir-y cdr)
(define Left (Dir 0 -1))
(define Right (Dir 0 1))
(define Up (Dir -1 0))
(define Down (Dir 1 0))

(define (forward pos dir)
  (Pos (+ (Pos-x pos) (Dir-x dir))
       (+ (Pos-y pos) (Dir-y dir))))

(struct Pack
  (board visited m n))

(define Key cons)
(define Key-pos car)

(define (valid? pack pos)
  (and (< -1 (Pos-x pos) (Pack-m pack))
       (< -1 (Pos-y pos) (Pack-n pack))))

(define (reflect/ pack pos dir)
  (spread-forward pack pos
                  (match dir
                    [(== Left) Down]
                    [(== Right) Up]
                    [(== Up) Right]
                    [(== Down) Left])))

(define (reflect\\ pack pos dir)
  (spread-forward pack pos
                  (match dir
                    [(== Left) Up]
                    [(== Right) Down]
                    [(== Up) Left]
                    [(== Down) Right])))

(define (split- pack pos dir)
  (match dir
    [(== Left) (spread-forward pack pos dir)]
    [(== Right) (spread-forward pack pos dir)]
    [(== Up) (spread-forward pack pos Left)
             (spread-forward pack pos Right)]
    [(== Down) (spread-forward pack pos Left)
               (spread-forward pack pos Right)]))

(define (split|| pack pos dir)
  (match dir
    [(== Left) (spread-forward pack pos Up)
               (spread-forward pack pos Down)]
    [(== Right) (spread-forward pack pos Up)
                (spread-forward pack pos Down)]
    [(== Up) (spread-forward pack pos dir)]
    [(== Down) (spread-forward pack pos dir)]))

(define (spread-forward pack pos dir)
  (spread pack (forward pos dir) dir))

(define (spread pack pos dir)
  (define key (Key pos dir))
  (when (and (valid? pack pos)
             (not (set-member? (Pack-visited pack) key)))
    (set-add! (Pack-visited pack) key)
    (match (aref (Pack-board pack) (Pos-x pos) (Pos-y pos))
      [#\. (spread-forward pack pos dir)]
      [#\/ (reflect/ pack pos dir)]
      [#\\ (reflect\\ pack pos dir)]
      [#\- (split- pack pos dir)]
      [#\| (split|| pack pos dir)])))

(define (beam board pos dir)
  (match-define (list m n) (vector-dims board 2))
  (define visited (mutable-set))
  (define pack (Pack board visited m n))
  (spread pack pos dir)
  (set-count (list->set (set-map visited Key-pos))))

(define (main)
  (define board (read-lines-as-vector2d))
  (match-define (list m n) (vector-dims board 2))

  (println (beam board (Pos 0 0) Right))
  (println (max (for/max 0 ([i m])
                  (max (beam board (Pos i 0) Right)
                       (beam board (Pos i (sub1 n)) Left)))
                (for/max 0 ([j n])
                  (max (beam board (Pos 0 j) Down)
                       (beam board (Pos (sub1 m) j) Up))))))

(main)

