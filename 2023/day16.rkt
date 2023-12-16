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

(define (valid? pack point)
  (and (< -1 (Pos-x point) (Pack-m pack))
       (< -1 (Pos-y point) (Pack-n pack))))

(define (reflect/ pack point dir)
  (match dir
    [(== Left) (turn pack point Down)]
    [(== Right) (turn pack point Up)]
    [(== Up) (turn pack point Right)]
    [(== Down) (turn pack point Left)]))

(define (reflect\\ pack point dir)
  (match dir
    [(== Left) (turn pack point Up)]
    [(== Right) (turn pack point Down)]
    [(== Up) (turn pack point Left)]
    [(== Down) (turn pack point Right)]))

(define (split- pack point dir)
  (match dir
    [(== Left) (turn pack point dir)]
    [(== Right) (turn pack point dir)]
    [(== Up) (turn pack point Left)
             (turn pack point Right)]
    [(== Down) (turn pack point Left)
               (turn pack point Right)]))

(define (split|| pack point dir)
  (match dir
    [(== Left) (turn pack point Up)
               (turn pack point Down)]
    [(== Right) (turn pack point Up)
                (turn pack point Down)]
    [(== Up) (turn pack point dir)]
    [(== Down) (turn pack point dir)]))

(define (turn pack point dir)
  (spread pack (forward point dir) dir))

(define (spread pack pos dir)
  (define key (cons pos dir))
  (when (and (valid? pack pos)
             (not (set-member? (Pack-visited pack) key)))
    (set-add! (Pack-visited pack) key)
    (match (aref (Pack-board pack) (Pos-x pos) (Pos-y pos))
      [#\. (turn pack pos dir)]
      [#\/ (reflect/ pack pos dir)]
      [#\\ (reflect\\ pack pos dir)]
      [#\- (split- pack pos dir)]
      [#\| (split|| pack pos dir)])))

(define (beam board pos dir)
  (match-define (list m n) (vector-dims board 2))
  (define visited (mutable-set))
  (define pack (Pack board visited m n))
  (spread pack pos dir)
  (set-count (list->set (set-map visited first))))

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

