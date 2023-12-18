#lang racket

(require "lib.rkt")

(define Pos cons)
(define Pos-x car)
(define Pos-y cdr)

(define Dir cons)
(define Dir-x car)
(define Dir-y cdr)

(define (pos+ x y)
  (Pos (+ (Pos-x x) (Dir-x y))
       (+ (Pos-y x) (Dir-y y))))

(define Left (Dir 0 -1))
(define Right (Dir 0 1))
(define Up (Dir -1 0))
(define Down (Dir 1 0))

(define (valid? board pos)
  (match-define (list m n) (vector-dims board 2))
  (and (< -1 (Pos-x pos) m)
       (< -1 (Pos-y pos) n)))

(struct Node
  (pos dir steps)
  #:transparent)

(define (forward board pos dir steps)
  (define new-pos (pos+ pos dir))
  (if (valid? board new-pos)
      (list (cons
             ;; node
             (Node new-pos dir (add1 steps))
             ;; cost
             (aref board (Pos-x new-pos) (Pos-y new-pos))))
      '()))

(define (turn board pos dir)
  (append* (map (λ (new-pos) (forward board pos new-pos 0))
                (match dir
                  [(== Left) (list Up Down)]
                  [(== Right) (list Up Down)]
                  [(== Up) (list Left Right)]
                  [(== Down) (list Left Right)]))))

(define (min-dist pos dir edgeof end?)
  (path-finding/dijkstra (Node pos dir 0) end? edgeof))

(define (go pos edgeof end?)
  (min (min-dist pos Right edgeof end?)
       (min-dist pos Down edgeof end?)))

(define (main)
  (define board (for/vector ([row (read-lines-as-vector2d)])
                  (for/vector ([c row])
                    (string->number (string c)))))

  (match-define (list m n) (vector-dims board 2))

  (define (edgeof node)
    (match node
      [(Node pos dir 3)
       (turn board pos dir)]
      [(Node pos dir steps)
       (append (forward board pos dir steps)
               (turn board pos dir))]))

  (define (edgeof-ultra node)
    (match node
      [(Node pos dir 10)
       (turn board pos dir)]
      [(Node pos dir steps)
       #:when (< steps 4)
       (forward board pos dir steps)]
      [(Node pos dir steps)
       (append (forward board pos dir steps)
               (turn board pos dir))]))

  (define (end? node)
    (equal? (Node-pos node) (Pos (sub1 m) (sub1 n))))

  (define (end-ultra? node)
    (and (equal? (Node-pos node) (Pos (sub1 m) (sub1 n)))
         (>= (Node-steps node) 4)))

  (println (go (Pos 0 0) edgeof end?))
  (println (go (Pos 0 0) edgeof-ultra end-ultra?)))

(main)

