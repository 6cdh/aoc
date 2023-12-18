#lang racket

(require "lib.rkt")

(define Pos cons)
(define Pos-x car)
(define Pos-y cdr)

(define Dir cons)
(define Dir-x car)
(define Dir-y cdr)

(define (pos+ pos dir)
  (Pos (+ (Pos-x pos) (Dir-x dir))
       (+ (Pos-y pos) (Dir-y dir))))

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
  (append* (map (λ (new-dir) (forward board pos new-dir 0))
                (match dir
                  [(== Left) (list Up Down)]
                  [(== Right) (list Up Down)]
                  [(== Up) (list Left Right)]
                  [(== Down) (list Left Right)]))))

(define (go pos edgeof end?)
  (path-finding/dijkstra (list (Node pos Right 0)
                               (Node pos Down 0))
                         end?
                         edgeof))

(define (main)
  (define board (for/vector ([row (read-lines-as-vector2d)])
                  (for/vector ([c row])
                    (string->number (string c)))))

  (match-define (list m n) (vector-dims board 2))

  (define (edgeof min-steps max-steps)
    (λ (node)
      (match node
        [(Node pos dir (== max-steps))
         (turn board pos dir)]
        [(Node pos dir steps)
         #:when (< steps min-steps)
         (forward board pos dir steps)]
        [(Node pos dir steps)
         (append (forward board pos dir steps)
                 (turn board pos dir))])))

  (define (end? node)
    (equal? (Node-pos node) (Pos (sub1 m) (sub1 n))))

  (define (end-ultra? node)
    (and (end? node) (>= (Node-steps node) 4)))

  (println (go (Pos 0 0) (edgeof 0 3) end?))
  (println (go (Pos 0 0) (edgeof 4 10) end-ultra?)))

(main)

