#lang racket

(require "lib.rkt")

(define *inf-int* (expt 2 64))

(define (where heights c)
  (let ([m (vector-length heights)]
        [n (vector-length (aref heights 0))])
    (for*/first ([i m]
                 [j n]
                 #:when (eq? c (aref heights i j)))
      (cons i j))))

(define (reachable? fromc toc)
  (let ([from (char->integer fromc)]
        [to (char->integer toc)])
    (<= from (add1 to))))

(define (adjof node)
  (match-let ([(cons i j) node])
    (for/list ([dij '((0 . 1) (0 . -1) (1 . 0) (-1 . 0))])
      (cons (+ i (car dij))
            (+ j (cdr dij))))))

;; explore all paths from `start` to every nodes
(define (bfs heights start)
  (let* ([m (vector-length heights)]
         [n (vector-length (aref heights 0))]
         [path (make-array m n *inf-int*)])
    (define (loop queue steps)
      (when (not (null? queue))
        (loop
         (for*/list ([node queue]
                     [adj (adjof node)]
                     #:when
                     (match-let ([(cons i1 j1) node]
                                 [(cons i2 j2) adj])
                       (and (<= 0 i2 (sub1 m))
                            (<= 0 j2 (sub1 n))
                            (= *inf-int* (aref path i2 j2))
                            (reachable? (aref heights i1 j1)
                                        (aref heights i2 j2)))))
           (aset! path (car adj) (cdr adj) (add1 steps))
           (cons (car adj) (cdr adj)))
         (add1 steps))))

    (loop (list start) 0)
    path))

(define (day12)
  (let* ([lines (read-lines)]
         [heights (list2d->vector2d lines)]
         [m (vector-length heights)]
         [n (vector-length (aref heights 0))]
         [beg (where heights #\S)]
         [dst (where heights #\E)])
    (aset! heights (car beg) (cdr beg) #\a)
    (aset! heights (car dst) (cdr dst) #\z)

    (let ([path (bfs heights dst)])
      ;; part 1
      (println (aref path (car beg) (cdr beg)))
      ;; part 2
      (println (for*/fold ([minv *inf-int*])
                          ([i m]
                           [j n]
                           #:when (eq? #\a (aref heights i j)))
                 (min minv (aref path i j)))))))

(time (day12))

