#lang racket

(provide solve)

(require "utils.rkt")

(struct Pos
  (x y))

(struct Rect
  (p1 p2))

(define (solve in)
  (define pos-vec
    (for/vector ([line (port->lines in)])
      (match-define (list x y) (map string->number (string-split line ",")))
      (Pos x y)))
  (define n (vector-length pos-vec))

  (define all-rects
    (for*/list ([i (in-range n)]
                [j (in-range i)])
      (Rect (vector-ref pos-vec i) (vector-ref pos-vec j))))

  (define all-sorted-rects (sort all-rects > #:key area))

  (define ans1 (area (first all-sorted-rects)))

  (define total (length all-rects))
  (define polygon
    (let ([vertexes (vector->list pos-vec)])
      (cons (last vertexes) vertexes)))
  (define ans2
    (for/first ([rect (in-list all-sorted-rects)]
                [i (in-naturals)]
                #:when (let ()
                         (when (= 0 (modulo i 100))
                           (display (format "\rcheck: ~a/~a" i total)))
                         (rectangle-inside-polygon? rect polygon)))
      (newline)
      (displayln (format "found result: ~ath" i))
      (area rect)))
  (values ans1 ans2))

(define (area rect)
  (match-define (Rect p1 p2) rect)
  (* (add1 (abs (- (Pos-x p1) (Pos-x p2))))
     (add1 (abs (- (Pos-y p1) (Pos-y p2))))))

(define (rectangle-inside-polygon? rect polygon)
  (match-define (Rect (Pos x1 y1) (Pos x2 y2)) rect)
  (and (point-not-outside? (Pos x1 y1) polygon)
       (point-not-outside? (Pos x2 y2) polygon)
       (point-not-outside? (Pos x1 y2) polygon)
       (point-not-outside? (Pos x2 y1) polygon)
       (line-not-outside-fast? x1 y1 x1 y2 polygon)
       (line-not-outside-fast? x1 y2 x2 y2 polygon)
       (line-not-outside-fast? x2 y2 x2 y1 polygon)
       (line-not-outside-fast? x2 y1 x1 y1 polygon)))

(define (line-not-outside? x1 y1 x2 y2 polygon)
  (define speedup 2)
  (define dx (* speedup (sgn (- x2 x1))))
  (define dy (* speedup (sgn (- y2 y1))))
  (if (= dx 0)
      (for*/and ([y (in-inclusive-range y1 y2 dy)])
        (point-not-outside? (Pos x1 y) polygon))
      (for*/and ([x (in-inclusive-range x1 x2 dx)])
        (point-not-outside? (Pos x y1) polygon))))

(define (line-not-outside-fast? x1 y1 x2 y2 polygon)
  (define dx (sgn (- x2 x1)))
  (define dy (sgn (- y2 y1)))
  (if (= dx 0)
      (line-not-outside2? (min y1 y2) (max y1 y2) (λ (y) (Pos x1 y)) polygon)
      (line-not-outside2? (min x1 x2) (max x1 x2) (λ (x) (Pos x y1)) polygon)))

(define (line-not-outside2? from to build-pos polygon)
  (cond [(> from to) #t]
        [(= from to)
         (point-not-outside? (build-pos from) polygon)]
        [(<= (abs (- to from)) 5000)
         (for/and ([i (in-inclusive-range from to)])
           (point-not-outside? (build-pos i) polygon))]
        [else
         (define mid (quotient (+ from to) 2))
         (cond [(not (point-not-outside? (build-pos mid) polygon))
                #f]
               [else
                (define th1 (parallel-run (line-not-outside2? from (sub1 mid) build-pos polygon)))
                (define th2 (parallel-run (line-not-outside2? (add1 mid) to build-pos polygon)))
                (and (thread-wait th1) (thread-wait th2))])]))

;; ray casting algorithm
(define (point-not-outside? point polygon)
  (match-define (Pos x y) point)
  (let/cc return
    (for/fold ([inside? #f])
              ([v1 (in-list polygon)]
               [v2 (in-list (cdr polygon))])
      (match-define (Pos x1 y1) v1)
      (match-define (Pos x2 y2) v2)

      (when (or (and (= x1 x x2) (<= (min y1 y2) y (max y1 y2)))
                (and (= y1 y y2) (<= (min x1 x2) x (max x1 x2))))
        (return #t))

      (if (and (<= (add1 (min y1 y2)) y (max y1 y2))
               (< x (intersect-x x y x1 y1 x2 y2)))
          (not inside?)
          inside?))))

;; x of the intersection point of
;; the ray from (x ,y) and the line segment from (x1, y1) to (x2, y2)
(define (intersect-x x y x1 y1 x2 y2)
  (+ (/ (* (- y y1) (- x2 x1))
        (- y2 y1))
     x1))

