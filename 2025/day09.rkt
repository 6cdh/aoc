#lang racket

(provide solve)

(require "utils.rkt")

(struct Pos
  (x y)
  #:transparent)

(struct Rect
  (p1 p2)
  #:transparent)

(define (solve in)
  (define pos-vec
    (for/vector ([line (port->lines in)])
      (match-define (list x y) (read-sep-numbers line ","))
      (Pos x y)))

  (define ans1 (part1 pos-vec))
  ; (define ans2 (part2/largest-rect-in-polygon pos-vec rectangle-inside-polygon?))
  ; (define ans2 (part2/largest-rect-in-polygon pos-vec rectangle-inside-polygon/fast?))
  (define ans2 (part2/ccrs pos-vec))
  (values ans1 ans2))

(define (area rect)
  (match-define (Rect p1 p2) rect)
  (* (add1 (abs (- (Pos-x p1) (Pos-x p2))))
     (add1 (abs (- (Pos-y p1) (Pos-y p2))))))

(define (part1 pos-vec)
  (for*/max 0 ([i (in-range (vector-length pos-vec))]
               [j (in-range i)])
    (area (Rect (vector-ref pos-vec i) (vector-ref pos-vec j)))))

(define (part2/largest-rect-in-polygon pos-vec rectangle-inside-polygon?)
  (define polygon
    (let ([vertexes (vector->list pos-vec)])
      (cons (last vertexes) vertexes)))
  (define all-rects
    (for*/list ([i (in-range (vector-length pos-vec))]
                [j (in-range i)])
      (Rect (vector-ref pos-vec i) (vector-ref pos-vec j))))
  (define all-sorted-rects (sort all-rects > #:key area))

  (for/first ([rect (in-list all-sorted-rects)]
              #:when (rectangle-inside-polygon? rect polygon))
    (area rect)))

;; == part 2: brute force in parallel (17 s) ==

;; the rectangle is not outside of the polygon if its 4 edges are all not outside of the polygon.
;; Let `n` be the number of vertexes of the polygon.
;; The time complexity is O(n^2 * 4 * edge * n). `edge` is the length of the edge of the rectangle,
;; is at most 100000.
;; So the time complexity is O(n^3 * 400000).

;; judge 4 vertexes of the rectangle first to speed up.
(define (rectangle-inside-polygon? rect polygon)
  (match-define (Rect (Pos x1 y1) (Pos x2 y2)) rect)
  (and (point-inside? (Pos x1 y1) polygon)
       (point-inside? (Pos x2 y2) polygon)
       (point-inside? (Pos x1 y2) polygon)
       (point-inside? (Pos x2 y1) polygon)
       (line-inside? x1 y1 x1 y2 polygon)
       (line-inside? x1 y2 x2 y2 polygon)
       (line-inside? x2 y2 x2 y1 polygon)
       (line-inside? x2 y1 x1 y1 polygon)))

(define (line-inside? x1 y1 x2 y2 polygon)
  (define dx (sgn (- x2 x1)))
  (define dy (sgn (- y2 y1)))
  (if (= dx 0)
      (line-inside-parallel? (min y1 y2) (max y1 y2) (λ (y) (Pos x1 y)) polygon)
      (line-inside-parallel? (min x1 x2) (max x1 x2) (λ (x) (Pos x y1)) polygon)))

(define (line-inside-parallel? from to build-pos polygon)
  (cond [(> from to) #t]
        [(= from to)
         (point-inside? (build-pos from) polygon)]
        [(<= (abs (- to from)) 5000)
         (for/and ([i (in-inclusive-range from to)])
           (point-inside? (build-pos i) polygon))]
        [else
         (define mid (quotient (+ from to) 2))
         (cond [(not (point-inside? (build-pos mid) polygon))
                #f]
               [else
                (define th1 (parallel-run (line-inside-parallel? from (sub1 mid) build-pos polygon)))
                (define th2 (parallel-run (line-inside-parallel? (add1 mid) to build-pos polygon)))
                (and (thread-wait th1) (thread-wait th2))])]))

;; ray casting algorithm
(define (point-inside? point polygon)
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

;; == part 2: fast implementation for judge rectangle inside polygon (170 ms) ==

;; rectangle is not inside of the polygon if any segments of the polygon
;; pass through any of the rectangle edges.
;; Time complexity: O(n^3)
(define (rectangle-inside-polygon/fast? rect polygon)
  (match-define (Rect (Pos rx1 ry1) (Pos rx2 ry2)) rect)
  (for/and ([p1 (in-list polygon)]
            [p2 (in-list (cdr polygon))])
    (match-define (Pos x1 y1) p1)
    (match-define (Pos x2 y2) p2)
    (cond [(= x1 x2) (segment-not-pass-rectangle? rx1 ry1 rx2 ry2 x1 y1 y2)]
          [(= y1 y2) (segment-not-pass-rectangle? ry1 rx1 ry2 rx2 y1 x1 x2)])))

(define (segment-not-pass-rectangle? rx1 ry1 rx2 ry2 x y1 y2)
  (or (<= x (min rx1 rx2))
      (<= (max rx1 rx2) x)
      (<= (max y1 y2) (min ry1 ry2))
      (<= (max ry1 ry2) (min y1 y2))))

;; == part2: coordinate compression and range sum in parallel (18 ms) ==

;; Is it possible to reach O(n^2)?
;; It means each rectangle takes O(1). This must require some preprocessing of the polygon.

;; Let's assign each tile a number `inside`, if it is inside the polygon or on the edge,
;; the `inside` is 1, otherwise 0. All `inside` values build a 2D array `inside`.
;; inside[i][j] means the `inside` value of the tile at (i, j).
;;
;; Then for example, if an horizontal edge (x1, y) -- (x2, y) of the rectangle
;; is not outside the polygon, it means
;; inside[x1][y] + inside[x1 + 1][y] + ... + inside[x2][y] = x2 - x1 + 1
;; In other words, we need to be able to query the range sum of each row and each column, in O(1).
;;
;; For each row or column, this can be done with a precomputed prefix sum array in O(n),
;; here `n `is the size of the row or column.
;;
;; There are ~100000 rows and columns, this is quite large.
;; But the number of vertexes of the polygon is small, almost n=500.
;; This polygon is sparse, so we can compress the polygon into n * n grid,
;; using coordinate compression.
;;
;; The basic idea of coordinate compression is to `scale` the polygon into the n * n grid,
;; without lose its vertexes information.
;; After that, we use flood fill algorithm to fill the grid with 0 and 1.
;; Then precompute the prefix sum of each row and each column.
;;
;; A optimization is to precompute the 2d prefix sum so prefix2d[x][y] means
;; the area of the rectangle (0, 0) -- (x, y).
;; And the area of the rectangle (x1, y1) -- (x2, y2) [x1 <= x2, y1 <= y2] is
;; prefix2d[x2][y2] - prefix2d[x2][y1-1] - prefix2d[x1][y2-1] + prefix2d[x1-1][y1-1].
;; Another optimization is iterate rectangles in parallel.

(define (part2/ccrs pos-vec)
  (define-values (x->rankx y->ranky area-sum2d) (area-sum2d-querier pos-vec))
  (define n (vector-length pos-vec))
  (define split-size 40)
  (define threads
    (for/list ([j-split (in-range 0 n split-size)])
      (parallel-run
        (for*/max 0 ([j (in-range j-split (min n (+ j-split split-size)))]
                     [i (in-range j)]
                     [rect (in-value (Rect (vector-ref pos-vec i) (vector-ref pos-vec j)))]
                     #:when (rectangle-inside-polygon/range-sum? rect area-sum2d x->rankx y->ranky))
          (area rect)))))
  (for/max 0 ([th (in-list threads)])
    (thread-wait th)))

;; convert rect to left-top and right-bottom representation
(define (normalize-rect rect)
  (match-define (Rect (Pos x1 y1) (Pos x2 y2)) rect)
  (Rect (Pos (min x1 x2) (min y1 y2))
        (Pos (max x1 x2) (max y1 y2))))

(define (rect->rank-rect rect x->rankx y->ranky)
  (match-define (Rect p1 p2) rect)
  (Rect (pos->rank-pos p1 x->rankx y->ranky)
        (pos->rank-pos p2 x->rankx y->ranky)))

(define (pos->rank-pos pos x->rankx y->ranky)
  (Pos (x->rankx (Pos-x pos)) (y->ranky (Pos-y pos))))

(define (rectangle-inside-polygon/range-sum? rect area-sum2d x->rankx y->ranky)
  (define rank-rect (rect->rank-rect (normalize-rect rect) x->rankx y->ranky))
  (match-define (Rect (Pos x1 y1) (Pos x2 y2)) rank-rect)
  (= (- (area-sum2d x2 y2)
        (area-sum2d x2 (sub1 y1))
        (area-sum2d (sub1 x1) y2)
        (- (area-sum2d (sub1 x1) (sub1 y1))))
     (area rank-rect)))

;; compress sparse values
(define (compress-values vals)
  (define sorted-xs (remove-duplicates (sort vals <)))
  (for/hash ([x (in-list sorted-xs)]
             [i (in-naturals)])
    (values x i)))

(define (area-sum2d-querier pos-vec)
  (define poses (vector->list pos-vec))

  (define x->rankx-hash (compress-values (map Pos-x poses)))
  (define (x->rankx x) (hash-ref x->rankx-hash x))
  (define xn (hash-count x->rankx-hash))

  (define y->ranky-hash (compress-values (map Pos-y poses)))
  (define (y->ranky y) (hash-ref y->ranky-hash y))
  (define yn (hash-count y->ranky-hash))

  (define rank-poses
    (for/list ([p (in-list poses)])
      (pos->rank-pos p x->rankx y->ranky)))

  (define grid (make-array (+ 2 xn) (+ 2 yn) 1))
  (fill-polygon! grid rank-poses)
  (flood-fill! grid)

  (define grid-area-sum (make-array xn yn 0))
  (define (area-sum x y)
    (cond [(< x 0) 0]
          [(< y 0) 0]
          [else (aref grid-area-sum x y)]))

  (for* ([x (in-range xn)]
         [y (in-range yn)])
    (define area (+ (area-sum (sub1 x) y)
                    (area-sum x (sub1 y))
                    (- (area-sum (sub1 x) (sub1 y)))
                    (sgn (aref grid (add1 x) (add1 y)))))
    (aset! grid-area-sum x y area))

  (values x->rankx y->ranky area-sum))

(define (fill-polygon! grid poses)
  (for ([p1 (in-list (cons (last poses) poses))]
        [p2 (in-list poses)])
    (match-define (Pos x1 y1) p1)
    (match-define (Pos x2 y2) p2)
    (for* ([x (in-inclusive-range (min x1 x2) (max x1 x2))]
           [y (in-inclusive-range (min y1 y2) (max y1 y2))])
      (aset! grid (add1 x) (add1 y) 2))))

(define (flood-fill! grid)
  (define-values (rows cols) (vector2d-size grid))
  (define (valid? pos)
    (and (< -1 (Position-x pos) rows)
         (< -1 (Position-y pos) cols)))

  (define init (Position 0 0))
  (vector2d-set! grid init 0)
  (let loop ([pending (list init)])
    (when (not (null? pending))
      (define pos (car pending))
      (define nexts
        (for/list ([next (list (move-up pos) (move-down pos) (move-left pos) (move-right pos))]
                   #:when (and (valid? next)
                               (= 1 (vector2d-ref grid next))))
          (vector2d-set! grid next 0)
          next))
      (loop (append nexts (cdr pending))))))
