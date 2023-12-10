#lang racket

(require "lib.rkt")

(define Point list)
(define Point-x first)
(define Point-y second)

(define (neighbors sketch point)
  (define (can-connect-back? to)
    (member point (points-connect-to sketch to)))

  (filter can-connect-back? (points-connect-to sketch point)))

(define (valid? sketch pos)
  (match-define (list m n) (array-dims sketch 2))
  (and (< -1 (Point-x pos) m)
       (< -1 (Point-y pos) n)))

(define (points-connect-to sketch point)
  (filter-map (λ (delta)
                (define new-point (map + delta point))
                (if (valid? sketch new-point)
                    new-point
                    #f))
              (match (aref sketch (Point-x point) (Point-y point))
                [#\| '((1 0) (-1 0))]
                [#\- '((0 1) (0 -1))]
                [#\L '((-1 0) (0 1))]
                [#\J '((-1 0) (0 -1))]
                [#\7 '((0 -1) (1 0))]
                [#\F '((0 1) (1 0))]
                [#\S '((0 1) (0 -1) (1 0) (-1 0))]
                [#\. '()]
                [c (error "unexpected tile" c)])))

(define (find-loop sketch init-point)
  (define visited (mutable-set))
  (define sides (mutable-set))

  (define (bfs dist points)
    (define new-points '())

    (for ([point points])
      (for ([nei (neighbors sketch point)])
        (set-add! sides (list point nei))
        (when (not (set-member? visited nei))
          (list-push-head! new-points nei)
          (set-add! visited nei))))

    (if (null? new-points)
        (list dist visited sides)
        (bfs (add1 dist) new-points)))

  (bfs 0 (list init-point)))

(define (find-s-pos sketch)
  (match-define (list m n) (array-dims sketch 2))
  (for*/first ([i m]
               [j n]
               #:when (char=? #\S (aref sketch i j)))
    (Point i j)))

;; part 2 BEGIN

(struct VerticalSide
  (x-up x-down y)
  #:transparent)

;; https://en.wikipedia.org/wiki/Point_in_polygon
;; original version
;; linear time complexity
(define (point-outside-polygon? point polygon)
  (even?
   (for/count ([line polygon])
     (and (< (VerticalSide-y line) (Point-y point))
          (<= (VerticalSide-x-up line) (Point-x point))
          (> (VerticalSide-x-down line) (Point-x point))))))

;; fast version
(define (point-outside-polygon-fast? polygon)
  ;; the side length is always 1, so
  ;; the count condition can be simplified to
  #;
  (and (< (VerticalSide-y side) (Point-y point))
       (= (VerticalSide-x-up side) (Point-x point)))
  ;; hashtable + linear count is efficient enough

  (define x-mapping (make-hash))
  (for ([side polygon])
    (hash-update! x-mapping (VerticalSide-x-up side)
                  (λ (old) (cons (VerticalSide-y side) old))
                  '()))
  (λ (point)
    (even?
     (for/count ([y (hash-ref x-mapping (Point-x point) '())])
       (< y (Point-y point))))))

(define (count-inside sketch side-points vert-sides)
  (match-define (list m n) (array-dims sketch 2))
  (define outside-polygon? (point-outside-polygon-fast? vert-sides))
  (for*/count ([x m]
               [y n])
    (not (or (set-member? side-points (Point x y))
             (outside-polygon? (Point x y))))))

(define (get-vertical-sides connect)
  (for*/list ([pair connect]
              [p (in-value (first pair))]
              [p2 (in-value (second pair))]
              #:when (and (= (Point-y p) (Point-y p2))
                          (<= (Point-x p) (Point-x p2))))
    (VerticalSide (Point-x p) (Point-x p2) (Point-y p))))

;; part2 END

;; debug function
;; replace weird tile characters with human readable unicode box drawing characters
;; which is defined in https://www.w3.org/TR/xml-entity-names/025.html
(define (preview! input)
  (for ([row input])
    (displayln
     (list->string
      (for/list ([c row])
        (match c
          [#\| #\u2502]
          [#\- #\u2500]
          [#\L #\u2514]
          [#\J #\u2518]
          [#\7 #\u2510]
          [#\F #\u250c]
          [#\S #\u253c]
          [c c]))))))

(define (main)
  (define sketch (read-lines-as-vector2d))
  (match-define (list dist side-points sides)
    (find-loop sketch (find-s-pos sketch)))
  (println dist)
  (println (count-inside sketch side-points (get-vertical-sides sides))))

(main)
