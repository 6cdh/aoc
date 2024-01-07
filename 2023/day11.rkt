#lang racket

(require "lib.rkt")

(define Point list)
(define Point-x first)
(define Point-y second)

(define (find-galaxies image)
  (for*/list ([(row i) (in-indexed image)]
              [(c j) (in-indexed row)]
              #:when (char=? #\# c))
    (Point i j)))

(define (empty-rows-before-vector image)
  (define empty-rows-before 0)
  (list->vector
   (for/list ([row image])
     (define tmp empty-rows-before)
     (when (andmap (λ (c) (char=? c #\.)) row)
       (set! empty-rows-before (add1 empty-rows-before)))
     tmp)))

(define (expand-coordinate image galaxy times)
  (define empty-rows-before (empty-rows-before-vector image))
  (define empty-cols-before (empty-rows-before-vector (transpose image)))

  (for/list ([g galaxy])
    (Point (+ (Point-x g) (* (sub1 times) (aref empty-rows-before (Point-x g))))
           (+ (Point-y g) (* (sub1 times) (aref empty-cols-before (Point-y g)))))))

(define (solve image times)
  (define galaxy-vec (list->vector (expand-coordinate image (find-galaxies image) times)))
  (for*/sum ([j (vector-length galaxy-vec)]
             [i j])
    (define g1 (aref galaxy-vec i))
    (define g2 (aref galaxy-vec j))
    (+ (absdiff (Point-x g1) (Point-x g2))
       (absdiff (Point-y g1) (Point-y g2)))))

(define (main)
  (define image (map string->list (read-lines)))
  (println (solve image 2))
  (println (solve image 1000000)))

(main)

