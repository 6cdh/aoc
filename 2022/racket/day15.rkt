#lang racket

(require "lib.rkt")
(require (only-in data/integer-set
                  make-integer-set
                  make-range
                  get-integer
                  [make-range empty-integer-set]
                  [count integer-set-count]
                  [union integer-set-union]
                  [subtract integer-set-subtract]))

(define (manhattan-dist x0 y0 x1 y1)
  (+ (abs (- x1 x0))
     (abs (- y1 y0))))

(define (read-input)
  (let ([lines (read-lines)])
    (for/fold ([sensors '()]
               [beacons (set)]
               #:result (cons sensors beacons))
              ([line lines])
      (match-let* ([numbers (~> (regexp-match* #rx"[-0-9]+" line)
                                (map string->number %))]
                   [(list sx sy bx by) numbers]
                   [sd (manhattan-dist sx sy bx by)])
        (values (cons (list sx sy sd) sensors)
                (set-add beacons (list bx by)))))))

(define (sensor-intersect-y-axis s y)
  (match-let* ([(list sx sy sd) s]
               [abs-diffx (- sd (abs (- y sy)))])
    (if (negative? abs-diffx)
        (empty-integer-set)
        (make-integer-set (list (cons (- sx abs-diffx) (+ sx abs-diffx)))))))

(define (sensors-intersect-y-axis sensors y)
  (for/fold ([iset (empty-integer-set)])
            ([s sensors])
    (integer-set-union iset (sensor-intersect-y-axis s y))))

(define (puzzle1 sensors beacons)
  (let* ([y #e2e6]
         [iset (sensors-intersect-y-axis sensors y)])
    (- (integer-set-count iset)
       (count (λ (b) (= y (second b))) (set->list beacons)))))

(define (puzzle2 sensors)
  (for/first ([y (add1 #e4e6)]
              #:when
              (~> (integer-set-subtract
                   (make-range 0 #e4e6)
                   (sensors-intersect-y-axis sensors y))
                  (integer-set-count %)
                  (positive? %)))
    (let ([x (~> (integer-set-subtract
                  (make-range 0 #e4e6)
                  (sensors-intersect-y-axis sensors y))
                 (get-integer %))])
      (+ (* #e4e6 x) y))))

(define (day15)
  (match-let ([(cons sensors beacons) (read-input)])
    (println (puzzle1 sensors beacons))
    (println (puzzle2 sensors))))

(time (day15))

