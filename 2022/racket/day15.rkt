#lang racket

(require "lib.rkt")
(require (only-in data/integer-set
                  make-integer-set
                  make-range
                  get-integer
                  [make-range empty-integer-set]
                  [count integer-set-count]
                  [union integer-set-union]))

;; Concept
;; diamond: perception area of a sensor
;; outer-line: a line that adjacent to a side of a diamond
;;                         and can't be perceived by the sensor
;;             a diamond have 4 outer-lines

(define *limitl* 0)
(define *limitr* #e4e6)

(struct Point (x y)
  #:methods gen:equal+hash
  [(define equal-proc
     (λ (pa pb _)
       (equal? (point->list pa) (point->list pb))))
   (define hash-proc
     (λ (pt _)
       (equal-hash-code (point->list pt))))
   (define hash2-proc
     (λ (pt _)
       (equal-secondary-hash-code (point->list pt))))])

(struct Line (p1 p2))

(define (point->list pt)
  (match-let ([(Point x y) pt])
    (list x y)))

(define (manhattan-dist pt1 pt2)
  (match-let ([(Point x1 y1) pt1]
              [(Point x2 y2) pt2])
    (+ (abs (- x2 x1))
       (abs (- y2 y1)))))

(define (read-input)
  (let ([lines (read-lines)])
    (for/fold ([sensors '()]
               [beacons (set)]
               #:result (cons sensors beacons))
              ([line lines])
      (match-let* ([numbers (~> (regexp-match* #rx"[-0-9]+" line)
                                (map string->number %))]
                   [(list sx sy bx by) numbers]
                   [spt (Point sx sy)]
                   [bpt (Point bx by)]
                   [sd (manhattan-dist spt bpt)])
        (values (cons (list spt sd) sensors)
                (set-add beacons bpt))))))

;; PUZZLE 1

;; (-> sensor Integer Integer-set)
(define (sensor-intersect-y-axis s y)
  (match-let* ([(list (Point sx sy) sd) s]
               [abs-diffx (- sd (abs (- y sy)))])
    (if (negative? abs-diffx)
        (empty-integer-set)
        (make-integer-set (list (cons (- sx abs-diffx) (+ sx abs-diffx)))))))

;; (-> (Listof sensor) Integer Integer-set)
(define (sensors-intersect-y-axis sensors y)
  (for/fold ([iset (empty-integer-set)])
            ([s sensors])
    (integer-set-union iset (sensor-intersect-y-axis s y))))

(define (puzzle1 sensors beacons)
  (let* ([y #e2e6]
         [iset (sensors-intersect-y-axis sensors y)])
    (- (integer-set-count iset)
       (count (λ (b) (= y (Point-y b)))
              (set->list beacons)))))

;; PUZZLE 2

;; (-> sensor (Listof Line))
;; get outer lines of a diamond that formed by a sensor
(define (outer-lines-of sensor)
  (match-let* ([(list (Point sx sy) sd) sensor]
               [up (Point sx (- sy sd 1))]
               [down (Point sx (+ sy sd 1))]
               [left (Point (- sx sd 1) sy)]
               [right (Point (+ sx sd 1) sy)])
    (list (Line up left)
          (Line up right)
          (Line left down)
          (Line right down))))

;; (-> Line Line (Listof Point))
;; only works for the lines have one or zero intersection
(define (line-intersection l1 l2)
  (define (vertical? l)
    (= (Point-x (Line-p1 l))
       (Point-x (Line-p2 l))))

  (define (slop-intercept x1 y1 x2 y2)
    (let* ([k ($ (- y1 y2) / (- x1 x2))]
           [b ($ y1 - (* k x1))])
      (cons k b)))

  (define (line-point-y k b x)
    ($ k * x + b))

  (match-let ([(Line (Point x1 y1) (Point x2 y2)) l1]
              [(Line (Point x3 y3) (Point x4 y4)) l2])
    (cond [(and (vertical? l1) (vertical? l2)) '()]
          [(vertical? l2) (line-intersection l2 l1)]
          [(vertical? l1) (match-let ([(cons k b) (slop-intercept x3 y3 x4 y4)])
                            (list (Point x1 (line-point-y k b x1))))]
          [else (match-let ([(cons k1 b1) (slop-intercept x1 y1 x2 y2)]
                            [(cons k2 b2) (slop-intercept x3 y3 x4 y4)])
                  (if (= k1 k2)
                      '()
                      (let* ([x (- ($ (- b1 b2) / (- k1 k2)))]
                             [y (line-point-y k1 b1 x)])
                        (list (Point x y)))))])))

(define (puzzle2 sensors)
  (define (in-limit pt)
    (and (<= *limitl* (Point-x pt) *limitr*)
         (<= *limitl* (Point-y pt) *limitr*)))

  (let* ([ul (Point *limitl* *limitl*)]
         [ur (Point *limitr* *limitl*)]
         [bl (Point *limitl* *limitr*)]
         [br (Point *limitr* *limitr*)]
         [lines (append*
                 (list (Line ul ur)
                       (Line ul bl)
                       (Line ur br)
                       (Line bl br))
                 (map outer-lines-of sensors))])
    (for*/first ([l1 lines]
                 [l2 lines]
                 [pt (line-intersection l1 l2)]
                 #:when
                 (and (in-limit pt)
                      (for/and ([s sensors])
                        (> (manhattan-dist pt (first s)) (second s)))))
      ($ #e4e6 * (Point-x pt) + (Point-y pt)))))

(define (day15)
  (match-let ([(cons sensors beacons) (read-input)])
    (println (puzzle1 sensors beacons))
    (println (puzzle2 sensors))))

(time (day15))

