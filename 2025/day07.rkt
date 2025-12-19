#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define grid (read-vector2d in))
  (define S (vector2d-find grid #\S))
  (values (count^ S grid) (count-timelines-dp S grid)))

(define (valid-pos? pos grid)
  (define-values (m n) (vector2d-size grid))
  (and (< -1 (Position-row pos) m)
       (< -1 (Position-col pos) n)))

(define (count^ beam grid)
  (define below (move-down beam))
  (if (valid-pos? below grid)
      (count^-rec beam below grid)
      0))

(define (count^-rec beam below grid)
  (match (vector2d-ref grid below)
    [#\| 0]
    [#\.
     (vector2d-set! grid below #\|)
     (count^ below grid)]
    [#\^
     (+ 1
        (count^ (move-left below) grid)
        (count^ (move-right below) grid))]))

;; == part 2: dynamic programming (top down style) ==

(define (count-timelines-dp S grid)
  (define-values (m n) (vector2d-size grid))

  (define/cache-vec (count-rec particle)
    #:vector (m n #f)
    #:cache [(Position-row particle) (Position-col particle)]

    (define below (move-down particle))
    (if (not (valid-pos? below grid))
        1
        (match (vector2d-ref grid below)
          [#\.
           (vector2d-set! grid below #\|)
           (count-rec below)]
          [#\|
           (count-rec below)]
          [#\^
           (+ (count-rec (move-left below))
              (count-rec (move-right below)))])))

  (count-rec S))

;; == part 2: BFS ==

; BFS, but does not track visited positions, instead,
; scan line by line.
(define (count-timelines-bfs S grid)
  (define-values (m n) (vector2d-size grid))
  ; `worlds` stores how many worlds exists when the particle reachs at each position.
  (define worlds (make-array m n 0))
  (vector2d-set! worlds S 1)

  ; At position `pos`, increase `many` worlds where the particle can reach.
  (define (increase! pos many)
    (vector2d-update! worlds pos (λ (old) (+ old many))))

  (define (bfs)
    (for* ([r (in-range (sub1 m))]
           [c (in-range n)])
      (define pos (Position r c))
      (define below (move-down pos))
      (define w (vector2d-ref worlds pos))
      (match (vector2d-ref grid below)
        [#\^
         (increase! (move-left below) w)
         (increase! (move-right below) w)]
        [_ (increase! below w)])))

  (bfs)
  (for/sum ([j (in-range n)])
    (vector2d-ref worlds (Position (sub1 m) j))))
