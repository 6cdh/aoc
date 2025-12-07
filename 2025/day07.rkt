#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define grid (read-vector2d in))
  (define S (vector2d-find grid #\S))
  (values (count^ S grid) (count-timelines-dp S grid)))

(define (valid? pos grid)
  (define-values (m n) (vector2d-size grid))
  (and (< -1 (Position-row pos) m)
       (< -1 (Position-col pos) n)))

(define (count^ beam grid)
  (define next (move-down beam))
  (if (not (valid? next grid))
      0
      (match (vector2d-ref grid next)
        [#\.
         (vector2d-set! grid next #\|)
         (count^ next grid)]
        [#\| 0]
        [#\^ (+ 1
                (count^ (move-left next) grid)
                (count^ (move-right next) grid))])))

;; == part 2: dynamic programming (top down style) ==

(define (count-timelines-dp S grid)
  (define-values (m n) (vector2d-size grid))

  (define/cache-vec (count-rec particle)
    #:vector (m n #f)
    #:cache ((Position-row particle) (Position-col particle))

    (define next (move-down particle))
    (if (not (valid? next grid))
        1
        (match (vector2d-ref grid next)
          [#\.
           (vector2d-set! grid next #\|)
           (count-rec next)]
          [#\|
           (count-rec next)]
          [#\^
           (+ (count-rec (move-left next))
              (count-rec (move-right next)))])))

  (count-rec S))

;; == part 2: BFS ==

; BFS, but does not track visited positions, instead,
; scan line by line.
(define (count-timelines-bfs S grid)
  (define-values (m n) (vector2d-size grid))
  ; `worlds` stores how many worlds the particle can reach it
  ; for each position.
  (define worlds (make-array m n 0))
  (vector2d-set! worlds S 1)

  ; increase `many` worlds where the particle can reach `pos`.
  (define (increase! pos many)
    (vector2d-update! worlds pos (λ (old) (+ old many))))

  (define (bfs)
    (for* ([i (in-range (sub1 m))]
           [j (in-range n)])
      (define pos (Position i j))
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
