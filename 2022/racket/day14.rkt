#lang racket

(require "lib.rkt")

(define (move1 pos Δ)
  (map + pos Δ))

(define (read-barriers)
  (let ([lines (read-lines)]
        [floor 0]
        [barriers (make-hash)])
    (for ([line lines])
      (let ([poses (~> (string-split line #px"[ >,-]")
                       (filter (negate string-empty?) %)
                       (map string->number %)
                       (chunks 2 %))])
        (for ([pos-from poses]
              [pos-to (cdr poses)])
          (set! floor (max floor (second pos-from) (second pos-to)))

          (let ([Δ (list (sgn (- (first pos-to) (first pos-from)))
                         (sgn (- (second pos-to) (second pos-from))))])
            (let fill ([pos pos-from])
              (hash-set! barriers pos #t)
              (when (not (equal? pos pos-to))
                (hash-set! barriers pos #t)
                (fill (move1 pos Δ))))))))
    (cons barriers floor)))

(define (emulate barriers floor ∞?)
  (let ([source '(500 0)])
    (define (fall-sand pos)
      (cond [(= floor (second pos)) pos]
            [(and ∞? (= (sub1 floor) (second pos))) pos]
            [else
             (or (for/first ([Δ '((0 1) (-1 1) (1 1))]
                             #:when (not (hash-has-key? barriers (move1 pos Δ))))
                   (fall-sand (move1 pos Δ)))
                 pos)]))

    (let loop ([cnt 0])
      (let ([final-pos (fall-sand source)])
        (cond [(= floor (second final-pos)) cnt] ; drop to abyss
              [(equal? source final-pos) (add1 cnt)] ; drop to infinity floor
              [else
               (hash-set! barriers final-pos #t)
               (loop (add1 cnt))])))))

(define (day14)
  (match-let ([(cons barriers floor) (read-barriers)])
    (println (emulate (hash-copy barriers) floor #f))
    (println (emulate (hash-copy barriers) (+ 2 floor) #t))))

(time (day14))
