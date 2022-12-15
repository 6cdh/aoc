#lang racket

(require "lib.rkt")

(define (move1 pos Δ)
  (map + pos Δ))

(define (add-line! barriers from to)
  (let ([Δ (list (sgn (- (first to) (first from)))
                 (sgn (- (second to) (second from))))])
    (let fill ([pos from])
      (hash-set! barriers pos #t)
      (when (not (equal? pos to))
        (fill (move1 pos Δ))))))

(define (string->positions str)
  (~> (string-split str #px"[ >,-]")
      (filter (negate string-empty?) %)
      (map string->number %)
      (chunks 2 %)))

(define (read-barriers)
  (let ([lines (read-lines)]
        [floor 0]
        [barriers (make-hash)])
    (for ([line lines])
      (let ([poses (string->positions line)])
        (for ([from poses]
              [to (cdr poses)])
          (set! floor (max floor (second from) (second to)))
          (add-line! barriers from to))))
    (cons barriers floor)))

(define (emulate node barriers inf-floor record)
  (when (and (not (hash-has-key? barriers node))
             (not (= inf-floor (second node))))
    (record node barriers)
    (for ([Δ '((0 1) (-1 1) (1 1))])
      (emulate (move1 node Δ) barriers inf-floor record))
    (hash-set! barriers node #t)))

(define (day14)
  (match-let ([(cons barriers floor) (read-barriers)])
    (let ([rocks (hash-count barriers)]
          [first-abyss -1])
      (emulate '(500 0) barriers (+ 2 floor)
               (λ (node bs)
                 (when (and (= floor (second node))
                            (= first-abyss -1))
                   (set! first-abyss
                         (- (hash-count bs) rocks)))))

      (println first-abyss)
      (println (- (hash-count barriers) rocks)))))

(time (day14))
