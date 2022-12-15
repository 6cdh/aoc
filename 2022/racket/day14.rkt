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

(define (emulate barriers floor ∞?)
  (define (trace-sand pos-lst)
    (let ([pos (car pos-lst)])
      (cond [(= floor (second pos)) pos-lst] ; with abyss
            [(and ∞? (= (sub1 floor) (second pos))) pos-lst] ; with infinity floor
            [else
             (or (for/first ([Δ '((0 1) (-1 1) (1 1))]
                             #:when (not (hash-has-key? barriers (move1 pos Δ))))
                   (trace-sand (cons (move1 pos Δ) pos-lst)))
                 pos-lst)])))

  (let count-sands ([cnt 0]
                    [pos-lst (list '(500 0))])
    (let* ([new-pos-lst (trace-sand pos-lst)]
           [pos (car new-pos-lst)])
      (cond [(= floor (second pos)) cnt] ; with abyss
            [(= 0 (second pos)) (add1 cnt)] ; with infinity floor
            [else
             (hash-set! barriers pos #t)
             (count-sands (add1 cnt) (cdr new-pos-lst))]))))

(define (day14)
  (match-let ([(cons barriers floor) (read-barriers)])
    (let ([res1 (emulate barriers floor #f)])
      (println res1)
      (println (+ res1 (emulate barriers (+ 2 floor) #t))))))

(time (day14))
