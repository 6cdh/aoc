#lang racket

(require "lib.rkt")

(struct PartNum
  (x y0 y1 num)
  #:transparent)

(define (parse-all-part-numbers input)
  (for*/list ([(line-vec x) (in-indexed input)]
              [line (in-value (vector->string line-vec))]
              [pos (regexp-match-positions* #px"[0-9]+" line)])
    (PartNum x (car pos) (cdr pos)
             (string->number (substring line (car pos) (cdr pos))))))

(define (neighbors input partnum)
  (define m (vector-length input))
  (define n (vector-length (aref input 0)))
  (match-define (PartNum x y0 y1 _num) partnum)

  (define (valid? pos)
    (and (< -1 (first pos) m)
         (< -1 (second pos) n)))

  (define ys (range (sub1 y0) (add1 y1)))
  (~> poses (append (map (λ (y) (list (sub1 x) y)) ys)
                    (list (list x (sub1 y0)) (list x y1))
                    (map (λ (y) (list (add1 x) y)) ys))
      (filter valid? poses)))

(define (input-is input is?)
  (λ (pos) (is? (aref input (first pos) (second pos)))))

(define (part1 input part-numbers)
  (define schematic-symbol?
    (input-is input (λ (c) (not (or (char-numeric? c) (char=? c #\.))))))
  (for/sum ([pn part-numbers]
            #:when (ormap schematic-symbol? (neighbors input pn)))
    (PartNum-num pn)))

(define (part2 input part-numbers)
  (define counter (make-hash))
  (define ratio (make-hash))
  (define gear? (input-is input (λ (c) (char=? c #\*))))

  (for ([pn part-numbers])
    (for ([nei (neighbors input pn)])
      (when (gear? nei)
        (hash-update! counter nei add1 0)
        (hash-update! ratio nei (λ (old) (* old (PartNum-num pn))) 1))))
  (for/sum ([(pos cnt) counter]
            #:when (= cnt 2))
    (hash-ref ratio pos)))

(define (main)
  (define input (read-lines-as-vector2d))
  (define part-numbers (parse-all-part-numbers input))

  (println (part1 input part-numbers))
  (println (part2 input part-numbers)))

(main)

