#lang racket

(require "lib.rkt")

(define (run-cpu)
  (let ([instructions (read-lines)])
    (for/fold ([x 1]
               [xs '()]
               #:result (list->vector (reverse xs)))
              ([ins instructions])
      (let ([ws (string-split ins)])
        (match (first ws)
          ["noop" (values x (cons x xs))]
          ["addx" (values (+ x (string->number (second ws)))
                          (append (list x x) xs))])))))

(define (day10)
  (let ([xs (run-cpu)])
    ;; part 1
    (println (~> (range 19 240 40)
                 (map (λ (t) (* (add1 t) (aref xs t))) %)
                 (sum %)))
    ;; part 2
    (for ([i 6])
      (for ([j 40])
        (let* ([t (+ (* i 40) j)]
               [x (aref xs t)])
          (if (<= (sub1 x) (modulo t 40) (add1 x))
              (display #\#)
              (display #\.))))
      (displayln ""))))

(time (day10))

