#lang racket

(require "lib.rkt")

(define (read-input)
  (define lines (read-lines))
  (list (cdr (string-split-in-spaces (first lines)))
        (cdr (string-split-in-spaces (second lines)))))

(define (ways total-time best)
  ;; slow method: brute force
  #;
  (for/count ([hold (in-range 0 (add1 total-time))])
    (> (* hold (- total-time hold)) best))

  ;; fast method: solve quadratic equation
  (match-let ([(list left right)
               (solve-quadratic-equation -1 total-time (- best))])
    (max 0 (- (exact-ceiling (sub1 right)) (exact-floor (add1 left)) -1))))

(define (solve times distances)
  (for/product ([t times]
                [d distances])
    (ways t d)))

(define (main)
  (match-define (list times-str distances-str) (read-input))
  (println (solve (map string->number times-str)
                  (map string->number distances-str)))
  (println (solve (list (string->number (string-append* times-str)))
                  (list (string->number (string-append* distances-str))))))

(main)

