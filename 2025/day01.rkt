#lang racket

(provide solve)

(define (solve in)
  (define instructions (port->lines in))
  (for/fold [[pos 50] [ans1 0] [ans2 0]
             #:result (values ans1 ans2)]
            ([rot (in-list instructions)])
    (define-values (new-pos zeros) (rotate-once pos rot))
    (values new-pos
            (if (= new-pos 0) (add1 ans1) ans1)
            (+ ans2 zeros))))

(define (rotate-once start-pos rot)
  (define-values (count-zeros direction)
    (match (string-ref rot 0)
      [#\L (values count-100s-left -1)]
      [#\R (values count-100s-right 1)]))

  (define rot-count (string->number (substring rot 1)))
  (define end-pos (+ start-pos (* direction rot-count)))
  (define zeros (count-zeros start-pos end-pos))
  (values (modulo end-pos 100) zeros))

;; count multiples of 100 for rightward rotation, in range (from, to]
;; where from <= to and 0 <= from < 100
(define (count-100s-right from to)
  (if (= to 0) 1 (quotient to 100)))

;; count multiples of 100 for leftward rotation, in range [to, from)
;; where to <= from and 0 <= from < 100
(define (count-100s-left from to)
  (cond [(> to 0) 0]
        [(= to 0) 1]
        [else (+ (quotient to -100) (if (> from 0) 1 0))]))
