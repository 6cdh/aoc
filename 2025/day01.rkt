#lang racket

(provide solve)

(define (solve in)
  (define instructions (port->lines in))
  (for/fold ([cur-pos 50]
             [ans1 0]
             [ans2 0]
             #:result (values ans1 ans2))
            ([rot instructions])
    (define-values (new-pos zeros) (rotate-once cur-pos rot))
    (values new-pos
            (if (= new-pos 0) (add1 ans1) ans1)
            (+ ans2 zeros))))

(define (rotate-once start rot)
  (define dir-ch (string-ref rot 0))
  (define steps (string->number (substring rot 1)))

  (define-values (counting-fn multiplier)
    (if (eq? dir-ch #\L)
        (values count-100s-left -1)
        (values count-100s-right 1)))
  (define end (+ start (* multiplier steps)))
  (define zeros (counting-fn start end))
  (values (modulo end 100) zeros))

;; count multiples of 100 for rightward rotation, in range (from, to]
;; where from <= to and 0 <= from < 100
;; 0 is counted.
(define (count-100s-right from to)
  (if (= to 0)
      1
      (quotient to 100)))

;; count multiples of 100 for leftward rotation, in range [to, from)
;; where to <= from and 0 <= from < 100
;; 0 is counted.
(define (count-100s-left from to)
  (if (> to 0)
      0
      (+ (quotient to -100)
         ; count 0
         (if (> from 0) 1 0))))
