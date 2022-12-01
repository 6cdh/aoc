#lang racket

(provide read-lines
         list-sum)

(define (read-lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        '()
        (cons line (read-lines)))))

(define (list-sum lst)
  (foldl + 0 lst))


