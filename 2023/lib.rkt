#lang racket

(provide digit-char->integer
         read-lines
         string-reverse
         product
         debugv)

(define (read-lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        '()
        (cons line (read-lines)))))

(define (digit-char->integer c)
  (- (char->integer c) (char->integer #\0)))

(define (string-reverse str)
  (list->string (reverse (string->list str))))

(define-syntax-rule (debugv expr)
  (let ([res expr])
    (println res)
    res))

(define (product lst)
  (foldl * 1 lst))
