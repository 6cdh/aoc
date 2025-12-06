#lang racket

(provide solve)

(require "utils.rkt")

(define (solve in)
  (define lines (port->lines in))
  (values (solve-math (parse lines)) (solve-math (parse2 lines))))

(define (solve-math exprs)
  (for/sum ([expr (in-list exprs)])
    (match expr
      [(cons "+" nums) (apply + nums)]
      [(cons "*" nums) (apply * nums)])))

(define (parse lines)
  (define rev-columns
    (~> rows (map string-split lines)
        (list-transpose (reverse rows))))
  (for/list ([col rev-columns])
    (match-define (cons op nums) col)
    (cons op (map string->number nums))))

(define (parse2 lines)
  (define char-columns
    (~> rows (map string->list lines)
        (list-transpose rows)))
  (define char-columns-no-whitespace
    (for/list ([col char-columns])
      (filter-not (curry eq? #\space) col)))
  (define columns
    (list-split char-columns-no-whitespace '()))

  (for/list ([col (in-list columns)])
    ; `col` is each column without whitespace
    (match col
      [(cons (list digits ... op) operands)
       (cons (string op)
             (cons (compose-number digits)
                   (map compose-number operands)))])))

(define (compose-number digits)
  (string->number (list->string digits)))
