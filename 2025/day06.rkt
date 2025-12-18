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
  (define columns (list-transpose (map string-split lines)))
  (for/list ([col (in-list columns)])
    (match-define (list nums ... op) col)
    (cons op (map string->number nums))))

(define (parse2 lines)
  (define character-columns (list-transpose (map string->list lines)))
  (define no-space-char-columns
    (for/list ([col (in-list character-columns)])
      (filter-not (λ (ch) (eq? ch #\space)) col)))
  (define expr-groups
    (list-split no-space-char-columns '()))

  (for/list ([expr-group (in-list expr-groups)])
    ; `expr-group` is each column without whitespace
    (match expr-group
      [(cons (list digit-chars ... op) operand-digit-lists)
       (cons (string op)
             (cons (compose-number digit-chars)
                   (map compose-number operand-digit-lists)))])))

(define (compose-number digit-chars)
  (string->number (list->string digit-chars)))
