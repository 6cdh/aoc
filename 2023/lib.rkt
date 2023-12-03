#lang racket

(provide digit-char->integer
         read-lines
         read-lines-as-vector2d
         string->vector
         string-reverse
         product
         debugv
         aref
         sum
         absdiff
         list->number
         vector->string
         ~>
         first
         second
         third
         fourth)

(define (read-lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        '()
        (cons line (read-lines)))))

(define (read-lines-as-vector2d)
  (list->vector (map string->vector (read-lines))))

(define (string->vector str)
  (list->vector (string->list str)))

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

;; (aref vector dims ...)
;; like `vector-ref`, but also work for nested vectors
(define-syntax aref
  (syntax-rules ()
    [(_ arr)
     arr]
    [(_ arr i idx ...)
     (aref (vector-ref arr i) idx ...)]))

(define (sum lst)
  (foldl + 0 lst))

(define (absdiff a b)
  (abs (- a b)))

(define (list->number lst)
  (string->number (list->string lst)))

(define (vector->string vec)
  (list->string (vector->list vec)))

(define-syntax-rule (~> x exprs ...)
  (let* ([x exprs] ...)
    x))

;; fast version of builtin functions
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

