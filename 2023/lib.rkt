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
         vector-update!
         for/min
         minimum
         chunks
         read-and-split-in-empty-line
         list-append-head!
         list-push-head!
         first
         second
         third
         fourth)

(define (read-lines in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        '()
        (cons line (read-lines in)))))

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
    (displayln (format "~a: ~v" (quote expr) res))
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

(define (vector-update! vec i updater)
  (vector-set! vec i (updater (vector-ref vec i))))

(define-syntax-rule
  (for/min init iter
    body ...
    last)
  (for/fold ([minv init])
            iter
    body ...
    (min minv last)))

(define (minimum lst)
  (foldl min (car lst) lst))

(define (chunks lst k)
  (if (<= (length lst) k)
      (list lst)
      (cons (take lst k) (chunks (drop lst k) k))))

(define (read-and-split-in-empty-line)
  (define input (port->string))
  (string-split input
                ;; platform compatibility
                (if (string-contains? input "\r")
                    "\r\n"
                    "\n\n")
                #:repeat? #t))

(define-syntax-rule (list-push-head! lst val)
  (set! lst (cons val lst)))

(define-syntax-rule (list-append-head! lst val)
  (set! lst (append val lst)))

;; fast version of builtin functions
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

