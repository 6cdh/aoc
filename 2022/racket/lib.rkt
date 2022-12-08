#lang racket

(require (for-syntax syntax/parse))

(provide read-lines
         sum
         $
         ~>
         string->set
         chunks
         make-array
         aref
         aset!
         aupd!
         maximum
         minimum
         product)

(define-syntax (~> stx)
  (syntax-parse stx
    #:datum-literals (%)
    [(_ v)
     #'v]
    [(_ v (fn args1 ... % args2 ...) rems ...)
     #'(~> (fn args1 ... v args2 ...) rems ...)]))

(define (read-lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        '()
        (cons line (read-lines)))))

(define (sum lst)
  (foldl + 0 lst))

(define-syntax $
  (syntax-rules ()
    [($ v)
     v]
    [($ v op v2 args ...)
     ($ (op v v2) args ...)]))

(define (string->set str)
  (~> str
      (string->list %)
      (list->set %)))

(define (chunks k lst)
  "groups the list `seq` with `k` element as a group"
  (~> (map cons lst (range 0 (length lst)))
      (group-by (λ (p) (quotient (cdr p) k)) %)
      (map (λ (g) (map car g)) %)))

(define-syntax make-array
  (syntax-rules ()
    [(_ n init)
     (make-vector n init)]
    [(_ m n dims ... init)
     (build-vector m (λ (_) (make-array n dims ... init)))]))

(define-syntax aref
  (syntax-rules ()
    [(_ arr)
     arr]
    [(_ arr i idx ...)
     (aref (vector-ref arr i) idx ...)]))

(define-syntax-rule (aset! arr idx ... i v)
  (vector-set! (aref arr idx ...) i v))

(define-syntax-rule (aupd! arr idx ... updater)
  (aset! arr idx ...
         (updater (aref arr idx ...))))

(define (maximum lst)
  (foldl max (car lst) lst))

(define (minimum lst)
  (foldl min (car lst) lst))

(define (product lst)
  (foldl * 1 lst))


