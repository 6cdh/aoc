#lang racket

(require (for-syntax syntax/parse))

(provide read-lines
         list-sum
         $
         ~>
         string->set
         chunks
         vector-update!)

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

(define (list-sum lst)
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

(define (vector-update! vec i updater)
  (vector-set! vec i (updater (vector-ref vec i))))
