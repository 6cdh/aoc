#lang racket

(provide digit-char->integer
         read-lines
         read-lines-as-vector2d
         string->vector
         string-reverse
         product
         debugv
         aref
         aset!
         make-array
         sum
         absdiff
         list->number
         vector->string
         ~>
         vector-update!
         for/min
         minimum
         maximum
         chunks
         read-and-split-in-empty-line
         list-append-head!
         list-push-head!
         for/count
         for*/count
         string-split-in-spaces
         solve-quadratic-equation
         string-find-index
         for/string
         string-replace-char
         counter
         counter-as-list
         sequence<?
         assert!
         vector-dims
         reverse-2d-list
         boolean->number
         repeat
         cachef-hash!
         sublist
         list2d->vector2d
         vector2d->list2d
         first
         second
         third
         fourth)

(define (read-lines [in (current-input-port)])
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
    (displayln (format "~a:" (quote expr)))
    (pretty-display res)
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

(define-syntax aset!
  (syntax-rules ()
    [(_ arr dim new-val)
     (vector-set! arr dim new-val)]
    [(_ arr dim1 dims ... new-val)
     (aset! (vector-ref arr dim1) dims ... new-val)]))

(define-syntax make-array
  (syntax-rules ()
    [(_ n init)
     (make-vector n init)]
    [(_ n args ...)
     (build-vector n (λ _ (make-array args ...)))]))

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

(define (maximum lst)
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
                    "\r\n\r\n"
                    "\n\n")
                #:repeat? #t))

(define-syntax-rule (list-push-head! lst val)
  (set! lst (cons val lst)))

(define-syntax-rule (list-append-head! lst val)
  (set! lst (append val lst)))

(define-syntax-rule
  (for/count iter
    body ...
    lastexpr)
  (for/sum iter
    body ...
    (if lastexpr
        1
        0)))

(define-syntax-rule
  (for*/count iter
    body ...
    lastexpr)
  (for*/sum iter
    body ...
    (if lastexpr
        1
        0)))

(define (string-split-in-spaces str)
  (string-split str " " #:repeat? #t))

(define (solve-quadratic-equation a b c)
  (sort (map (λ (+-)
               (/ (+- (- b)
                      (sqrt (- (* b b) (* 4 a c))))
                  (* 2 a)))
             (list + -))
        <))

(define (string-find-index str char)
  (or (for/first ([(c i) (in-indexed str)]
                  #:when (char=? c char))
        i)
      -1))

(define-syntax-rule
  (for/string iter
    body ...)
  (list->string
   (for/list iter
     body ...)))

(define (string-replace-char str from to)
  (string-replace str (string from) (string to)))

(define (counter seq)
  (for/fold ([h (hash)])
            ([v seq])
    (hash-update h v add1 0)))

(define (counter-as-list seq)
  (hash->list (counter seq)))

(define (sequence<? seq1 seq2 eq? cmp)
  (for/first ([v1 seq1]
              [v2 seq2]
              #:when (not (eq? v1 v2)))
    (< (cmp v1) (cmp v2))))

(define-syntax-rule (assert! expr msg)
  (when (not expr)
    (error (format "assert fail: ~a: ~a" msg (quote expr)))))

(define (vector-dims vec k)
  (if (= k 1)
      (list (vector-length vec))
      (cons (vector-length vec) (vector-dims (aref vec 0) (sub1 k)))))

(define (reverse-2d-list uni)
  (apply map list uni))

(define (boolean->number b)
  (if b 1 0))

(define (repeat lst k)
  (if (= k 1)
      lst
      (append lst (repeat lst (sub1 k)))))

(define-syntax-rule (cachef-hash! fn)
  (set! fn (cachef-hash fn)))

(define (cachef-hash fn)
  (let ([cache (make-hash)]
        [ori-fn fn])
    (λ args
      (when (not (hash-has-key? cache args))
        (hash-set! cache args (apply ori-fn args)))
      (hash-ref cache args))))

(define (sublist lst from len)
  (take (drop lst from) len))

(define (list2d->vector2d lstlst)
  (list->vector (map list->vector lstlst)))

(define (vector2d->list2d vecvec)
  (map vector->list (vector->list vecvec)))

;; fast version of builtin functions
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

