#lang racket

(require "lib.rkt")

(define (read-fs)
  (let ([lines (read-lines)]
        [dirs (make-hash)]
        [pwd '("/")])
    (for ([line lines])
      (match (string-split line)
        [(list "$" "cd" "/") (set! pwd '("/"))]
        [(list "$" "cd" "..") (set! pwd (cdr pwd))]
        [(list "$" "cd" dir) (set! pwd (cons dir pwd))]
        [(list "$" "ls") null]
        [(list "dir" dir) null]
        [(list size file) (for ([i (length pwd)])
                            (let ([prefix (drop pwd i)])
                              (hash-update! dirs prefix
                                            (λ (old) (+ old (string->number size)))
                                            0)))]))
    dirs))

(define (day07)
  (let* ([dirs (read-fs)]
         [dir-sizes (hash-values dirs)])
    (println (~> dir-sizes
                 (filter (λ (s) (<= s #e1e5)) %)
                 (sum %)))

    (let* ([root (maximum dir-sizes)]
           [at-least-free (- #e3e7 (- #e7e7 root))])
      (println (~> dir-sizes
                   (filter (λ (s) (>= s at-least-free)) %)
                   (minimum %))))))

(time (day07))

