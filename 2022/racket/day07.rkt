#lang racket

(require "lib.rkt")

(define (read-fs)
  (let ([lines (read-lines)]
        ;; fs : (Hash string (or Int fs))
        [fs (make-hash (list (cons "/" (make-hash))))]
        [pwd '("/")])
    (for ([line lines])
      (match (string-split line)
        [(list "$" "cd" "/") (set! pwd '("/"))]
        [(list "$" "cd" "..") (set! pwd (cdr pwd))]
        [(list "$" "cd" dir) (set! pwd (cons dir pwd))]
        [(list "$" "ls") (void)]
        [(list "dir" dir) (insert! fs (reverse pwd) dir (make-hash))]
        [(list size file) (insert! fs (reverse pwd) file (string->number size))]))
    fs))

(define (insert! fs pwd name val)
  (if (null? pwd)
      (hash-set! fs name val)
      (insert! (hash-ref fs (car pwd))
               (cdr pwd)
               name val)))

(define (for-each-dir-size fs fn)
  (if (number? fs)
      fs
      (let ([size
             (for/sum ([(_ val) fs])
               (for-each-dir-size val fn))])
        (fn size)
        size)))

(define (day07)
  (let* ([fs (read-fs)]
         [dir-sizes '()])
    (for-each-dir-size fs (λ (s) (set! dir-sizes (cons s dir-sizes))))

    (println (~> dir-sizes
                 (filter (λ (s) (<= s #e1e5)) %)
                 (sum %)))

    (let* ([root (maximum dir-sizes)]
           [at-least-free (- #e3e7 (- #e7e7 root))])
      (println (~> dir-sizes
                   (filter (λ (s) (>= s at-least-free)) %)
                   (minimum %))))))

(time (day07))

